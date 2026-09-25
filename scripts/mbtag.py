"""Identify YouTube-ripped tracks on MusicBrainz and write clean ID3 tags.

Evidence per track (strongest first):
  - MusicBrainz URL relationship to the YouTube video (exact),
  - "Provided to YouTube by" block in the description (official Topic uploads),
  - AcoustID fingerprint (fpcalc),
  - ListenBrainz metadata lookup on artist/title parsed from the video title.
decide() ranks candidate recordings, resolve() turns the best one into canonical MB names.
"""
import json, os, pathlib, re, subprocess, time, unicodedata, urllib.error, urllib.parse, urllib.request
from difflib import SequenceMatcher

UA = "rofrol-ytmb/0.1 ( rofrol@gmail.com )"
ACOUSTID_KEY = "1vOwZtEn"  # public client key embedded in beets' chroma plugin
LB_CONFIGS = [pathlib.Path.home() / "Library/Application Support/listenbrainz-mpd/config.toml",  # macOS
              pathlib.Path(os.environ.get("XDG_CONFIG_HOME", pathlib.Path.home() / ".config")) / "listenbrainz-mpd/config.toml"]
CACHE = pathlib.Path(os.environ.get("XDG_CACHE_HOME", pathlib.Path.home() / ".cache")) / "ytmb"
_last = {}


def lb_token():
    """ListenBrainz token (optional, only improves matching): $LISTENBRAINZ_TOKEN or the listenbrainz-mpd config."""
    if os.environ.get("LISTENBRAINZ_TOKEN"):
        return os.environ["LISTENBRAINZ_TOKEN"]
    for cfg in LB_CONFIGS:
        m = re.search(r'^token\s*=\s*"([^"]+)"', cfg.read_text(), re.M) if cfg.exists() else None
        if m:
            return m.group(1)
    return None


def http(url, data=None, host_interval=1.1, headers=None):
    """GET/POST JSON with a per-host rate limit; returns None on 404."""
    host = urllib.parse.urlparse(url).netloc
    wait = _last.get(host, 0) + host_interval - time.time()
    if wait > 0:
        time.sleep(wait)
    h = {"User-Agent": UA, "Accept": "application/json"}
    h.update(headers or {})
    for attempt in range(5):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, data=data, headers=h), timeout=30) as r:
                _last[host] = time.time()
                return json.load(r)
        except urllib.error.HTTPError as e:
            _last[host] = time.time()
            if e.code == 404:
                return None
            if e.code in (429, 502, 503):
                time.sleep(3 * (attempt + 1))
                continue
            raise
        except (urllib.error.URLError, TimeoutError):
            time.sleep(3 * (attempt + 1))
    return None


# ---------------------------------------------------------------- text parsing

def fix(s):
    s = unicodedata.normalize("NFKC", s or "")
    for a, b in {"⧸": "/", "’": "'", "‘": "'", "“": '"', "”": '"'}.items():
        s = s.replace(a, b)
    return s


NOISE = re.compile(
    r"\s*[\(\[][^\)\]]*(official|video|audio|lyric|lyrics|hd|4k|visualizer|clip|oficial|teledysk|remaster(ed)? \d{4}?|explicit|tiktok)[^\)\]]*[\)\]]"
    r"|\s*\|\s*(official|audio oficial|lyrics).*$"
    r"|\s+(official (music )?video|hd|4k)\s*$",
    re.I,
)


def parse_title(channel, title):
    """(artist, title) candidates from a YouTube title; also the cleaned title and channel."""
    t = re.sub(r"\s+", " ", fix(title).replace("_", " ")).strip()
    clean = NOISE.sub("", t).strip(" -|")
    ch = fix(channel).replace("_", " ").strip()
    ch = re.sub(r"\s*-\s*Topic$|VEVO$|\s+Official$|TV$", "", ch).strip()
    cands = []
    m = re.split(r"\s+[-–—]\s+", clean, maxsplit=1)
    if len(m) == 2:
        a, b = m[0].strip(), m[1].strip()
        f = re.search(r"\s*[\(\[]?\b(ft\.?|feat\.?|featuring)\s+([^\)\]]+)[\)\]]?", b, re.I)
        b2 = (b[: f.start()] + b[f.end():]).strip() if f else b
        cands.append((a, b2))
        cands.append((b2, a))  # reversed "Title - Artist"
    else:
        m = re.split(r"\s*[|:]\s+", clean, maxsplit=1)
        if len(m) == 2:
            cands.append((m[0].strip(), m[1].strip()))
        cands.append((ch, clean))
    return cands, clean, ch


def provided_block(desc):
    m = re.search(r"Provided to YouTube by [^\n]*\n\n([^\n]+)\n\n([^\n]+)", desc or "")
    if not m:
        return None
    parts = [p.strip() for p in m.group(1).split(" · ")]
    return {"title": parts[0], "artists": parts[1:], "album": m.group(2).strip()}


# ---------------------------------------------------------------- lookups

def mb_url(ytid):
    for u in (f"https://www.youtube.com/watch?v={ytid}", f"http://www.youtube.com/watch?v={ytid}"):
        r = http("https://musicbrainz.org/ws/2/url?" + urllib.parse.urlencode({"resource": u, "inc": "recording-rels release-rels", "fmt": "json"}))
        if r:
            return [{"type": rel.get("type"), "recording": rel.get("recording", {}).get("id"),
                     "title": rel.get("recording", {}).get("title"), "release": rel.get("release", {}).get("id")}
                    for rel in r.get("relations", [])]
    return []


def acoustid(path):
    try:
        out = subprocess.run(["fpcalc", "-json", "-length", "120", str(path)], capture_output=True, text=True, timeout=120)
        fp = json.loads(out.stdout)
    except Exception as e:
        return {"error": str(e)}
    data = urllib.parse.urlencode({"client": ACOUSTID_KEY, "meta": "recordings", "duration": int(fp["duration"]),
                                   "fingerprint": fp["fingerprint"]}).encode()
    r = http("https://api.acoustid.org/v2/lookup", data=data, host_interval=0.4,
             headers={"Content-Type": "application/x-www-form-urlencoded"})
    res = []
    for x in (r or {}).get("results", []):
        for rec in x.get("recordings", []) or []:
            if rec.get("title"):
                res.append({"score": x["score"], "mbid": rec["id"], "title": rec["title"], "duration": rec.get("duration"),
                            "artists": [a["name"] for a in rec.get("artists", [])]})
    return {"results": res[:8], "status": (r or {}).get("status")}


def lb_lookup(artist, title, token):
    if not token:
        return None
    r = http("https://api.listenbrainz.org/1/metadata/lookup/?" + urllib.parse.urlencode({"artist_name": artist, "recording_name": title}),
             host_interval=0.3, headers={"Authorization": "Token " + token})
    return r if r and r.get("recording_mbid") else None


def mb_recording(mbid):
    CACHE.mkdir(exist_ok=True)
    f = CACHE / f"rec-{mbid}.json"
    if f.exists():
        return json.loads(f.read_text())
    r = http(f"https://musicbrainz.org/ws/2/recording/{mbid}?inc=artist-credits&fmt=json")
    if r is not None:
        f.write_text(json.dumps(r, ensure_ascii=False))
    return r


def collect(path, ytid, channel, title, desc, duration, yt_artist=None, yt_track=None):
    """Gather all evidence for one file into a dict (JSON-serialisable)."""
    cands, clean, ch = parse_title(channel, title)
    if yt_artist and yt_track:  # yt-dlp's own music metadata (YouTube Music / Topic)
        cands.insert(0, (yt_artist, yt_track))
    d = {"ytid": ytid, "channel": channel, "yt_title": title, "clean": clean, "chan_clean": ch, "duration": duration,
         "desc": (desc or "")[:3000], "provided": provided_block(desc), "cands": cands}
    d["mb_url"] = mb_url(ytid)
    d["acoustid"] = acoustid(path) if duration < 900 else {"skipped": "long"}
    token = lb_token()
    q = [(", ".join(d["provided"]["artists"]), d["provided"]["title"])] if d["provided"] else []
    d["lb"] = [{"q": [a, t], "r": lb_lookup(a, t, token)} for a, t in (q + cands[:3]) if a and t]
    return d


# ---------------------------------------------------------------- scoring

VERSION_WORDS = re.compile(r"\b(remix|mix|edit|live|acoustic|cover|instrumental|extended|stripped|version|dub|karaoke|sped|slowed|mashup)\b", re.I)


def norm(s):
    s = unicodedata.normalize("NFKD", fix(s).lower())
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = re.sub(r"\b(the|feat|ft|featuring|and|i)\b|&", " ", s)
    return re.sub(r"[^a-z0-9а-я]+", " ", s).strip()


def sim(a, b):
    a, b = norm(a), norm(b)
    if not a or not b:
        return 0.0
    if a == b:
        return 1.0
    r = SequenceMatcher(None, a, b).ratio()
    if a in b or b in a:  # "Turn The Lights Off" vs "Turn the Lights Off (radio edit)"
        r = max(r, 0.85 * min(len(a), len(b)) / max(len(a), len(b)) + 0.15)
    return r


def versions(s):
    return {w.lower() for w in VERSION_WORDS.findall(s or "")}


def score_candidate(d, title, artists):
    """How well an MB (title, artists) matches what the YouTube side says (0..1)."""
    artists = artists if isinstance(artists, list) else [artists]
    pairs = list(d["cands"])
    if d["provided"]:
        pairs.insert(0, (", ".join(d["provided"]["artists"]), d["provided"]["title"]))
    best = 0.0
    for a, t in pairs:
        # artist may be one part of "A, B & C" -> compare against each
        as_ = max([sim(a, " ".join(artists))] + [sim(x, a) for x in artists] +
                  [1.0 if norm(x) and norm(x) in norm(a) else 0 for x in artists])
        s = 0.6 * sim(t, title) + 0.4 * as_
        s -= 0.15 * len(versions(title) ^ versions(d["clean"]))  # remix vs original etc.
        best = max(best, s)
    return best


def decide(d):
    """Ranked [(score, mbid, {methods})], best first."""
    cands = []
    for rel in d["mb_url"]:
        if rel.get("recording"):
            cands.append((1.5, "mb-url", rel["recording"]))
    for x in d["acoustid"].get("results", []):
        s = score_candidate(d, x["title"], x["artists"])
        if x.get("duration") and abs(x["duration"] - d["duration"]) > 40:
            s -= 0.1
        cands.append((s + 0.3 * (x["score"] - 0.5), "acoustid", x["mbid"]))
    for x in d["lb"]:
        if x["r"]:
            cands.append((score_candidate(d, x["r"]["recording_name"], x["r"]["artist_credit_name"]) + 0.05, "lb-lookup", x["r"]["recording_mbid"]))
    by = {}
    for s, m, mbid in cands:
        e = by.setdefault(mbid, [0, set()])
        e[0] = max(e[0], s)
        e[1].add(m)
    # boost MBIDs supported by several sources
    return sorted(((s + 0.1 * (len(ms) - 1), mbid, ms) for mbid, (s, ms) in by.items()), key=lambda x: -x[0])


def from_recording(d, rec):
    ac = rec.get("artist-credit", [])
    return {"mbid": rec["id"], "title": rec["title"],  # MB may redirect merged recordings -> returned id
            "artist": "".join(a["name"] + a.get("joinphrase", "") for a in ac),
            "artist_mbids": [a["artist"]["id"] for a in ac],
            "mb_length": round((rec.get("length") or 0) / 1000),
            "check": round(score_candidate(d, rec["title"], [a["name"] for a in ac]), 2)}


def mb_search(artist, title):
    CACHE.mkdir(exist_ok=True)
    q = f'artist:"{artist}" AND recording:"{title}"'
    f = CACHE / ("search-" + re.sub(r"[^\w-]", "_", q)[:150] + ".json")
    if f.exists():
        return json.loads(f.read_text())
    r = http("https://musicbrainz.org/ws/2/recording?" + urllib.parse.urlencode({"query": q, "limit": 10, "fmt": "json"})) or {}
    f.write_text(json.dumps(r, ensure_ascii=False))
    return r


def mb_search_fallback(d):
    """Direct MB recording search for parsed "Artist - Title" pairs; needs a close name + duration match."""
    if versions(d["clean"]):  # remixes/covers/live: a stripped-title search picks the wrong version
        return None
    for a, t in d["cands"][:2]:
        a = re.sub(r"\s+(and|x|vs\.?)\s+", " & ", a, flags=re.I)
        t = re.sub(r"\s*[\(\[].*?[\)\]]", "", t).strip()
        if not a or not t or len(t) > 60:
            continue
        for rec in mb_search(a, t).get("recordings", []):
            if rec.get("score", 0) < 90 or not rec.get("length"):
                continue
            if abs(rec["length"] / 1000 - d["duration"]) > 30:
                continue
            row = from_recording(d, rec)
            if row["check"] >= 0.85 and not re.search(r"\b(clip|video|MV)\b", rec["title"], re.I):
                return row
    return None


def resolve(d):
    """Proposal dict with status auto | review | nomatch."""
    ranked = decide(d)
    row = {"ytid": d["ytid"], "channel": d["channel"], "yt_title": d["yt_title"], "duration": round(d["duration"]),
           "mbid": "", "artist": "", "title": "", "artist_mbids": [], "score": 0, "runner_up": 0, "method": "", "status": "",
           "check": 0, "alternatives": []}
    if ranked:
        s, mbid, ms = ranked[0]
        row.update(score=round(s, 2), method="+".join(sorted(ms)),
                   runner_up=round(next((x[0] for x in ranked[1:] if x[1] != mbid), 0), 2))
        row["alternatives"] = [{"score": round(x[0], 2), "mbid": x[1], "method": "+".join(sorted(x[2]))} for x in ranked[1:4]]
        if s >= 0.75:
            rec = mb_recording(mbid)
            if rec:
                row.update(from_recording(d, rec))
    video = re.compile(r"\s*[\(\[][^\(\[]*\b(clip|video|videoclip|MV)\b[^\)\]]*[\)\]]\s*$", re.I)
    if row["mbid"] and video.search(row["title"]):
        # MB "video" recording (e.g. from the YouTube URL rel) -> prefer the audio recording of the same song
        base = video.sub("", row["title"])
        for alt in row["alternatives"]:
            rec = mb_recording(alt["mbid"])
            if rec and not video.search(rec["title"]) and sim(base, rec["title"]) >= 0.9:
                alt_row = from_recording(d, rec)
                if sim(alt_row["artist"], row["artist"]) >= 0.8:
                    row.update(alt_row)
                    row["method"] += ">audio"
                    break
    if row["mbid"] and d["provided"]:
        # official "Topic" upload: its artist list is authoritative
        pa = d["provided"]["artists"]
        row["check"] = min(row["check"], round(max(sim(", ".join(pa), row["artist"]), sim(pa[0], row["artist"])), 2))
    if row["mbid"] and re.search(r"\b(clip|video|videoclip|MV)\b", row["title"], re.I):
        row["check"] = min(row["check"], 0.7)  # MB "video" recording, prefer the audio one
    if row["mbid"] and (row["score"] >= 1.0 or "mb-url" in row["method"]) and row["check"] >= 0.8:
        row["status"] = "auto"
    elif row["mbid"] and row["check"] >= 0.6:
        row["status"] = "review"
    elif (hit := mb_search_fallback(d)):
        row.update(hit, status="review", method="mb-search")
    else:
        row.update(status="nomatch", mbid="", artist_mbids=[])
        # fallback: names only, no MBID
        if d["provided"]:
            row["artist"], row["title"] = ", ".join(d["provided"]["artists"]), d["provided"]["title"]
        else:
            row["artist"], row["title"] = d["cands"][0]
    return row


# ---------------------------------------------------------------- tags

def write_tags(path, row, album=None):
    """Set TPE1/TIT2 (+ MusicBrainz IDs) keeping the YouTube channel in TXXX:YouTube Channel."""
    from mutagen.id3 import ID3, TPE1, TIT2, TALB, UFID, TXXX
    t = ID3(path)
    old = t.get("TPE1")
    if old and not t.get("TXXX:YouTube Channel"):
        t.add(TXXX(encoding=3, desc="YouTube Channel", text=list(old.text)))
    t.delall("TPE1"); t.add(TPE1(encoding=3, text=[row["artist"]]))
    t.delall("TIT2"); t.add(TIT2(encoding=3, text=[row["title"]]))
    if album:
        t.delall("TALB"); t.add(TALB(encoding=3, text=[album]))
    t.delall("UFID:http://musicbrainz.org"); t.delall("TXXX:MusicBrainz Artist Id")
    if row.get("mbid"):
        # MPD maps this UFID to MUSICBRAINZ_TRACKID; listenbrainz-mpd sends it as recording_mbid
        t.add(UFID(owner="http://musicbrainz.org", data=row["mbid"].encode("ascii")))
        t.add(TXXX(encoding=3, desc="MusicBrainz Artist Id", text=row["artist_mbids"]))
    t.save(path)
