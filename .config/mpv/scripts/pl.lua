json = require "json"
mp.add_forced_key_binding(
    "'",
    "print-playlist",
    function()
        pl = json.decode(mp.get_property("playlist", "[]"))
        for _, v in pairs(pl) do
            print (v["current"] and ">" or " ", v["filename"])
        end
    end
)
