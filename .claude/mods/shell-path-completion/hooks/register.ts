import type { PromptAutocompleteSuggestion, Register } from 'claude-code'

/** Matches shown at most when no common prefix can be inserted. */
const MAX_ROWS = 200

/**
 * The longest common prefix of `names`.
 */
function commonPrefix(names: readonly string[]): string {
  // By code points, so a cut never splits a surrogate pair.
  let prefix = Array.from(names[0] ?? '')
  for (const name of names) while (!name.startsWith(prefix.join(''))) prefix.pop()
  return prefix.join('')
}

/**
 * Where the path at the cursor begins: at a double quote still open before
 * the cursor, so a quoted path may hold spaces, else at the token. Single
 * quotes count only at the token's start, as prose is full of apostrophes.
 */
export function pathStart(text: string, cursor: number, tokenStart: number): number {
  let open = -1
  for (let i = 0; i < cursor; i++) if (text[i] === '"') open = open < 0 ? i : -1
  return open >= 0 && open < tokenStart ? open : tokenStart
}

/**
 * What Tab should offer for `part`, the last component of a path, given the
 * directory's entries (directories end with `/`): the sole match, else the
 * matches' common prefix when it is longer than `part`, else every match.
 * Hidden entries match only a `part` that starts with a dot.
 */
export function completions(part: string, names: readonly string[]): string[] {
  const matches = names
    .filter((name) => name.startsWith(part) && (part.startsWith('.') || !name.startsWith('.')))
    // Sort by name alone, as a shell does: `a/` before `a-b/`.
    .sort((a, b) => (a.replace(/\/$/, '') < b.replace(/\/$/, '') ? -1 : 1))
  if (matches.length <= 1) return matches
  const prefix = commonPrefix(matches)
  return prefix.length > part.length ? [prefix] : matches.slice(0, MAX_ROWS)
}

/**
 * Shell-style path completion for the typeahead. Claude Code shows these rows
 * beneath its own and Tab takes the first, so offering only the common prefix
 * makes Tab extend the path the way a shell does; once no longer prefix is
 * shared, the rows list every match. `@` mentions, slash commands and `!`
 * shell mode stay the engine's: the engine does not ask plugins there.
 */
export const register: Register = (on) => {
  on('prompt.autocomplete', async ($, e, next) => {
    const result = await next(e)
    const start = pathStart(e.text, e.cursor, e.start)
    const token = e.text.slice(start, e.cursor)
    const quote = token[0] === '"' || token[0] === "'" ? token[0] : ''
    const path = token.slice(quote.length)
    const slash = path.lastIndexOf('/')
    // A path needs a slash; a lone leading one at the start is a slash command.
    if (token.startsWith('@') || slash < 0 || (start === 0 && slash === 0)) return result

    const dir = path.slice(0, slash + 1)
    const part = path.slice(slash + 1)
    let listed = dir
    if (dir.startsWith('~/')) {
      const home = await $.env.get('HOME')
      if (!home) return result
      listed = home + dir.slice(1)
    }

    let names: string[]
    try {
      const entries = await $.fs.list(listed)
      names = await Promise.all(
        entries
          .filter((entry) => entry.name.startsWith(part))
          .map(async (entry) => {
            let kind = entry.kind
            // A symbolic link completes as what it leads to.
            if (kind === 'other') kind = await $.fs.stat(listed + entry.name).then((s) => s.kind, () => 'other' as const)
            return kind === 'dir' ? entry.name + '/' : entry.name
          }),
      )
    } catch {
      return result
    }

    const rows: PromptAutocompleteSuggestion[] = completions(part, names)
      .filter((name) => name !== part)
      .map((name) => {
        const path = dir + name
        const text = quote || !/\s/.test(path) ? quote + path : '"' + path
        // Taking a row replaces the engine's token, which starts after any
        // space inside the quotes.
        return { text: text.slice(e.start - start) }
      })
    // With several matches and no longer prefix, Tab must not pick one: it
    // takes the first row, so that row keeps the token as typed. The engine
    // then closes the menu; arrows choose a match before Tab.
    if (rows.length > 1) {
      rows.unshift({ text: e.token, label: `${e.token}  (${rows.length} matches, ↓ to choose)` })
    }
    return rows.length ? { suggestions: [...result.suggestions, ...rows] } : result
  })
}
