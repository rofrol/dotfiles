import { expect, mock, test } from 'claude-code/testing'
import { completions, pathStart } from '../hooks/register'

const dirs: Record<string, { name: string; kind: 'file' | 'dir' | 'other' }[]> = {
  dir: [
    { name: 'alphabet-one', kind: 'dir' },
    { name: 'alphabet-two', kind: 'dir' },
    { name: 'beta', kind: 'other' },
    { name: '.hidden', kind: 'file' },
    { name: 'my file', kind: 'file' },
    { name: 'my folder', kind: 'dir' },
  ],
  'dir/my folder': [{ name: 'alpha', kind: 'file' }],
  '/home/me': [{ name: 'projects', kind: 'dir' }],
}

/** The engine beneath the plugin: no rows of its own, `dirs` on disk. */
const engine = (on: any) => {
  mock.env(on, { HOME: '/home/me' })
  on('prompt.autocomplete', () => ({ suggestions: [] }))
  on('fs.list', (_$: unknown, e: { path: string }) => {
    // The engine hands the path resolved against the session's directory.
    const key = Object.keys(dirs).find((dir) => e.path === dir || e.path.endsWith('/' + dir))
    if (!key) throw new Error('ENOENT')
    return { value: dirs[key]!.map((entry) => ({ ...entry, size: 0, mtimeMs: 0 })) }
  })
  on('fs.stat', () => ({ value: { kind: 'dir' } }))
}

const engines = new Set<unknown>()

const rows = async ($: any, on: any, text: string) => {
  if (!engines.has(on)) (engines.add(on), engine(on))
  const start = text.lastIndexOf(' ') + 1
  const result = await $.prompt.autocomplete({ text, cursor: text.length, token: text.slice(start), start })
  return result.suggestions.map((row: { text: string }) => row.text)
}

test('completions: sole match, common prefix, then every match', () => {
  expect(completions('alp', ['alphabet-one/', 'alphabet-two/', 'beta/'])).toEqual(['alphabet-'])
  expect(completions('alphabet-', ['alphabet-two/', 'alphabet-one/'])).toEqual(['alphabet-one/', 'alphabet-two/'])
  expect(completions('b', ['alphabet-one/', 'beta/'])).toEqual(['beta/'])
  expect(completions('', ['.git/', 'src/'])).toEqual(['src/'])
  expect(completions('.g', ['.git/', '.github/'])).toEqual(['.git'])
  expect(completions('a', ['a-b/', 'a/'])).toEqual(['a/', 'a-b/'])
})

test('Tab on a partial path offers the common prefix', async ($, on) => {
  expect(await rows($, on, 'ls dir/alp')).toEqual(['dir/alphabet-'])
})

test('a common prefix already typed lists every match', async ($, on) => {
  expect(await rows($, on, 'ls dir/alphabet-')).toEqual(['dir/alphabet-one/', 'dir/alphabet-two/'])
})

test('a symbolic link to a directory completes with a slash', async ($, on) => {
  expect(await rows($, on, 'ls dir/be')).toEqual(['dir/beta/'])
})

test('a name with a space opens a quote', async ($, on) => {
  expect(await rows($, on, 'cat dir/my f')).toEqual([])
  expect(await rows($, on, 'cat dir/my')).toEqual(['"dir/my f'])
  expect(await rows($, on, 'cat "dir/my fi')).toEqual(['file'])
})

test('a path inside an open double quote may hold spaces', async ($, on) => {
  expect(pathStart('ls "a b/c', 9, 5)).toBe(3)
  expect(pathStart('say "hi" to a/b', 15, 12)).toBe(12)
  // The row replaces only the engine's token, `folder/al`.
  expect(await rows($, on, 'ls "dir/my folder/al')).toEqual(['folder/alpha'])
})

test('a common prefix keeps surrogate pairs whole', () => {
  expect(completions('', ['\u{1F600}a', '\u{1F601}b'])).toEqual(['\u{1F600}a', '\u{1F601}b'])
})

test('a tilde expands to HOME for listing but stays in the text', async ($, on) => {
  expect(await rows($, on, 'see ~/pro')).toEqual(['~/projects/'])
})

test('mentions, slash commands, plain words and missing directories add nothing', async ($, on) => {
  expect(await rows($, on, '@dir/alp')).toEqual([])
  expect(await rows($, on, '/mod')).toEqual([])
  expect(await rows($, on, 'hello wor')).toEqual([])
  expect(await rows($, on, 'ls nope/x')).toEqual([])
})
