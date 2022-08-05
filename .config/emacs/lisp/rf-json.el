;;; -*- lexical-binding: t; -*-

(rf/require 'json-mode)

;; How to tell `prettier` to use json parser instead of default javascript parser for package.json?
;; M-: (prettier--parsers) RET? Should be (json-stringify json). It is (json-stringify json json5)
;; You're using js-mode for your package.json. Use json-mode and it should work.
;; I know this isn't ideal, really it should be using the same parser that Prettier CLI uses regardless of mode.
;; I'm planning to change that. Hope you can work around it for now using json-mode.
;; https://github.com/jscheid/prettier.el/issues/101#issuecomment-1134520701)

;; not needed
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;; tsconfig.json is not a json5 file, it is a jsonc file
;; https://github.com/microsoft/TypeScript-Website/issues/1019
;; There is also jsonc aka "JSON with comments", created by Microsoft and used by Visual Studio Code
;; https://stackoverflow.com/questions/14851903/what-is-jsonc-are-jsonc-and-json-c-different
;; https://stackoverflow.com/questions/66428689/trying-to-load-tsconfig-as-a-json-object
;; https://www.typescriptlang.org/docs/handbook/tsconfig-json.html
;; https://stackoverflow.com/questions/61996234/requiring-a-json-with-comments-in-node-js
;; Allow comments in tsconfig.json https://github.com/microsoft/TypeScript/issues/4987
;; https://www.typescriptlang.org/tsconfig
;; require('typescript').parseConfigFileTextToJson() exposed TypeScript's config parser for any third-party libraries who care to use it. As far as I've seen, it's understood that tsconfig files usually contain comments.
;; https://www.reddit.com/r/typescript/comments/8na5vb/comment/dzus6a9/

;; using python https://stackoverflow.com/questions/435847/emacs-mode-to-edit-json/7934783#7934783

(provide 'rf-json)