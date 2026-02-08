;;; treesit-env-recipe-placeholder.el --- Minimal Tree-sitter recipes -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/treesit-env
;; Package-Requires: ((emacs "30.2") (cl-lib "1.0"))
;; Keywords: tree-sitter, tools, maint
;; License: CC0

;;; Commentary:
;; I have no desire to be the lifelong caretaker of a recipe database,
;; which is why this file is named "placeholder".
;; If you find this variable name hideous, or if the recipes don't
;; align with your needs, feel free to ignore this file and provide
;; your own data via `treesit-env-source`.
;;
;; Note that `treesit-env-source` is entirely optional. The `treesit-env`
;; macro itself handles registration, so you can just define everything
;; directly in your init.el if you prefer.
;;
;; I'd rather spend my time writing code than manually maintaining a
;; list of URLs. I've provided `treesit-env-dump-recipes` to make it
;; easy for the community to export and share their own refined recipes.
;;
;; [NOTICE] Please do NOT send me your personal recipes or updates.
;;
;; ---
;; [HOW TO CONFIGURE REVISIONS & ABI COMPATIBILITY]
;;
;; Since Tree-sitter grammars and Emacs are in a transitional period, you have
;; three ways to manage grammar versions (revisions). Choose the one that
;; fits your needs:
;;
;; (Note: You can check your Emacs' supported grammar ABI version by
;; evaluating `(treesit-library-abi-version)`.)
;;
;; 1. EXPLICIT PINNING (Fastest & Most Stable)
;;    Set `:revision "v0.20.1"` in a recipe. This is the fastest because
;;    it skips version searching, and it's the most stable for your config.
;;
;; 2. BRUTE-FORCE AUTO-RESOLUTION (Recommended for Ease)
;;    There are two ways to enable this:
;;    - Global: Set `(setq treesit-env-default-revision-auto t)` in your init.el.
;;    - Local:  Add `:revision auto` to a specific recipe.
;;    This automatically finds the latest version compatible with your
;;    `treesit-env-abi-max` (or Emacs default).
;;
;; 3. LATEST/HEAD (Default behavior, Potentially Unstable)
;;    If you leave `:revision` unspecified and `treesit-env-default-revision-auto`
;;    is nil, the installer always fetches the latest 'master/main' branch.
;;    This MAY CAUSE ERRORS if the grammar's ABI exceeds your Emacs support.
;;; Code:

(require 'treesit-env)

(defconst treesit-env-recipe-placeholder
  (treesit-env-recipes
   ;; --- 1. Basic Setup & Standard Grammars ---
   (css)
   (html)
   (java)
   (python)
   (ruby)
   (toml)
   (json :mode js-json-mode)
   (rust :mode "\\.rs\\'")
   (bash :mode "\\.sh\\'" "\\.bash\\'" "\\.bash_profile\\'" "\\.bashrc\\'"
         :interpreter "bash" "sh")

   ;; --- 2. Advanced Mode Mapping & Aliases ---
   (c-sharp :mode csharp-mode :use csharp-ts-mode)
   (javascript :use js-ts-mode :mode js-mode javascript-mode)
   (yaml :vc grammars)
   (lua :vc grammars :mode "\\.lua\\'" :interpreter "\\<lua\\(?:jit\\)?")
   
   ;; --- 3. Recursive Dependency Resolution ---
   (c :deps cpp)
   (cpp :mode c++-mode :use c++-ts-mode :deps c)
   (go :mode "\\.go\\'")
   (gomod :use go-mod-ts-mode :vc "camdencheek/tree-sitter-go-mod" :mode "/go\\.mod\\'" :deps go)

   ;; --- 4. Custom Layouts & Paths ---
   (typescript :deps tsx :vc "tree-sitter/tree-sitter-typescript"
               :src-path "typescript/src" :mode "\\.ts\\'")
   (tsx :deps typescript :vc "tree-sitter/tree-sitter-typescript"
        :src-path "tsx/src" :mode "\\.tsx\\'")
   (cmake :vc "uyha/tree-sitter-cmake"
          :mode "\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'")
   (dockerfile :vc "camdencheek/tree-sitter-dockerfile"
               :mode "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'")

   ;; --- 5. Complex & High-Maintenance Grammars ---
   (php :src-path "php/src"
        :deps phpdoc html javascript jsdoc css
        :interpreter "php\\(?:-?[34578]\\(?:\\.[0-9]+\\)*\\)?"
        :mode
        "\\.\\(?:php[s345]?\\|phtml\\)\\'"
        "\\.\\(?:php\\|inc\\|stub\\)\\'"
        "/\\.php_cs\\(?:\\.dist\\)?\\'")
   (phpdoc :vc "claytonrcarter/tree-sitter-phpdoc")
   (jsdoc)
   (elixir :vc "elixir-lang/tree-sitter-elixir"
           :mode "\\.elixir\\'" "\\.ex\\'" "\\.exs\\'" "mix\\.lock" :deps heex)
   (heex :vc "phoenixframework/tree-sitter-heex" :mode "\\.[hl]?eex\\'" :deps html))
  "A placeholder list of Tree-sitter recipes.

Register these with `(treesit-env-source treesit-env-recipe-placeholder)`.")

(provide 'treesit-env-recipe-placeholder)

;;; treesit-env-recipe-placeholder.el ends here
