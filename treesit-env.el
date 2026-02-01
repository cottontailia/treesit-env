;;; treesit-env.el --- Environmental manager for tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/treesit-env
;; Version: 0.5.0
;; Package-Requires: ((emacs "30.2") (cl-lib "1.0"))
;; Keywords: tree-sitter, tools, maint
;; License: CC0

;;; Commentary:
;; This package provides an environment manager for Tree-sitter grammars.
;; Inspired by the declarative style of `use-package', it allows users to
;; manage grammar recipes, mode remappings, and on-demand installation
;; in a single, unified setup.
;;
;; Key features:
;; - On-demand installation with recursive dependency resolution.
;; - Brute-force revision discovery from Git history for ABI compatibility.
;; - Centralized management of recipes and major-mode remappings.
;; - Custom compiler support via `treesit-env-compiler-cc' and `c++'.

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'treesit-env-dsl)

(defconst treesit-env-version "0.5.0"
  "Current version of the treesit-env package.")

(defvar treesit--library-not-found-warning-emitted)

(defgroup treesit-env nil
  "Environment manager for Tree-sitter grammars."
  :group 'tools)

(defcustom treesit-env-compiler-cc nil
  "Command for the C compiler.
Can be a string (e.g., \"gcc\") or a list (e.g., (\"zig\" \"cc\" \"-O3\"))."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Command")
                 (repeat :tag "Command + Args" string))
  :group 'treesit-env)

(defcustom treesit-env-compiler-c++ nil
  "Custom C++ compiler command.
Can be a string (e.g., \"g++\") or a list of strings (e.g., (\"zig\" \"c++\"))."
  :type '(choice (const :tag "Default (System)" nil)
                 (string :tag "Command String")
                 (repeat :tag "Command + Args List" string))
  :group 'treesit-env)

(defcustom treesit-env-default-revision-auto nil
  "If non-nil, resolve a compatible revision when NOT specified in the recipe.
This acts as a fallback for recipes that lack a :revision keyword."
  :type 'boolean
  :group 'treesit-env)

(defcustom treesit-env-auto-retry-fetch-step 10
  "Number of commits to deepen in each search step for a compatible revision.
Larger values speed up the search but increase network traffic."
  :type 'integer
  :group 'treesit-env)

(defcustom treesit-env-auto-retry-fetch-limit 10
  "Maximum number of fetch attempts to find a compatible revision.
The search stops if no compatibility is found within this limit."
  :type 'integer
  :group 'treesit-env)

(defcustom treesit-env-abi-max nil
  "If non-nil, override the maximum ABI version allowed by treesit-env.
When nil, the value returned by `treesit-library-abi-version' is used.
Setting this to a lower version (e.g., 14) forces both auto-resolution 
and manual installations to respect this limit."
  :type '(choice (const :tag "Use Emacs default" nil)
                 (integer :tag "Max ABI Version"))
  :group 'treesit-env)

(defvar treesit-env-recipes nil
  "Internal database of registered language recipes.")

(defvar treesit-env--active-recipes nil
  "List of recipes that have been activated in the current session.")

(defvar treesit-env--skipped-languages nil
  "List of languages for which installation was declined by the user.")

(defvar treesit-env--installing-stack nil
  "Stack of languages currently being installed to prevent recursion.")

(defun treesit-env--message (lang-str msg &rest args)
  "Display a formatted message with [treesit-env] [%s] prefix."
  (apply #'message (concat "[treesit-env] [%s] " msg) lang-str args))

(defun treesit-env--warning (lang-str msg &rest args)
  "Display a warning and message with [treesit-env] [%s] prefix."
  (let ((body (apply #'format (concat "[%s] " msg) lang-str args)))
    (display-warning 'treesit-env body :warning)
    (message "[treesit-env] %s" body)))

(defun treesit-env--abort (lang-str msg &rest args)
  "Display an error and abort with [treesit-env] [%s] prefix."
  (let ((body (apply #'format (concat "[%s] " msg) lang-str args)))
    (display-warning 'treesit-env body :error)
    (user-error "[treesit-env] %s" body)))

(defun treesit-env--get-file-abi-from-buffer ()
  "Extract the Tree-sitter ABI version from the current buffer."
  (goto-char (point-min))
  (when (re-search-forward "LANGUAGE_VERSION[ \t]+\\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun treesit-env--get-file-abi (file-path)
  "Extract the Tree-sitter ABI version from FILE-PATH.
Returns nil if the file does not exist or version is not found."
  (when (and file-path (file-exists-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (treesit-env--get-file-abi-from-buffer))))

(defun treesit-env--resolve-compatible-revision (repo-root rel-src lang-str)
  "Search for and checkout a revision compatible with the current Emacs ABI."
  (let* ((abi-max (or treesit-env-abi-max (treesit-library-abi-version)))
         (source-dir (expand-file-name (or rel-src "src") repo-root))
         (default-directory source-dir)
         (git-path (file-relative-name "parser.c" repo-root))
         (current-abi (or (treesit-env--get-file-abi "parser.c") most-positive-fixnum))
         (attempt 0)
         (last-count 0))
    (while (and (> current-abi abi-max) (< attempt treesit-env-auto-retry-fetch-limit))
      (cl-incf attempt)
      (let ((default-directory repo-root))
        (treesit-env--message lang-str "Deepening history... (attempt %d/%d)" 
                              attempt treesit-env-auto-retry-fetch-limit)
        (call-process "git" nil nil nil "fetch" "--quiet" "--deepen" 
                      (number-to-string treesit-env-auto-retry-fetch-step)))
      (with-temp-buffer
        (let ((default-directory repo-root))
          (call-process "git" nil t nil "log" "--format=%H" "--" git-path))
        (let* ((all-commits (split-string (buffer-string) "\n" t))
               (total-count (length all-commits))
               (new-commits (when (> total-count last-count)
                              (cl-subseq all-commits last-count total-count)))
               (checked 0))
          (setq last-count total-count)
          (if (null new-commits)
              (progn
               (treesit-env--message lang-str 
                                     "No new commits found (may have reached repository root)")
               (setq attempt treesit-env-auto-retry-fetch-limit))
            (treesit-env--message lang-str "Checking %d new commits..." (length new-commits))
            (dolist (commit new-commits)
              (unless (<= current-abi abi-max)
                (cl-incf checked)
                (when (zerop (mod checked 5))
                  (treesit-env--message lang-str "Progress: %d/%d new commits" 
                                        checked (length new-commits)))
                (with-temp-buffer
                  (let ((default-directory repo-root))
                    (call-process "git" nil t nil "show" (format "%s:%s" commit git-path)))
                  (setq current-abi (or (treesit-env--get-file-abi-from-buffer) most-positive-fixnum)))
                (when (<= current-abi abi-max)
                  (let ((default-directory repo-root))
                    (treesit-env--message lang-str 
                                          "✓ Found compatible revision %s (ABI %d)" 
                                          commit current-abi)
                    (call-process "git" nil nil nil "checkout" "--quiet" commit)))))))))))

(defmacro treesit-env--with-compiler-hack (&rest body)
  "Temporarily override compiler detection to use `treesit-env` custom compilers."
  `(let ((old-call-process (symbol-function 'call-process))
         (old-exec-find (symbol-function 'executable-find)))
     (cl-letf* ((normalize (lambda (custom)
                             (cond ((listp custom) custom)
                                   ((stringp custom) (split-string-and-unquote custom))
                                   (t (list custom)))))
                (get-base-name (lambda (path)
                                 (downcase (file-name-sans-extension (file-name-nondirectory (or path ""))))))
                ((symbol-function 'executable-find)
                 (lambda (command)
                   (let* ((base (funcall get-base-name command))
                          (custom (cond ((member base '("cc" "gcc" "c99" "clang")) treesit-env-compiler-cc)
                                        ((member base '("c++" "g++" "clang++")) treesit-env-compiler-c++)))
                          (parts (when custom (funcall normalize custom))))
                     (if parts
                         (let ((base-cmd (car parts)))
                           (or (funcall old-exec-find base-cmd) base-cmd))
                       (funcall old-exec-find command)))))
                ((symbol-function 'call-process)
                 (lambda (program &optional infile destination display &rest args)
                   (let* ((base (funcall get-base-name program))
                          (custom (cond ((member base '("cc" "gcc" "c99" "clang")) treesit-env-compiler-cc)
                                        ((member base '("c++" "g++" "clang++")) treesit-env-compiler-c++)))
                          (parts (when custom (funcall normalize custom))))
                     (if parts
                         (let ((real-prog (car parts))
                               (str-args (mapcar (lambda (x) (substring-no-properties (format "%s" x)))
                                                 (append (cdr parts) args))))
                           (when (and (member "-shared" str-args)
                                      (not (cl-some (lambda (a) (string-match-p "\\.\\(o\\|obj\\|c\\|cc\\|cpp\\)$" a)) str-args)))
                             (let ((obj-files (directory-files default-directory nil "\\.\\(o\\|obj\\)\\'")))
                               (when obj-files
                                 (setq str-args (append str-args obj-files)))))
                           (apply old-call-process real-prog infile destination display str-args))
                       (apply old-call-process program infile destination display args))))))
       ,@body)))

(defun treesit-env--buffer-context-p (recipe)
  "Return non-nil if the current buffer context matches RECIPE."
  (let ((file-name (or buffer-file-name ""))
        (triggers (plist-get recipe :triggers))
        (target    (plist-get recipe :target))
        (fallback  (plist-get recipe :fallback))
        (inte-list (plist-get recipe :interpreter)))
    (or (and target    (or (eq major-mode target)   (derived-mode-p target)))
        (and fallback (or (eq major-mode fallback) (derived-mode-p fallback)))
        (and inte-list 
             (let ((current-interp (cl-find-if (lambda (entry) (eq (cdr entry) major-mode)) 
                                               interpreter-mode-alist)))
               (and current-interp (member (car current-interp) inte-list))))
        (let ((modes (cl-remove-if-not #'symbolp triggers))
              (regexps (cl-remove-if-not #'stringp triggers)))
          (or (memq major-mode modes)
              (cl-some (lambda (re) (string-match-p re file-name)) regexps))))))

(defun treesit-env--sync-to-emacs (recipe)
  "Update Emacs mode alists based on the availability of the grammar in RECIPE."
  (let* ((lang (plist-get recipe :lang))
         (lang-str (plist-get recipe :lang-str))
         (target (plist-get recipe :target))
         (fallback (plist-get recipe :fallback))
         (available (treesit-language-available-p lang))
         (triggers (plist-get recipe :triggers))
         (inte-list (plist-get recipe :interpreter)))

    (unless (fboundp fallback)
      (defalias fallback
        (lambda ()
          (interactive)
          (fundamental-mode)
          (setq major-mode fallback)
          (setq mode-name (format "%s-Install" lang-str)))))

    (dolist (trigger triggers)
      (if (stringp trigger)
          (setq auto-mode-alist
                (cons (cons trigger (if available target fallback))
                      (cl-delete trigger auto-mode-alist :key #'car :test #'equal)))
        (setq major-mode-remap-alist
              (cl-delete trigger major-mode-remap-alist :key #'car :test #'eq))
        (when available
          (push (cons trigger target) major-mode-remap-alist))))

    (dolist (inte inte-list)
      (setq interpreter-mode-alist
            (cons (cons inte (if available target fallback))
                  (cl-delete inte interpreter-mode-alist :key #'car :test #'equal))))))

(defun treesit-env--execute-install (recipe)
  "Perform the actual fetch and installation process for RECIPE."
  (let* ((lang (plist-get recipe :lang))
         (lang-str (plist-get recipe :lang-str))
         (deps (plist-get recipe :deps))
         (target (plist-get recipe :target))
         (revision (or (plist-get recipe :revision) (and treesit-env-default-revision-auto 'auto)))
         (rel-src (plist-get recipe :src-path))
         (source-info (assoc lang treesit-language-source-alist))
         (url (nth 1 source-info))
         (target-buf (current-buffer))
         (temp-dir (make-temp-file (concat "treesit-env-work-" lang-str "-") t))
         (repo-root (expand-file-name "repo" temp-dir))
         (source-dir (expand-file-name (or rel-src "src") repo-root))
         (parser-file (expand-file-name "parser.c" source-dir)))
    (unless (memq lang treesit-env--installing-stack)
      (push lang treesit-env--installing-stack)
      (unwind-protect
          (progn
            (dolist (dep deps)
              (unless (treesit-language-available-p dep)
                (let ((dep-recipe (cl-find dep treesit-env--active-recipes :key (lambda (r) (plist-get r :lang)) :test #'eq)))
                  (when (and (not dep-recipe) (assoc dep treesit-env-recipes))
                    (setq dep-recipe (treesit-env--apply-internal dep :activate nil)))
                  (when dep-recipe (treesit-env--execute-install dep-recipe)))))

            (treesit-env--message lang-str "Fetching (%s)..." (or revision "latest"))
            (if (or (null revision) (eq revision 'auto))
                (call-process "git" nil nil nil "clone" "--quiet" "--depth" "1" url repo-root)
              (call-process "git" nil nil nil "clone" "--quiet" "--depth" "1" "--branch" revision url repo-root))

            (unless (file-exists-p parser-file)
              (if (and rel-src (not (string-empty-p rel-src)))
                  (treesit-env--abort lang-str "Source missing: :src-path \"%s\"" rel-src)
                (treesit-env--abort lang-str "Source missing: Try setting :src-path \"src\" etc.")))

            (when (eq revision 'auto)
              (treesit-env--resolve-compatible-revision repo-root rel-src lang-str))

            (let ((abi-max (or treesit-env-abi-max (treesit-library-abi-version)))
                  (abi-got (treesit-env--get-file-abi parser-file)))
              (when (and abi-got (> abi-got abi-max))
                (treesit-env--abort 
                 lang-str 
                 "ABI %d too new (max %d). Try setting :revision \"...\" etc." 
                 abi-got abi-max)))

            (treesit-env--message lang-str "Installing grammar...")
            (let ((treesit-language-source-alist
                   (cons (list lang repo-root nil rel-src)
                         (cl-delete lang treesit-language-source-alist :key #'car))))
              (treesit-env--with-compiler-hack (treesit-install-language-grammar lang)))

            (treesit-env--sync-to-emacs recipe)
            (setq treesit--library-not-found-warning-emitted nil)
            (run-with-idle-timer 0.5 nil
                                 (lambda ()
                                   (when (buffer-live-p target-buf)
                                     (with-current-buffer target-buf
                                       (when (treesit-env--buffer-context-p recipe)
                                         (let ((has-error nil))
                                           (condition-case err-sub
                                               (progn (funcall target) (font-lock-ensure))
                                             (treesit-query-error (setq has-error t))
                                             (error (treesit-env--message lang-str "Mode activation error: %S" err-sub)))
                                           (if has-error
                                               (treesit-env--warning lang-str "Installed! PLEASE RESTART EMACS to fix query mismatch.")
                                             (treesit-env--message lang-str "Installed!")))))))))
        (when (file-exists-p temp-dir) (delete-directory temp-dir t))
        (setq treesit-env--installing-stack (cl-delete lang treesit-env--installing-stack))))))

(cl-defun treesit-env--apply-internal
    (lang &key use vc revision src-path deps mode interpreter activate)
  "Internal function to register a language and apply its configuration.
Returns the generated recipe plist."
  (let* ((lang-str (symbol-name lang))
         (default-recipe (cdr (assoc lang treesit-env-recipes)))
         (final-vc   (or vc (plist-get default-recipe :vc)))
         (final-rev  (or revision (plist-get default-recipe :revision)))
         (final-src  (or src-path (plist-get default-recipe :src-path)))
         (final-use  (or use (plist-get default-recipe :use)))
         (final-deps (or deps (plist-get default-recipe :deps)))
         (final-mode (let ((saved (plist-get default-recipe :mode)))
                       (if (and mode saved)
                           (cl-remove-duplicates (append mode saved) :test #'equal)
                         (or mode saved))))
         (final-inte (let ((saved (plist-get default-recipe :interpreter)))
                       (if (and interpreter saved)
                           (cl-remove-duplicates (append interpreter saved) :test #'equal)
                         (or interpreter saved))))
         (target-ts-mode (or final-use (intern (concat lang-str "-ts-mode"))))
         (fallback-mode (or (car (last (cl-remove-if-not #'symbolp final-mode)))
                            (intern (concat lang-str "-mode"))))
         (final-url (pcase final-vc
                      ('grammars
                       (format "https://github.com/tree-sitter-grammars/tree-sitter-%s" lang-str))
                      ((pred stringp)
                       (if (string-match-p "^http\\|git@" final-vc)
                           final-vc
                         (format "https://github.com/%s" final-vc)))
                      ('nil
                       (format "https://github.com/tree-sitter/tree-sitter-%s" lang-str))
                      (_ (format "https://github.com/%s" final-vc))))
         (all-triggers (or final-mode (list fallback-mode)))
         (recipe (list :lang lang :lang-str lang-str :triggers all-triggers
                       :target target-ts-mode :fallback fallback-mode
                       :use final-use :vc final-vc :revision final-rev :src-path final-src
                       :deps final-deps :mode final-mode :interpreter final-inte)))

    (setq treesit-language-source-alist
          (cons (list lang final-url final-rev final-src)
                (cl-delete lang treesit-language-source-alist :key #'car :test #'eq)))

    (setq treesit-env--active-recipes
          (cons recipe (cl-delete lang treesit-env--active-recipes
                                  :key (lambda (r) (plist-get r :lang)) :test #'eq)))

    (when (treesit-language-available-p lang)
      (treesit-env--sync-to-emacs recipe))
    (when activate
      (unless (treesit-language-available-p lang)
        (add-hook 'find-file-hook #'treesit-env--check-and-install-all)))

    recipe))

(defun treesit-env--parse-args-to-plist (lang-sym args)
  "Parse DSL ARGS for LANG-SYM into a property list."
  (let* ((schema '((:use . single)
                   (:vc . single)
                   (:revision . single)
                   (:src-path . single)
                   (:deps . list)
                   (:mode . list)
                   (:interpreter . list)))
         (p (condition-case err
                (treesit-env-dsl-parse args schema)
              (error
               (let ((args-str (mapconcat #'prin1-to-string args " ")))
                 (error "[treesit-env] Recipe definition error in (%s %s): %s"
                        lang-sym
                        args-str
                        (error-message-string err)))))))
    (list :lang `',lang-sym
          :use (treesit-env-dsl-quote (plist-get p :use))
          :vc (treesit-env-dsl-quote (plist-get p :vc))
          :revision (treesit-env-dsl-quote (plist-get p :revision))
          :src-path (treesit-env-dsl-quote (plist-get p :src-path))
          :deps (treesit-env-dsl-quote-list (plist-get p :deps))
          :mode (treesit-env-dsl-quote-list (plist-get p :mode))
          :interpreter (treesit-env-dsl-quote-list (plist-get p :interpreter)))))

;;;###autoload
(defmacro treesit-env-recipes (&rest entries)
  "Define a list of language recipes using the DSL.

Usage:
  (treesit-env-recipes
    (lua :vc \"name/repo\" :mode \"\\\\.lua\\\\='\")
    (bash :mode \"\\\\.sh\\\\='\" sh-mode :interpreter \"bash\" \"sh\"))

Full set of keywords (per language):
  (lang-sym
    :use target-ts-mode      ; Target ts-mode name
    :vc \"user/repo\"          ; GitHub path, URL, or \\='grammars
    :revision \"v1.0.0\"       ; Git branch, tag, or \"auto\"
    :src-path \"src\"          ; Path to directory containing parser.c
    :deps c cpp              ; Language dependencies (no parens needed)
    :mode \"\\\\.ext\\\\='\" mode     ; File patterns and/or modes to remap
    :interpreter \"sh\" \"py\")  ; Interpreter names (shebang sigils)"
  `(list ,@(mapcar (lambda (entry)
                     (let* ((lang (car entry))
                            (args (cdr entry))
                            (plist (treesit-env--parse-args-to-plist lang args)))
                       `(list ,@plist)))
                   entries)))

;;;###autoload
(defun treesit-env-source (recipes)
  "Register RECIPES into the internal database."
  (dolist (recipe recipes)
    (let ((lang (plist-get recipe :lang)))
      (setf (alist-get lang treesit-env-recipes) recipe))))

;;;###autoload
(defmacro treesit-env (&rest args)
  "Activate languages with optional overrides.

This macro registers languages for the current session. If a base recipe
exists (via `treesit-env-source`), provided keywords will merge with it.

Keyword rules:
- APPEND:   `:mode`, `:interpreter` (Add to base values)
- OVERRIDE: `:use`, `:vc`, `:revision`, `:src-path`, `:deps`

Revision \"auto\":
- If `:revision` is \"auto\", the installer attempts to find a revision
  compatible with the current Emacs ABI version.
- Note: This process may take time due to git fetch retries and does
  NOT guarantee a solution.
- You can tune this via `treesit-env-auto-retry-fetch-step` and
  `treesit-env-auto-retry-fetch-limit`.
- See `treesit-env-default-revision-auto` for a global fallback.

Batch processing:
- Multiple languages can be specified (e.g., lua yaml).
- All provided keywords will be applied to EACH language.
- To use different settings for each language, use separate `treesit-env` calls.

Full set of keywords:
  (treesit-env lang
    :use target-ts-mode      ; Override: target ts-mode name
    :vc \"user/repo\"          ; Override: github path or \\='grammars
    :revision \"auto\"         ; Override: \"auto\" or specific rev
    :src-path \"src\"          ; Override: relative path to parser.c
    :deps c cpp              ; Override: dependencies (no parens needed)
    :mode \"\\\\.ext\\\\='\" mode     ; Append: additional patterns/modes
    :interpreter \"sh\" \"py\")  ; Append: additional shebang sigils"
    (declare (indent 0))
  (let (langs body)
    (while (and args (not (keywordp (car args))))
      (let ((arg (pop args)))
        (if (listp arg)
            (setq langs (append langs arg))
          (push arg langs))))
    (setq langs (nreverse langs)
          body args)
    `(progn
       ,@(mapcar
          (lambda (lang)
            (let ((p (treesit-env--parse-args-to-plist lang body)))
              `(let* ((lang-sym ',lang)
                      (local-recipe (list ,@p))
                      (base-recipe  (cdr (assoc lang-sym treesit-env-recipes))))
                 (treesit-env--apply-internal
                  lang-sym
                  :use (or (plist-get local-recipe :use) (plist-get base-recipe :use))
                  :vc (or (plist-get local-recipe :vc) (plist-get base-recipe :vc))
                  :revision (or (plist-get local-recipe :revision) (plist-get base-recipe :revision))
                  :src-path (or (plist-get local-recipe :src-path) (plist-get base-recipe :src-path))
                  :deps (or (plist-get local-recipe :deps) (plist-get base-recipe :deps))
                  :mode (or (plist-get local-recipe :mode) (plist-get base-recipe :mode))
                  :interpreter (or (plist-get local-recipe :interpreter) (plist-get base-recipe :interpreter))
                  :activate t))))
          langs))))

(defun treesit-env--check-and-install-all ()
  "Check all activated recipes and prompt to install missing grammars."
  (dolist (recipe treesit-env--active-recipes)
    (let ((lang (plist-get recipe :lang))
          (lang-str (plist-get recipe :lang-str)))
      (when (and (not (treesit-language-available-p lang))
                 (not (memq lang treesit-env--skipped-languages))
                 (treesit-env--buffer-context-p recipe))
        (if (y-or-n-p (format "%s grammar is missing. Install? " lang-str))
            (treesit-env--execute-install recipe)
          (push lang treesit-env--skipped-languages))))))

;;;###autoload
(defun treesit-env-install-all ()
  "Install all activated grammars that are not yet available on the system."
  (interactive)
  (let ((langs (cl-remove-if #'treesit-language-available-p
                             (mapcar (lambda (r) (plist-get r :lang)) 
                                     treesit-env--active-recipes))))
    (cond
     ((null langs)
      (message "[treesit-env] No missing grammars to install."))
     ((y-or-n-p (format "Install %d missing grammars? This may take some time... " (length langs)))
      (dolist (lang langs)
        (let ((recipe (cl-find lang treesit-env--active-recipes 
                               :key (lambda (r) (plist-get r :lang)) :test #'eq)))
          (when recipe
            (treesit-env--execute-install recipe))))
      (message "[treesit-env] Finished checking/installing all languages."))
     (t
      (message "[treesit-env] Installation cancelled.")))))

;;;###autoload
(defun treesit-env-reinstall (lang-sym)
  "Force a reinstallation of the activated grammar for LANG-SYM."
  (interactive
   (list (intern (completing-read
                  "Reinstall grammar: "
                  (mapcar (lambda (r) (symbol-name (plist-get r :lang)))
                          treesit-env--active-recipes)))))
  (let ((recipe (cl-find lang-sym treesit-env--active-recipes
                         :key (lambda (r) (plist-get r :lang)) :test #'eq)))
    (when (and recipe (y-or-n-p (format "Force reinstall %s grammar? " lang-sym)))
      (let* ((lang-str (symbol-name lang-sym))
             (ext (or module-file-suffix
                      (pcase system-type
                        ('darwin ".dylib") ('windows-nt ".dll") (_ ".so"))))
             (lib-file (expand-file-name
                        (concat "libtree-sitter-" lang-str ext)
                        (expand-file-name "tree-sitter" user-emacs-directory))))
        (when (file-exists-p lib-file) (delete-file lib-file))
        (treesit-env--execute-install recipe)))))

;;;###autoload
(defun treesit-env-reset-skips ()
  "Clear the record of skipped installations for the current session."
  (interactive)
  (setq treesit-env--skipped-languages nil)
  (message "[treesit-env] Reset all skip flags."))

;;;###autoload
(defun treesit-env-all ()
  "Activate all languages registered in `treesit-env-recipes'."
  (interactive)
  (dolist (recipe treesit-env-recipes)
    (treesit-env--apply-internal (car recipe) :activate t)))

;;;###autoload
(defun treesit-env-dump-recipes ()
  "Generate and display a `treesit-env-recipes` block for the active config."
  (interactive)
  (let ((dump-buffer "*treesit-env-dump*")
        (recipes (reverse treesit-env--active-recipes)))
    (with-current-buffer (get-buffer-create dump-buffer)
      (erase-buffer)
      (lisp-data-mode)
      (insert "(treesit-env-recipes\n")
      (dolist (recipe recipes)
        (let* ((lang (plist-get recipe :lang))
               (use  (plist-get recipe :use))
               (vc   (plist-get recipe :vc))
               (rev  (plist-get recipe :revision))
               (src  (plist-get recipe :src-path))
               (deps (plist-get recipe :deps))
               (mode (plist-get recipe :mode))
               (inte (plist-get recipe :interpreter))
               (line (list lang)))
          (when use (setq line (append line (list :use use))))
          (when vc (setq line (append line (list :vc vc))))
          (when rev (setq line (append line (list :revision rev))))
          (when src (setq line (append line (list :src-path src))))
          (when deps (setq line (append line (cons :deps (if (listp deps) deps (list deps))))))
          (when mode (setq line (append line (cons :mode (if (listp mode) mode (list mode))))))
          (when inte (setq line (append line (cons :interpreter (if (listp inte) inte (list inte))))))
          (insert (format "  %s\n" (prin1-to-string line)))))
      (insert ")")
      (indent-region (point-min) (point-max))
      (display-buffer (current-buffer)))
    (message "[treesit-env] Configuration dumped to %s" dump-buffer)))

(provide 'treesit-env)

;;; treesit-env.el ends here
