;;; treesit-env-dsl.el --- DSL parsing engine for treesit-env -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Cottontailia

;; Author: Cottontailia
;; Additional-Author: AI Assistant
;; URL: https://github.com/cottontailia/treesit-env
;; Keywords: lisp, dsl, parsing
;; Package-Requires: ((emacs "29.1") (cl-lib "1.0"))
;; License: CC0

;;; Commentary:

;; Lightweight DSL parsing engine for `treesit-env'.
;; It parses argument lists and transforms them into structured plists
;; based on a predefined schema (single, list, or flag).
;;
;; Supported Schema Types:
;; - `single': Exactly one value (required).
;; - `list'  : One or more values (required).
;; - `flag'  : Optional flag. Uses the next non-keyword value if present,
;;             otherwise defaults to `t'.
;; - `:_head': Defines required initial values at the start of the list.
;;
;; This library also provides helpers like `treesit-env-dsl-quote'
;; to ensure safe symbol handling during macro expansion.

;;; Code:

(require 'cl-lib)

(defun treesit-env-dsl-parse (args schema)
  "Parse ARGS according to SCHEMA and return a plist.

- `single': Requires exactly one value.
- `list':   Requires at least one value.
- `flag':   Optional; consumes next non-keyword if present.
            (0 or nil is false, otherwise true).
- `:_head': If in SCHEMA, ARGS must start with at least one value."
  (let* ((head-config (assq :_head schema))
         (result nil)
         (list-keys nil)
         (current-key (and head-config :_head))
         (current-type (and head-config (or (cdr head-config) 'list))))
    (when (eq current-type 'list)
      (push :_head list-keys))
    (while args
      (let* ((item (pop args))
             (entry (and (keywordp item) (assq item schema))))
        (cond
         (entry
          (when current-key
            (cond
             ((and (eq current-type 'single) (not (plist-member result current-key)))
              (error "Keyword [%s] expects a value" current-key))
             ((and (eq current-type 'list) (not (plist-get result current-key)))
              (error "Keyword [%s] expects at least one value"
                     (if (eq current-key :_head) "Initial position" current-key)))))
          (let ((type (cdr entry)))
            (if (eq type 'flag)
                (let ((val (if (and args (not (keywordp (car args))))
                               (pop args)
                             t)))
                  (setq result (plist-put result item
                                          (and val (not (and (numberp val) (<= val 0)))))
                        current-key nil
                        current-type nil))
              (setq current-key item
                    current-type type)
              (when (eq type 'list)
                (cl-pushnew item list-keys)))))
         ((keywordp item)
          (error "Unknown keyword [%s]. Allowed: %s" item (mapcar #'car schema)))
         (t
          (if (null current-key)
              (error "Unexpected value '%s'. Initial arguments are not allowed here" item)
            (if (eq current-type 'single)
                (setq result (plist-put result current-key item)
                      current-key nil)
              (setq result (plist-put result current-key
                                      (cons item (plist-get result current-key))))))))))
    (when current-key
      (cond
       ((and (eq current-type 'single) (not (plist-member result current-key)))
        (error "Keyword [%s] expects a value but reached end" current-key))
       ((and (eq current-type 'list) (not (plist-get result current-key)))
        (error "Keyword [%s] expects at least one value but reached end"
               (if (eq current-key :_head) "Initial position" current-key)))))
    (dolist (key list-keys)
      (setq result (plist-put result key (nreverse (plist-get result key)))))
    result))

(defun treesit-env-dsl-quote (val)
  "Handle quoting for VAL during macro expansion with unquote support."
  (cond
   ((and (listp val) (eq (car val) '\,)) (cadr val))
   ((or (keywordp val) (numberp val) (stringp val) (booleanp val) (null val) (eq val t)) val)
   (t `',val)))

(defun treesit-env-dsl-quote-list (lst)
  "Apply `treesit-env-dsl-quote' to all elements in LST.
The return value is a `(list ...)` form for macro expansion."
  (if (null lst) nil `(list ,@(mapcar #'treesit-env-dsl-quote lst))))

;;; Debugging Utility

(defmacro treesit-env-dsl-debug (schema &rest args)
  "Parse ARGS with SCHEMA and show the result in a dedicated buffer."
  (let ((parsed (treesit-env-dsl-parse args schema)))
    `(let ((buf (get-buffer-create "*treesit-env-dsl-debug*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (emacs-lisp-mode)
           (insert ";; Resulting Property List (plist)\n")
           (insert ";; Optimized for DSL macro expansion.\n\n")
           (pp ',parsed (current-buffer))
           (goto-char (point-min))))
       (display-buffer buf)
       (message "Parsing results dumped to *treesit-env-dsl-debug*"))))

(provide 'treesit-env-dsl)

;;; treesit-env-dsl.el ends here
