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
;; Parses keyword-based argument lists into structured plists according
;; to a schema.
;;
;; Supported schema types:
;;   single   - exactly one value
;;   list     - one or more values (collected in insertion order)
;;              Quoted values are automatically unwrapped:
;;                'symbol -> symbol
;;                '(a b) -> a, b (flattened)
;;
;; All list values are accumulated in insertion order using an internal
;; tail-tracked list builder.  There is no
;; "collect in reverse, nreverse at the end" convention.
;;
;; This library also provides `treesit-env-dsl-quote' for safe symbol
;; handling during macro expansion.

;;; Code:

(require 'cl-lib)

;;; Internal Tail-Tracked List Builder
;;
;; An O(1)-append list builder used to accumulate parsed values in
;; insertion order without a final nreverse step.
;; Stores (HEAD . TAIL): HEAD is the first cons cell, TAIL the last.

(cl-defstruct (treesit-env-dsl--tl
               (:constructor treesit-env-dsl--tl--make (head tail)))
  "Internal tail-tracked list builder.
HEAD is the first cons cell; TAIL is the last cons cell."
  head tail)

(defun treesit-env-dsl--tl-new ()
  "Return a new empty tail-tracked list builder."
  (treesit-env-dsl--tl--make nil nil))

(defun treesit-env-dsl--tl-empty-p (tl)
  "Return non-nil if TL contains no elements."
  (null (treesit-env-dsl--tl-head tl)))

(defun treesit-env-dsl--tl-append! (tl item)
  "Append ITEM to TL destructively and return TL.  O(1)."
  (let ((cell (cons item nil)))
    (if (treesit-env-dsl--tl-empty-p tl)
        (setf (treesit-env-dsl--tl-head tl) cell
              (treesit-env-dsl--tl-tail tl) cell)
      (setcdr (treesit-env-dsl--tl-tail tl) cell)
      (setf (treesit-env-dsl--tl-tail tl) cell)))
  tl)

(defun treesit-env-dsl--tl-value (tl)
  "Return the list built by TL."
  (treesit-env-dsl--tl-head tl))

;;; Parser

(defun treesit-env-dsl-parse (args schema)
  "Parse ARGS according to SCHEMA and return a plist.

SCHEMA is an alist of (KEYWORD . TYPE) entries where TYPE can be:
  - `single': Requires exactly one value.
  - `list'  : Requires at least one value (collected into a list).
              Quoted values are automatically unwrapped:
                \\='symbol -> symbol
                \\='(a b)  -> a, b (flattened)

All list values are accumulated in insertion order using an internal
tail-tracked list builder.  No \\\"collect in reverse, nreverse\\\" convention
is used.

Example:
  (treesit-env-dsl-parse \\='(:revision \\\"main\\\" :deps c cpp)
                         \\='((:revision . single) (:deps . list)))
  => (:revision \\\"main\\\" :deps (c cpp))"
  (let* ((result nil)
         (result-tails nil)   ; plist: key -> tl for that key's value
         (current-key nil)
         (current-type nil)
         ;; Direct pointer to the tl of the current list key.
         ;; Avoids a plist-get on result-tails for every appended value.
         (current-tl nil))

    ;; Helper: append VALUE to the current key's tl.
    ;; plist-put into result is called only on the first append
    ;; (empty -> non-empty); subsequent appends mutate existing cons cells
    ;; via tl-append!, so the pointer already in result remains valid.
    (cl-labels
        ((append-to-key (value)
           (let ((was-empty (treesit-env-dsl--tl-empty-p current-tl)))
             (treesit-env-dsl--tl-append! current-tl value)
             (when was-empty
               (setq result (plist-put result current-key
                                       (treesit-env-dsl--tl-value current-tl))))))

         (init-list-key (key)
           (let ((existing (plist-get result-tails key)))
             (if existing
                 (setq current-tl existing)
               (let ((tl (treesit-env-dsl--tl-new)))
                 (setq result-tails (plist-put result-tails key tl)
                       current-tl tl))))))

      (while args
        (let* ((item (pop args))
               (entry (and (keywordp item) (assq item schema))))
          (cond
           ;; New keyword
           (entry
            ;; Validate that the previous keyword got its required value(s)
            (when current-key
              (cond
               ((and (eq current-type 'single)
                     (not (plist-member result current-key)))
                (error "syntax error: keyword %S expects a value"
                       current-key))
               ((and (eq current-type 'list)
                     (not (plist-get result current-key)))
                (error "syntax error: keyword %S expects at least one value"
                       current-key))))

            (let ((type (cdr entry)))
              (cond
               ;; single: no accumulator needed
               ((eq type 'single)
                (setq current-key item
                      current-type 'single
                      current-tl nil))

               ;; list: initialize tl accumulator
               ((eq type 'list)
                (setq current-key item
                      current-type 'list)
                (init-list-key item))

               (t
                (error "internal error: unknown schema type %S for keyword %S"
                       type item)))))

           ;; Unknown keyword
           ((keywordp item)
            (error "syntax error: unknown keyword %S" item))

           ;; Value
           (t
            (if (null current-key)
                (error "syntax error: unexpected value %S \
— must be preceded by a keyword" item)
              (cond
               ;; single: exactly one value, then reset state
               ((eq current-type 'single)
                (setq result (plist-put result current-key item)
                      current-key nil
                      current-type nil
                      current-tl nil))

               ;; list: flat-append atoms or lists, O(1) via tail tracking
               ((eq current-type 'list)
                (cond
                 ;; Quoted value: unwrap 'sym -> sym, '(a b) -> a, b
                 ((and (listp item) (eq (car item) 'quote) (consp (cdr item)))
                  (let ((quoted-val (cadr item)))
                    (if (listp quoted-val)
                        (dolist (elem quoted-val)
                          (append-to-key elem))
                      (append-to-key quoted-val))))
                 ;; Regular list: flatten
                 ((listp item)
                  (dolist (elem item)
                    (append-to-key elem)))
                 ;; Atom: add as-is
                 (t
                  (append-to-key item))))))))))

      ;; Final validation
      (when current-key
        (cond
         ((and (eq current-type 'single)
               (not (plist-member result current-key)))
          (error "syntax error: keyword %S requires a value but input ended"
                 current-key))
         ((and (eq current-type 'list)
               (not (plist-get result current-key)))
          (error "syntax error: keyword %S requires at least one value"
                 current-key)))))

    result))

;;; Quoting Utilities

(defun treesit-env-dsl-quote (val)
  "Quote VAL for macro expansion with unquote support.

Rules:
  (\\, X)        -> X  (unquote: X is embedded for direct evaluation)
  (quote X)     -> unchanged
  (function X)  -> unchanged  (#\\='X passes through as-is)
  self-evaluating -> unchanged  (keywords, numbers, strings, booleans)
  otherwise     -> (quote VAL)"
  (cond
   ;; Unquote: strip (\\, EXPR) -> EXPR so it evaluates directly in macro expansion
   ((and (consp val) (eq (car val) '\,)) (cadr val))
   ;; Already quoted: pass through unchanged
   ((and (listp val) (eq (car val) 'quote)) val)
   ;; Function reference: #'foo -> (function foo) -> pass through
   ((and (consp val) (eq (car val) 'function)) val)
   ;; Self-evaluating types
   ((or (keywordp val) (numberp val) (stringp val) (booleanp val)) val)
   (t `',val)))

(defun treesit-env-dsl-quote-list (lst)
  "Apply `treesit-env-dsl-quote' to all elements in LST.
Returns a `(list ...)' form for macro expansion, or nil for empty lists."
  (if (null lst) nil `(list ,@(mapcar #'treesit-env-dsl-quote lst))))

;;; Debugging Utility

(defmacro treesit-env-dsl-debug (schema &rest args)
  "Parse ARGS with SCHEMA at expansion time and show result in a buffer."
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
