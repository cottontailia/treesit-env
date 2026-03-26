;;; treesit-env-tl.el --- Internal tail-tracked list builder -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Cottontailia

;; Author: Cottontailia
;; URL: https://github.com/cottontailia/treesit-env
;; Keywords: lisp, internal
;; Package-Requires: ((emacs "29.1") (cl-lib "1.0"))
;; License: CC0

;;; Commentary:

;; Internal O(1)-append list builder used by treesit-env.
;; Stores (HEAD . TAIL) where HEAD is the first cons cell and TAIL is the last.
;;
;; Ownership note for `treesit-env--tl-extend!':
;;   extend! takes ownership of the cons cells it receives; the TL's tail
;;   pointer will point into ITEMS after the call.  Any subsequent
;;   `treesit-env--tl-append!' will therefore mutate ITEMS's last cdr.
;;   Pass (copy-sequence ITEMS) if the caller needs to retain ITEMS
;;   independently.

;;; Code:

(require 'cl-lib)

;;; Struct

(cl-defstruct (treesit-env--tl
               (:constructor treesit-env--tl--make (head tail)))
  "Internal tail-tracked list builder.
HEAD is the first cons cell; TAIL is the last cons cell."
  head tail)

;;; Constructor

(defun treesit-env--tl-new ()
  "Return a new empty tail-tracked list builder."
  (treesit-env--tl--make nil nil))

;;; Predicates

(defun treesit-env--tl-empty-p (tl)
  "Return non-nil if TL contains no elements."
  (null (treesit-env--tl-head tl)))

;;; Mutators

(defun treesit-env--tl-append! (tl item)
  "Append ITEM to TL destructively and return TL.  O(1)."
  (let ((cell (cons item nil)))
    (if (treesit-env--tl-empty-p tl)
        (setf (treesit-env--tl-head tl) cell
              (treesit-env--tl-tail tl) cell)
      (setcdr (treesit-env--tl-tail tl) cell)
      (setf (treesit-env--tl-tail tl) cell)))
  tl)

(defun treesit-env--tl--last-cell (lst)
  "Return the last cons cell of LST, or nil if LST is nil."
  (when lst
    (let ((p lst))
      (while (cdr p)
        (setq p (cdr p)))
      p)))

(defun treesit-env--tl-extend! (tl items)
  "Append ITEMS (a proper list) to TL destructively and return TL.
If ITEMS is nil, TL is returned unchanged.  O(length ITEMS).
TL takes ownership of ITEMS's cons cells: any subsequent mutation of TL
via `treesit-env--tl-append!' will also modify the tail of ITEMS.
Pass (copy-sequence ITEMS) if the caller needs to retain ITEMS independently."
  (when items
    (let ((items-tail (treesit-env--tl--last-cell items)))
      (if (treesit-env--tl-empty-p tl)
          (setf (treesit-env--tl-head tl) items)
        (setcdr (treesit-env--tl-tail tl) items))
      (setf (treesit-env--tl-tail tl) items-tail)))
  tl)

;;; Accessor

(defun treesit-env--tl-value (tl)
  "Return the list built by TL."
  (treesit-env--tl-head tl))

(provide 'treesit-env-tl)

;;; treesit-env-tl.el ends here
