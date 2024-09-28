;;; trace-mode.el --- Enhanced mode for viewing function traces -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/trace-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created: 22 September 2024
;; Keywords: tools, debug, lisp, trace

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package provides a major-mode for viewing trace results generated with
;; the tracing functions from trace.el
;;
;; `trace-mode' provides highlighting, folding and font-locking to make
;; differentiating the output easier at a glance. Code folding
;; support is provided for hideshow.el. It also defines movement commands for
;; structural navigation.
;;
;;; Code:

(require 'trace)
(require 'elisp-mode)                   ; `elisp--eval-last-sexp-print-value'


(defun trace-mode--prefix (level &optional function)
  (format "%s%s%d %s"
          (mapconcat #'char-to-string (make-string (max 0 (1- level)) ?|) " ")
          (if (> level 1) " " "")
          level
          (if function (concat "<- " (symbol-name function) ": ") "-> ")))

;; Note(09/23/24): use `cl-prin1' to print objects? It hides objects in
;; `cl-print-ellipsis' that exceed `print-level'/`print-length' -
;; `cl-print-expand-ellipsis' prints stuff hidden in ellipses
(defun trace-mode--print (prefix value &optional ctx output)
  (let ((print-circle t)
        (print-escape-newlines t)
        ;; XXX(09/22/24): print settings for tracing
        (eval-expression-print-length 10)
        (eval-expression-print-level 2))
    (if noninteractive
        (let ((msg (concat prefix (prin1-to-string value)
                           (when (and ctx (zerop (length ctx)))
                             (prin1-to-string ctx)))))
          (princ (if (eq ?\n (aref msg (1- (length msg))))
                     (substring msg 0 -1) msg)
                 standard-output))
      (with-current-buffer trace-buffer
        (or output (setq output (current-buffer)))
        (setq-local window-point-insertion-type t)
        (goto-char (point-max))
        (let ((deactivate-mark nil))    ; Protect deactivate-mark.
          (princ prefix output)
          (elisp--eval-last-sexp-print-value value output)
          (when ctx
            (if (stringp ctx)
                (princ ctx output)
              (elisp--eval-last-sexp-print-value ctx output)))
          (terpri output))))))

(defun trace-mode--entry-message (function level args context)
  "Override for `trace--entry-message'.
FUNCTION, LEVEL, ARGS, and CONTEXT are passed to `trace--entry-message'."
  (unless inhibit-trace
    (trace-mode--print
     (trace-mode--prefix level) (cons function args) (funcall context))))

(defun trace-mode--exit-message (function level value context)
  "Override for `trace--exit-message'.
FUNCTION, LEVEL, VALUE, and CONTEXT are passed to `trace--exit-message'."
  (unless inhibit-trace
    (trace-mode--print
     (trace-mode--prefix level function) value (funcall context))))

(defun trace-mode-toggle-formatting ()
  (interactive)
  (if (advice-member-p 'trace-mode--entry-message 'trace--entry-message)
      (progn (advice-remove 'trace--entry-message 'trace-mode--entry-message)
             (advice-remove 'trace--exit-message 'trace-mode--exit-message))
    (advice-add 'trace--entry-message :override #'trace-mode--entry-message)
    (advice-add 'trace--exit-message :override #'trace-mode--exit-message)))

;;; Override messages
(trace-mode-toggle-formatting)


(defun trace-mode--forward-sexp-1 (arg)
  "Skip over oen trace level.
ARG is +1 or -1 for direction."
  (let* ((cur-arrow "->")
         (level
          (progn (beginning-of-line)
                 (and (looking-at "\\(\\(?:| \\)*[0-9]+\\) \\([<>-]+\\)")
                      (prog1 (match-string 1)
                        (setq cur-arrow (match-string 2))))))
         (end-re (format "^%s %s" level (if (string= cur-arrow "<-") "->" "<-"))))
    (when level
      (if (or (and (string= "->" cur-arrow) (< arg 0))
              (and (string= "<-" cur-arrow) (> arg 0)))
          (zerop (forward-line arg))
        (catch 'term
          (progn (while (not (looking-at-p end-re))
                   (unless (zerop (forward-line arg))
                     (throw 'term nil)))
                 t))))))

;; Necessary for hideshow to hide levels properly
(defun trace-mode--forward-sexp (&optional arg)
  "Function for `forward-sexp-function'.
ARG comes from `forward-sexp', which see."
  (or arg (setq arg 1))
  (let ((cnt (abs arg))
        (sign (if (> arg 0) 1 -1)))
    (catch 'term
      (while (> cnt 0)
        (beginning-of-line)
        (when (looking-at-p (regexp-quote trace-separator))
          (or (zerop (forward-line sign))
              (throw 'term nil)))
        (trace-mode--forward-sexp-1 sign)
        (throw 'term nil)
        (cl-decf cnt)))
    cnt))

(defvar hs-special-modes-alist)
(with-eval-after-load 'hideshow
  (unless (assoc 'trace-mode hs-special-modes-alist)
    (push '(trace-mode
            "^\\(?:| \\)*[0-9]+ ->"
            "^\\(?:| \\)*[0-9]+ <-")
          hs-special-modes-alist)))

(defun trace-mode--hl-line-range ()
  "Return the bounds of the call at point for `hl-line-range-function'."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\(?:| \\)*[0-9]+ \\(->\\|<-\\)")
        (let ((forward-p (equal "->" (match-string 1))))
          (cons (if forward-p (point) (line-beginning-position 2))
                (progn
                  (funcall (if forward-p #'forward-sexp #'backward-sexp))
                  (if forward-p (line-beginning-position 2) (point)))))
      (cons (point) (line-beginning-position 2)))))


;;; Font-locking
;; XXX(09/22/24): split for `font-lock-maximum-decoration'
(defvar trace-mode-keywords
  `((,(concat "^" (string-chop-newline trace-separator))
     . font-lock-comment-face)
    (,(rx bow (or "nil" "t") eow) . font-lock-constant-face)
    ("^\\(\\(?:| \\)+\\)?\\([0-9]+\\) \\(->\\) (\\([^ ]+\\)"
     (1 font-lock-comment-face nil t)
     (2 '(:inherit font-lock-number-face :weight bold))
     (3 '(:inherit font-lock-keyword-face :weight bold))
     (4 font-lock-function-name-face))
    ("^\\(\\(?:| \\)+\\)?\\([0-9]+\\) \\(<-\\) \\([^:]+\\)\\(:\\) \\([^\n]+\\)"
     (1 font-lock-comment-face nil t)
     (2 'font-lock-number-face)
     (3 'font-lock-operator-face)
     (4 'font-lock-property-use-face)
     (5 'font-lock-escape-face)))
  ;; (6 'font-lock-string-face)
  "Font-locking for `trace-mode'.")


;;; Commands

;;;###autoload
(defun trace-mode-buffer (&optional create)
  "Return trace output buffer.
CREATE a new buffer when necessary if non-nil."
  (let ((buf (if create
                 (get-buffer-create trace-buffer)
               (get-buffer trace-buffer))))
    (or buf (user-error "No trace-buffer"))
    (with-current-buffer buf
      (when (eq major-mode 'fundamental-mode)
        (trace-mode))
      (current-buffer))))

(defvar trace-mode-display-action
  '((display-buffer--maybe-same-window
     display-buffer-reuse-window
     display-buffer--maybe-pop-up-frame-or-window
     display-buffer-use-some-window)
    (body-function
     . (lambda (win)
         (with-selected-window win
           (set-window-point win (point-max))
           (recenter (- -1 scroll-margin)))))
    (dedicated . t)
    (allow-no-window . t))
  "Display buffer action for trace buffer.")

;;;###autoload
(defun trace-mode-display-results (&optional and-go)
  "Show the trace results and maybe enable `trace-mode'.
If AND-GO, or the trace buffer was already visible when called, pop
to the result buffer."
  (interactive "P")
  (let ((buf (trace-mode-buffer)))
    (when (and buf (not (eq buf (current-buffer))))
      (if (or and-go (get-buffer-window buf 'visible))
          (pop-to-buffer buf trace-mode-display-action)
        (display-buffer buf trace-mode-display-action)))))

;;;###autoload
(defun trace-mode-clear ()
  "Clear trace results buffer."
  (interactive)
  (when-let ((buf (get-buffer trace-buffer)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer))))))

(defun trace-mode-untrace ()
  "Untrace traced function on current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(?:| \\)*[0-9]+ \\(?:->\\|<-\\) (?\\([^ ]+\\)")
      (let ((fn (intern (match-string 1))))
        (when (and fn (y-or-n-p (format "Untrace %S? " fn)))
          (message "untracing %S" fn)
          (untrace-function fn)
          ;; Support for `tracing-minor-mode'
          (when (boundp 'tracing--current)
            (setq tracing--current (delq fn tracing--current))))))))


(defvar-keymap trace-mode-map
  :doc "Keymap in `trace-mode'."
  :parent special-mode-map
  "j" #'next-line
  "k" #'previous-line
  "h" #'backward-char
  "l" #'forward-char
  "u" #'trace-mode-untrace
  "f" #'forward-sexp
  "b" #'backward-sexp
  "n" #'forward-paragraph
  "p" #'backward-paragraph)

;;;###autoload
(define-derived-mode trace-mode special-mode "Trace"
  "Major mode for displaying trace results.

\\{trace-mode-map}"
  :abbrev-table nil
  :syntax-table emacs-lisp-mode-syntax-table
  (setq buffer-read-only nil)
  (let ((sep (regexp-quote (string-chop-newline trace-separator))))
    ;; Variables to set for hideshow to work
    (setq-local paragraph-separate sep)
    (setq-local comment-start sep))
  (setq-local comment-end "")
  (setq-local hs-hide-comments-when-hiding-all nil)
  (setq-local forward-sexp-function #'trace-mode--forward-sexp)
  (setq-local font-lock-defaults (list trace-mode-keywords))
  (setq-local hl-line-range-function #'trace-mode--hl-line-range)
  (hl-line-mode))

(provide 'trace-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; trace-mode.el ends here
