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
;;; TODO(09/22/24): split for `font-lock-maximum-decoration'
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
     (5 'font-lock-escape-face)
     (6 'font-lock-string-face)))
  "Font-locking for `trace-mode'.")


;;; Commands

;;;###autoload
(defun trace-mode-buffer (&optional and-go create)
  "Return trace output buffer.
Create a new buffer when necessary if CREATE is non-nil.
If AND-GO, pop to output buffer."
  (let ((buf (if create
                 (get-buffer-create trace-buffer)
               (get-buffer trace-buffer))))
    (or buf (user-error "No trace-buffer"))
    (with-current-buffer buf
      (when (eq major-mode 'fundamental-mode)
        (trace-mode))
      (if and-go
          (pop-to-buffer (current-buffer))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun trace-mode-display-results (&optional and-go)
  "Show the trace results and maybe enable `trace-mode'.
If AND-GO is non-nil, pop to the result buffer."
  (interactive "P")
  (trace-mode-buffer and-go))

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
  :parent special-mode-map
  "j" #'next-line
  "k" #'previous-line
  "h" #'backward-char
  "l" #'forward-char
  "u" #'trace-mode-untrace
  "f" #'forward-sexp
  "b" #'backward-sexp
  "n" #'forward-paragraph
  "p" #'backward-paragraph
  ;; "TAB" #'hs-toggle-hiding
  )

;;;###autoload
(define-derived-mode trace-mode special-mode "Trace"
  "Major mode for displaying trace results.

\\{trace-output-mode-map}"
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
  ;; TODO(09/22/24): wrap trace insertion to abbreviate sexps according to
  ;; `print-level' and `print-length'
  (setq-local print-length (- (window-width nil t) 10)
              print-level 3)
  (hl-line-mode))

(provide 'trace-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; trace-mode.el ends here
