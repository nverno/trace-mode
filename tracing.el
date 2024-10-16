;;; tracing.el --- Minor mode during function tracing -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

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
;; Minor mode to manage and display information about currently traced
;; functions.
;;; Code:

(require 'trace)


(defgroup tracing nil
  "Active function tracing."
  :group 'trace
  :prefix "tracing-")

(defcustom tracing-mode-line-prefix " Tr"
  "Tracing minor mode prefix."
  :type 'string)

(defface tracing-mode-line-active-face
  '((t (:inherit success :foreground "hotpink" :weight bold)))
  "Face for active trace count."
  :group 'trace)

(defcustom tracing-mode-line '(:eval (tracing--mode-line-text))
  "Mode line for `tracing-minor-mode'."
  :type 'sexp
  :risky t)


(defvar tracing--current nil "Active trace names.")

(defvar tracing--batch nil "Non-nil when doing batch action.")


;;; Mode-Line

(defun tracing-mode-line-toggle-inhibit (event)
  "Toggle `inhibit-trace' from the mode-line EVENT."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (setq inhibit-trace (not inhibit-trace))
    (force-mode-line-update t)))

(defun tracing--mode-line-text ()
  "Return text to display in mode-line."
  (when (or (derived-mode-p 'trace-mode)
            (mode-line-window-selected-p))
    (let* (face (suffix
                 (if inhibit-trace
                     (progn (setq face 'error) "off")
                   (let ((cnt (length tracing--current)))
                     (when (> cnt 0)
                       (setq face 'tracing-mode-line-active-face))
                     (number-to-string cnt)))))
      (concat
       tracing-mode-line-prefix ":"
       (apply #'propertize suffix
              'help-echo (apply
                          #'format "%s\nmouse-1: %s tracing"
                          (if inhibit-trace
                              (list "Tracing is inhibited" "Enable")
                            (list "Tracing is active" "Inhibit")))
              'local-map (make-mode-line-mouse-map
                          'mouse-1 #'tracing-mode-line-toggle-inhibit)
              (and face (list 'face face)))))))

(defun tracing-add (funcs &optional remove)
  "Track FUNCS and maybe enable/disable `tracing-minor-mode'.
If REMOVE is non-nil, remove FUNCS from tracking."
  (setq tracing--current (if remove
                             (seq-remove
                              (lambda (e) (memq e funcs)) tracing--current)
                           (seq-uniq (append funcs tracing--current))))
  (cond ((zerop (length tracing--current))
         (tracing-minor-mode -1))
        (tracing-minor-mode (force-mode-line-update))
        (t (tracing-minor-mode 1))))

;; -------------------------------------------------------------------
;;; Tracing List

(defun tracing-list-untrace ()
  "Untrace function in list."
  (interactive nil tracing-list-mode)
  (when-let ((args (get-text-property (point) 'trace-args)))
    (apply #'untrace-function args)
    (revert-buffer nil t t)))

(defvar-keymap tracing-list-keymap
  :doc "Keymap on links in list of traced functions."
  :parent button-map
  "u" #'tracing-list-untrace
  "<mouse-2>" #'tracing-list-untrace)

(defun tracing--list-print (&rest _args)
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (insert (propertize (format "%s" "Traced functions") 'face 'outline-1))
    (insert "\n\n")
    (dolist (fn (sort tracing--current :key #'symbol-name))
      (insert-text-button (symbol-name fn)
                          'face 'button
                          'keymap tracing-list-keymap
                          'trace-args (list fn)
                          'action (lambda (_) (describe-function fn))
                          'follow-link t
                          'help-echo "mouse-1, RET: describe function; \
mouse-2: untrace function")
      (insert "\n"))
    (when pos (goto-char pos))))

(defun tracing-list-functions ()
  "List functions currently traced."
  (interactive nil tracing-minor-mode)
  (unless tracing-minor-mode
    (user-error "No active trace"))
  (help-setup-xref (list #'tracing-list-functions)
		   (called-interactively-p 'interactive))
  (with-help-window (get-buffer-create "*Traced Functions*")
    (tracing--list-print)
    (setq-local revert-buffer-function #'tracing--list-print)))

(defvar-keymap tracing-prefix-map
  :prefix 'tracing-prefix-map
  "j" #'trace-mode-display-results
  "q" #'untrace-all
  "l" #'tracing-list-functions)

(defvar-keymap tracing-minor-mode-map
  :doc "Keymap active `tracing-minor-mode'."
  "<f2> D" #'tracing-prefix-map)

(easy-menu-define tracing-minor-mode-menu tracing-minor-mode-map
  "Tracing Menu"
  '("Tracing"
    ["Display results" trace-mode-display-results t]
    ["Untrace all" untrace-all t]
    ["List traced functions" tracing-list-functions t]))

;;;###autoload
(define-minor-mode tracing-minor-mode
  "Minor mode active during tracing."
  :lighter tracing-mode-line
  :keymap tracing-minor-mode-map
  :global t
  :interactive nil
  :group 'trace
  (unless tracing-minor-mode
    (setq tracing--current nil)))


;;; Advices

(defun tracing-add@trace-function-internal (func &rest _)
  "Advise `trace-function-internal' to track FUNC."
  (or tracing--batch (tracing-add (list func))))

(defun tracing-remove@untrace-function (func)
  "Advise `untrace-function' to remove FUNC tracking."
  (or tracing--batch (tracing-add (list func) t)))

(defun tracing-remove-all@untrace-all (orig)
  "Advice around `untrace-all', ORIG, to disable `tracing-minor-mode'."
  (let ((tracing--batch t))
    (funcall orig)
    (tracing-minor-mode -1)))

;;;###autoload
(defun tracing-enable ()
  "Enable `tracing-minor-mode' to track actively traced functions."
  (interactive)
  (advice-add 'trace-function-internal
              :after #'tracing-add@trace-function-internal
              '((name . "tracing-add")))
  (advice-add 'untrace-function :after #'tracing-remove@untrace-function
              '((name . "tracing-remove")))
  (advice-add 'untrace-all :around #'tracing-remove-all@untrace-all
              '((name . "tracing-remove-all"))))

(defun tracing-disable ()
  "Disable `tracing-minor-mode'."
  (interactive)
  (advice-remove 'trace-function-internal #'tracing-add@trace-function-internal)
  (advice-remove 'untrace-function #'tracing-remove@untrace-function)
  (advice-remove 'untrace-all #'tracing-remove-all@untrace-all))


(provide 'tracing)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; tracing.el ends here
