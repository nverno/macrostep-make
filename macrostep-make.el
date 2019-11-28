;;; macrostep-make.el --- macrostep for makefile macros -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/macrostep-make
;; Package-Requires: 
;; Created:  6 May 2017

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

;;; Description:

;; Expands make variables using macrostep-expand interface. Variables
;; are retrieved from make's internal database using 'make -prns'. They are
;; updated when the buffer's file's modication time changes.
;;
;; - macrostep interface:
;; 	+ macrostep-sexp-bounds-function
;;	+ macrostep-sexp-at-point-function
;;	+ macrostep-environment-at-point-function
;;	+ macrostep-expand-1-function
;;	+ macrostep-print-function
;;
;; Could do:
;; - option to expand external commands -- $(shell ...)
;; - or show results of commands like patsubst, dir, etc.
;;
;;; Code:
(eval-when-compile
  (require 'dash)
  (require 'cl-lib)
  (require 'subr-x))
(require 'make-mode)
(require 'macrostep)
(require 'nvp-make-completion)          ; use make's database from 'make -prns'

;;; Variables - not implemented
(defvar macrostep-make-use-shell t
  "If non-nil, call `shell-file-name' to evaluate makefile 'shell' variables.")

;; unused: do a shell command
(defun macrostep-make--do-shell (cmd)
  (when (string-prefix-p "shell" cmd)
    (let ((res (shell-command-to-string
                (format "%s %s '%s'" shell-file-name shell-command-switch
                        (substring cmd 5)))))
      (string-trim res))))

;;; Candidates from make database
;; form: (<name> [value] <type> [file] [lineno])
;; where value, file, and lineno may be nil
(defsubst macrostep-make--value (name)
  (-when-let (val (assoc name (nvp-makecomp-candidates 'variables)))
    (or (nth 1 val) " ")))

;; pos is in variable or function position, preceded by '[^$]$[{(]'
(defsubst macrostep-make-v-or-f-p (pos)
  (and (memq (char-before pos) '(?{ ?\())
       (eq (char-before (1- pos)) ?$)
       (not (eq (char-before (- pos 2)) ?$))))

;; variable including preceding $[{(] and any trailing [})]
(defun macrostep-make-botap ()
  (save-excursion
    (skip-chars-forward "$({" (line-end-position))
    (-when-let ((beg . end) (bounds-of-thing-at-point 'symbol))
      (and (macrostep-make-v-or-f-p beg)
           (cons (- beg 2)
                 (if (memq (char-after end) '(?} ?\)))
                     (1+ end)
                   end))))))
(put 'make-macro 'bounds-of-thing-at-point #'macrostep-make-botap)


;;; Macrostep interface

(put 'macrostep-make-non-macro 'error-conditions
     '(macrostep-make-non-macro error))
(put 'macrostep-make-non-macro 'error-message
     "Text around point is not a macro call.")

(put 'macrostep-make-not-found 'error-conditions
     '(macrostep-make-not-found error))
(put 'macrostep-make-not-found 'error-message
     "Macro value not found for: ")

;;;###autoload
(defun macrostep-make-hook ()
  "Main hook to set variables for `macrostep' to function."
  (setq macrostep-sexp-bounds-function          #'macrostep-make-sexp-bounds)
  (setq macrostep-sexp-at-point-function        #'macrostep-make-sexp-at-point)
  (setq macrostep-environment-at-point-function #'ignore)
  (setq macrostep-expand-1-function             #'macrostep-make-expand-1)
  (setq macrostep-print-function                #'macrostep-make-print))

;;;###autoload
(add-hook 'makefile-mode-hook #'macrostep-make-hook)

;; return the bounds of the macro at point
(defun macrostep-make-sexp-bounds ()
  (or (bounds-of-thing-at-point 'make-macro)
      (signal 'macrostep-make-non-macro nil)))

;; return => `macrostep-expand-1-function'
(defun macrostep-make-sexp-at-point (start end)
  (cons start end))

;; expand macro
(defun macrostep-make-expand-1 (region _ignore)
  (let* ((macro-name (buffer-substring-no-properties
                      (+ 2 (car region))
                      (if (memq (char-before (cdr region)) '(?} ?\)))
                          (1- (cdr region))
                        (cdr region))))
         (macro-value (macrostep-make--value macro-name)))
    (unless macro-value
      (signal 'macrostep-make-not-found (list macro-name)))
    (if (string= "" macro-value)
        " "                             ;so original variable is replaced
      macro-value)))

;; Starting positions of variables, from last to first
(defun macrostep-make--variable-positions (&optional start end)
  (and start (goto-char start))
  (let (pos)
    
    (while (re-search-forward "\\(?:\\|[^\\]\\)\\(\$[\(\{]\\)"
                              (or end (line-end-position)) 'move)
      (push (match-beginning 1) pos)
      (goto-char (match-end 0)))
    pos))

;; insert expansion and propertize any nested macros
(defun macrostep-make-print (sexp _env)
  (let* ((beg (point))
         (end (progn
                (insert sexp)
                (point))))
    (goto-char beg)
    ;; this regex should find start of unescaped variables
    (while (re-search-forward "\\(?:\\|[^\\]\\)\\(\$[\(\{]\\)" end t)
      (put-text-property (match-beginning 1) (1+ (match-beginning 1))
                         'macrostep-macro-start t))
    (goto-char end)))

(provide 'macrostep-make)
;;; macrostep-make.el ends here
