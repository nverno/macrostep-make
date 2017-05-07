;;; macrostep-make ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/make-tools
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

;; Expands make variables using macrostep-expand interface.
;;
;; - creates an association list of macro -> value
;; - currently is able to parse included makefiles and add their macro
;;   definitions. If `macrostep-make-use-shell' is non-nil, then included
;;   makefiles that require the $(shell) command can be found by calling
;;   `shell-file-name'.
;;
;; - macrostep interface:
;; 	+ macrostep-sexp-bounds-function
;;	+ macrostep-sexp-at-point-function
;;	+ macrostep-environment-at-point-function
;;	+ macrostep-expand-1-function
;;	+ macrostep-print-function
;;
;; PROBLEMS:
;; - when expanding variable with blank definition, it isn't replaced
;;   with original variable
;;
;; TODO:
;; - Deal with '+='
;; - Either do variable substitution when storing values or do multi-level
;;   macroexpansion, like elisp.

;;; Code:
(eval-when-compile
  (require 'subr-x)

  (unless (fboundp 'ignore-errors)
    (defmacro ignore-errors (&rest body)
      `(condition-case nil (progn ,@body) (error nil)))))

(require 'macrostep)
(require 'make-mode)

;;; Variables
(defvar macrostep-make-use-shell t
  "If non-nil, call `shell-file-name' to evaluate makefile 'shell' variables.")

;; -------------------------------------------------------------------
;;; Utils

;; Bounds of macros at point

;; determine the bounds of macro at point
(defun macrostep-make-bounds-of-macro-at-point ()
  (save-excursion
    (skip-chars-backward "^$)}:#= \t\n" (line-beginning-position))
    (when (and (eq ?\$ (char-before))
               (memq (char-after) '(?\( ?\{)))
      (let ((start (1- (point)))
            (end (progn
                   (skip-chars-forward "^)}" (line-end-position))
                   (1+ (point)))))
        (cons start end)))))

(put 'make-macro 'bounds-of-thing-at-point
     'macrostep-make-bounds-of-macro-at-point)

;; -------------------------------------------------------------------
;;; Parsing macro values

;; Assume point is after the '=' in a macro assignment.
;; This function then returns the macro's value,
;; taking into account possibly escaped lines.  The point is moved to the
;; end of the value which may span multiple lines
(defun macrostep-make--read-value ()
  (let (res bnds done)                  ;store value in res
    (while (not done)                   ;done is true when line is not escaped
      (setq bnds (macrostep-make--read-line)) ;bounds of current line
      (if (not (macrostep-make--escaped-p (point)))
          (setq done t)                 ;finished
        (forward-char -1)               ;skip first '\'
        (while (macrostep-make--escaped-p (point))
          (forward-char -1))            ;skip over anymore '\'
        (skip-syntax-backward " " (line-beginning-position)) ;skip whitespace
        (setf (cdr bnds) (point)))      ;update new endpoint
      (push (buffer-substring-no-properties (car bnds) (cdr bnds))
            res)                        ;add string to result
      (and (not done)
           (forward-line)))
    (mapconcat 'identity (nreverse res) " ")))

;; is the current point escaped?
(defun macrostep-make--escaped-p (pos)
  (and (eq ?\\ (char-before pos))
       (not (macrostep-make--escaped-p (1- pos)))))

;; read single line of macro value, returning (start . end)
(defun macrostep-make--read-line ()
  (let ((beg (progn
               (skip-syntax-forward " " (line-end-position))
               (point)))
        (end (progn
               (end-of-line)
               (skip-syntax-backward " " (line-beginning-position))
               (point))))
    (cons beg end)))

;; -------------------------------------------------------------------
;;; Macro Table
;;
;; The macro table is a simple association list of form
;; ((macro-name . macro-value) ...)

;; table of macros and their values
(defvar-local macrostep-make--table nil)

;; Remember a macro and its value if not already present.
;; If the macro is in the table but its value has changed,
;; unless NO-UPDATE is non-nil, update its value.
;; Returns non-nil if MACRO-NAME isn't 0 length
(defun macrostep-make--remember-macro (macro-name macro-value
                                                  &optional no-update)
  (when (not (zerop (length macro-name)))
    (let ((val (assoc macro-name macrostep-make--table)))
      (if (not val)
          (setq macrostep-make--table
                (cons (cons macro-name macro-value)
                      macrostep-make--table))
        ;; unless NO-UPDATE, update macro's value if different
        (and (not no-update)
             (not (string= macro-value (cdr val)))
             (setcdr val macro-value)))
      t)))

;; retrieve a macro-value from the table, nil if MACRO-NAME isn't present
;; looks up value in macrostep-make--table unless MACRO-TABLE is non-nil
(defun macrostep-make--get-value (macro-name &optional macro-table)
  (when (or macro-table
            makefile-need-macro-pickup
            (not macrostep-make--table))
    (macrostep-make--update-macro-table))
  (let ((val (assoc macro-name (or macro-table macrostep-make--table))))
    (and val (cdr val))))

;; Create/update association table for macros and values
(defun macrostep-make--update-macro-table ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))                   ;go through each line
      (forward-comment (point-max))       ;skip over comments
      (or (macrostep-make--grab-macro)    ;if macro, get that
          (beginning-of-line)             ;try again
          (macrostep-make--grab-include)) ;if include, merge its table
      (forward-line))))

;; check if line is a macro definition.  If so, parse its value and
;; update the table with the macro and its value.
;; Returns non-nil if line contained a macro definition, otherwise nil
(defun macrostep-make--grab-macro ()
  (when (re-search-forward makefile-macroassign-regex
                           (line-end-position) 'move)
    (goto-char (match-beginning 1))
    (let ((macro-name (buffer-substring-no-properties
                       (point) (progn
                                 (skip-chars-forward "^ \t:#=*")
                                 (point))))
          (macro-value (progn
                         (skip-chars-forward "^=")
                         (forward-char) ;skip the "="
                         (macrostep-make--read-value))))
      (macrostep-make--remember-macro macro-name macro-value))))

;; check if line is an include. If so, return the macro table from
;; the included makefile if it can be determined. This is likely shit
;; since shell calls might be required.
(defun macrostep-make--grab-include ()
  (when (re-search-forward "^\\s-*-?\\(?:include\\)\\s-*\\([^ \t\n]*\\)"
                           (line-end-position) 'move)
    (let ((syntax (syntax-ppss)))
      (unless (or (nth 3 syntax)    ;not in string or comment
                  (nth 4 syntax))
        ;; substitute variables from macro table into file name
        ;; TODO: could run shell commands as option
        (let ((file (macrostep-make--substitute-variables
                     (match-string-no-properties 1))))
          (when (file-exists-p file)     ;do it all over again in new file
            (macrostep-make--merge-table ;merge included table into local
             (macrostep-make--get-file-table file))))))))

;; parse included FILE, return its macro table
(defun macrostep-make--get-file-table (file)
  (with-temp-buffer
    (insert-file-contents file)
    (macrostep-make--update-macro-table) ;update macros
    macrostep-make--table))

;; Merge values from included table into local table.
;; TODO: check if this is the right thing to do?
;; The values in the local table take precedence over those in the included.
(defun macrostep-make--merge-table (new-table)
  (dolist (elem new-table)
    (macrostep-make--remember-macro (car elem) (cdr elem) 'no-update)))

;; -------------------------------------------------------------------
;;; Variable substitution

;; return starting positions of variables, from last to first
;; looks in region from START or current point to END or end of line
(defun macrostep-make--variable-positions (&optional start end)
  (and start (goto-char start))
  (let (pos)
    ;; this regex should find start of unescaped variables
    (while (re-search-forward "\\(?:\\|[^\\]\\)\\(\$[\(\{]\\)"
                              (or end (line-end-position)) 'move)
      (push (match-beginning 1) pos)    ;push starting location
      (goto-char (match-end 0)))        ;onto the next one
    pos))

;; find variable name from starting position BEG if non-nil, or point.
;; this involves skipping the preceding $[\(\{]
;; and skipping forward to matching closing bracket
;; returns variable name and leaves point at end of variable
(defun macrostep-make--variable-name (&optional beg)
  (and beg (goto-char beg))
  (forward-char)                        ;skip '$'
  (let ((start (1+ (point)))            ;start after opening brace
        (end (progn                     ;point at matching brace - 1
               (forward-list)
               (1- (point)))))
    (buffer-substring-no-properties start end)))

;; substitute macros with their values from MACRO-TABLE in string STR
;; if MACRO-TABLE is nil, use macrostep-make--table by default
;; Note: substituting is done from rightmost to leftmost variable.
;; also, after a variable is substituted, the string from the beginning
;; of the substitution is again checked for new variables that may have
;; been added, and their positions prepened to the variable position stack,
;; so nested variables are expanded if they are known
(defun macrostep-make--substitute-variables (str &optional macro-table)
  (unless macro-table
    (setq macro-table (buffer-local-value 'macrostep-make--table
                                          (current-buffer))))
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((pos (macrostep-make--variable-positions)) ;positions of variables
          beg)                                       ;next variable beginning
      (while (and pos
                  (setq beg (pop pos))) ;replace each from back to front
        (let* ((var-name (macrostep-make--variable-name beg))
               (macro-value              ;replace if value is known
                (or (and macrostep-make-use-shell ;shell command, or lookup
                         (macrostep-make--do-shell var-name))
                    (macrostep-make--get-value var-name macro-table))))
          (when macro-value
            (delete-region beg (point)) ;delete variable
            (goto-char beg)             ;goto beginning of insertion
            (insert macro-value)        ;insert value
            ;; there may be new variables in inserted text, so
            ;; update new positions at the front of the list
            ;; NOTE: todo successive expansions, don't expand them all here
            (goto-char beg)             ;back to beginning of insertion
            (setq pos (append (macrostep-make--variable-positions) pos))))))
    (buffer-string)))

;; do a shell command
(defun macrostep-make--do-shell (cmd)
  (when (string-prefix-p "shell" cmd)
    (let ((res (shell-command-to-string
                (concat shell-file-name " -c " (substring cmd 5)))))
      (string-trim res))))

;; -------------------------------------------------------------------
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
(defun macrostep-make-mode-hook ()
  "Main hook to set variables for `macrostep' to function."
  (setq macrostep-sexp-bounds-function          #'macrostep-make-sexp-bounds)
  (setq macrostep-sexp-at-point-function        #'macrostep-make-sexp-at-point)
  (setq macrostep-environment-at-point-function #'ignore)
  (setq macrostep-expand-1-function             #'macrostep-make-expand-1)
  (setq macrostep-print-function                #'macrostep-make-print))

;;;###autoload
(add-hook 'makefile-mode-hook #'macrostep-make-mode-hook)

;; return the bounds of the macro at point
(defun macrostep-make-sexp-bounds ()
  (or (bounds-of-thing-at-point 'make-macro)
      (signal 'macrostep-make-non-macro nil)))

;; nothing special here
(defun macrostep-make-sexp-at-point (start end)
  (cons start end))

;; expand macro
(defun macrostep-make-expand-1 (region _ignore)
  (let ((macro-name (buffer-substring-no-properties
                     (+ 2 (car region))   ;skip $( or ${
                     (1- (cdr region))))) ;skip ) or }
    (or (macrostep-make--get-value macro-name)
        (signal 'macrostep-make-not-found `(,macro-name)))))

;; simple insertion of value
(defun macrostep-make-print (expansion &rest _ignore)
  (insert expansion))

(provide 'macrostep-make)
;;; macrostep-make.el ends here
