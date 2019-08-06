;; Gathering macros by parsing buffer
;; superceded by read make's database

(defun macrostep-make-bounds-of-macro-at-point ()
  (save-excursion
    (skip-chars-backward "^$)}:#= \t\n" (line-beginning-position))
    (when (and (eq ?$ (char-before))
               (not (eq ?$ (char-before (1- (point)))))
               (memq (char-after) '(?\( ?\{)))
      (let ((start (1- (point)))
            (end (progn
                   (skip-chars-forward "^ \t)}" (line-end-position))
                   (1+ (point)))))
        (and (memq (char-after) '(?\) ?\}))
             (cons start end))))))

(put 'make-macro 'bounds-of-thing-at-point 'macrostep-make-bounds-of-macro-at-point)

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
  (save-excursion
    (goto-char pos)
    (eq (logand 1 (skip-chars-backward "\\")) 1)))

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

;; so we can update possibly missed variables when called from
;; `makefile-pickup-everything'
(advice-add 'makefile-pickup-everything :after #'macrostep-make--update-macro-table)

;; table of macros and their values
(defvar-local macrostep-make--table nil)

;; Remember a macro and its value if not already present.
;; If the macro is in the table but its value has changed,
;; unless NO-UPDATE is non-nil, update its value.
;; Returns non-nil if MACRO-NAME isn't 0 length
(defun macrostep-make--remember-macro (macro-name macro-value &optional no-update)
  (when (not (zerop (length macro-name)))
    (let ((val (assoc macro-name macrostep-make--table)))
      (if (not val)
          (setq macrostep-make--table
                (cons (cons macro-name macro-value) macrostep-make--table))
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
(defun macrostep-make--update-macro-table (&rest _ignored)
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
        ;; runs shell commands if `macrostep-make-use-shell' is non-nil
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
    (setq macro-table (buffer-local-value 'macrostep-make--table (current-buffer))))
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
                    (macrostep-make--get-value var-name))))
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

