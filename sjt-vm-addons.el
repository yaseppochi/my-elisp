(provide 'sjt-vm-addons)
(defun sjt/vm-parse-structured-header (field-body &optional sepchar keep-quotes)
  "Return a list of parameters parsed from FIELD-BODY per RFC 2047. #### CHECK!!
SEPCHAR is a character used to separate parameters in the field body.
KEEP-QUOTES non-nil means to preserve double quotation marks in the field body.
API and semantics are VM-conforming."
  (if (null field-body)
      ()
    ;; #### use with-temp-buffer instead?
    (let ((work-buffer (vm-make-work-buffer)))
      (buffer-disable-undo work-buffer)
      (with-current-buffer work-buffer
       (unwind-protect
	   (let ((list nil)
		 ;; not a regexp, use double \\ not quad \\\\
		 (nonspecials "^\"\\( \t\n\r\f") 
		 start s char sp+sepchar)
	     (if sepchar
		 (setq nonspecials (concat nonspecials (list sepchar))
		       sp+sepchar (concat "\t\f\n\r " (list sepchar))))
	     (insert field-body)
	     (goto-char (point-min))
	     (skip-chars-forward "\t\f\n\r ")
	     (setq start (point))
	     (while (not (eobp))
	       (skip-chars-forward nonspecials)
	       (setq char (char-after))
	       (cond ((looking-at "[ \t\n\r\f]")
		      ;; remove whitespace from parameter
		      (delete-char 1))
		     ((= char ?\\)
		      ;; have escaped character, add to parameter
		      (forward-char 1)
		      (if (not (eobp))
			  (forward-char 1)))
		     ((and sepchar (= char sepchar))
		      ;; parameter is complete, add to list
		      (setq s (buffer-substring start (point)))
		      (if (or (null (string-match "^[\t\f\n\r ]+$" s))
			      (not (string= s "")))
			  (setq list (cons s list)))
		      ;; skip whitespace and blank or empty parameters
		      (skip-chars-forward sp+sepchar)
		      (setq start (point)))
		     ;; #### this can't succeed, it's not CFWS
		     ;; presumably "[ \t\n\r\f]" was meant but already present
		     ;;((looking-at " \t\n\r\f")
		     ;; (skip-chars-forward " \t\n\r\f"))
		     ((= char ?\")
		      ;; have quoted string
		      (let ((done nil))
			(if keep-quotes
			    (forward-char 1)
			  (delete-char 1))
			(while (not done)
			  ;; check for escapes or close quote
			  (if (null (re-search-forward "[\\\\\"]" nil t))
			      ;; #### missing close quote, check RFC
			      (setq done t)
			    (setq char (char-before))
			    (cond ((char-equal char ?\\)
				   ;; escaped character, skip it and continue
				   (delete-char -1)
				   (if (eobp)
				       (setq done t)
				     (forward-char 1)))
				  ;; quote, process keep-quotes
				  (t (if (not keep-quotes)
					 (delete-char -1))
				     (setq done t)))))))
		     ((= char ?\()
		      ;; have comment; find end and delete
		      (let ((done nil)
			    (pos (point))
			    (parens 1))
			(forward-char 1)
			(while (not done)
			  ;; comments nest!
			  (if (null (re-search-forward "[\\\\()]" nil t))
			      ;; #### missing close paren, check RFC
			      (setq done t)
			    (setq char (char-before))
			    (cond ((char-equal char ?\\)
				   (if (eobp)
				       (setq done t)
				     (forward-char 1)))
				  ((char-equal char ?\()
				   (setq parens (1+ parens)))
				  (t
				   (setq parens (1- parens)
					 done (zerop parens))))))
			(delete-region pos (point))))))
	     ;; process last parameter
	     (setq s (buffer-substring start (point)))
	     (if (and (null (string-match "^[\t\f\n\r ]+$" s))
		      (not (string= s "")))
		 (setq list (cons s list)))
	     (nreverse list))
	(and work-buffer (kill-buffer work-buffer)))))))

;; Originally I had RFC 2184 as the reference, but it is totally broken
;; (inconsistent specifications of initial fragment count, example ignores
;; charset/language processing in last fragment).
(defconst sjt/rfc822-specials ".()<>@,;:\\\"[]")
(defconst sjt/rfc2231-tspecials "()<>@,;:\\\"/[]?=")
(defconst sjt/rfc2231-aspecials "'*%")
;; Produce 94 ASCII characters not SPACE or CTL (including DEL).
;; (let* ((i #x7f) (attchars nil))
;;   (while (>= i 32)
;;     (decf i))
;;     (setq attchars (cons (int-to-char i) attchars))
;;   (insert (apply #'concat (list attchars))))
(defconst sjt/rfc2231-attribute-characters
  "0-9A-Za-z!#$&+-.^_`{|}~")
(defconst sjt/rfc2231-token-characters
  (concat sjt/rfc2231-attribute-characters sjt/rfc2231-aspecials))
(defconst sjt/broken-token-characters
  (concat sjt/rfc2231-token-characters "?="))
;; Should produce 94.
;; (apply #'+ (mapcar #'length (list sjt/rfc2231-aspecials
;;				  sjt/rfc2231-tspecials
;;				  sjt/rfc2231-attribute-characters)))
(defun sjt/extend-extent-and-wait (e)
  (set-extent-endpoints e (extent-start-position e) (point))
  (sit-for 1))

(defun sjt/vm-parse-rfc2231-segment-string (s)
  "Wrapper for sjt/vm-parse-rfc2231-segment."
  (with-temp-buffer
    ;; (message "%S" s)
    (insert s)
    (goto-char (point-min))
    (skip-chars-forward " \t")
    (sjt/vm-parse-rfc2231-segment)))

(defun sjt/vm-parse-rfc2231-segment (&optional end)
  "Parse RFC 2231 segment at point up to END (defaulting to end of line),
returning (NAME NUMBER CHARSET LANG VALUE).  Advance point to end of segment.
NAME, CHARSET, and LANG are strings.  NUMBER is an integer.

Assumes line is already unfolded.

FRAGMENT := NAME ['*' [NUMBER '*'] '=' [[CHARSET] <'> [LANGUAGE] <'>] VALUE
where NAME, CHARSET, and LANGUAGE are tokens, NUMBER is a decimal natural
number, and VALUE is a token or quoted string.
In the return value NAME, LANGUAGE, and VALUE are strings, NUMBER is a fixnum,
and CHARSET is a symbol.  VALUE may be encoded but is not delimited by quotes."
  (interactive (list (point-at-eol)))

  (let (name number charset lang value	; return values
	     start extendedp)	; temporary variables
    (setq start (point))

    ;; Parse attribute name.
    (skip-chars-forward sjt/rfc2231-attribute-characters)
    (setq name (buffer-substring start (point)))

    ;; Parse segment information.
    (unless (= ?= (char-after))
      ;; check that we got *
      (if (not (= ?* (char-after)))
	  (error 'args-out-of-range "expected ?* or ?= at point")
	(forward-char 1)		; skip over *
	;; look for a sequence number
	(setq start (point))
	(skip-chars-forward "0-9")
	(if (= start (point))
	    ;; we've not moved, so no number
	    (setq extendedp t)		; but we have a charset or language
	  ;; we've moved, scarf the number
	  (setq number (string-to-number (buffer-substring start (point))))
	  (when (= ?* (char-after))
	    (setq extendedp t)		; we have a charset or language
	    (forward-char 1))		; skip over *
	  )))

    ;; Parse =.
    (unless (= ?= (char-after))
      ;; check that we got =
      (error 'args-out-of-range "expected ?= at point"))
    (forward-char 1)			; skip over =

    ;; Parse charset or language information.
    ;; Charset/lang is allowed only on first segment or only segment.
    (if (not (and extendedp (or (eql number 0) (eql number nil))))
	(setq charset extendedp)
      (setq start (point))
      (skip-chars-forward sjt/rfc2231-attribute-characters)
      (unless (= ?\' (char-after))
	;; check that we got '
	(error 'args-out-of-range "expected ?\' at point"))
      (unless (= start (point))
	(setq charset (downcase (buffer-substring start (point)))))
      (forward-char 1)			; skip over '
      (setq start (point))
      (skip-chars-forward sjt/rfc2231-attribute-characters)
      (unless (= ?\' (char-after))
	;; check that we got '
	(error 'args-out-of-range "expected ?\' at point"))
      (unless (= start (point))
	(setq lang (buffer-substring start (point))))
      (forward-char 1)			; skip over '
      )

    ;; Parse value.
    (setq start (point))
    (if extendedp
	;; Must not contain a quoted string.
	(progn (skip-chars-forward sjt/rfc2231-token-characters)
	       (setq value (buffer-substring start (point))))
      ;; #### We don't get quoted strings from VM,
      ;; and it might contain MIME-words and newlines. :-(
      (setq value (buffer-substring start))
      (when ; (and 
	     (search-forward "=?" nil t)
		; (y-or-n-p (format "Decode MIME words: %s " value)))
	(vm-decode-mime-encoded-words start)
	(setq value (buffer-substring start))))

    ;; Return list of parsed items.
;     (message "%S" (list name number charset lang
; 			(substring value 0 (min (length value) 40))))
    (list name number charset lang value)))

(defun vm-mime-get-xxx-parameter (name param-list)
  "Return the parameter NAME from PARAM-LIST.

If parameter value continuations was used, i.e. the parameter was split into
shorter pieces, rebuild it from them."
  ;; (message "Retrieving parameter %s at %s in %s" name (point) (current-buffer))
  (let ((parsed-parameters (mapcar #'sjt/vm-parse-rfc2231-segment-string
				   param-list))
	(count -1)
	parsed-parameter charset value-charset can-display need-conversion
	start)
    (setq parsed-parameters (delete-if (lambda (x) (not (string= x name)))
				       parsed-parameters
				       :key #'first))
    ;; The test here is to return nil, not the empty string, if there are
    ;; parameters left.
    (when parsed-parameters
      (setq parsed-parameters (sort* parsed-parameters #'< :key #'second))
      (with-temp-buffer
	(setq start (point-min))
	(while parsed-parameters
	  (setq parsed-parameter (pop parsed-parameters)
		number (second parsed-parameter)
		charset (third parsed-parameter)
		;; Ignore language.
		value (fifth parsed-parameter)
		count (1+ count))
; 	  (message "%s" (if (null number)
; 			    "only segment"
; 			  (format "segment %d" number)))

	  ;; error-checking, and determine if we can decode the charset and
	  ;; how to do so
	  (cond ((and (= count 0) (null number)))
		((eql count number))
		(t (error 'args-out-of-range "expected segment" count
			  "but got" number)))
	  (when (and (= count 0) (setq value-charset charset))
	    (setq can-display
		  (vm-mime-charset-internally-displayable-p value-charset))
	    (unless can-display
	      (setq need-conversion
		    (vm-mime-can-convert-charset value-charset))))
; 	  (message "%S %S" value-charset parsed-parameter)
; 	  (sit-for 1)

	  ;; insert the value at the end of the buffer
	  (insert value)

	  ;; decode octets if needed
	  (when charset
	    (goto-char start)
	    (while (re-search-forward "%\\([0-9A-Fa-f][0-9A-Fa-f]\\)" nil t)
	      (goto-char (match-end 0))
	      (insert (int-to-char (string-to-number (match-string 1) 16)))
	      (delete-region (match-beginning 0) (match-end 0))))

	  ;; go to end of buffer and reset start
	  (goto-char (point-max))
	  (setq start (point)))
	(if (and (not can-display) need-conversion)
	    (setq charset (vm-mime-charset-convert-region
			   value-charset (point-min) (point-max)))
	  (setq charset value-charset))
	(when (or can-display need-conversion)
	  (vm-mime-charset-decode-region charset (point-min) (point-max)))
;  	(unless (y-or-n-p (format "Return %S " (buffer-substring)))
;  	  (error 'invalid-argument "Something's wrong."))
	(buffer-substring)))))

(defun sjt/decode-rfc2231-token (start end coding)
  (interactive "r\nzCoding system: ")
  (while (re-search-forward "%\\([0-9A-Fa-f][0-9A-Fa-f]\\)" nil t)
    (goto-char (match-end 0))
    (insert (int-to-char (string-to-number (match-string 1) 16)))
    (delete-region (match-beginning 0) (match-end 0)))
  (decode-coding-region (point) (point-max) coding))

;; #### This function definition is INCOMPLETE!!
; (defun sjt/vm-get-filename-from-content-disposition ()
;   (interactive)
;   (let ((case-fold-search t)
; 	(message-buffer (current-buffer))
; 	(value nil)
; 	(encoding nil)
; 	(parameters nil)
; 	(parameter nil)
; 	(start nil))
;     (unless (looking-at #r"^content-disposition\s-*:")
;       ;; #### there should be a proper parameter parsing function in VM
;       (error 'args-out-of-range "not looking-at content-disposition"))
;     (goto-char (match-end 0))
;     (skip-chars-forward "\t ")
;     (setq start (point))
;     ;; #### there should be a proper FWS unfolding function in VM
;     (while (and (search-forward "\n" nil t) (looking-at #r"[ \t]"))
;       (end-of-line))
;     (setq parameters
; 	  (sjt/vm-parse-structured-header (buffer-substring start (point))
; 					  ?\; t))
;     (setq parameters (delete-if
; 		      (lambda (x)
; 			(not (string= "filename"
; 				      (substring-no-properties x 0 8))))
; 		      parameters))
;     (setq parameters (stable-sort parameters #'string<))
;     (message "%S" parameters)))
;     (setq value ""
; 	  start 0)
;     (while parameters
;       (setq parameter (pop parameters))
;       (unless (string= "filename" (substring-no-properties parameter 0 8))
; 	(error 'invalid-state "parameter is not filename component" filename))
;       (setq parameter (substring-no-properties parameter 8))
;       (if (= (aref parameter 0) ?=)
; 	  (if (= (aref parameter 1) ?\")
; 	      (setq value
; 		    (concat value (substring-no-properties parameter 2 -1)))
; 	    (setq value (concat value (substring-no-properties parameter 1))))
; 	(when (= (aref parameter 0) ?*)
; 	  (setq parameter (substring-no-properties parameter 1)))
; 	(if (= (string-to-number parameter) start)
; 	    (incf start)
; 	  (error 'invalid-state "out-of-order filename component at " start))
; 	(
;     ))

;; #### This function is UNCHANGED from vm-mime.el eversion.
; (defun vm-mime-get-xxx-parameter-internal (name param-list)
;   "Return the parameter NAME from PARAM-LIST."
;   (let ((match-end (1+ (length name)))
; 	(name-regexp (concat (regexp-quote name) "="))
; 	(case-fold-search t)
; 	(done nil))
;     (while (and param-list (not done))
;       (if (and (string-match name-regexp (car param-list))
; 	       (= (match-end 0) match-end))
; 	  (setq done t)
; 	(setq param-list (cdr param-list))))
;     (and (car param-list)
; 	 (substring (car param-list) match-end))))

;; #### This function is UNCHANGED from vm-mime.el eversion.
; (defun vm-mime-get-parameter (layout param)
;   (let ((string (vm-mime-get-xxx-parameter 
; 		 param (cdr (vm-mm-layout-type layout)))))
;     (if string (vm-decode-mime-encoded-words-in-string string))))

;; #### This function is UNCHANGED from vm-mime.el eversion.
; (defun vm-mime-get-disposition-parameter (layout param)
;   (let ((string (vm-mime-get-xxx-parameter 
; 		 param (cdr (vm-mm-layout-disposition layout)))))
;     (if string (vm-decode-mime-encoded-words-in-string string))))

