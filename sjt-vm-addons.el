(provide 'sjt-vm-addons)

(require 'rfc5234-core)

;;; End-user utilities (not just VM)

(defun sjt/find-quoted-hex (&optional quote-chars)
  "Search forward from point for quoted hex code.

Optional QUOTE-CHARS is a string containing characters that are recognized
as hex-quoting characters, \"=%\" by default."
  (interactive "i")
  (re-search-forward (concat ?[ (or quote-chars "%=") ?] "[0-9A-Fa-f]\{2\}")))


(defun sjt/replace-quoted-hex-with-bytes (start end-marker &optional buffer)
  "Replace quoted hex with bytes in region.

The character at START is taken as the quote character.  Replaces until end
of region or parsing fails.
BUG: The quote character is deleted before the parsing of the hex code, and
not replaced if the parse fails."
  (interactive "r")
  (save-excursion
    (goto-char start buffer)
    ;; Handle both MIME quoted-printable (?= quoted) and RFC 2231 and
    ;; URL-escaped (?%-quoted) encodings.
    ;; #### Assumes first character (at point) is quote character.
    ;; #### Currently stops if it finds an unescaped character.
    (let ((quote-char (char-after)))
      (while (and (= (char-after) quote-char)
		  (< (point) end-marker))
	(delete-char 1)
	(insert (int-to-char
		 (string-to-number (buffer-substring (point) (+ (point) 2))
				   16)))
	(delete-char 2)))))

;;; RFC 5322 lexical tokens
;;
;; Parenthesization and content extraction is left to calling code.

;; #### Do we need the *-re versions?
;; #### We should optimize by leaving out the return values for non-tokens.

;; Mly's rfc822.el uses regexps a lot more, and probably is faster for that.

;; RFC 5322 utility classes

(defconst rfc5322-quoted-pair-re
  (concat #r"\\[" rfc5234-vchar rfc5234-wsp "]")
  "Match one RFC 5322 quoted pair.")

(defsubst rfc5322-parse-quoted-pair ()
  "Parse one RFC 5322 quoted pair."
  (when (looking-at rfc5322-quoted-pair-re)
    (forward-char 2)
    (list (- (point) 2) (point))))	; for rfc5322-skip-qcontent

;; These two defconsts aren't real RFC 5322 ABNF.
(defconst rfc5322-vchars-re (concat "[" rfc5234-vchar "]+"))
(defconst rfc5322-wschars-re (concat rfc5234-wsp-re "+"))

(defconst rfc5322-specials #r"].()<>@,;:\\\"[")
(defconst rfc5322-specials-re (concat "[" rfc5322-specials "]"))

;; RFC 5322 whitespace and comments
;; These are classed together because comments are semantic nulls in
;; RFC 5322 processing.  Higher levels never see them.

;; Don't use rfc5234 LWS.
(defconst rfc5322-fws-re
  (concat "\\(?:" rfc5234-wsp-re "*" rfc5234-crlf "\\)?"
	  rfc5234-wsp-re "+")
  "Match RFC 5322 folded whitespace.")	; #### or with obs-fws

(defsubst rfc5322-parse-fws ()
  "Parse RFC 5322 folding whitespace.

This definition tries to prevent blank lines itself, but care should be
taken to avoid repetition of this expression when parsing."
  (when (looking-at rfc5322-fws-re)
    (let ((start (point)))
      (goto-char (match-end 0))
      (list start (point)))))

(defconst rfc5322-ctext-re "[]!-'*-[^-~]"
  "Match RFC 5322 ctext.")

(defsubst rfc5322-parse-ctext ()
  "Parse one RFC 5322 ctext character."
  (when (looking-at rfc5322-ctext-re)
    (forward-char 1)
    (list (1- (point)) (point))))

;; #### I think this is an accurate translation of the ABNF.
;; However, it is unclear what is to be done with an unclosed open parenthesis
;; followed by a comment.  This code parses as an open parenthesis, followed
;; by an optional run of ctext, followed by a comment.  Perhaps it would be
;; better to parse the *inner* open parenthesis as unclosed.  That would be
;; much more complicated to implement, though.
(defsubst rfc5322-parse-ccontent ()
  "Parse RFC 5322 ccontent and position point at end."
  (cond ((rfc5322-parse-quoted-pair))
	((looking-at rfc5322-ctext-re) (forward-char 1))
	((char= ?\( (char-after)) (rfc5322-parse-comment))))

(defsubst rfc5322-parse-comment ()
  "Parse RFC 5322 comment, position point at end, and return (START END).

If not looking-at \"(\" when called, return nil.
If the opening parenthesis is not matched, move past the parenthesis,
returning (START (1+ START))."
  (when (char= ?\( (char-after))
    (let ((start (point)))
      (forward-char 1)
      (while (or (rfc5322-parse-fws) (rfc5322-parse-ccontent)))
      (if (char= ?\) (char-after))
	  (forward-char 1)
	(goto-char (1+ start)))
      (list start (point)))))

(defsubst rfc5322-parse-cfws ()
  "Match RFC 5322 CFWS, position point at end, and return (START END)."
  (let ((start (point)))
    (while (or (rfc5322-parse-fws) (rfc5322-parse-comment)))
    (when (not (= start (point)))	; If not moved, return nil.
      (list start (point)))))

;; RFC 5322 atoms

(defconst rfc5322-atext-re
  (concat "[" rfc5234-alpha rfc5234-digit "!#$%&'*+-/=_^_`{|}~" "]+")
  "This is a non-empty sequence of atext as defined in RFC 5322.")

(defsubst rfc5322-parse-atom ()
  "Parse RFC 5322 atom.  Returns (START END) for the atom only.

Point may be past END.  Original point is forgotten."
  (let ((start (point)) end)
    ;; We don't care if the CFWS parses succeed.
    ;; Note that atom atom allows a nonempty blank line.
    (rfc5322-parse-cwfs)
    (cond ((looking-at rfc5322-atext-re)
	   (setq start (point))
	   (goto-char (match-end 0))
	   (setq end (point))
	   (rfc5322-parse-cwfs)
	   (list start end))
	  (t (goto-char start)
	     nil))))

(defsubst rfc5322-parse-dot-atom ()
  "Parse RFC 5322 dot atom.  Returned (START END) bounds the atom.

If the parse fails (no atom), resets point to original position.
This does not permit CFWS within the dot-atom.
RFC 822 doesn't define this element and doesn't explicitly allow CFWS, but
the examples regularly include it.  RFC 2822 defines it the same way, and
in addresses obsoletes CFWS.
Point may be past END.  Original point is forgotten."
  (let ((origin (point)) start end)
    ;; We don't care if the CFWS parses succeed.
    (rfc5322-parse-cwfs)
    (if (and (looking-at rfc5322-atext-re)
	     (setq start (point))
	     (goto-char (match-end 0))
	     (setq end (point))
	     (while (and (char= ?\. (char-after))
			 ;; #### This probably signals at end-of-buffer.
			 (progn (forward-char 1) t)
			 (looking-at rfc5322-atext-re)
			 (goto-char (match-end 0))
			 (setq end (point))))
	     (= end (point)))
	(progn
	  (rfc5322-parse-cwfs)
	  (list start end))
      (goto-char origin)
      nil)))

;; RFC 5322 quoted strings

(defconst rfc5322-qtext-re "[\x21\x23-\x5B\x5D-\x7E]+")

;; #### Make the parse/skip nomenclature consistent.  Document it.
;; Maybe we don't need it?  This function needs to return status.
(defsubst rfc5322-parse-qcontent ()
  "Somewhat more aggressive than RFC 5322 definition."
  (let ((origin (point)))		; use 'origin for if-moved
    (while (or (when (looking-at rfc5322-qtext-re)
		 (goto-char (match-end 0)))
	       (rfc5322-parse-quoted-pair)))
    (setq end (point))
    (unless (= origin end)
      (list origin end))))

(defsubst rfc5322-parse-quoted-string ()
  "Parse RFC 5322 quoted string.

Returned (START END) bounds the quote, including the DQUOTEs."
  (let ((origin (point)) start end)
    ;; We don't care if the CFWS parses succeed.
    (rfc5322-parse-cwfs)
    (if (and (char= ?\" (char-after))
	     (setq start (point))
	     (progn (forward-char 1) t)
	     (while (or (rfc5322-parse-fws)
			(rfc5322-parse-qcontent)))
	     (char= ?\" (char-after))
	     (progn (forward-char 1) t)
	     (setq end (point)))
	(progn
	  (rfc5322-parse-cwfs)
	  (list start end))
      (goto-char origin)
      nil)))

;; RFC 5322 higher-level utilities

(defsubst rfc5322-parse-word ()
  (or (rfc5322-parse-atom)
      (rfc5322-parse-quoted-string)))

(defsubst rfc5322-parse-phrase ()
  (let ((origin (point)) start end)
    (rfc5322-parse-cwfs)
    (setq start (point))
    (while (setq val (rfc5322-parse-word))
      (setq end (nth 1 val)))
    (if end
	(list start end)
      (goto-char origin)
      nil)))

(defsubst rfc5322-parse-unstructured ()
  (let ((start (point)))
    (while (if (rfc5322-parse-fws)
	       (if (not (looking-at rfc5322-vchars-re))
		   (error 'invalid-state
			  (format "blank line in field at position %d"
				  (point)))
		 (goto-char (match-end 0)))
	     (when  (looking-at rfc5322-vchars-re)
		 (goto-char (match-end 0)))))
    (when (looking-at rfc5322-wschars)
      (goto-char (match-end 0)))
    (list start (point))))

;;; Header parsing

;; #### This should be sjt/vm-parse-structured-FIELD.
(defun sjt/vm-parse-structured-header (field-body &optional sepchar keep-quotes)
  "Return a list of parameters parsed from FIELD-BODY per RFC 2047. #### CHECK!!
SEPCHAR is a character used to separate parameters in the field body, default
  semicolon.
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
	       (cond ((null char))
		     ((looking-at "[ \t\n\r\f]")
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

;;; RFC 2231 handling

;; RFC 2231 provides a robust protocol to handle multilingual or
;; multicharset values in the header.  RFC 2047 is frequently used
;; for Japanese in the 'name' parameter of Content-Type, but this
;; is prohibited by the RFC (not sure why).  Pragmatically, long
;; parameter values will need to be split across lines to stay within
;; the mandated 76-character line limit.  Doing this with MIME words
;; is problematic because of less robust treatment by MUAs of FWS.

;; RFC 2231 is specifically for use in parameters in Content-Type and
;; Content-Disposition fields.  Like RFC 2047, RFC 2231 permits
;; arbitrary text to be encoded as multiple RFC 5322 atoms or quoted
;; text that fits within the recommended line length.  (Unlike RFC
;; 2047, RFC 2231 doesn't mandate a maximum line length.)

(defconst sjt/rfc822-specials rfc5322-specials)
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

;; Used in my version of vm-mime-get-xxx-parameter.
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

SEGMENT := NAME '*=' ENCODED-VALUE
         | NAME '*0=' VALUE
         | NAME '*0*=' [CHARSET] <'> [LANGUAGE] <'> ENCODED-VALUE
         | NAME '*' NUMBER '=' VALUE
         | NAME '*' NUMBER '*=' ENCODED-VALUE

where NAME, CHARSET, and LANGUAGE are tokens, NUMBER is a decimal natural
number, VALUE is a token or quoted string, and ENCODED-VALUE is %-encoded.
In the return value NAME, LANGUAGE, and VALUE are strings, NUMBER is a fixnum,
and CHARSET is a symbol.  VALUE may be encoded but is not delimited by quotes.

It's not clear from RFC 2231 whether in the third form the apostrophes can be
omitted if CHARSET and LANGUAGE are both empty, but parser treats them as
required."
  (interactive (list (point-at-eol)))

  (let (name number charset lang value	; return values
	extendedp)			; temporary variables

    (labels
	((parse-value (type)
	   (let ((start (point)))
	     (skip-chars-forward type end)
	     (unless (= start (point))
	       (buffer-substring start (point)))))
	 )

      ;; Parse attribute name.
      (skip-chars-forward " \t" end)
      (setq name (or (parse-value sjt/rfc2231-attribute-characters)
		     (error 'args-out-of-range "expected non-empty name")))

      ;; Parse segment information.
      (unless (= ?= (char-after))
	;; check that we got *
	(if (not (= ?* (char-after)))
	    (error 'args-out-of-range "expected ?* or ?= at point")
	  (forward-char 1)		; skip over *
	  ;; look for a sequence number
	  (setq number (parse-value "0-9"))
	  (if (null number)
	      (setq extendedp t)	; no number, but encoded value
	    (when (= ?* (char-after))
	      (setq extendedp t)	; we have an encoded value
	      (forward-char 1))		; skip over *
	    )))

      ;; Parse =.
      (unless (= ?= (char-after))
	;; check that we got =
	(error 'args-out-of-range "expected ?= at point"))
      (forward-char 1)			; skip over =

      ;; Parse charset or language information.
      ;; Charset/lang is allowed only on first segment or only segment.
      (when (and extendedp (or (eql number 0) (eql number nil)))
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

(defun sjt/decode-rfc2231-token (start end coding)
  (interactive "r\nzCoding system: ")
  (while (re-search-forward "%\\([0-9A-Fa-f][0-9A-Fa-f]\\)" nil t)
    (goto-char (match-end 0))
    (insert (int-to-char (string-to-number (match-string 1) 16)))
    (delete-region (match-beginning 0) (match-end 0)))
  (decode-coding-region (point) (point-max) coding))

(defconst sjt/parse-rfc2231-filename-parameter-re
   (concat #r"\(filename\*\)"		; 1 "filename*"
	   #r"\(?:\([0-9]+\)\*\)?="	; 2 ordinal
	   ;; #### Optional "'" is not right.
	   #r"\(?:\([^']*\)'\)?"	; 3 encoding
	   #r"\(?:\([^']*\)'\)?"	; 4 language
	   #r"\([^ ;]*\);?"		; 5 text
	   ))

(defun sjt/test-parse-rfc2231-filename-parameter-re ()
  (interactive)
  (back-to-indentation)
  (save-match-data
    (looking-at sjt/parse-rfc2231-filename-parameter-re)
    (list (match-string 1)
	  (match-string 2)
	  (match-string 3)
	  (match-string 4)
	  (match-string 5))))

(defun sjt/forward-header-field (&optional count buffer)
  "Move forward COUNT \(default 1\) header fields in BUFFER \(default current\).
If negative, move backward.  Return t if COUNT fields were moved, else nil.
The header field includes the trailing newline if any.  Does not change mark."
  (interactive "pi")
  ;; #### Should save-excursion here?
  (set-buffer (or buffer (current-buffer)))
  (setq count (or count 1))

  (labels
      ((bohp ()
	 ;; beginning-of-header-p
	 ;; #### Close enough as long as we count From as part of the header.
	 (or (bobp)
	     (save-excursion
	       (forward-line -1)
	       (and (eolp) (bolp)))))
       (eohp ()
	 ;; end-of-header-p
	 (or (eobp)
	     (and (eolp) (bolp))))
       (bohfp ()
	 ;; beginning-of-header-field-p
	 (looking-at "^[A-Za-z]"))	; #### Not RFC-ly correct.
       (hfbp ()
	 ;; header-field-boundary-p
	 ;; Same as end-of-header-field-p except at the beginning of header.
	 ;; beginning of header is annoying to check in mbox.
	 (or (eohp) (bohfp))))
    (let ((direction (signum count))
	  (count (abs count))
	  (pt (point))
	  (done nil))
      ;; Move once to handle middle of line case.
      (cond ((< direction 0) (beginning-of-line))
	    ((> direction 0) (forward-line 1)))
      ;; If we moved and we're at a boundary, count it.
      (when (and (= pt (point)) (hfbp))
	(decf count))
      ;; Do remaining movement.
      (while (and (> count 0) (not done))
	(setq pt (point))
	(while (not (hfbp))
	  (forward-line direction))
	(unless (= pt (point))
	  (decf count))
	(when (or (and (< direction 0) (bohp))
		  (and (> direction 0) (eohp)))
	  (setq done t))))))
    

(defun sjt/mark-header-field (&optional count)
  "Mark the text from point until encountering the end of a header field.
With optional argument COUNT, mark COUNT fields.  COUNT negative means to
mark backwards."
  (interactive "p")
  (mark-something 'sjt/mark-header-field 'sjt/forward-header-field count))


(defun sjt/check-field (field-name-re)
  (interactive "sField name regexp: ")
  (unless (looking-at (concat #r"^\(" field-name-re #r"\)\s-*:"))
    (error 'args-out-of-range (format "not looking-at %s" field-name-re))))


;; #### Should use #'sjt/vm-parse-rfc2231-segment.
(defun sjt/vm-get-filename-from-content-disposition ()
  (interactive)
  (let ((case-fold-search t)
	(value nil)
	(encoding nil)
	(parameters nil)
	(parameter nil)
	(start nil))
    (save-match-data
      (sjt/check-field "content-disposition")
      (goto-char (match-end 0))
      (setq parameters
	    (sjt/vm-parse-structured-header (buffer-substring
					     (point)
					     (save-excursion
					       (sjt/forward-header-field 1)
					       (point)))
					    ?\; t))
      (setq parameters (delete-if
			(lambda (x)
			  (not (string-match "^filename" x)))
			parameters))
      (setq parameters (stable-sort parameters #'string<))
      (setq value "")
      (setq start 0)
      (while parameters
	(setq parameter (pop parameters))
	(string-match sjt/parse-rfc2231-filename-parameter-re parameter)
	(let ((filename (match-string 1 parameter))
	      (ordinal (match-string 2 parameter))
	      (enc (match-string 3 parameter))
	      (language (match-string 4 parameter))
	      (text (match-string 5 parameter)))
	  (setq ordinal (if ordinal (string-to-number ordinal) 0))
	  (when enc (setq enc (intern (downcase enc))))
	  (when language (setq language (downcase language)))
	  (when (not (string= filename "filename*"))
	    (error 'args-out-of-range "not a filename parameter" filename))
	  (unless (= start ordinal)
	    (error 'args-out-of-range "missing filename parameter" start))
	  (if (= start 0)
	      (setq encoding enc)
	    (when (or enc language)
	      (error 'args-out-of-range "encoding or language after start"
		     enc language)))
	  (setq value (concat value text))
	  (incf start))))
    ;; #### should use parse-rfc2231-token
    (with-temp-buffer
      (insert value)
      (goto-char (point-min))
      (while (not (eobp))
	(if (not (equal (char-after) ?%))
	    (forward-char 1)
	  (delete-char 1)
	  (insert (int-to-char
		   (string-to-number (buffer-substring (point) (+ (point) 2))
				     16)))
	  (delete-char 2)))
      (decode-coding-region (point-min) (point-max) encoding)
      (setq value (buffer-substring)))
    (message "%s" value)
    value))


;;; Monkey-patch VM stuff

(defun vm-mime-get-xxx-parameter (name param-list)
  "Return the parameter NAME from PARAM-LIST.

If parameter value continuations was used, i.e. the parameter was split into
shorter pieces, rebuild it from them."
  ;; (message "Retrieving parameter %s at %s in %s"
  ;;          name (point) (current-buffer))
  (let ((parsed-parameters (mapcar #'sjt/vm-parse-rfc2231-segment-string
				   param-list))
	(count -1)
	parsed-parameter charset value-charset can-display need-conversion
	start number value)
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

;; #### This function is UNCHANGED from vm-mime.el version.
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

