(require 'rfc2047)
(provide 'coding-utilities)

(defvar ok-charsets '(ascii))

;; #### Probably should combine next two.
;;;###autoload
(defun sjt/find-bad-charsets ()
  (interactive)
  (goto-char (point-min))
  (while (memq (char-charset (char-after)) ok-charsets)
    (forward-char)))

(defun find-non-Japanese-characters (&optional charsets)
  (interactive "xCharset list: ")
  (setq charsets (or charsets '(ascii japanese-jisx0208)))
  (while (member (char-charset (char-after)) charsets) (forward-char)))

(defun sjt/decode-gbk-region (start end)
  (interactive "r")
  (let ((process-coding-system-alist '(("iconv" . (utf-8 . binary)))))
    ;;(message "%d %d" (marker-position sm) (marker-position em))
    ;;(sit-for 3)
    (call-process-region start end "iconv" t t nil "-t" "utf-8" "-f" "gbk")))
(defun sjt/decode-base64-gbk-region (start end)
  (interactive "r")
  (let ((sm (set-marker (make-marker) start))
	(em (set-marker (make-marker) end)))
    (base64-decode-region sm em)
    ;;(message "%d %d" (marker-position sm) (marker-position em))
    (sjt/decode-gbk-region sm em)))

;;;###autoload
(defun sjt/decode-region (b e codec)
  "Decode the current region with the method tagged by CODEC.
CODEC is a single character.  The following codecs are defined:
  b BASE64
  e EUC-JP
  g GBK
  i ISO-2022-JP
  r RFC 2047
  s Shift JIS
  u UTF-8"
  (interactive "r\ncCodec [begirsu]: ")
  (cond
	((eq codec ?b) (base64-decode-region b e))
	((eq codec ?e) (decode-coding-region b e 'euc-jp))
	((eq codec ?g) (sjt/decode-gbk-region b e))
	((eq codec ?i) (decode-coding-region b e 'iso-2022-jp))
	((eq codec ?r) (rfc2047-decode-region b e))
	((eq codec ?s) (decode-coding-region b e 'shift_jis))
	((eq codec ?u) (decode-coding-region b e 'utf-8))))

;;;###autoload
(defun sjt/encode-region (b e codec)
  "Encode the current region with the method tagged by CODEC.
CODEC is a single character.  The following codecs are defined:
  b BASE64
  e EUC-JP
  g GB2312
  i ISO-2022-JP
  r RFC 2047
  s Shift JIS
  u UTF-8"
  (interactive "r\ncCodec [begirsu]: ")
  (cond
	((eq codec ?b) (base64-encode-region b e))
	((eq codec ?e) (encode-coding-region b e 'euc-jp))
	((eq codec ?g) (encode-coding-region b e 'gb2312))
	((eq codec ?i) (encode-coding-region b e 'iso-2022-jp))
	((eq codec ?r) (rfc2047-encode-region b e))
	((eq codec ?s) (encode-coding-region b e 'shift_jis))
	((eq codec ?u) (encode-coding-region b e 'utf-8))))

(defconst sjt/extended-segment-parser-re
  (concat "\033%/"			; extended segment escape
	  "[0-4]"			; if we grok it, we should know width
	  "\\([\200-\377][\200-\377]\\)" ; length specification, base 128
	  "\\([^\002]*\\)"		; TODO: should match only Latin 1
	  "\002")			; ASCII STX = START OF TEXT
  "Regexp to detect and parse X Compound Text extended segment parameters.")

(defconst sjt/coding-system-x11-aliases
  ;; TODO: add to this list.
  '(("iso8859-15" . iso-8859-15))
  "Coding system aliases used in X Compound Text extended segments.

NB: we sanity check all uses of this mapping, so even if the particular
XEmacs doesn't know that coding system, please report as a bug if you see
a broken paste of the form \"^[%/1\200\227iso8859-15\002...\".")

(defun sjt/find-coding-system-from-alias (name)
  (setq name (downcase name))
  (cond ((find-coding-system (intern name)))
        ((let ((pair (assoc name sjt/coding-system-x11-aliases)))
           (when pair (find-coding-system (cdr pair)))))
        ;; we could try guessing here, eg a prefix "iso" -> "iso-"
        (t nil)))

(defun sjt/select-convert-from-text (selection type value)
  "Converts a text selection to a Lisp string, handling X11 extended segments."
  (when (stringp value)
    (catch 'unconvertible
      (while (string-match sjt/extended-segment-parser-re value)
	(let* ((cs (find-coding-system (sjt/find-coding-system-from-alias
                                        (match-string 2 value))))
	       (segstart (match-end 0))
	       (lenspec (match-string 1 value))
	       (length (+ (* 128 (- (char-to-int (aref lenspec 0)) 128))
			  (- (char-to-int (aref lenspec 1))
                             128
                             (length (match-string 2))
                             1)))
	       (segend (+ segstart length)))
	  (unless cs (throw 'unconvertible nil))
	  (setq value (concat (substring value 0 (match-beginning 0))
			      (decode-coding-string
			       (substring value segstart segend)
                               cs)
			      ;; should eliminate while in favor of
			      ;; tail-recursion on this substring
			      (substring value segend))))))
    value))

(defun sjt/toggle-compound-text-converter ()
  "Toggle the recognition and conversion of compound text extended segments."
  (interactive)
  (when (emacs-version>= 21 4)
    (let ((cvt (cond ((eq (cdr (assoc 'TEXT selection-converter-in-alist))
			  'select-convert-from-text)
                      'sjt/select-convert-from-text)
                     (t 'select-convert-from-text)))
          (alist selection-converter-in-alist))
      ;; no mercy!!
      (while alist
        (when (memq (cdar alist)
                    '(select-convert-from-text sjt/select-convert-from-text))
          (setcdr (car alist) cvt))
        (setq alist (cdr alist)))
      (message "%s" cvt))))

;; OK, do it! ... or not ...
;(sjt/toggle-compound-text-converter)

