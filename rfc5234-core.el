;;; rfc5234-core --- RFC 5234 core ABNF element definitions

(provide 'rfc5234-core)

;; In supporting RFC 5322 parsing, it is frequently convenient to have both
;; a regular expression version and a character class version.  Define
;; regular expressions from classes as needed.

;; Ranges assume ASCII collation order, but XEmacs doesn't pay attention
;; to POSIX collation order, so this is OK.

;; RFC 5234 core definitions of character classes represented as strings
(defconst rfc5234-alpha "A-Za-z")	; case insensitive per RFC
(defconst rfc5234-bit "01")
(defconst rfc5234-char "\x00-\x7F")
(defconst rfc5234-cr "\r")
(defconst rfc5234-ctl "\x00-\x1F\x7F")
(defconst rfc5234-digit "0-9")
(defconst rfc5234-dquote "\"")
(defconst rfc5234-hexdig "0-9A-Fa-f")	; case insensitive per RFC
(defconst rfc5234-htab "\t")
(defconst rfc5234-lf "\n")
(defconst rfc5234-octet "\x00-\xFF")
(defconst rfc5234-sp " ")
(defconst rfc5234-vchar "!-~")
(defconst rfc5234-wsp (concat rfc5234-sp rfc5234-htab))

;; RFC 5234 core definitions of tokens represented as regular expressions
(defconst rfc5234-crlf-re (concat rfc5234-cr rfc5234-lf))
(defconst rfc5234-wsp-re (concat "[" rfc5234-wsp "]"))
(defconst rfc5234-lwsp
  (concat #r"\(?:\(?:" rfc5234-crlf-re #r"\)?" rfc5234-wsp-re #r"\)*")
  "Deprecated, see RFC.")

