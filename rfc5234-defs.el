;;; rfc5234-defs --- RFC 5234 core ABNF element definitions

(defvar rfc5234-prefixed)

(when rfc5234-prefixed
  ;; RFC 5234 core definitions of character classes represented as strings
  ;; #### Ranges assume ASCII collation order.
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
  (defconst rfc5234-wsp " \t")
  ;; RFC 5234 core definitions of tokens represented as regular expressions
  (defconst rfc5234-crlf "\r\n")
  (defconst rfc5234-lwsp #r"\(?:\(?:\r\n\)*[ \t]\)*") ; deprecated, see RFC
  )

(unless rfc5234-prefixed
  ;; RFC 5234 core definitions of character classes represented as strings
  ;; #### Ranges assume ASCII collation order.
  (defconst alpha "A-Za-z")		; case insensitive per RFC
  (defconst bit "01")
  (defconst char "\x00-\x7F")
  (defconst cr "\r")
  (defconst ctl "\x00-\x1F\x7F")
  (defconst digit "0-9")
  (defconst dquote "\"")
  (defconst hexdig "0-9A-Fa-f")		; case insensitive per RFC
  (defconst htab "\t")
  (defconst lf "\n")
  (defconst octet "\x00-\xFF")
  (defconst sp " ")
  (defconst vchar "!-~")
  (defconst wsp " \t")
  ;; RFC 5234 core definitions of tokens represented as regular expressions
  (defconst crlf "\r\n")
  (defconst lwsp #r"\(?:\(?:\r\n\)*[ \t]\)*") ; deprecated, see RFC
  )

