# my-elisp

Unlike __my-python__ which is mostly casually developed scripts for personal use, this collection of modules includes both casual shortcuts as well as more serious projects intended for inclusion in either XEmacs's standard Lisp library, or for submission to the VM mail user agent which is maintained separately, although packaged for XEmacs.

1.  bibtex-addons.el, marquee.el, utf16-tsv-to-utf8-csv.el

    Collections of utility functions for personal use.

2.  buffers-tab-hacks.el

    Various experiments for use with XEmacs's implementation of tabbed windows.

3.  coding-utilities.el

    Some utilities for working with various encodings, including editor commands for encoding and decoding both coded character sets and various MIME content transfer encodings.  Also includes extensions to handle GBK and GB 18030 encodings for Chinese which XEmacs's aging implementation lacks, as well as handling extended segments sometimes used for UTF-8 in ISO-2022-based encodings.  These two features are implemented with fairly high quality code. However, they are mostly going to be obsoleted by work in progress (very early stage) to implement the WHAT-WG recommended suite of encodings for the Web, which are generally supersets of the commonly used encodings, and so tend to be more robust to mislabeled MIME charsets in email and on the Web.

4.  jwz-thread.el, rfc5234-core.el, save-attachments.el, sjt-vm-addons.el

    These files contain what are intended to be production-quality additions to, or reimplementations of, facilities provided by the VM mail user agent for Emacsen.
    
