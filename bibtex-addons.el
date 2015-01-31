;; (add-hook 'bibtex-clean-entry-hook 'bibtex-clean-isbn-function)
(defconst bibtex-clean-isbn-re (format #r",\s-*%c\s-*isbn\s-*=\s-*{978-0-}" ?\n))
(defun bibtex-clean-isbn-function ()
  "Remove an unused isbn entry.
For use in `bibtex-clean-entry-hook'."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward bibtex-clean-isbn-re nil t)
      (replace-match ""))))

