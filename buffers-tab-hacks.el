;;; buffers-tab-hacks.el --- Stephen's hacks for the buffers tab control

;; Copyright (C) 2016 Stephen J. Turnbull

;; Author: Stephen J. Turnbull <stephen@xemacs.org>
;; Created: 2016-06-17
;; Keywords: tab control, buffer list, gutter

;; This file is not part of XEmacs, but it is specific to XEmacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 2 of the License, or (at your
;; option) any later version.  Other terms may be available from the author.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in GNU.
 
;;; Commentary:

;; Various hacks for VM.
;;
;; 

;;; Code:

(defun buffers-tab-omit-vm-nonsummary (candidate ignore)
  "Return nil if CANDIDATE is a VM buffer, but not a Summary buffer.

Also ignore empty folder Summary buffers.
When an element of `buffers-tab-filter-functions', omit such buffers."
  (or (and (eq (symbol-value-in-buffer 'major-mode candidate) 'vm-summary-mode)
	   (> (length (symbol-value-in-buffer 'vm-message-list candidate)) 0))
      (not (memq (symbol-value-in-buffer 'major-mode candidate)
		 '(vm-mode vm-presentation-mode vm-virtual-mode vm-summary-mode)))))

(defun buffers-tab-sort-by-folder-size (buffers)
  (sort* buffers #'< :key (lambda (buffer)
			    (length (symbol-value-in-buffer 'vm-message-list
							    buffer)))))

(defun sjt/test-buffer-handling ()
  (interactive)
  (let ((l (buffer-list))
	(b (get-buffer "INBOX Summary")))
    (unless b (error 'invalid-state "no INBOX folder"))
    (setq l (delete-if-not (lambda (x)
			     (and (buffers-tab-omit-vm-nonsummary x b)
				  (select-buffers-tab-buffers-by-mode x b)))
			   l))
    (setq l (mapcar (lambda (bb)
		      (list (length (symbol-value-in-buffer 'vm-message-list
							    bb))
			    (substring (buffer-name bb) 0 6)))
		    (buffers-tab-sort-by-folder-size l)))
    l))

(defun buffers-tab-truncate-vm-summary-name (buffer)
  "Remove \" Summary\" from VM summary buffer names.

Return the result of calling `format-buffers-tab-line' on the value."
  (let ((name (format-buffers-tab-line buffer)))
    (if (eq 'vm-summary-mode (symbol-value-in-buffer 'major-mode buffer))
	(substring name 0 -8)
      name)))

(provide 'buffers-tab-hacks)
;;; buffers-tab-hacks.el ends here
