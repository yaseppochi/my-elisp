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
  (or (null candidate)			; #### shouldn't be needed!
                                        ; Why are we passed nil here? See also
                                        ; #'buffers-tab-sort-by-folder-date.
      (and (eq (symbol-value-in-buffer 'major-mode candidate) 'vm-summary-mode)
	   (> (length (symbol-value-in-buffer 'vm-message-list candidate)) 0))
      (not (memq (symbol-value-in-buffer 'major-mode candidate)
		 '(vm-mode vm-presentation-mode vm-virtual-mode vm-summary-mode)))))

(defun buffers-tab-sort-by-folder-size (buffers)
  (sort* buffers #'< :key (lambda (buffer)
			    (length (symbol-value-in-buffer 'vm-message-list
							    buffer)))))

;; In fact, this seems quite fast enough, with 43 VM buffers.
;; #### We can speed this up by computing timestamps and saving them in
;; buffer-locals before sorting.
;; #### We can speed this up more by checking timestamps at mail getting time.
(defun sjt/vm-youngest-date-of-folder (buffer)
  (with-current-buffer buffer
    (let ((date (vm-so-sortable-datestring (car vm-message-list)))
	  date2)
      (loop
	for msg in (cdr vm-message-list)
	do
	(setq date2 (vm-so-sortable-datestring msg))
	(when (string< date date2)
	  (setq date date2)))
      date)))

(defun buffers-tab-sort-by-folder-date (buffers)
  "Sort by the date of the youngest message in the folder."
  (if (and buffers			; #### shouldn't be needed!
                                        ; Why are we passed nil here? See also
                                        ; #'buffers-tab-omit-vm-nonsummary.
	   (eq (symbol-value-in-buffer 'major-mode (car buffers))
	       'vm-summary-mode))
      ;; Don't use #'string>, it's a macro from bbdb.
      (nreverse (sort* buffers #'string< :key #'sjt/vm-youngest-date-of-folder))
    (sort* buffers #'>
	   :key (lambda (buffer)
		  (with-current-buffer buffer
		    ;; #'visited-file-modtime should return zero if unvisited.
		    (car (visited-file-modtime)))))))

(defun sjt/test-buffer-handling (&optional func)
  (interactive "S")
  (if func
      (unless (member func '(buffers-tab-sort-by-folder-size
				buffers-tab-sort-by-folder-date))
	(error 'wrong-type-argument "buffers-tab sort function" func))
    (setq func buffers-tab-sort-function))
  (let ((l (buffer-list))
	(b (get-buffer "INBOX Summary")))
    (unless b (error 'invalid-state "no INBOX folder"))
    (setq l (delete-if-not (lambda (x)
			     (and (buffers-tab-omit-vm-nonsummary x b)
				  (select-buffers-tab-buffers-by-mode x b)))
			   l))
    (setq l (mapcar (lambda (bb)
		      (list (cond ((eq func #'buffers-tab-sort-by-folder-size)
				   (length (symbol-value-in-buffer
					    'vm-message-list
					    bb)))
				  ((eq func #'buffers-tab-sort-by-folder-date)
				   (sjt/vm-youngest-date-of-folder bb)))
			    (substring (buffer-name bb) 0 6)))
		    (funcall func l)))
    (cons func l)))

(defun buffers-tab-truncate-vm-summary-name (buffer)
  "Remove \" Summary\" from VM summary buffer names.

Return the result of calling `format-buffers-tab-line' on the value."
  (let ((name (format-buffers-tab-line buffer)))
    (if (eq 'vm-summary-mode (symbol-value-in-buffer 'major-mode buffer))
	(substring name 0 -8)
      name)))

(defun sjt/switch-to-buffer-list-excludes ()
  "List buffers which are VM buffers but not Summary buffers."
  (delete-if (lambda (x) (buffers-tab-omit-vm-nonsummary x x)) (buffer-list)))

(defvar sjt/switch-to-buffer-exclude-list-function
  #'current-buffer
  "Return list of buffers excluded from completion in `sjt/switch-to-buffer'.

Default is `current-buffer' for backward compatibility.")

(defun sjt/switch-to-buffer (bufname &optional norecord)
  "Select buffer BUFNAME in the current window.
BUFNAME may be a buffer or a buffer name and is created if it did not exist.
Optional second arg NORECORD non-nil means do not put this buffer at the
front of the list of recently selected ones.

WARNING: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead.  That avoids messing with
the window-buffer correspondences."
  (interactive
   (list (read-buffer "Switch to buffer: "
		      (other-buffer (current-buffer))
		      nil
		      (cons (current-buffer)
			    (funcall
			     sjt/switch-to-buffer-exclude-list-function)))))
  ;; #ifdef I18N3
  ;; #### Doc string should indicate that the buffer name will get
  ;; translated.
  ;; #endif
  (if (eq (minibuffer-window) (selected-window))
      (error "Cannot switch buffers in minibuffer window"))
  (if (window-dedicated-p (selected-window))
      (error "Cannot switch buffers in a dedicated window"))
  (let (buf)
    (if (null bufname)
	(setq buf (other-buffer (current-buffer)))
      (setq buf (get-buffer bufname))
      (if (null buf)
	  (progn
	    (setq buf (get-buffer-create bufname))
	    (set-buffer-major-mode buf))))
    (push-window-configuration)
    (set-buffer buf)
    (set-window-buffer (last-nonminibuf-window) buf norecord)
    buf))

(provide 'buffers-tab-hacks)
;;; buffers-tab-hacks.el ends here
