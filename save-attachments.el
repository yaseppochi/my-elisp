;;; save-attachments --- save all attachments in a VM folder

;; Copyright (C) 2018 Stephen J. Turnbull

;; Author: Stephen J. Turnbull <stephen@xemacs.org>
;; Created: 2016-07-10
;; Keywords: VM, attachments

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

;; Save all attachments in a VM folder (including virtual folders).
;;
;; We need to improve parsing of RFC 2231-encoded fields, and handle
;; RFC 2047 encoding too.

(defvar sjt/debug-save-attachments nil)

(defun sjt/vm-author-split-address-of (message)
  (labels ((get-mailbox (access message)
	     (vm-su-do-principal message)
	     (let* ((message (vm-real-message-of message))
		    (field-body  (funcall access message)))
	       (when sjt/debug-save-attachments
		 (message "%s %s" access field-body)
		 (sit-for 0.1))
	       (and field-body (split-string field-body "@")))))
    (let* ((sa (get-mailbox #'vm-from-of message))
	   (mb (nth 0 sa)))
      (cond ((null mb) "*no from or reply-to*")
	    ((not (string= mb "ryuugaku")) sa)
	    (t (or (get-mailbox #'vm-reply-to-of message) sa)))
      )))


(defun sjt/staff-address-p (address)
  (or (string-match ".*tsukuba\\.ac\\.jp$" (nth 1 address))
      (member address
	      '(("naoko.kaida" "gmail.com")
		("kurino" "econ.keio.ac.jp")
		))))


(defun sjt/vm-intl-students-author-split-address-alist (&optional folder)
  "Return an alist of (MESSAGE-NUMBER MAILBOX) pairs for VM FOLDER.

The mailbox is the purported author.
Mailman author_is_list is handled."
  (set-buffer (or folder "(intl-students)"))
  (let ((ml vm-message-list)
	m al sa)
    (while ml
      (setq m (car ml)
	    ml (cdr ml)
	    sa (sjt/vm-author-split-address-of m))
      (unless (and (not (string= "ryuugaku" (nth 0 sa)))
		   (sjt/staff-address-p sa))
	(setq al (cons (cons (vm-number-of m) sa)
		       al))))
    (nreverse al)))


(defvar sjt/academic-year "2018")
(defun sjt/attachment-root ()
  (file-name-as-directory (expand-file-name sjt/academic-year "~/VM/FILES")))

;; #### These need to be checked against RFC 5322.
(defconst sjt/mailbox-re #r"\<[-a-zA-Z0-9._]+")
(defconst sjt/domain-re #r"[-a-zA-Z0-9._]+\>")
(defconst sjt/address-re
  (concat #r"\(" sjt/mailbox-re #r"\)@\(" sjt/domain-re #r"\)"))

(defun sjt/scarf-addresses-from-presentation ()
  (save-excursion
    ;; #### This sequence is from vm-reply.el:vm-yank-message-presentation.
    ;; Gotta be a better way!
    (vm-select-folder-buffer-and-validate 1 (vm-interactive-p))
    (vm-present-current-message)
    (vm-show-current-message)
    (vm-select-folder-buffer)
    (when vm-presentation-buffer
      (set-buffer vm-presentation-buffer))

    (goto-char (point-min))
    (search-forward "\n\n")
    ;; #### This regular expression is a hack.
    (let (addresses)
      (while (re-search-forward sjt/address-re nil t)
	(let ((split-address (list (match-string 1) (match-string 2))))
	  (unless (sjt/staff-address-p split-address)
	    (push split-address addresses))))
      (delete-dups addresses))))

(defvar sjt/read-attachment-directory-additional-addresses nil)
(defun sjt/read-attachment-directory ()
  (vm-select-folder-buffer)
  (let* ((prompt "Save attachments for: ")
	 (dir (sjt/attachment-root))
	 (split-address
	  (sjt/vm-author-split-address-of (car vm-message-pointer)))
	 (default (nth 0 split-address))
	 (must-match nil))
    (setq sjt/read-attachment-directory-additional-addresses
	  (mapcar #'car (sjt/scarf-addresses-from-presentation)))
    (when (sjt/staff-address-p split-address)
      (setq default
	    (or (car sjt/read-attachment-directory-additional-addresses) "")))
    (list
     (expand-file-name
      (read-file-name prompt dir default must-match default
		      'sjt/read-attachment-directory-additional-addresses)
      dir))))


(defun sjt/vm-intl-student-save-all-attachments (attachment-directory
						 &optional dryrun)
  
  (interactive (nconc (sjt/read-attachment-directory)
		      (list current-prefix-arg)))
  
  (let* ((gitdir (expand-file-name ".git" attachment-directory))
	 (DRYRUN (if dryrun " [DRY RUN]" ""))
	 (msg1 (format "Arguments: %s %s %s" attachment-directory dryrun DRYRUN))
	 (msg2 (format "\n%s exists: %s"
		       attachment-directory
		       (if (file-exists-p attachment-directory) "yes" "no")))
	 (msg3 (if (file-exists-p attachment-directory)
		   (format "\n%s exists: %s"
			   gitdir
			   (if (file-exists-p gitdir) "yes" "no"))
		 ""))
	 (createdir (unless (file-exists-p attachment-directory)
		      (y-or-n-p (format "Create %s? " attachment-directory))))
	 (msg4 (if createdir
		   (format "\nCreating %s.%s" attachment-directory DRYRUN)
		 ""))
	 (msg5 (if (file-exists-p gitdir)
		   (format "\nUsing existing git repo.%s%s%s%s"
			   "\n  Adding all files."
			   DRYRUN
			   "\n  Committing."
			   DRYRUN)
		 (format "\nInitializing git.%s" DRYRUN)))
	 (msg6 (format "\nSaving all attachments.%s" DRYRUN))
	 (msg7 (format "\n  Adding all files.%s\n  Committing.%s"
		       DRYRUN DRYRUN)))
    (if (> (length DRYRUN) 0)
	(with-displaying-help-buffer
	  (lambda ()
	    (princ (concat msg1 msg2 msg3 msg4 msg5 msg6 msg7 "\n"))))
      (message msg1)			; Display attachment directory name.
      (message msg2)			; Check existence.
      (message msg3)			; ... and for GITDIR.
      (unless (file-exists-p attachment-directory)
	(message msg4)
	(make-directory attachment-directory))
      ;; #### Should check for directoryness and write access.
      (message msg5)
      (let ((default-directory attachment-directory))
	(if (not (file-exists-p gitdir))
	    (shell-command "git init")
	  (shell-command "git add .")
	  (shell-command "git commit -m 'Commit for save-all-attachments.'")))
      (message msg6)
      (let ((vm-mime-delete-after-saving t))
	(vm-save-all-attachments nil attachment-directory))
      (message msg7)
      (let ((default-directory attachment-directory))
	(shell-command "git add .")
	(shell-command "git commit -m 'Commit for save-all-attachments.'"))
      (message "Done!")
      )))
