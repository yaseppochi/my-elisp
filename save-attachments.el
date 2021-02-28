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

(provide 'save-attachments)

(defvar sjt/debug-save-attachments nil)

;;; Various stuff

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

(defun sjt/scarf-filenames ()
  (interactive)
  (goto-char (point-min))
  (save-match-data
    (let ((filenames nil))
      (while (search-forward "filename=" nil t)
	(when (re-search-forward
	       #r"=?\GB\(?:K\|2312\|18030\)\?\([bBqQ]\)\?\([^?]+\)\?="
	       (point-at-eol) t)
	  (setq filenames (cons (match-string 2) filenames))))
      (pop-to-buffer (get-buffer-create "Filenames in message"))
      (erase-buffer)
      (setq filenames (nreverse filenames))
      (while filenames
	(insert (pop filenames) "\n"))
      (goto-char (point-min))
      (let ((sol (point)))
	(while (not (eobp))
	  (end-of-line)
	  (sjt/decode-base64-gbk-region sol (point))
	  (forward-line 1)
	  (setq sol (point))))
      )))

;;; Handle students

;; #### Need similar functions for faculty, candidates, and mailing lists.
;; If all return nil, default to current month.

(defun sjt/construct-directory-for-student ()
  "Check if author or recipient is known student and return directory or nil.

Also returns nil if not in an appropriate vm-mode."
  (let* ((m (vm-current-message))
	 (student (cond ((assoc (vm-su-from m) sjt/students-all))
			((assoc (vm-su-to m) sjt/students-all))
			(t nil))))
    (when student
      (expand-file-name (concat "~/edu/students/" (nth 1 student))))))

(defun add-sk (x)
  (concat x (cond ((string-match-p "@.*\\." x) "")
		  ((string-match-p "@" x) ".tsukuba.ac.jp")
		  (t "@sk.tsukuba.ac.jp"))))

(defun sjt/student-id-to-email-list (sid name)
  "Translate a student ID number to a list of three addresses at domains
sk.tsukuba.ac.jp, s.tsukuba.ac.jp, and u.tsukuba.ac.jp."
  (let ((last7 (substring sid 2)))
    `((,(concat "s" last7 "@s.tsukuba.ac.jp") ,name)
      (,(concat "s" last7 "@sk.tsukuba.ac.jp") ,name)
      (,(concat "s" last7 "@u.tsukuba.ac.jp") ,name))))

;; Original version
; (defun sjt/vm-save-all-attachments (&optional count
; 				    directory
; 				    no-delete-after-saving)
;   "Like `vm-save-all-attachments' but treats default DIR specially.
;
; Specifically it matches author and recipient against `sjt/students-all'."
;   (interactive
;    (list current-prefix-arg
; 	 (let ((default-dir (or (sjt/construct-directory-for-student)
; 				vm-mime-all-attachments-directory
; 				vm-mime-attachment-save-directory
; 				default-directory)))
; 	   (vm-read-file-name "Attachment directory: "
; 			      default-dir
; 			      default-dir
; 			      nil nil
; 			      vm-mime-save-all-attachments-history))))
;   (vm-save-all-attachments count directory no-delete-after-saving))

;; Now with git protection!
;; #### Issues:
;; 1.  The current version doesn't pick up new files.
;; 2.  This is hard to do based on the current attachment because file listing
;;     is embedded in #'vm-mime-operate-on-attachments.
;; 3.  But git add -A does all files (including in subdirs), which is generally
;;     undesirable.
;; I think the resolution is to bite the bullet on "all files," and delete the
;; ones we don't want around (eg, in old FILES/yyyy/mm directories), which is
;; approximately the same amount of space in the worst case (OA files that are
;; zip archives internally), and in cases where the same file gets sent
;; repeatedly is a win.
;; Merge ...-internal back into main function?
;; DIRECTORY must be a string.  COUNT is ignored.
(defun sjt/git-add-attachments (count logbuf)
  (if (null logbuf)
      (error 'args-out-of-range "logbuf must be non-nil")
    (let ((successes 0)
	  (failures 0)
	  added-attachments)
      (vm-mime-operate-on-attachments
       count
       :name "git adding"
       :included vm-mime-saveable-types
       :excluded vm-mime-saveable-type-exceptions
       :action
       (lambda (msg layout type file)
	 (if (or (null file) (string= file ""))
	     (setq failures (+ 1 failures))
	   (push file added-attachments)
	   (setq successes (+ 1 successes)))))
      (insert-string (format "%s %s\n%s\n" successes failures added-attachments)
		     logbuf)
      added-attachments)))

;; #### I don't understand why this is needed.  It seemed to work before....
(defun utf8-shell-command (command &optional output-buffer error-buffer)
  "Execute COMMAND in inferior shell, handling non-ASCII arguments.
OUTPUT-BUFFER and ERROR-BUFFER are as in #'shell-command."
  (with-temp-buffer
    (insert command)
    (goto-char (point-min))
    (skip-chars-forward "^ ")
    (delete-char 1)
    (shell-command-on-region
     (point) (point-max)
     (concat "xargs " (buffer-substring (point-min) (point)))
     output-buffer nil error-buffer)))


(defun sjt/vm-save-all-attachments
  (&optional count directory no-delete-after-saving)

  "Like `vm-save-all-attachments' but treats default DIRECTORY specially.
Also commit to a git repo, either in DIRECTORY or DIRECTORY's parent.

Specifically it matches author and recipient against `sjt/students-all'."

  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (expand-file-name
	   (let ((default-dir (or (sjt/construct-directory-for-student)
				  vm-mime-all-attachments-directory
				  vm-mime-attachment-save-directory
				  default-directory)))
	     (vm-read-file-name "Attachment directory: "
				default-dir
				default-dir
				nil nil
				vm-mime-save-all-attachments-history))))
	 nil))

  ;; #### #'save-excursion seems completely horked.  Grr.
  (let* ((old-directory default-directory)
	 (vmbuf (current-buffer))
	 (logbuf (get-buffer-create " *Save all attachments log*"))
	 (exists (file-exists-p directory))
	 (gitdir1 (expand-file-name ".git/" directory))
	 (gitdir2 (expand-file-name "../.git/" directory))
	 (gitdir (cond ((and exists (file-directory-p gitdir1)) gitdir1)
		       ((and exists (file-directory-p gitdir2)) gitdir2)
		       (t gitdir1)))
	 attachments)
    (labels ((dolog (s)
	       (insert-string s logbuf))
	     (docmd (cmd)
	       (let ((default-directory directory))
		 (dolog (format "%s\n  (in %s)\n" cmd default-directory))
		 (utf8-shell-command cmd logbuf)
		 (goto-char (point-max) logbuf)))
	     (doadd (files)
	       (let ((default-directory directory)
		     (added nil))
		 (loop
		   for attachment in files
		   do
		   (dolog (format "trying to add %s\n  in %s\n"
				  attachment default-directory))
		   (if (not (file-exists-p attachment))
		       (dolog (format "didn't find %s in %s\n"
				      attachment default-directory))
		     (docmd (format "git add -f %s" attachment))
		     (setq added t)))
		 added))
	     (docommit (files msg)
	       (if (not (doadd files))
		   (dolog (format "No files available to commit in %s (%s).\n"
				  default-directory (buffer-name)))
		 (dolog "  Adding all files.\n  Committing.\n")
		 (docmd (format "git commit -m '%s'" msg)))))
      (set-buffer logbuf)
      (erase-buffer)
      (dolog (format "set default-directory to %s in %s\n" directory logbuf))
      (setq default-directory directory)
      (set-buffer vmbuf)
      (dolog (format "Arguments: %s %s %s\n"
		     count no-delete-after-saving directory))
      (dolog (format "%s exists: %s\n" directory (if exists "yes" "no")))
      (dolog (format "%s exists: %s\n"
		     gitdir
		     (if (file-exists-p gitdir) "yes" "no")))
      (when (not exists)
	(dolog (format "Creating %s.\n" directory))
	(make-directory directory))
      (dolog (format "set default-directory to %s in %s\n" directory vmbuf))
      (setq default-directory directory)
      (dolog (if (file-exists-p gitdir)
		 "Using existing git repo.\n"
	       "Initializing git repo.\n"))
      (dolog (format "after dolog default-directory is %s\n" default-directory))
      (when (not (file-exists-p gitdir))
	(docmd "git init"))
      (setq attachments (sjt/git-add-attachments count logbuf))
      (dolog "Check for modified or untracked in added-attachments.\n")
      (dolog (format "default directory is %s\n" directory))
      (docommit attachments "Pre-commit for save-all-attachments.")
      (when (prog2
		(switch-to-buffer logbuf)
		(y-or-n-p "Commit now? ")
	      (set-buffer vmbuf))
	(dolog "Saving all attachments.\n")
	(let ((vm-mime-delete-after-saving t))
	  ;; #### Does this need to log in logbuf?
	  (vm-save-all-attachments count directory no-delete-after-saving)
	  (docommit attachments "Post-commit for save-all-attachments.")))
      (dolog "Done!\n")
      (setq default-directory old-directory)
      (pop-to-buffer logbuf))))

;;; handle candidates

(defvar sjt/academic-year "2018")
(defun sjt/attachment-root () (file-name-as-directory "~/VM/CANDIDATES"))

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
  
  (sjt/vm-save-all-attachments dryrun attachment-directory))