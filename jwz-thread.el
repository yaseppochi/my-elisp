;;; jwz-thread.el --- Jamie Zawinski's message threading algorithm (for VM?)

;; Author: Stephen J. Turnbull <stephen@xemacs.org>
;; Created: 11 December 2011
;; Last-Modified: 17 June 2012

;; Commentary:

;; This section doesn't belong here, but it shouldn't pollute vm-thread either.

;; --------------------------------------------------------------------------
;; Theory of operation
;;
;; NOTE: There's something entirely bogus about the whole approach of
;; vm-sort and vm-thread as of 2011-12-27.
;;
;; A basic reference on threading is Jamie Zawinski's algorithm and the
;; discussion in http://www.jwz.org/doc/threading.html.  An alternative
;; reference is http://www.jwz.org/doc/draft-ietf-imapext-thread-12.txt,
;; the IMAP Threading Extension draft by Mark Crispin and Kenneth Murchison.
;;
;; JWZ Concept  VM implementation                Comments
;; Container[1] (Message) ID Symbol              Has non-JWZ junk (dates)
;;              Subject Symbol                   Interned subject
;; Message      VM Message            
;; id_table     vm-thread-obarray                Contains id-syms
;;              vm-thread-subject-obarray[2]     Contains subject-syms
;; [1] The algorithms used below are implemented assuming that Containers are
;;     symbols.  `get' and `put' are used to maintain Container properties.

;; [2] Rename this: see also vm-folder, vm-mark, vm-summary, vm-vars.

;; Main entry point is `vm-toggle-threads-display'?
;; Call tree:
;; vm-toggle-threads-display
;;   vm-select-folder-buffer-and-validate
;;     vm-check-for-killed-summary
;;     vm-check-for-killed-presentation
;;   vm-update-summary-and-mode-line
;;     vm-update-draft-count
;;     vm-check-for-killed-summary
;;     vm-toolbar-update-toolbar
;;     vm-build-threads-if-unbuilt
;;       vm-build-threads
;;     vm-do-needed-renumbering
;;     vm-do-needed-summary-rebuild
;;     vm-do-needed-mode-line-update
;;     vm-update-message-summary
;;     vm-do-needed-folders-summary-update
;;     vm-force-mode-line-update
;;   vm-set-summary-redo-start-point
;;   vm-sort-messages
;;     vm-select-folder-buffer-and-validate
;;     vm-error-if-folder-empty
;;     vm-error-if-folder-read-only
;;     vm-display
;;     vm-build-threads-if-unbuilt
;;     vm-build-thread-lists
;;     vm-remove-message-order
;;     vm-physically-move-message
;;     vm-mark-folder-modified-p
;;     vm-clear-modification-flag-undos
;;     vm-reverse-link-messages
;;     vm-present-current-message
;;     vm-update-summary-and-mode-line
;;     vm-sort-insert-auto-folder-names

;; Code:

;; Variables:

(defvar jwz-thread-mode nil
  "Non-nil if folder is threaded using JWZ's algorithm.
Currently only available in VM.")
(make-variable-buffer-local 'jwz-thread-mode)

;; #### Maybe there's a better way?
(defvar jwz-thread-roots nil)
;; #### I worry about this buffer-local because VM uses multiple buffers
;; per folder.  But virtual folders probably need a local variable.
(make-variable-buffer-local 'jwz-thread-roots)

(defvar jwz-thread-debug-buffer-name "*jwz-thread debug*"
  "Name of buffer for jwz-thread debugging output.")
(defvar jwz-thread-debug-buffer nil
  "Buffer for jwz-thread debugging output.")

;; #### this is wrong, we need to do all functions provided by jwz-thread in
;; case vm-thread uses them
(defconst jwz-thread-exported-function-list '(
					      "build-thread-list"
					      "build-threads"
					      "check-thread-integrity"
					      "kill-thread-subtree"
					      "th-child-messages-of"
					      "th-message-of"
					      "th-messages-of"
					      "th-oldest-date-of"
					      "th-thread-date-of"
					      "th-youngest-date-of"
					      "thread-count"
					      "thread-indentation-of"
					      "thread-indentation"
					      "thread-list"
					      "thread-root"
					      "thread-root-p"
					      "thread-root-sym"
					      "thread-subtree"
					      "thread-symbol"
					      "unthread-message-and-mirrors"
					      "unthread-message")
  "List of names of functions exported by vm-thread.el to other VM components.
The \"vm-\" prefix is omitted.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to the VM MUA (mostly accessor/mutator defsubsts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #### Check these for defsubst-ability.
;; #### Consider moving any functions that can be implemented entirely in the
;; jwz namespace out of this section.

;; Miscellaneous utilities

(defsubst jwz-sortable-date (container)
  (vm-so-sortable-datestring (car (vm-th-messages-of container))))

(defsubst jwz-mua-message-list ()
  vm-message-list)

(defun jwz-set-mua-message-list ()
  (interactive)
  (setq vm-message-list (jwz-thread-make-message-list jwz-thread-roots)))

;; Message management

;; Jamie actually did some research on this.  Shame on him! :-)
(defsubst jwz-message-in-reply-to (message)
  "Retrieve the Message-ID in MESSAGE's In-Reply-To field, if any."
  ;; The `or' form is a hack to get vm-parse to parse nil. :-)
  (car (vm-parse (or (vm-get-header-contents message "In-Reply-To:" " ") "")
		 "[^<]*\\(<[^>]+>\\)")))

(defsubst jwz-message-container (message)
  (vm-th-thread-symbol message))

(defsubst jwz-message-references (message)
  "Return referenced messages, most recent first."
  ;; `vm-references' caches the value so must make a copy.
  (reverse (vm-references message)))

(defsubst jwz-message-subject (message)
  (vm-su-subject message))

(defsubst jwz-message-canonical-subject (message)
  (let ((subject (vm-so-sortable-subject message)))
    (and subject (> (length subject) 0) subject)))

;; VM-specific functions (for debugging?)

(defsubst jwz-message-month (message)
  (substring (vm-su-month message) 0 3))

(defsubst jwz-message-monthday (message)
  (vm-su-monthday message))

;; Container management (could be abstracted)

(defun jwz-container-check-parent-child (parent child)
  "Signal `jwz-thread-error' if PARENT and CHILD are not parent and child.
Otherwise return nil.  PARENT and CHILD are containers."
  (cond ((and (null parent) (null child))
	 (error 'vm-thread-error "parent and child should not both be null"))
	((null parent)
	 (unless (null (vm-th-parent-of child))
	   (error 'vm-thread-error "not a root message" child)))
	((null child)
	 (unless (null (vm-th-children-of parent))
	   (error 'vm-thread-error "not a leaf message" parent)))
	((not (and (eq parent (vm-th-parent-of child))
		   (memq child (vm-th-children-of parent))))
	 (error 'vm-thread-error "parent-child link corrupt" parent child))))

;; #### Shouldn't this use check-parent-child?
(defun jwz-container-check-family-consistency (container)
  "Signal 'jwz-thread-error if parent or children of CONTAINER are inconsistent.
Else return zero."
  (unless (or (null (vm-th-parent-of container))
	      (memq container (vm-th-children-of (vm-th-parent-of container))))
    (error 'vm-thread-error "confused ancestry" container))
  (mapc (lambda (x)
	  (unless (eq (vm-th-parent-of x) container)
	    (error 'vm-thread-error "confused posterity" container x)))
	(vm-th-children-of container)))

(defsubst jwz-container-parent (container)
  (vm-th-parent-of container))

(defun jwz-container-reparent (child old new)
  "Change the parent of CHILD from OLD to NEW.
OLD or NEW may be nil, signifying that CHILD was or will become a root.
\(jwz-container-reparent child old old) is a no-op.
All arguments are containers.
Use `jwz-reparent-carefully' to check for ancestry loops."
  ;; Check consistency of relationship of OLD to CHILD.
  (jwz-container-check-parent-child old child)
  (when old
    (vm-th-delete-child old child))
  (vm-th-set-parent child new))

(defsubst jwz-container-messages (container)
  (vm-th-messages-of container))

;; VM looks for the longest match; Jamie says just use the first one.
(defsubst jwz-container-message (container)
  (vm-th-message-of container))

(defsubst jwz-container-add-message (container message)
    (vm-th-add-message-to-symbol container message))

(defsubst jwz-container-subject (container)
  (jwz-message-subject (jwz-container-message container)))

(defsubst jwz-container-date (container)
  (get container 'jwz-thread-date))

(defsubst jwz-container-set-date (container)
  (unless (jwz-container-date container)
    (put container 'jwz-thread-date (jwz-sortable-date container))))

(defsubst jwz-find-container (message-id)
  "Find a container for MESSAGE-ID, creating it if needed."
  (vm-th-intern-id message-id))

(defsubst vm-thread-root-sym (m)
  "Returns interned symbol of the root message of M.  M can be
either a message or the interned symbol of M.  Threads should
have been built for this function to work.  

See also: `vm-thread-root'.

Horrible name, for compatibility with VM."
  (jwz-find-container m))

(defsubst jwz-container-children (container)
  (vm-th-children-of container))

(defsubst jwz-container-set-children (container children)
  (vm-th-set-children-of container children))

(defsubst jwz-find-subject (subject)
  (let ((entry (vm-th-intern-subject subject)))
    (unless (boundp entry) (set entry nil))))

;; JWZ APIs: accessors and initializers for container tables.
;; #### It would appear that VM doesn't need these.  They are referenced
;; in a number of places, but as (a) a flag to other modules that threads
;; have been initialized and (b) as a signal to the thread engine that it
;; should reinitialize itself.

(defsubst jwz-container-table ()
  vm-thread-obarray)

(defsubst jwz-container-initialize-table-maybe ()
; #### always initialize
;  (unless (vectorp vm-thread-obarray)
    (setq vm-thread-obarray (make-vector 641 0)))
;    )

(defsubst jwz-initialize-subject-table-maybe ()
  (setq vm-thread-subject-obarray (make-vector 641 0)))

;; VM-specific functions (called by VM).

;; This is really of interest only for computing thread indentation AFAICS.
;; But need to check other usage (see list elsewhere).
(defun jwz-build-thread-list (message)
  "Return the list of ancestors of MESSAGE, eldest first.
The list includes MESSAGE.
Each message is represented by a symbol interned in vm-thread-obarray."
  (with-current-buffer (vm-buffer-of message)
    (let ((container (jwz-message-container message)))
      (nreverse (labels ((build-thread-list (sym)
			   (let ((parent-sym (jwz-container-parent sym)))
			     (cons sym (and parent-sym
					    (build-thread-list parent-sym))))))
		  (build-thread-list container))))))

(defun jwz-thread-make-message-list (root-list)
  "Return the threaded list of messages from ROOT-LIST.
Walks the container forest in post-order, and produces a list of MUA message
objects (not containers)."
  (apply #'append (mapcar #'jwz-container-messages
			  (jwz-thread-make-container-list root-list))))

;; vm-thread functions referenced by other files:
;; (*) currently commented out
;; (**) not defined in vm-thread.el
;; vm-build-thread-lists: vm-sort.el
;; -> vm-thread-list
;; -> vm-check-thread-integrity
;; vm-build-threads: vm-avirtual.el, vm-delete.el, vm-edit.el, vm-folder.el, vm-macro.el, vm-mark.el, vm-message.el, vm-motion.el, vm-sort.el
;; => REIMPLEMENT
;; vm-check-thread-integrity: vm-delete.el, vm-edit.el, vm-summary.el
;; => REIMPLEMENT
;; vm-collapse-all-threads(**vm-summary): vm-menu.el
;; vm-collapse-thread(**vm-summary): vm-motion.el
;; => REIMPLEMENT - should be able to simply chase cdrs, checking depth
;; vm-collapsed-root-p(**vm-summary): vm-motion.el
;; => REIMPLEMENTATION UNNECESSARY
;; vm-expand-all-threads(**vm-summary): vm-menu.el
;; vm-expand-thread(**): vm-delete.el, vm-motion.el
;; vm-kill-thread-subtree: vm-delete.el
;; vm-mark-thread-subtree(**): vm-menu.el
;; vm-skip-collapsed-sub-threads(**): vm-motion.el
;; vm-th-child-messages-of(*): vm-mark.el
;; vm-th-message-of: vm-sort.el
;; vm-th-messages-of: vm-motion.el
;; vm-th-oldest-date-of: vm-sort.el(*)
;; vm-th-thread-date-of: vm-sort.el
;; vm-th-youngest-date-of: vm-sort.el
;; vm-thread-count: vm-delete.el, vm-motion.el, vm-summary.el
;; vm-thread-indentation-of(**): vm-summary.el
;; vm-thread-indentation: vm-motion.el, vm-ps-print.el, vm-summary.el
;; vm-thread-list: vm-mark.el(*), vm-motion.el, vm-sort.el
;; vm-thread-root-p: vm-folder.el, vm-summary.el
;; => REIMPLEMENT
;   "Returns t if message M is known to be a thread root, nil
; otherwise.  No exceptions are thrown for errors."
;; vm-thread-root-sym: vm-sort.el
;; => REIMPLEMENT
;   "Returns interned symbol of the root message of M.  M can be
; either a message or the interned symbol of M.  Threads should
; have been built for this function to work.  
;
; See also: `vm-thread-root'."
;; vm-thread-root: vm-delete.el, vm-folder.el, vm-motion.el, vm-summary.el
;; => REIMPLEMENT
;   "Returns the root message of M.  M can be either a message or
; the interned symbol of a message.  If there are multiple messages with
; the same root message ID, one of them is chosen arbitrarily.  Threads
; should have been built for this function to work."
;; vm-thread-subtree: vm-delete.el, vm-folder.el, vm-mark.el, vm-summary.el
;; vm-thread-symbol: vm-delete.el, vm-mark.el, vm-sort.el, vm-summary.el
;; vm-toggle-thread(**): vm-menu.el
;; vm-toggle-threads-display(**): vm-menu.el
;; vm-unthread-message-and-mirrors: vm-delete.el, vm-edit.el, vm-virtual.el
;; vm-unthread-message: vm-message.el

;; end VM MUA interface -- no vm-functions below here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; JWZ threading
;; cf. http://www.jwz.org/doc/threading.html
;;
;; TODO:
;; 1. Provide interface to MUA.
;; 2. Make MUA robust to empty containers.
;; 3. Provide a way to clean up empty containers in obarray.
;; 4. Provide a way to reinitialize all globals and start from scratch.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'jwz-thread)

;; the mode code

(defun jwz-thread-mode (mode)
  "Set `jwz-thread-mode' according to MODE.
If MODE is a non-negative number, enable.
If MODE is a negative number, disable.
If MODE is nil, toggle."
  (interactive "p")
  (setq jwz-thread-mode (if (null mode) (not jwz-thread-mode) (>= mode 0)))

  ;; Insinuate JWZ into VM.
  (mapc (lambda (fnname) (jwz-thread-insinuate-function "jwz" fnname))
	jwz-thread-exported-function-list)

  ;; Show threads if enabled.
  (when (and jwz-thread-mode vm-summary-show-threads)
    (message "vm-message-list shows %d messages." (length vm-message-list))
    (jwz-thread-thread-message-list vm-message-list)))

(defun jwz-thread-insinuate-function (prefix function)
  "Install the appropriate definition of a thread function.
PREFIX is a string, either \"vm\" or \"jwz\".
FUNCTION is a function name from `jwz-thread-exported-function-list'.
If PREFIX is \"vm\", the function definition is installed from the
  'vm-thread-function property on the function symbol being insinuated.
Otherwise, the existing definition of the vm-thread function is copied to the
  'vm-thread-function property if that is nil, and if the corresponding jwz
  function exists, the vm-thread function's definition is set to the
  jwz-thread version.

If you think this is a hack, you're right.  But what else could I do?"
  (let ((vm-function (intern (concat "vm-" function)))
	(jwz-function (intern (concat "jwz-" function))))
    (cond ((string= prefix "vm")
	   (if (get vm-function 'vm-thread-function)
	       (fset vm-function (get vm-function 'vm-thread-function))
	     (warn "no 'vm-thread-function definition for %s" vm-function)))
	  ((string= prefix "jwz")
	   (unless (get function 'vm-thread-function)
	     (put function 'vm-thread-function (symbol-function vm-function)))
	   (when (fboundp jwz-function)
	     (fset vm-function (symbol-function jwz-function))))
	  (t (error 'args-out-of-range "namespace prefix" prefix)))))

;; the threading code

(defun jwz-thread-thread-message-list (message-list)
  "Use JWZ's algorithm to build threads for MESSAGE-LIST in `jwz-thread-roots'.
Return the list of thread roots."

  (message "message-list shows %d messages." (length message-list))

  ;; 1. Build id_table.
  (jwz-thread-build-tree message-list)

  ;; 2. Find the root set.
  (message "Finding root set ...")
  (jwz-thread-find-roots)
  (message "Found %d roots." (length jwz-thread-roots))

  ;; 3. Discard id_table.  (We don't do this.)
  ;; 4. Prune empty containers.
  (message "Pruning empty containers ...")
  (jwz-thread-prune-empty-containers)
  (message "After pruning %d roots." (length jwz-thread-roots))

  ;; 5. Group root set by subject.
  (message "Grouping threads with same subject ...")
  (jwz-thread-merge-by-subject)
  (message "After merging %d roots." (length jwz-thread-roots))

  ;; 6. Now you're done threading!  (We don't compress data structures.)
  ;; 7. Now, sort the siblings.
  (message "Sorting sibling sets ...")
  ;; Although the sort is destructive, I don't think it can be in-place.
  (setq jwz-thread-roots
	(jwz-thread-sort jwz-thread-roots #'jwz-container-date nil t))
  (message "Sorted %d roots." (length jwz-thread-roots))
  (message "JWZ threading algorithm completed.")
  jwz-thread-roots)

;; The Algorithm:

;;  1. For each message:
;;
;;    A. If id_table contains an empty Container for this ID:
;;         - Store this message in the Container's message slot. 
;;       Else:
;;         - Create a new Container object holding this message;
;;         - Index the Container by Message-ID in id_table. 
;;
;;    B. For each element in the message's References field:
;;
;;         - Find a Container object for the given Message-ID:
;;           - If there's one in id_table use that;
;;           - Otherwise, make (and index) one with a null Message. 
;;
;;         - Link the References field's Containers together in the order
;;           implied by the References header.
;;           - If they are already linked, don't change the existing links.
;;           - Do not add a link if adding that link would introduce a loop:
;;             that is, before asserting A->B, search down the children of B
;;             to see if A is reachable, and also search down the children
;;             of A to see if B is reachable. If either is already reachable
;;             as a child of the other, don't add the link.
;;
;;    C. Set the parent of this message to be the last element in References.
;;       Note that this message may have a parent already: this can happen
;;       because we saw this ID in a References field, and presumed a parent
;;       based on the other entries in that field. Now that we have the actual
;;       message, we can be more definitive, so throw away the old parent and
;;       use this new one. Find this Container in the parent's children list,
;;       and unlink it.
;;
;;       Note that this could cause this message to now have no parent, if it
;;       has no references field, but some message referred to it as the
;;       non-first element of its references. (Which would have been some
;;       kind of lie...)
;;
;;       Note that at all times, the various ``parent'' and ``child'' fields
;;       must be kept inter-consistent. 

(defun jwz-thread-build-tree (message-list)
  "Build the thread tree for MESSAGE-LIST from References and In-Reply-To.
Messages are MUA messages, not containers."

  (jwz-container-initialize-table-maybe)
; #### temporarily show progress
;  (mapc #'jwz-thread-thread-message message-list))
  (let ((count 0))
    (mapc (lambda (m)
	    (setq count (1+ count))
            (jwz-thread-thread-message m)
            (message "%d messages threaded" count))
	  message-list)))

;; #### Maybe this function should handle the case where child already
;; has a parent?
(defun jwz-container-reparent-carefully (child parent)
  "Give CHILD the parent PARENT.
Check for an ancestry loop and abort without reparenting if there is one."
  ;; #### Eventually we can get rid of the *-consistency checks.
  (jwz-container-check-family-consistency parent)
  (if (jwz-container-parent child)
      (jwz-container-check-family-consistency child)
    (catch 'loop-detected
      (let ((ancestor (jwz-container-parent child)))
	(while ancestor
	  (if (eq ancestor parent)
	      (throw 'loop-detected nil)
	    (setq ancestor (jwz-container-parent ancestor))))
	(setq ancestor (jwz-container-parent parent))
	(while ancestor
	  (if (eq ancestor child)
	      (throw 'loop-detected nil)
	    (setq ancestor (jwz-container-parent ancestor)))))
      (jwz-container-reparent child nil parent))))

(defun jwz-thread-thread-message (message)
  "Add MESSAGE to the thread tree."
  (let ((container (jwz-message-container message))
	(references (jwz-message-references message))
	(in-reply-to (jwz-message-in-reply-to message)))
    (jwz-container-add-message container message)
    (jwz-container-set-date container)
    ;; #### Could check here for already-threaded (ie, duplicate Message-ID).
    ;; Most likely this is just a dupe due to multiple delivery paths (ie, no
    ;; harm done if we rethread), but in theory it could be a different message.
    ;; Old VM threading has a notion of "canonical message" for the Message-ID,
    ;; but I don't think this makes sense.
    (when in-reply-to
      (setq references (cons in-reply-to (delete in-reply-to references))))
    (let ((parent (jwz-container-parent container)))
      ;; If there's a parent of container, clear it -- we know better now.
      (when parent
	(jwz-container-reparent container parent nil))
      (while references
	(setq parent (jwz-find-container (car references))
	      references (cdr references))
	;; #### We should only use another message's references if the message
	;; itself didn't provide a parent.  Whether to implement this in
	;; reparent-carefully or not needs care.
	(jwz-container-reparent-carefully container parent)
	(setq container parent)))))

;; 2. Find the root set.
;;
;;    Walk over the elements of id_table, and gather a list of the Container
;;    objects that have no parents.
;;
;; 3. Discard id_table. We don't need it any more.

(defun jwz-thread-find-roots ()
  "Populate `jwz-thread-roots' with root messages."
  (setq jwz-thread-roots nil)
  (mapatoms (lambda (x)
	      (unless (jwz-container-parent x)
		(push x jwz-thread-roots)))
	    (jwz-container-table)))

;; 4. Prune empty containers.
;;    Recursively walk all containers under the root set.
;;    For each container:
;;
;;    A. If it is an empty container with no children, nuke it.
;;
;;       Note: Normally such containers won't occur, but they can show up
;;       when two messages have References lines that disagree. For example,
;;       assuming A and B are messages, and 1, 2, and 3 are references for
;;       messages we haven't seen:
;;
;;         A has references: 1, 2, 3
;;         B has references: 1, 3 
;;
;;       There is ambiguity as to whether 3 is a child of 1 or of 2. So,
;;       depending on the processing order, we might end up with either
;;
;;          -- 1
;;             |-- 2
;;                 \-- 3
;;                     |-- A
;;                     \-- B
;;
;;       or
;;
;;          -- 1
;;             |-- 2            <--- non root childless container!
;;             \-- 3
;;                 |-- A
;;                 \-- B
;;
;;    B. If the Container has no Message, but does have children, remove this
;;       container but promote its children to this level (that is, splice
;;       them in to the current child list.)
;;
;;       Do not promote the children if doing so would promote them to the
;;       root set -- unless there is only one child, in which case, do.

(defun jwz-thread-prune-and-promote-children (container)
  "Prune empty children of CONTAINER, and promote their grandchildren."
  (mapc (lambda (child)
          (when (null (jwz-container-messages child))
	    (when (jwz-container-children child)
	      (jwz-thread-prune-and-promote-children child)
	      (mapc (lambda (grandchild)
		      (jwz-container-reparent grandchild child container))
		    (jwz-container-children child)))
	    ;; No need to delete the root from the obarray, we might need
	    ;; it some day.
	    (jwz-container-reparent child container nil)))
	(copy-list (jwz-container-children container))))

(defun jwz-thread-prune-empty-containers ()
  (mapc #'jwz-thread-prune-and-promote-children jwz-thread-roots)
  (message "After pruning/promoting non-roots, %d roots remain"
	   (length jwz-thread-roots))
  (mapc (lambda (root)
	  (when (and (null (jwz-container-messages root))
		     (null (cdr (jwz-container-children root)))
		     ;; Although there must have been a child at some point (to
		     ;; contribute the Reference), it might have been deleted
		     ;; if we are reusing the container tree.
		     (car (jwz-container-children root)))
	    ;; No need to delete the root from the obarray, we might need
	    ;; it some day.
	    (jwz-container-reparent (car (jwz-container-children root))
				    root nil)
	    (setq jwz-thread-roots (append (delq root jwz-thread-roots)
					   (jwz-container-children root)))))
	(copy-list jwz-thread-roots)))

;; 5. Group root set by subject.
;;
;;    If any two members of the root set have the same subject, merge them.
;;    This is so that messages which don't have References headers at all
;;    still get threaded (to the extent possible, at least.)

(defun jwz-thread-merge-by-subject ()
  (jwz-initialize-subject-table-maybe)
  (mapc #'jwz-thread-maybe-add-subject jwz-thread-roots)
  (mapc #'jwz-thread-merge-containers-by-subject (copy-list jwz-thread-roots)))

;;    A. Construct a new hash table, subject_table, which associates subject
;;       strings with Container objects.
;;
;;    B. For each Container in the root set:
;;
;;       - Find the subject of that sub-tree:
;;         - If there is a message in the Container, the subject is the
;;           subject of that message.
;;         - If there is no message in the Container, then the Container will
;;           have at least one child Container, and that Container will have
;;           a message. Use the subject of that message instead.
;;         - Strip ``Re:'', ``RE:'', ``RE[5]:'', ``Re: Re[4]: Re:'' and so on.
;;         - If the subject is now "", give up on this Container.

(defun jwz-container-subject-container (container)
  "Return a container containing a usable subject for CONTAINER.
Might be a child of CONTAINER."
  (catch 'subject
    ;; #### Is a full recursive search necessary?
    (labels ((post-order (current)
		 (when (jwz-container-message current)
		   ;; #### We could look harder for a subject.
		   (throw 'subject
			  (if (> (length (jwz-message-canonical-subject
					  (jwz-container-message current)))
				 0)
			      current
			    nil)))))
      container))
  nil)

;;         - Add this Container to the subject_table if:
;;           - There is no container in the table with this subject, or
;;           - This one is an empty container and the old one is not: the
;;             empty one is more interesting as a root, so put it in the
;;             table instead.
;;           - The container in the table has a ``Re:'' version of this
;;             subject, and this container has a non-``Re:'' version of this
;;             subject. The non-re version is the more interesting of the two. 

(defun jwz-thread-maybe-add-subject (root)
  (let ((subject-container (jwz-container-subject-container root)))
    (when subject-container
      (let* ((subject (jwz-message-canonical-subject
		       (jwz-container-message subject-container)))
	     (entry (jwz-find-subject subject))
	     ;; #### this is a VM-related hack!
	     (old-container (symbol-value entry))
	     (old-message (and old-container
			       (jwz-container-message old-container))))
	(when (or (null old-container)
		  (and (not (jwz-container-message root))
		       old-message)
		  (< (length (jwz-message-subject
			      (jwz-container-message subject-container)))
		     (length (jwz-message-subject old-message))))
	  (set entry root))))))

;;    C. Now the subject_table is populated with one entry for each subject
;;       which occurs in the root set. Now iterate over the root set, and
;;       gather together the difference.
;;
;;       For each Container in the root set:
;;
;;       - Find the subject of this Container (as above.)
;;       - Look up the Container of that subject in the table.
;;       - If it is null, or if it is this container, continue.
;;
;;       - Otherwise, we want to group together this Container and the one in
;;         the table. There are a few possibilities:
;;
;;         - If both are dummies, append one's children to the other, and
;;           remove the now-empty container.
;;
;;         - If one container is a empty and the other is not, make the
;;           non-empty one be a child of the empty, and a sibling of the other
;;           ``real'' messages with the same subject (the empty's children.)
;;
;;         - If that container is a non-empty, and that message's subject does
;;           not begin with ``Re:'', but this message's subject does, then make
;;           this be a child of the other.
;;
;;         - If that container is a non-empty, and that message's subject
;;           begins with ``Re:'', but this message's subject does not, then
;;           make that be a child of this one -- they were misordered. (This
;;           happens somewhat implicitly, since if there are two messages, one
;;           with Re: and one without, the one without will be in the hash
;;           table, regardless of the order in which they were seen.)
;;
;;         - Otherwise, make a new empty container and make both msgs be a
;;           child of it. This catches the both-are-replies and
;;           neither-are-replies cases, and makes them be siblings instead of
;;           asserting a hierarchical relationship which might not be true.
;;
;;           (People who reply to messages without using ``Re:'' and without
;;           using a References line will break this slightly. Those people
;;           suck.) 
;;
;;       (It has occurred to me that taking the date or message number into
;;       account would be one way of resolving some of the ambiguous cases,
;;       but that's not altogether straightforward either.)

(defsubst jwz-thread-delete-root (root)
  "Remove ROOT from the thread root list.
Modifies `jwz-thread-roots'."
  (setq jwz-thread-roots (delq root jwz-thread-roots)))

(defun jwz-thread-merge-containers-by-subject (root)
  (let* ((root-container (jwz-container-subject-container root))
	 (message (jwz-container-message root-container))
	 (entry (when message
		  (jwz-find-subject (jwz-message-canonical-subject message))))
	 ;; #### VM-based hack.
	 (subject-container (symbol-value entry)))
    (unless (or (eq root subject-container)
		(= 0 (length (jwz-message-canonical-subject
			      (jwz-container-message subject-container)))))
      ;; #### This cond can be optimized somewhat, but be careful to preserve
      ;; the ordering of conditions.
      (cond ((and (null message)
		  (null (jwz-container-message subject-container)))
	     (mapc (lambda (child)
		     (jwz-container-reparent child root subject-container))
		   (jwz-container-children root))
	     (jwz-thread-delete-root root))
	    ((null message)
	     (jwz-container-reparent subject-container nil root)
	     (jwz-thread-delete-root subject-container)
	     (set entry root))
	    ((null (jwz-container-message subject-container))
	     (jwz-container-reparent root nil subject-container)
	     (jwz-thread-delete-root root))
	    ;; These length comparisons are hacks.
	    ((> (length (jwz-container-subject root))
		(length (jwz-container-subject subject-container)))
	     (jwz-container-reparent root nil subject-container)
	     (jwz-thread-delete-root root))
	    ;; According to Jamie, this case shouldn't happen.
	    ((< (length (jwz-container-subject root))
		(length (jwz-container-subject subject-container)))
	     (warn "Something Jamie said shouldn't happen did: wrong order:
  root %s\nsubject-table %s" root subject-container)
	     (jwz-container-reparent subject-container nil root)
	     (jwz-thread-delete-root subject-container)
	     (set entry root))
	    (t
	     (let ((new-root
		    (make-symbol
		     "<same-name-different-symbol@gargle-gargle-howl>")))
	       (jwz-container-reparent root nil new-root)
	       (jwz-thread-delete-root root)
	       (jwz-container-reparent subject-container nil new-root)
	       (jwz-thread-delete-root subject-container)
	       (setq jwz-thread-roots (cons new-root jwz-thread-roots))
	       (set entry new-root)))))))

;; 6. Now you're done threading!
;;    Specifically, you no longer need the ``parent'' slot of the Container
;;    object, so if you wanted to flush the data out into a smaller,
;;    longer-lived structure, you could reclaim some storage as a result.

;; 7. Now, sort the siblings.
;;    At this point, the parent-child relationships are set. However, the
;;    sibling ordering has not been adjusted, so now is the time to walk the
;;    tree one last time and order the siblings by date, sender, subject, or
;;    whatever. This step could also be merged in to the end of step 4, above,
;;    but it's probably clearer to make it be a final pass. If you were
;;    careful, you could also sort the messages first and take care in the
;;    above algorithm to not perturb the ordering, but that doesn't really
;;    save anything. 

;; jwz-thread-sort
;;
;; I'm not sure I agree that it's clearer to walk the tree again.
;; However, we do want to be able to change the sort key, and there is no
;; need to rethread.
;;
;; As Jamie points out, threading and sorting are different, although if you
;; stably sort by date and then subject, you'll get a view that resembles
;; threading.  Also, a thread sort is different from simply sorting the
;; sibling set, because the sort key may be synthesized from the next
;; generation (eg, the date of a message might be taken to be the date of the
;; most recent message in the subthread rooted at the given message).  (VM
;; does seem to get this more or less right, but it seems to be expensive.)

;; #### There's got to be a better API.  
(defun jwz-thread-sort (root-list get-key reverse synthesize-key)
  "Sort sibling sets descended from ROOT-LIST on the attribute GET-KEY.
Returns the sorted list.
  GET-KEY must be a function to retrieve the sort key for each container.  It
must take a container as argument, and return a string, even if the container
contains no message.  Numbers should be consistently formatted in fixed width
and right-justified.  Multiple keys can be joined with NULs \(this gives
prefix-is-less behavior).
  REVERSE, if non-nil, means to sort in decreasing order of GET-KEY.
  SYNTHESIZE-KEY, if non-nil, must be a function of a list of containers
returning a string, which will be used to generate the key for each container
based on its list of children, or the symbol t.  When SYNTHESIZE-KEY is t, the
key is generated by using GET-KEY on the last element of the sorted children, ."
  ;; first, recursively sort descendents 
  (mapc (lambda (x)
	  (when (jwz-container-children x)
	    (jwz-container-set-children x (jwz-thread-sort
					   (jwz-container-children x)
					   get-key reverse
					   synthesize-key)))
	  (put x 'key
	       (cond ((or (null (jwz-container-children x))
			  (null synthesize-key))
		      (funcall get-key x))
		     ((eq synthesize-key t)
		      (funcall get-key (car (last (jwz-container-children x)))))
		     (t (funcall synthesize-key (jwz-container-children x))))))
	root-list)
  ;; now, sort the root list
  ;; #### sort is stable in XEmacs; how about Emacs?
  (stable-sort root-list
	       (if reverse (lambda (a b) (string< b a)) #'string<)
	       :key (lambda (x) (get x 'key))))

(defun jwz-thread-make-container-list (root-list)
  "Return the threaded list of containers starting from ROOT-LIST.
Walks the container forest in post-order, and produces a list of containers."
  ;; #### What Emacsen is labels available in?
  (labels ((post-order (root)
	     (cons root (apply #'append
			       (mapcar #'post-order
				       (jwz-container-children root))))))
    (apply #'nconc (mapcar #'post-order root-list))))

;; debugging, etc

(defun jwz-thread-debug-summary (&optional roots)
  "Display a simple summary buffer of threads rooted at containers in ROOTS."
  (interactive "i")
  (setq roots (or roots
		  (jwz-thread-thread-message-list (jwz-mua-message-list))))
  (let ((containers (jwz-thread-make-container-list roots)))
    (mapc (lambda (c)
	    (put c 'thread-level (let ((count 0) (c1 c))
				   (while (setq c1 (jwz-container-parent c1))
				     (setq count (1+ count)))
				   count)))
	  containers)
    (switch-to-buffer (get-buffer-create jwz-thread-debug-buffer-name))
    (erase-buffer)
    (while containers
      (let* ((c (pop containers))
	     (m (car (jwz-container-messages c))))
	(insert (format "%3d %3s %2s %s %s\n"
			(get c 'thread-level)
			(if m (jwz-message-month m) "   ")
			(if m (jwz-message-monthday m) "  ")
			(format "%s" (let ((c1 (jwz-container-parent c)))
				       (if c1
					   (substring (symbol-name c1) 1 7)
					 "*root*")))
			c))))))

;;; jwz-thread.el ends here
