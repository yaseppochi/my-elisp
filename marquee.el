;;; marquee.el --- provide a vertically scrolling realtime-updateable marquee

;; Copyright 2016 Stephen J. Turnbull <stephen@xemacs.org>

;;; Commentary

;; A marquee to display on screen allowing realtime updates for errata
;; during exams and similar applications.

;; Basic design: event-driven (Emacs provides the event loop).
;; Kinds of events:
;; 1.  Timer -- scroll on expiration
;;     a.  scroll by paragraph (long time out)
;;     b.  scroll within paragraph (short time out UNIMPLEMENTED)
;; 2.  Add paragraph using minibuffer
;; 3.  Delete paragraph (UNIMPLEMENTED)

(defvar buffer-marquee nil)
(make-variable-buffer-local 'buffer-marquee)

(defun marquee-scroll-by-paragraph ()
  "Scroll buffer-marquee.

buffer-marquee is a buffer-local variable containing a deque of extents.

To scroll, copy content of head of deque to end of buffer, wrap it in
an extent, add the extent to the end of the deque, delete the head
content, and finally remove the head.

Leaves point at beginning of visible region."
  (when (> (length buffer-marquee) 18)
    (let ((extent-to-move (first buffer-marquee))
	  (current-end (point-max)))
      (goto-char current-end)
      (insert (extent-string extent-to-move))
      (goto-char (point-min))
      (nconc buffer-marquee (list (make-extent current-end (point-max))))
      (delete-region (extent-start-position extent-to-move)
		     (extent-end-position extent-to-move))
      (setq buffer-marquee (cdr buffer-marquee)))))

(defun marquee-add-paragraph (line)
  "Insert paragraph from LINE at beginning of buffer.

LINE is wrapped to fit window width, and wrapped in an extent which is
prepended to buffer-marquee.

Leaves point at beginning of visible region."
  (interactive "sAdd paragraph: ")
  (goto-char (point-min))
  (open-line 1)
  (insert line)
  (setq buffer-marquee (cons (make-extent (point-min) (1+ (point)))
			     buffer-marquee))
  (narrow-to-region (point-min) (1+ (point)))
  (goto-char (point-min))
  (when (> (length line) 55)
    (fill-paragraph 1))
  (widen))

(define-key global-map [(control c) M A] #'marquee-add-paragraph)

