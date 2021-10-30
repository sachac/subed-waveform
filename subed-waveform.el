;;; subed-waveform.el --- Display waveforms next to subtitles  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package extends subed.el to display waveforms. subed.el is not
;; yet available as a package but can be downloaded from
;; https://github.com/rndusr/subed .
;;
;; If you use use-package and straight,

;; `subed-waveform-minor-mode' lets you see the waveform waveform for
;; the current subtitle in order to make it easier to adjust times.
;; It requires FFMPEG, which should be at the location specified by
;; `subed-ffmpeg-executable'.
;;
;; If images are not displayed, you may want to make sure `max-image-size'
;; is set to a value that allows short, wide images.  The following code:
;;
;; (add-hook 'subed-mode (lambda () (setq-local max-image-size nil)))
;;
;; may do the trick.
;;
;;; Code:

(defgroup subed-waveform nil
  "Minor mode for displaying waveforms in `subed-mode' buffers."
  :group 'subed
  :prefix "subed-waveform")

(defcustom subed-waveform-ffmpeg-executable "ffmpeg"
  "Path to the FFMPEG executable used for generating waveforms."
  :type 'file
  :group 'subed-waveform)

(defcustom subed-waveform-ffmpeg-filter-args #'subed-waveform-fancy-filter
  "Additional arguments for the showwavespic filter.
To change the foreground color, use something like
\":colors=white\".  You can also set it to a function.  The
function will be called with WIDTH and HEIGHT as parameters, and
should return a string to include in the filter.  See
`subed-waveform-fancy-filter' for an example."
  :type '(choice
          (string :tag "Extra arguments to include")
          (function :tag "Function to call with the width and height"))
  :group 'subed-waveform)

(defcustom subed-waveform-height 40
  "Height of waveform in pixels."
  :type 'integer
  :group 'subed-waveform)

(defcustom subed-waveform-pixels-per-second 75
  "Number of pixels used for displaying one second."
  :type 'integer
  :group 'subed-waveform)

(defcustom subed-waveform-maximum-msecs 40000
  "Maximum length in milliseconds for displayed waveforms.
If non-nil, show waveforms only for subtitles with duration less
than or equal to the specified limit."
  :type '(choice
          (integer :tag "MS")
          (const nil :tag "No limit"))
  :group 'subed-waveform)

(defcustom subed-waveform-sample-msecs 2000
  "Number of milliseconds to play when jumping around a waveform.
0 or nil means don't play a sample."
  :type 'integer
  :group 'subed-waveform)

(defvar-local subed-waveform--sample-timer nil "Timer used for sampling.
Resets MPV position when done.")
(defvar-local subed-waveform--overlay nil)
(defvar-local subed-waveform--ffmpeg-process nil)
(defvar-local subed-waveform--enable-loop-over-current-subtitle-after-sample nil)
(defvar-local subed-waveform--enable-point-to-player-sync-after-sample nil)

(defvar subed-waveform-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'subed-waveform-set-start)
    (define-key map [S-down-mouse-1] #'subed-waveform-reduce-start-time)
    (define-key map [S-drag-mouse-1] #'subed-waveform-reduce-start-time)
    (define-key map [C-down-mouse-1] #'subed-waveform-set-start-and-copy-to-previous)
    (define-key map [S-mouse-1] #'ignore)
    (define-key map [C-mouse-1] #'ignore)
    (define-key map [mouse-2] #'subed-waveform-jump-to-timestamp)
    (define-key map [mouse-3] #'subed-waveform-set-stop)
    (define-key map [S-down-mouse-3] #'subed-waveform-increase-stop-time)
    (define-key map [S-drag-mouse-3] #'subed-waveform-increase-stop-time)
    (define-key map [C-down-mouse-3] #'subed-waveform-set-stop-and-copy-to-next)
    (define-key map [S-mouse-3] #'ignore)
    (define-key map [C-mouse-3] #'ignore)
    map)
  "Keymap for clicking on a waveform.")

(defmacro subed-waveform--with-event-context (event &rest body)
  "Evaluate BODY in the context of the buffer and position stored in EVENT."
  (declare (debug t))
  `(let ((marker (plist-get (cdr (elt (cadr ,event) 7)) :marker)))
     (with-current-buffer (marker-buffer marker)
       ,@body)))

;;; Adjusting timestamps

(defun subed-waveform--mouse-event-to-ms (event)
  "Return the millisecond position of EVENT."
  (let* ((x (car (elt (cadr event) 8)))
         (width (car (elt (cadr event) 9))))
    (floor (+ (* (/ (* 1.0 x)
                    width)
                 (- (plist-get (cdr (elt (cadr event) 7)) :stop-ms)
                    (plist-get (cdr (elt (cadr event) 7)) :start-ms)))
              (plist-get (cdr (elt (cadr event) 7)) :start-ms)))))

(defun subed-waveform-set-start (event)
  "Set the current subtitle start based on EVENT."
  (interactive "e")
  (subed-waveform--with-event-context
   event
   (save-excursion
     (subed-set-subtitle-time-start (subed-waveform--mouse-event-to-ms event))
     (subed--run-subtitle-time-adjusted-hook))))

(defun subed-waveform-set-start-and-copy-to-previous (event)
  "Set the start time of the current subtitle.
Copy it to the end time of the previous subtitle, leaving a gap
of `subed-subtitle-spacing'."
  (interactive "e")
  (subed-waveform--with-event-context
   event
   (save-excursion
     (let ((ms (subed-waveform--mouse-event-to-ms event)))
       (subed-set-subtitle-time-start ms)
       (save-excursion
         (when (subed-backward-subtitle-end)
           (subed-set-subtitle-time-stop (- ms subed-subtitle-spacing))))
       (subed--run-subtitle-time-adjusted-hook)))))

(defun subed-waveform-set-stop-and-copy-to-next (event)
  "Set the stop time of the current subtitle.
Copy it to the start time of the next subtitle, leaving a gap of
`subed-subtitle-spacing'."
  (interactive "e")
  (subed-waveform--with-event-context
   event
   (save-excursion
     (let ((ms (subed-waveform--mouse-event-to-ms event)))
       (subed-set-subtitle-time-stop ms)
       (save-excursion
         (when (subed-forward-subtitle-time-start)
           (subed-set-subtitle-time-start (+ ms subed-subtitle-spacing))))
       (subed--run-subtitle-time-adjusted-hook)))))

(defun subed-waveform-set-stop (event)
  "Set the current subtitle stop based on EVENT."
  (interactive "e")
  (subed-waveform--with-event-context
   event
   (save-excursion
     (subed-set-subtitle-time-stop (subed-waveform--mouse-event-to-ms event))
     (subed--run-subtitle-time-adjusted-hook))))

(defun subed-waveform-reduce-start-time (event)
  "Make this subtitle start `subed-milliseconds-adjust' milliseconds earlier."
  (interactive "e")
  (pp (car event))
  (subed-waveform--with-event-context
   event
   (save-excursion
     (subed-adjust-subtitle-time-start
      (if (eq (car event) 'S-drag-mouse-1)
	        (let ((x1 (car (elt (elt event 2) 2)))
		            (x2 (car (elt (elt event 1) 2))))
            (floor
	           (/ (- x1 x2)
		            (* 0.001 subed-waveform-pixels-per-second))))
        (- subed-milliseconds-adjust))
      nil t)
     (subed--run-subtitle-time-adjusted-hook))))

(defun subed-waveform-increase-stop-time (event)
  "Make this subtitle stop `subed-milliseconds-adjust' milliseconds later."
  (interactive "e")
  (subed-waveform--with-event-context
   event
   (save-excursion
     (subed-adjust-subtitle-time-stop
      (if (eq (car event) 'S-drag-mouse-3)
	        (let ((x1 (car (elt (elt event 2) 2)))
		            (x2 (car (elt (elt event 1) 2))))
            (floor (/ (- x1 x2)
		                  (* 0.001 subed-waveform-pixels-per-second))))
	      subed-milliseconds-adjust)
      nil t)
     (subed--run-subtitle-time-adjusted-hook))))

;;; Sampling

(defun subed-waveform-jump-to-timestamp (event)
  "Jump to the timestamp at EVENT and play a short sample.
The `subed-waveform-sample-msecs' variable specifies the duration
of the sample.  Jump to the specified position afterwards so that
you can use it in `subed-split-subtitle' and other commands."
  (interactive "e")
  (subed-waveform--with-event-context
   event
   (let* ((ms (subed-waveform--mouse-event-to-ms event))
          (ts (subed-msecs-to-timestamp ms)))
     (subed-mpv-jump ms)
     (message "%s" ts)
     (if (> (or subed-waveform-sample-msecs 0) 0)
         (subed-waveform-play-sample
          ms
          (min
           (- (plist-get (cdr (elt (cadr event) 7)) :stop-ms)
	            ms)
           subed-waveform-sample-msecs))
       (subed-mpv-jump ms)))))

(defun subed-waveform--restore-mpv-position (reset-msecs)
  "Jump back to RESET-MSECS."
  (subed-mpv-pause)
  (subed-mpv-jump reset-msecs)
  (when subed-waveform--enable-point-to-player-sync-after-sample
    (subed-enable-sync-point-to-player t))
  (when subed-waveform--enable-loop-over-current-subtitle-after-sample
    (subed-enable-loop-over-current-subtitle t))
  (setq subed-waveform--enable-loop-over-current-subtitle-after-sample nil
        subed-waveform--enable-point-to-player-sync-after-sample nil))

(defun subed-waveform-play-sample (msecs &optional duration-ms)
  "Play starting at MSECS position for DURATION-MS seconds.
If DURATION is unspecified, use `subed-waveform-sample-msecs.'"
  (subed-mpv-jump msecs)
  (subed-mpv-unpause)
  (when (subed-loop-over-current-subtitle-p)
    (setq subed-waveform--enable-loop-over-current-subtitle-after-sample t)
    (subed-disable-loop-over-current-subtitle t))
  (when (subed-sync-point-to-player-p)
    (setq subed-waveform--enable-point-to-player-sync-after-sample t)
    (subed-disable-sync-point-to-player t))
  (if (timerp subed-waveform--sample-timer) (cancel-timer subed-waveform--sample-timer))
  (setq subed-waveform--sample-timer
        (run-at-time (/ (or duration-ms subed-waveform-sample-msecs) 1000.0) nil
                     #'subed-waveform--restore-mpv-position
                     msecs)))

;;; Displaying the waveform

(defun subed-waveform-image-data (input-file
                                  width height
                                  &optional start-ms stop-ms
                                  callback)
  "Returns a string representing the image data in PNG format.
INPUT-FILE is the input file.  WIDTH and HEIGHT are the
dimensions in pixels.  If specified, START-MS (inclusive) and
STOP-MS (exclusive) are the times in milliseconds.  The result
can be used in `create-image.'  If CALLBACK is a fucntion, call
ffmpeg asynchronously, then call the function with the image data."
  (let* ((start-s (if start-ms (/ start-ms 1000.0)))
         (end-s (if stop-ms (/ stop-ms 1000.0)))
         (args
          (append
           (list "-i" input-file)
           (list
            "-loglevel"
            "8"
            "-filter_complex"
            (format "[0:a]aselect='%s',asetpts='N/SR/TB'[p];[p]showwavespic=s=%dx%d%s"
                    (cond
                     ((and start-s end-s) (format "between(t,%.3f,%.3f)" start-s end-s))
                     (start-s (format "gte(t,%.3f)" start-s))
                     (end-s (format "lte(t,%.3f)" start-s))
                     (t "1"))
                    width height
                    (cond
                     ((functionp subed-waveform-ffmpeg-filter-args)
                      (funcall subed-waveform-ffmpeg-filter-args width height))
                     ((stringp subed-waveform-ffmpeg-filter-args)
                      subed-waveform-ffmpeg-filter-args)
                     (t "")))
            "-frames:v" "1"
            "-f" "image2" "-"))))
    (if (functionp callback)
        (let* ((buffer (generate-new-buffer " *temp*" t))
               (process ))
	        (when (process-live-p subed-waveform--ffmpeg-process)
	          (quit-process subed-waveform--ffmpeg-process))
	        (setq subed-waveform--ffmpeg-process
		            (apply 'start-process "ffmpeg" buffer
                       subed-ffmpeg-executable args))
          (set-process-sentinel
           subed-waveform--ffmpeg-process
           (lambda (process event)
             (when (save-match-data (string-match "finished" event))
               (with-current-buffer (process-buffer process)
                 (funcall callback (encode-coding-string (buffer-string) 'binary)))))))
      (with-temp-buffer
        (apply 'call-process subed-ffmpeg-executable nil t nil args)
        (encode-coding-string (buffer-string) 'binary)))))

(defun subed-waveform-fancy-filter (width height)
  "Displays green waveforms on a dark green background with a grid.
WIDTH and HEIGHT are given in pixels."
	(concat
	 ":colors=#9cf42f[fg];"
   (format "color=s=%dx%d:color=#44582c,drawgrid=width=iw/10:height=ih/5:color=#9cf42f@0.1[bg];"
           width height)
   "[bg][fg]overlay=format=auto,drawbox=x=(iw-w)/2:y=(ih-h)/2:w=iw:h=1:color=#9cf42f"))

(defun subed-waveform--update-overlay-with-image-data (image-data &rest props)
  "Update the subed waveform."
  (with-current-buffer (marker-buffer (plist-get props :marker))
    (overlay-put subed-waveform--overlay
                 'before-string
                 (propertize
                  "x"
                  'display
                  (apply 'create-image image-data nil t
                         props)
                  'keymap subed-waveform-map))))

(defun subed-waveform-show (&optional force)
  "Display the waveform for the current subtitle.
By default, this command updates the waveform only if the start
or stop times have changed.  With \\[universal-argument] prefix,
always update the waveform."
  (interactive "P")
  (when subed-mpv-video-file
    (save-excursion
      (unless (overlayp subed-waveform--overlay)
        (setq subed-waveform--overlay
              (make-overlay (point)
                            (point))))
      (when (subed-jump-to-subtitle-time-stop)
        (end-of-line)
        (unless (eq (overlay-start subed-waveform--overlay) (point))
          (move-overlay subed-waveform--overlay (point) (point))
          (overlay-put subed-waveform--overlay 'before-string nil))
        (let* ((start-ms (subed-subtitle-msecs-start))
               (stop-ms (subed-subtitle-msecs-stop))
               (current-image-props
                (if (overlay-get subed-waveform--overlay 'before-string)
                    (cdr (get-text-property 0 'display (overlay-get subed-waveform--overlay 'before-string)))))
               (width (* (- stop-ms start-ms) subed-waveform-pixels-per-second 0.001))
               (marker (point-marker)))
          (when (and
                 (or (null subed-waveform-maximum-msecs)
                     (<= (- stop-ms start-ms) subed-waveform-maximum-msecs))
                 (or force
                      (null current-image-props)
		                  (null (plist-get current-image-props :start-ms))
		                  (null (plist-get current-image-props :stop-ms))
                      (not (= start-ms (plist-get current-image-props :start-ms)))
                      (not (= stop-ms (plist-get current-image-props :stop-ms)))))
            (subed-waveform-image-data
             subed-mpv-video-file
             width
             subed-waveform-height
	           start-ms
	           stop-ms
             (lambda (image-data)
	             (subed-waveform--update-overlay-with-image-data
		            image-data
		            :start-ms start-ms
                :stop-ms stop-ms
                :marker marker)))))))))

(defun subed-waveform-show-maybe (&rest _)
  "Update waveform overlay if needed.
This function ignores its arguments and is useful for hooks."
  (subed-waveform-show))

(defun subed-waveform-remove ()
  "Remove waveform overlay."
  (interactive)
  (when (overlayp subed-waveform--overlay)
    (delete-overlay subed-waveform--overlay)))

;;;###autoload
(define-minor-mode subed-waveform-minor-mode
  "Display waveforms for subtitles. Update on motion."
  nil nil nil
  (if subed-waveform-minor-mode
      (progn
        (add-hook 'before-save-hook #'subed-waveform-remove nil t)
        (add-hook 'after-save-hook #'subed-waveform-show-maybe nil t)
        (add-hook 'subed-subtitle-motion-hook #'subed-waveform-show-maybe nil t)
        (add-hook 'after-change-motion-hook #'subed-waveform-show-maybe nil t)
        (add-hook 'subed-subtitle-time-adjusted-hook #'subed-waveform-show-maybe nil t))
    (subed-waveform-remove)
    (remove-hook 'before-save-hook #'subed-waveform-remove t)
    (remove-hook 'after-save-hook #'subed-waveform-show-maybe t)
    (remove-hook 'subed-subtitle-motion-hook #'subed--show t)
    (remove-hook 'subed-subtitle-time-adjusted-hook #'subed-waveform-show-maybe t)
    (remove-hook 'after-change-motion-hook #'subed-waveform-show-maybe t)))

(provide 'subed-waveform)

;;; subed-waveform.el ends here
