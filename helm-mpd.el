;;; helm-mpd.el - Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-02-14 02:09:24 tuemura>
;;
;;; Code:

(require 'helm)
(require 'libmpdee)

(defgroup helm-mpd nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defcustom helm-mpd-host "localhost"
  "Default host for `helm-mpd'."
  :group 'helm-mpd
  :type 'string)

(defcustom helm-mpd-port 6600
  "Default port for `helm-mpd'."
  :group 'helm-mpd
  :type 'integer)

(defmacro* helm-mpd-with-conn ((var host port &rest args) &rest body)
  "Evaluate BODY with an MPD connection named VAR."
  `(let ((,var (mpd-conn-new ,host ,port ,@args)))
     (unwind-protect
         (progn ,@body)
       (mpd-close-connection ,var))))

(defun helm-mpd-read-host-and-port ()
  (if current-prefix-arg
      (list (read-string (format "Host (default: %s): " helm-mpd-host)
                         nil nil helm-mpd-host)
            (read-number "Port: " helm-mpd-port))
    (list helm-mpd-host helm-mpd-port)))

;; ----------------------------------------------------------------
;; Current playlist
;; ----------------------------------------------------------------

(defun helm-mpd-current-playlist-actions (conn)
  (helm-make-actions
   "Play song" (lambda (song) (mpd-play conn (getf song 'Id) t))
   "Delete song(s)" (lambda (_ignore)
                      (dolist (song (helm-marked-candidates))
                        (mpd-delete conn (getf song 'Id) t)))
   "Swap song(s)" (lambda (first)
                    (let ((cs (helm-marked-candidates))
                          (second nil))
                      (cond ((= (length cs) 2)
                             (setq first (car cs)
                                   second (cadr cs)))
                            ((and (= (length cs) 1) (not (eq first (car cs))))
                             (setq second (car cs))))
                      (if second
                          (mpd-swap conn (list (getf first 'Id)) (list (getf second 'Id)) t)
                        (message "That action can be performed only with two candidates."))))))

(defun helm-mpd-current-playlist-candidates (conn)
  "Get current playlist."
  (mapcar (lambda (song)
            (cons (getf song 'Title) song))
          (mpd-get-playlist-entry conn)))

(defun helm-mpd-build-current-playlist-source (conn)
  (helm-build-sync-source "Current playlist"
    :candidates (helm-mpd-current-playlist-candidates conn)
    :action (helm-mpd-current-playlist-actions conn)
    :migemo t))

(defun helm-mpd-current-playlist (host port)
  "Helm for current MPD playlist."
  (interactive (helm-mpd-read-host-and-port))
  (helm-mpd-with-conn (conn host port)
                      (helm :sources (helm-mpd-build-current-playlist-source conn)
                            :buffer "*helm-mpd-current-playlist*")))

;; ----------------------------------------------------------------
;; Libraries
;; ----------------------------------------------------------------

(defun helm-mpd-library-candidates (conn)
  "Get all files and directories in the MPD database."
  (let ((ls nil))
    (mpd-list-directory-recursive conn (lambda (obj directoryp)
                                         (push (cons obj (cons (if directoryp
                                                                   'directory
                                                                 'file)
                                                               obj))
                                               ls)))
    ls))

(defun helm-mpd-library-actions (conn)
  (helm-make-actions
   "Enqueue song(s)" (lambda (_ignore)
                       (dolist (obj (helm-marked-candidates))
                         (mpd-enqueue conn (cdr obj))))))

(defun helm-mpd-build-library-source (conn)
  (helm-build-sync-source "Library"
    :candidates (helm-mpd-library-candidates conn)
    :action (helm-mpd-library-actions conn)))

(defun helm-mpd-library (host port)
  "Helm for MPD library."
  (interactive (helm-mpd-read-host-and-port))
  (helm-mpd-with-conn (conn host port)
                      (helm :sources (helm-mpd-build-library-source conn)
                            :buffer "*helm-mpd-library*")))

;; ----------------------------------------------------------------
;; Play lists
;; ----------------------------------------------------------------

(defun helm-mpd-playlist-candidates (conn)
  "Get all playlists."
  (let ((ls nil))
    (mpd-get-directory-info conn nil
                            (lambda (obj type)
                              (when (eq type 'playlist)
                                (push obj ls))))
    ls))

(defun helm-mpd-new-playlist-actions (conn)
  (helm-make-actions
   "Save current playlist to file" (lambda (pname)
                                     (mpd-save-playlist conn pname))))

(defun helm-mpd-playlist-actions (conn)
  (helm-make-actions
   "Load playlist(s)" (lambda (_ignore)
                        (mpd-load-playlist conn (helm-marked-candidates)))
   "Remove playlist(s)" (lambda (_ignore)
                          (mpd-remove-playlist conn (helm-marked-candidates)))))

(defun helm-mpd-build-playlist-source (conn)
  (helm-build-sync-source "Playlists"
    :candidates (helm-mpd-playlist-candidates conn)
    :action (helm-mpd-playlist-actions conn)))

(defun helm-mpd-build-new-playlist-source (conn)
  (helm-build-dummy-source "Create playlist"
    :action (helm-mpd-new-playlist-actions conn)))

(defun helm-mpd-playlist (host port)
  (interactive (helm-mpd-read-host-and-port))
  (helm-mpd-with-conn (conn helm-mpd-host helm-mpd-port)
                      (helm :sources (list (helm-mpd-build-playlist-source conn)
                                           (helm-mpd-build-new-playlist-source conn))
                            :buffer "*helm-mpd-playlist*")))

;; ----------------------------------------------------------------
;; Put together
;; ----------------------------------------------------------------

(defun helm-mpd (host port)
  "Helm for MPD."
  (interactive (helm-mpd-read-host-and-port))
  (helm-mpd-with-conn (conn host port)
                      (helm :sources (list (helm-mpd-build-current-playlist-source conn)
                                           (helm-mpd-build-library-source conn)
                                           (helm-mpd-build-playlist-source conn)
                                           (helm-mpd-build-new-playlist-source conn))
                            :buffer "*helm-mpd*")))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
