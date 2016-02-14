;;; helm-mpd.el - Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-02-14 17:43:34 tuemura>
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
         (progn
           (setq *helm-mpd-conn* ,var)
           ,@body)
       (mpd-close-connection ,var))))

(defun helm-mpd-read-host-and-port ()
  (if current-prefix-arg
      (list (read-string (format "Host (default: %s): " helm-mpd-host)
                         nil nil helm-mpd-host)
            (read-number "Port: " helm-mpd-port))
    (list helm-mpd-host helm-mpd-port)))

;; ----------------------------------------------------------------
;; Common
;; ----------------------------------------------------------------

(defun helm-mpd-refresh (conn)
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (mpd-update conn)
      (helm-force-update))))

(defun helm-mpd-map (conn)
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v `(("C-c u" . ,(helm-mpd-refresh conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

;; ----------------------------------------------------------------
;; Current playlist
;; ----------------------------------------------------------------

(defun helm-mpd-play-song (conn)
  (lambda (song)
    (mpd-play conn (getf song 'Id) t)))

(defun helm-mpd-delete-songs (conn)
  (lambda (_ignore)
    (dolist (song (helm-marked-candidates))
      (mpd-delete conn (getf song 'Id) t)
      (message "Delete %s from the current playlist" (getf song 'Title)))))

(defun helm-mpd-run-delete-songs (conn)
  "Run `helm-mpd-delete-songs' action from `helm-mpd-build-current-playlist-source'."
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (helm-mpd-delete-songs conn)))))

(defun helm-mpd-swap-songs (conn)
  (lambda (first)
    (let ((cs (helm-marked-candidates))
          (second nil))
      (cond ((= (length cs) 2)
             (setq first (car cs)
                   second (cadr cs)))
            ((and (= (length cs) 1) (not (eq first (car cs))))
             (setq second (car cs))))
      (if second
          (progn
            (mpd-swap conn (list (getf first 'Id)) (list (getf second 'Id)) t)
            (message "Swap %s and %s" (getf first 'Title) (getf second 'Title)))
        (message "That action can be performed only with two candidates.")))))

(defun helm-mpd-run-swap-songs (conn)
  "Run `helm-mpd-swap-songs' action from `helm-mpd-build-current-playlist-source'."
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (helm-mpd-swap-songs conn)))))

(defun helm-mpd-current-playlist-actions (conn)
  (helm-make-actions
   "Play song" (helm-mpd-play-song conn)
   "Delete song(s)" (helm-mpd-delete-songs conn)
   "Swap song(s)" (helm-mpd-swap-songs conn)))

(defun helm-mpd-current-playlist-map (conn)
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-D" . ,(helm-mpd-run-delete-songs conn))
                 ("M-S" . ,(helm-mpd-run-swap-songs conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-current-playlist-candidates (conn)
  "Get current playlist."
  (lambda ()
    (mapcar (lambda (song)
              (cons (getf song 'Title) song))
            (mpd-get-playlist-entry conn))))

(defun helm-mpd-build-current-playlist-source (conn)
  (helm-build-sync-source "Current playlist"
    :candidates (helm-mpd-current-playlist-candidates conn)
    :action (helm-mpd-current-playlist-actions conn)
    :keymap (helm-mpd-current-playlist-map conn)
    :migemo t))

;;;###autoload
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
  (lambda ()
    (let ((ls nil))
      (mpd-list-directory-recursive conn (lambda (obj directoryp)
                                           (push (cons obj (cons (if directoryp
                                                                     'directory
                                                                   'file)
                                                                 obj))
                                                 ls)))
      ls)))

(defun helm-mpd-library-actions (conn)
  (helm-make-actions
   "Enqueue song(s)" (lambda (_ignore)
                       (dolist (obj (helm-marked-candidates))
                         (mpd-enqueue conn (cdr obj))))))

(defun helm-mpd-build-library-source (conn)
  (helm-build-sync-source "Library"
    :candidates (helm-mpd-library-candidates conn)
    :action (helm-mpd-library-actions conn)
    :keymap (helm-mpd-map conn)))

;;;###autoload
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
  (lambda ()
    (let ((ls nil))
      (mpd-get-directory-info conn nil
                              (lambda (obj type)
                                (when (eq type 'playlist)
                                  (push obj ls))))
      ls)))

(defun helm-mpd-load-playlists (conn)
  (lambda (_ignore)
    (let ((playlists (helm-marked-candidates)))
      (mpd-load-playlist conn playlists)
      (message "Load playlists %s" playlists))))

(defun helm-mpd-remove-playlists (conn)
  (lambda (_ignore)
    (let ((playlists (helm-marked-candidates)))
      (mpd-remove-playlist conn playlists)
      (message "Remove playlists %s" playlists))))

(defun helm-mpd-run-remove-playlists (conn)
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (helm-mpd-remove-playlists conn)))))

(defun helm-mpd-new-playlist-actions (conn)
  (helm-make-actions
   "Save current playlist to file" (lambda (pname)
                                     (mpd-save-playlist conn pname)
                                     (message "Save the current playlist as %s" pname))))

(defun helm-mpd-playlist-actions (conn)
  (helm-make-actions
   "Load playlist(s)" (helm-mpd-load-playlists conn)
   "Remove playlist(s)" (helm-mpd-remove-playlists conn)))

(defun helm-mpd-playlist-map (conn)
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-D" . ,(helm-mpd-run-remove-playlists conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-playlist-source (conn)
  (helm-build-sync-source "Playlists"
    :candidates (helm-mpd-playlist-candidates conn)
    :action (helm-mpd-playlist-actions conn)
    :keymap (helm-mpd-playlist-map conn)))

(defun helm-mpd-build-new-playlist-source (conn)
  (helm-build-dummy-source "Create playlist"
    :action (helm-mpd-new-playlist-actions conn)
    :keymap (helm-mpd-map conn)))

;;;###autoload
(defun helm-mpd-playlist (host port)
  (interactive (helm-mpd-read-host-and-port))
  (helm-mpd-with-conn (conn helm-mpd-host helm-mpd-port)
                      (helm :sources (list (helm-mpd-build-playlist-source conn)
                                           (helm-mpd-build-new-playlist-source conn))
                            :buffer "*helm-mpd-playlist*")))

;; ----------------------------------------------------------------
;; Put together
;; ----------------------------------------------------------------

;;;###autoload
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
