;;; helm-mpd.el - Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-05 21:22:16 tuemura>
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

(defun helm-mpd-read-host-and-port ()
  "Read MPD host and port, and return a MPD connection."
  (if current-prefix-arg
      (mpd-conn-new (read-string (format "Host (default: %s): " helm-mpd-host)
                                 nil nil helm-mpd-host)
                    (read-number "Port: " helm-mpd-port))
    mpd-inter-conn))

(defmacro defclosure (name vars &optional docstring &rest body)
  "Define a closure."
  (declare (indent defun)
           (doc-string 3))
  (let ((let-vars (apply 'concatenate 'list
                         (mapcar (lambda (x)
                                   (unless (eq (aref (symbol-name x) 0) ?&)
                                     (list (list x x))))
                                 vars)))
        (doc nil))
    (if (stringp docstring)
        (setq doc (list docstring))
      (setq body (cons docstring body)))
    `(defun ,name ,vars
       ,@doc
       (lexical-let ,let-vars
         ,@body))))

;; ----------------------------------------------------------------
;; Common
;; ----------------------------------------------------------------

(defclosure helm-mpd-refresh (conn)
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
;; (Optional) Tag editor
;; 
;; Available when `emacs-id3.el' <https://github.com/uemurax/emacs-id3> is installed.
;; ----------------------------------------------------------------

(defun helm-mpd-has-tag-editor-p ()
  "Return t if Emacs has an id3 tag editor."
  (and (locate-library "emacs-id3")
       (executable-find "mid3v2")
       t))

(defcustom helm-mpd-tag-edit-buffer-name "*helm-mpd-tag-edit*"
  "Tag editor buffer name."
  :group 'helm-mpd
  :type 'string)

(defcustom helm-mpd-library-directory (expand-file-name "~/Music")
  "Local MPD library directory."
  :group 'helm-mpd
  :type 'string)

(defun helm-mpd-spawn-tag-editor (&rest songs)
  (switch-to-buffer helm-mpd-tag-edit-buffer-name)
  (erase-buffer)
  (apply #'id3-read-with-mid3v2 t
         (mapcar (lambda (song) (expand-file-name song helm-mpd-library-directory))
                 songs))
  (id3-edit-mode)
  (goto-char (point-min)))

;; ----------------------------------------------------------------
;; Lyrics
;; ----------------------------------------------------------------

(defcustom helm-mpd-lyrics-directory
  (expand-file-name "~/.lyrics")
  "Lyrics directory."
  :group 'helm-mpd
  :type 'string)

(defun helm-mpd-format-lyrics (song)
  (format "%s - %s.txt"
          (getf song 'Artist)
          (getf song 'Title)))

;; ----------------------------------------------------------------
;; Current playlist
;; ----------------------------------------------------------------

(defclosure helm-mpd-play-song (conn)
  (lambda (song)
    (mpd-play conn (getf song 'Id) t)))

(defclosure helm-mpd-delete-songs (conn)
  (lambda (_ignore)
    (dolist (song (helm-marked-candidates))
      (mpd-delete conn (getf song 'Id) t)
      (message "Delete %s from the current playlist" (getf song 'Title)))))

(defclosure helm-mpd-run-delete-songs (conn)
  "Run `helm-mpd-delete-songs' action from `helm-mpd-build-current-playlist-source'."
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (helm-mpd-delete-songs conn)))))

(defclosure helm-mpd-run-delete-songs-persistent (conn)
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-attrset 'delete-action (cons (helm-mpd-delete-songs conn) 'never-split))
      (helm-execute-persistent-action 'delete-action)
      (message nil)
      (helm-force-update))))

(defclosure helm-mpd-swap-songs (conn)
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

(defclosure helm-mpd-run-swap-songs (conn)
  "Run `helm-mpd-swap-songs' action from `helm-mpd-build-current-playlist-source'."
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (helm-mpd-swap-songs conn)))))

(defclosure helm-mpd-move (conn n)
  (lambda (song)
    (let ((pos (getf song 'Pos)))
      (if pos
          (mpd-move conn (list pos) (list (+ pos n)))
        (message "Invalid song.")))))

(defclosure helm-mpd-run-move-down-persistent (conn)
  (lambda (n)
    (interactive "p")
    (with-helm-alive-p
      (helm-attrset 'move-down-action (cons (helm-mpd-move conn n) 'never-split))
      (helm-execute-persistent-action 'move-down-action)
      (message nil)
      (helm-force-update))))

(defclosure helm-mpd-run-move-up-persistent (conn)
  (lambda (n)
    (interactive "p")
    (funcall (helm-mpd-run-move-down-persistent conn) (- n))))

(defclosure helm-mpd-edit-songs (conn)
  (lambda (_ignore)
    (apply #'helm-mpd-spawn-tag-editor
           (mapcar (lambda (song)
                     (getf song 'file))
                   (helm-marked-candidates)))))

(defclosure helm-mpd-run-edit-songs (conn)
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (helm-mpd-edit-songs conn)))))

(defun helm-mpd-edit-lyrics (song)
  (find-file (expand-file-name (helm-mpd-format-lyrics song)
                               helm-mpd-lyrics-directory)))

(defun helm-mpd-run-edit-lyrics ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-mpd-edit-lyrics)))
(put 'helm-mpd-run-edit-lyrics 'helm-only t)

(defun helm-mpd-current-playlist-actions (conn)
  (helm-make-actions
   "Play song" (helm-mpd-play-song conn)
   "Delete song(s)" (helm-mpd-delete-songs conn)
   "Swap song(s)" (helm-mpd-swap-songs conn)
   "Move song up" (helm-mpd-move conn -1)
   "Move song down" (helm-mpd-move conn 1)
   (when (helm-mpd-has-tag-editor-p)
     "Edit song(s)")
   (helm-mpd-edit-songs conn)
   "Edit lyrics" 'helm-mpd-edit-lyrics))

(defun helm-mpd-current-playlist-map (conn)
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-D" . ,(helm-mpd-run-delete-songs conn))
                 ("C-c d" . ,(helm-mpd-run-delete-songs-persistent conn))
                 ("M-S" . ,(helm-mpd-run-swap-songs conn))
                 ("C-c C-n" . ,(helm-mpd-run-move-down-persistent conn))
                 ("C-c C-p" . ,(helm-mpd-run-move-up-persistent conn))
                 ,@(when (helm-mpd-has-tag-editor-p)
                     (list (cons "M-E" (helm-mpd-run-edit-songs conn))))
                 ("M-L" . helm-mpd-run-edit-lyrics)))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defclosure helm-mpd-current-playlist-candidates (conn)
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
(defun helm-mpd-current-playlist (conn)
  "Helm for current MPD playlist."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (helm-mpd-build-current-playlist-source conn)
        :buffer "*helm-mpd-current-playlist*"))

;; ----------------------------------------------------------------
;; Libraries
;; ----------------------------------------------------------------

;;; ----------------------------------------------------------------
;;; Songs
;;; ----------------------------------------------------------------

(defclosure helm-mpd-song-candidates (conn &optional filter)
  "Get all files and directories in the MPD database."
  (lambda ()
    (cl-labels ((get-songs (conn)
                           (if (consp filter)
                               (mpd-search conn (car filter) (cdr filter))
                             (mpd-get-directory-songs conn))))
      (mapcar (lambda (song)
                (cons (getf song 'file) song))
              (get-songs conn)))))

(defun helm-mpd-enqueue (conn songs)
  (mpd-enqueue conn
               (mapcar (lambda (song)
                         (getf song 'file))
                       songs)))

(defclosure helm-mpd-enqueue-files (conn)
  (lambda (_ignore)
    (helm-mpd-enqueue conn (helm-marked-candidates))))

(defun helm-mpd-song-actions (conn)
  (helm-make-actions
   "Enqueue song(s)" (helm-mpd-enqueue-files conn)
   (when (helm-mpd-has-tag-editor-p)
     "Edit song(s)")
   (helm-mpd-edit-songs conn)
   "Edit lyrics" 'helm-mpd-edit-lyrics))

(defun helm-mpd-song-map (conn)
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-E" . ,(helm-mpd-run-edit-songs conn))
                 ("M-L" . helm-mpd-run-edit-lyrics)))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-song-source (conn &optional filter)
  (helm-build-sync-source "Songs"
    :candidates (helm-mpd-song-candidates conn filter)
    :action (helm-mpd-song-actions conn)
    :keymap (helm-mpd-song-map conn)))

;;;###autoload
(defun helm-mpd-songs (conn &optional filter)
  "Helm for songs in MPD library."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (helm-mpd-build-song-source conn filter)
        :buffer "*helm-mpd-songs*"))

;;; ----------------------------------------------------------------
;;; Artists
;;; ----------------------------------------------------------------

(defclosure helm-mpd-artist-candidates (conn)
  (lambda ()
    (mpd-get-artists conn)))

(defclosure helm-mpd-enqueue-artists (conn)
  (lambda (_ignore)
    (helm-mpd-enqueue conn
                      (mpd-search conn 'artist (helm-marked-candidates)))))

(defclosure helm-mpd-helm-for-artists (conn)
  (lambda (_ignore)
    (helm-mpd-library conn `(artist . ,(helm-marked-candidates)))))

(defclosure helm-mpd-run-helm-for-artists (conn)
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (helm-mpd-helm-for-artists conn)))))

(defun helm-mpd-artist-actions (conn)
  (helm-make-actions
   "Enqueue artist(s)' songs" (helm-mpd-enqueue-artists conn)
   "Helm for artist(s)" (helm-mpd-helm-for-artists conn)))

(defun helm-mpd-artist-map (conn)
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-H" . ,(helm-mpd-run-helm-for-artists conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-artist-source (conn)
  (helm-build-sync-source "Artists"
    :candidates (helm-mpd-artist-candidates conn)
    :action (helm-mpd-artist-actions conn)
    :keymap (helm-mpd-artist-map conn)
    :migemo t))

;;;###autoload
(defun helm-mpd-artists (conn)
  "Helm for artists in MPD library."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (helm-mpd-build-artist-source conn)
        :buffer "*helm-mpd-artists*"))

;;; ----------------------------------------------------------------
;;; Albums
;;; ----------------------------------------------------------------

(defclosure helm-mpd-album-candidates (conn &optional filter)
  (lambda ()
    (let ((artist (if (and (consp filter) (eq (car filter) 'artist))
                      (cdr filter)
                    nil)))
      (mpd-get-artist-albums conn artist))))

(defclosure helm-mpd-enqueue-albums (conn)
  (lambda (_ignore)
    (helm-mpd-enqueue conn
                      (mpd-search conn 'album (helm-marked-candidates)))))

(defclosure helm-mpd-helm-for-albums (conn)
  (lambda (_ignore)
    (helm-mpd-library conn `(album . ,(helm-marked-candidates)))))

(defclosure helm-mpd-run-helm-for-albums (conn)
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (helm-mpd-helm-for-albums conn)))))

(defun helm-mpd-album-actions (conn)
  (helm-make-actions
   "Enqueue album(s)' songs" (helm-mpd-enqueue-albums conn)
   "Helm for album(s)' songs" (helm-mpd-album-songs conn)))

(defun helm-mpd-album-map (conn)
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-H" . ,(helm-mpd-run-helm-for-albums conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-album-source (conn &optional filter)
  (helm-build-sync-source "Albums"
    :candidates (helm-mpd-album-candidates conn filter)
    :action (helm-mpd-album-actions conn)
    :keymap (helm-mpd-album-map conn)
    :migemo t))

;;;###autoload
(defun helm-mpd-albums (conn &optional filter)
  "Helm for albums in MPD library."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (helm-mpd-build-album-source conn filter)
        :buffer "*helm-mpd-albums*"))

;;; ----------------------------------------------------------------
;;; Put together
;;; ----------------------------------------------------------------

(defun helm-mpd-build-library-source (conn &optional filter)
  (concatenate 'list
               (list (helm-mpd-build-song-source conn filter))
               (unless filter
                 (list (helm-mpd-build-artist-source conn)))
               (unless (and filter (eq (car filter) 'album))
                 (list (helm-mpd-build-album-source conn filter)))))

;;;###autoload
(defun helm-mpd-library (conn &optional filter)
  "Helm for MPD library."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (helm-mpd-build-library-source conn filter)
        :buffer "*helm-mpd-library*"))

;; ----------------------------------------------------------------
;; Play lists
;; ----------------------------------------------------------------

(defclosure helm-mpd-playlist-candidates (conn)
  "Get all playlists."
  (lambda ()
    (let ((ls nil))
      (mpd-get-directory-info conn nil
                              (lambda (obj type)
                                (when (eq type 'playlist)
                                  (push obj ls))))
      ls)))

(defclosure helm-mpd-save-playlist (conn)
  (lambda (pname)
    (mpd-save-playlist conn pname)
    (message "Save the current playlist as %s" pname)))

(defclosure helm-mpd-load-playlists (conn)
  (lambda (_ignore)
    (let ((playlists (helm-marked-candidates)))
      (mpd-load-playlist conn playlists)
      (message "Load playlists %s" playlists))))

(defclosure helm-mpd-remove-playlists (conn)
  (lambda (_ignore)
    (let ((playlists (helm-marked-candidates)))
      (mpd-remove-playlist conn playlists)
      (message "Remove playlists %s" playlists))))

(defclosure helm-mpd-run-remove-playlists (conn)
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (helm-mpd-remove-playlists conn)))))

(defclosure helm-mpd-run-remove-playlists-persistent (conn)
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-attrset 'delete-action (cons (helm-mpd-remove-playlists conn) 'never-split))
      (helm-execute-persistent-action 'delete-action)
      (message nil)
      (helm-force-update))))

(defun helm-mpd-new-playlist-actions (conn)
  (helm-make-actions
   "Save current playlist to file" (helm-mpd-save-playlist conn)))

(defun helm-mpd-playlist-actions (conn)
  (helm-make-actions
   "Load playlist(s)" (helm-mpd-load-playlists conn)
   "Remove playlist(s)" (helm-mpd-remove-playlists conn)))

(defun helm-mpd-playlist-map (conn)
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-D" . ,(helm-mpd-run-remove-playlists conn))
                 ("C-c d" . ,(helm-mpd-run-remove-playlists-persistent conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-existing-playlist-source (conn)
  (helm-build-sync-source "Playlists"
    :candidates (helm-mpd-playlist-candidates conn)
    :action (helm-mpd-playlist-actions conn)
    :keymap (helm-mpd-playlist-map conn)))

(defun helm-mpd-build-new-playlist-source (conn)
  (helm-build-dummy-source "Create playlist"
    :action (helm-mpd-new-playlist-actions conn)
    :keymap (helm-mpd-map conn)))

(defun helm-mpd-build-playlist-source (conn)
  (list (helm-mpd-build-existing-playlist-source conn)
        (helm-mpd-build-new-playlist-source conn)))

;;;###autoload
(defun helm-mpd-playlist (conn)
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (helm-mpd-build-playlist-source conn)
        :buffer "*helm-mpd-playlist*"))

;; ----------------------------------------------------------------
;; Put together
;; ----------------------------------------------------------------

;;;###autoload
(defun helm-mpd (conn)
  "Helm for MPD."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (concatenate 'list
                              (list (helm-mpd-build-current-playlist-source conn))
                              (helm-mpd-build-library-source conn)
                              (helm-mpd-build-playlist-source conn))
        :buffer "*helm-mpd*"))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
