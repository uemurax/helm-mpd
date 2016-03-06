;;; helm-mpd.el - Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-07 00:14:20 tuemura>
;;
;;; Code:

(require 'helm)
(require 'libmpdee)

(defgroup helm-mpd nil
  "Predefined configurations for `helm-mpd.el'."
  :group 'helm)

(defcustom helm-mpd-host "localhost"
  "Default host for `helm-mpd'."
  :group 'helm-mpd
  :type 'string)

(defcustom helm-mpd-port 6600
  "Default port for `helm-mpd'."
  :group 'helm-mpd
  :type 'integer)

(defcustom helm-mpd-timeout 10
  "Default timeout for `helm-mpd'."
  :group 'helm-mpd
  :type 'integer)

(defvar helm-mpd-inter-conn (mpd-conn-new helm-mpd-host helm-mpd-port helm-mpd-timeout)
  "Default MPD connection for `helm-mpd'.")

(defun helm-mpd-read-host-and-port ()
  "Read MPD host and port, and return a MPD connection."
  (if current-prefix-arg
      (mpd-conn-new (read-string (format "Host (default: %s): " helm-mpd-host)
                                 nil nil helm-mpd-host)
                    (read-number "Port: " helm-mpd-port)
                    helm-mpd-timeout)
    helm-mpd-inter-conn))

(defun filter-variables (vars f)
  (apply 'concatenate 'list
         (mapcar (lambda (x)
                   (unless (eq (aref (symbol-name x) 0) ?&)
                     (list (funcall f x))))
                 vars)))

(defmacro defclosure (name vars &optional docstring &rest body)
  "Define a closure.
BODY should return a `lambda' form."
  (declare (indent defun)
           (doc-string 3))
  (let ((let-vars (filter-variables vars (lambda (x) (list x x))))
        (doc nil))
    (if (stringp docstring)
        (setq doc (list docstring))
      (setq body (cons docstring body)))
    `(defun ,name ,vars
       ,@doc
       (lexical-let ,let-vars
         ,@body))))

(defmacro helm-mpd-defaction (name vars &rest body)
  "Define a `helm-mpd' action. It defines three closures `helm-mpd-NAME',
`helm-mpd-run-NAME' and `helm-mpd-run-NAME-persistent'.

BODY must return a helm action.

`helm-mpd-NAME' is used as a helm action.

You can bind some key to `helm-mpd-run-NAME' in helm key map.

`helm-mpd-run-NAME-persistent' is similar to `helm-mpd-run-NAME'
but does not exit helm session."
  (declare (indent defun)
           (doc-string 3))
  (let* ((action (intern (format "helm-mpd-%s" name)))
         (run-action (intern (format "helm-mpd-run-%s" name)))
         (run-action-doc (format "Run `%s' action from helm session." action))
         (persistent-action (intern (format "helm-mpd-run-%s-persistent" name)))
         (persistent-action-doc (format "Run `%s' action without exiting helm session." action))
         (vars1 (filter-variables vars 'identity)))
    `(progn
       (defclosure ,action ,vars
         ,@body)
       (defclosure ,run-action ,vars
         ,run-action-doc
         (lambda ()
           (interactive)
           (with-helm-alive-p
             (helm-exit-and-execute-action (,action ,@vars1)))))
       (defclosure ,persistent-action ,vars
         ,persistent-action-doc
         (lambda ()
           (interactive)
           (with-helm-alive-p
             (helm-attrset 'mpd-persistent-action (cons (,action ,@vars1) 'never-split))
             (helm-execute-persistent-action 'mpd-persistent-action)
             (message nil)
             (helm-force-update)))))))

;; ----------------------------------------------------------------
;; Common
;; ----------------------------------------------------------------

(defclosure helm-mpd-refresh (conn)
  "Update MPD library and recalculate helm candidates."
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (mpd-update conn)
      (helm-force-update))))

(helm-mpd-defaction goto-top (conn)
  "Go back to the top of `helm-mpd' session."
  (lambda (_ignore)
    (helm-mpd conn)))

(defun helm-mpd-format-time (sec)
  "Format SEC."
  (when sec
    (format-time-string "%M:%S" (list 0 sec 0 0))))

(defun helm-mpd-mode-line (conn)
  "Mode line format for `helm-mpd'."
  `(:eval (let ((song (mpd-get-current-song ,conn))
                (status (mpd-get-status ,conn)))
            (run-with-timer 1 nil 'force-mode-line-update t)
            (list (case (getf status 'state)
                    ((play) "Playing")
                    ((pause) "Paused")
                    ((stop) "Stopped"))
                  ": "
                  (funcall helm-mpd-song-format song)
                  " [" (helm-mpd-format-time (getf status 'time-elapsed))
                  "/" (helm-mpd-format-time (getf status 'time-total))
                  "]"
                  " [" (when (> (getf status 'repeat) 0) "r")
                  (when (> (getf status 'random) 0) "z")
                  (when (> (string-to-int (getf status 'single)) 0) "s")
                  "]"))))

(defclass helm-mpd-source (helm-source-sync)
  ((mpd-conn :initarg :mpd-conn)))

(defun helm-mpd-build-mpd-source (conn name &rest args)
  (apply #'helm-make-source name 'helm-mpd-source
         :mpd-conn conn
         args))

(defun helm-mpd-display-mode-line-ad (source &optional force)
  "Advice for `helm-display-mode-line'."
  (let ((conn0 (assoc 'mpd-conn source)))
    (when conn0
      (let ((conn (cdr conn0)))
        (setq mode-line-format (helm-mpd-mode-line conn))
        (when force
          (force-mode-line-update))))))

(advice-add 'helm-display-mode-line :after 'helm-mpd-display-mode-line-ad)

(defclosure helm-mpd-simple-mpd-action (fun conn)
  "Make a simple MPD action."
  (lambda ()
    (interactive)
    (funcall fun conn)))

(defun helm-mpd-action-map (conn)
  "Keymap for MPD action in `helm-mpd' session."
  (let ((m (make-sparse-keymap)))
    (dolist (v '(("p" . mpd-pause)
                 ("s" . mpd-stop)
                 ("r" . mpd-toggle-repeat)
                 ("z" . mpd-toggle-random)
                 ("y" . mpd-toggle-single)
                 (">" . mpd-next)
                 ("<" . mpd-prev)
                 ("u" . mpd-update)
                 ("c" . mpd-clear-playlist)))
      (define-key m (kbd (car v)) (helm-mpd-simple-mpd-action (cdr v) conn)))
    m))

(defun helm-mpd-map (conn)
  "Parent keymap of all `helm-mpd' keymaps."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v `(("C-c u" . ,(helm-mpd-refresh conn))
                 ("C-c t" . ,(helm-mpd-run-goto-top conn))
                 ("C-c C-c" . ,(helm-mpd-action-map conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

;; ----------------------------------------------------------------
;; Song format
;; ----------------------------------------------------------------

(defface helm-mpd-artist-face
  '((t (:inherit font-lock-keyword-face)))
  "Artist face."
  :group 'helm-mpd)

(defface helm-mpd-title-face
  '((t (:inherit font-lock-function-name-face)))
  "Title face."
  :group 'helm-mpd)

(defface helm-mpd-album-face
  '((t (:inherit font-lock-type-face)))
  "Album face."
  :group 'helm-mpd)

(defcustom helm-mpd-song-format
  (lambda (song)
    (mapconcat (lambda (x)
                 (propertize (or (getf song (car x)) "") 'face (cdr x)))
               '((Artist . helm-mpd-artist-face)
                 (Title . helm-mpd-title-face)
                 (Album . helm-mpd-album-face))
               " "))
  "Function to format a song."
  :group 'helm-mpd
  :type 'function)

(defun helm-mpd-format-songs (songs)
  "Format song candidates."
  (mapcar (lambda (song)
            (cons (funcall helm-mpd-song-format song) song))
          songs))

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
  "Edit SONGS."
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
  "File name for SONG's lyrics."
  (format "%s - %s.txt"
          (getf song 'Artist)
          (getf song 'Title)))

;; ----------------------------------------------------------------
;; Current playlist
;; ----------------------------------------------------------------

(defclosure helm-mpd-play-song (conn)
  "Play the selected song."
  (lambda (song)
    (mpd-play conn (getf song 'Id) t)))

(helm-mpd-defaction delete-songs (conn)
  "Delete selected songs from the current playlist."
  (lambda (_ignore)
    (dolist (song (helm-marked-candidates))
      (mpd-delete conn (getf song 'Id) t)
      (message "Delete %s from the current playlist" (getf song 'Title)))))

(helm-mpd-defaction swap-songs (conn)
  "Swap two selected songs in the current playlist."
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

(defclosure helm-mpd-move (conn n)
  "Move the selected song by N."
  (lambda (song)
    (let ((pos (getf song 'Pos)))
      (if pos
          (mpd-move conn (list pos) (list (+ pos n)))
        (message "Invalid song.")))))

(defclosure helm-mpd-run-move-down-persistent (conn)
  "Move down the selected song without exiting helm session."
  (lambda (n)
    (interactive "p")
    (with-helm-alive-p
      (helm-attrset 'move-down-action (cons (helm-mpd-move conn n) 'never-split))
      (helm-execute-persistent-action 'move-down-action)
      (message nil)
      (helm-force-update))))

(defclosure helm-mpd-run-move-up-persistent (conn)
  "Move up the selected song without exiting helm session."
  (lambda (n)
    (interactive "p")
    (funcall (helm-mpd-run-move-down-persistent conn) (- n))))

(helm-mpd-defaction edit-songs (conn)
  "Edit selected songs."
  (lambda (_ignore)
    (apply #'helm-mpd-spawn-tag-editor
           (mapcar (lambda (song)
                     (getf song 'file))
                   (helm-marked-candidates)))))

(defun helm-mpd-edit-lyrics (song)
  "Edit SONG's lyrics."
  (find-file (expand-file-name (helm-mpd-format-lyrics song)
                               helm-mpd-lyrics-directory)))

(defun helm-mpd-run-edit-lyrics ()
  "Run `helm-mpd-edit-lyrics' from helm session."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-mpd-edit-lyrics)))
(put 'helm-mpd-run-edit-lyrics 'helm-only t)

(defun helm-mpd-current-playlist-actions (conn)
  "Actions for `helm-mpd-current-playlist'."
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
  "Keymap in `helm-mpd-current-playlist'."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-D" . ,(helm-mpd-run-delete-songs conn))
                 ("C-c d" . ,(helm-mpd-run-delete-songs-persistent conn))
                 ("M-S" . ,(helm-mpd-run-swap-songs conn))
                 ("C-c s" . ,(helm-mpd-run-swap-songs-persistent conn))
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
    (helm-mpd-format-songs (mpd-get-playlist-entry conn))))

(defun helm-mpd-build-current-playlist-source (conn)
  "Build sources for `helm-mpd-current-playlist'."
  (helm-mpd-build-mpd-source conn "Current playlist"
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
  "Get all songs in MPD library."
  (lambda ()
    (cl-labels ((get-songs (conn)
                           (if (consp filter)
                               (mpd-search conn (car filter) (cdr filter))
                             (mpd-get-directory-songs conn))))
      (helm-mpd-format-songs (get-songs conn)))))

(defun helm-mpd-enqueue (conn songs)
  "Enqueue SONGS."
  (mpd-enqueue conn
               (mapcar (lambda (song)
                         (getf song 'file))
                       songs)))

(defclosure helm-mpd-enqueue-files (conn)
  "Enqueue selected songs."
  (lambda (_ignore)
    (helm-mpd-enqueue conn (helm-marked-candidates))))

(defun helm-mpd-song-actions (conn)
  "Actions for `helm-mpd-songs'."
  (helm-make-actions
   "Enqueue song(s)" (helm-mpd-enqueue-files conn)
   (when (helm-mpd-has-tag-editor-p)
     "Edit song(s)")
   (helm-mpd-edit-songs conn)
   "Edit lyrics" 'helm-mpd-edit-lyrics))

(defun helm-mpd-song-map (conn)
  "Keymap in `helm-mpd-songs'."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-E" . ,(helm-mpd-run-edit-songs conn))
                 ("M-L" . helm-mpd-run-edit-lyrics)))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-song-source (conn &optional filter)
  "Build sources for `helm-mpd-songs'."
  (helm-mpd-build-mpd-source conn "Songs"
                             :candidates (helm-mpd-song-candidates conn filter)
                             :action (helm-mpd-song-actions conn)
                             :keymap (helm-mpd-song-map conn)
                             :migemo t))

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
  "Get all artists in MPD library."
  (lambda ()
    (mapcar (lambda (x)
              (propertize x 'face 'helm-mpd-artist-face))
            (mpd-get-artists conn))))

(defclosure helm-mpd-enqueue-artists (conn)
  "Enqueue all songs of selected artists."
  (lambda (_ignore)
    (helm-mpd-enqueue conn
                      (mpd-search conn 'artist (helm-marked-candidates)))))

(helm-mpd-defaction helm-for-artists (conn)
  "Run `helm-mpd-library' for selected artists."
  (lambda (_ignore)
    (helm-mpd-library conn `(artist . ,(helm-marked-candidates)))))

(defun helm-mpd-artist-actions (conn)
  "Actions for `helm-mpd-artists'."
  (helm-make-actions
   "Enqueue artist(s)' songs" (helm-mpd-enqueue-artists conn)
   "Helm for artist(s)" (helm-mpd-helm-for-artists conn)))

(defun helm-mpd-artist-map (conn)
  "Keymap in `helm-mpd-artists'."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-H" . ,(helm-mpd-run-helm-for-artists conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-artist-source (conn)
  "Build sources for `helm-mpd-artists'."
  (helm-mpd-build-mpd-source conn "Artists"
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
  "Get all albums in MPD library."
  (lambda ()
    (let ((artist (if (and (consp filter) (eq (car filter) 'artist))
                      (cdr filter)
                    nil)))
      (mapcar (lambda (x)
                (propertize x 'face 'helm-mpd-album-face))
              (mpd-get-artist-albums conn artist)))))

(defclosure helm-mpd-enqueue-albums (conn)
  "Enqueue all songs in selected albums."
  (lambda (_ignore)
    (helm-mpd-enqueue conn
                      (mpd-search conn 'album (helm-marked-candidates)))))

(helm-mpd-defaction helm-for-albums (conn)
  "Run `helm-mpd-library' for selected albums."
  (lambda (_ignore)
    (helm-mpd-library conn `(album . ,(helm-marked-candidates)))))

(defun helm-mpd-album-actions (conn)
  "Actions for `helm-mpd-albums'."
  (helm-make-actions
   "Enqueue album(s)' songs" (helm-mpd-enqueue-albums conn)
   "Helm for album(s)' songs" (helm-mpd-helm-for-albums conn)))

(defun helm-mpd-album-map (conn)
  "Keymap in `helm-mpd-albums'."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-H" . ,(helm-mpd-run-helm-for-albums conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-album-source (conn &optional filter)
  "Build sources for `helm-mpd-albums'."
  (helm-mpd-build-mpd-source conn "Albums"
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
  "Build sources for `helm-mpd-library'."
  (concatenate 'list
               (list (helm-mpd-build-song-source conn filter))
               (unless filter
                 (list (helm-mpd-build-artist-source conn)))
               (unless (and filter (eq (car filter) 'album))
                 (list (helm-mpd-build-album-source conn filter)))))

;;;###autoload
(defun helm-mpd-library (conn &optional filter)
  "Helm for MPD library.

This is a mixture of `helm-mpd-songs', `helm-mpd-artists' and `helm-mpd-albums'."
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
  "Save the current playlist."
  (lambda (pname)
    (mpd-save-playlist conn pname)
    (message "Save the current playlist as %s" pname)))

(defclosure helm-mpd-load-playlists (conn)
  "Load selected playlists."
  (lambda (_ignore)
    (let ((playlists (helm-marked-candidates)))
      (mpd-load-playlist conn playlists)
      (message "Load playlists %s" playlists))))

(helm-mpd-defaction remove-playlists (conn)
  "Remove selected playlists."
  (lambda (_ignore)
    (let ((playlists (helm-marked-candidates)))
      (mpd-remove-playlist conn playlists)
      (message "Remove playlists %s" playlists))))

(defun helm-mpd-new-playlist-actions (conn)
  "Actions for new playlists."
  (helm-make-actions
   "Save current playlist to file" (helm-mpd-save-playlist conn)))

(defun helm-mpd-playlist-actions (conn)
  "Actions for existing playlists."
  (helm-make-actions
   "Load playlist(s)" (helm-mpd-load-playlists conn)
   "Remove playlist(s)" (helm-mpd-remove-playlists conn)))

(defun helm-mpd-playlist-map (conn)
  "Keymap in `helm-mpd-playlist'."
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m (helm-mpd-map conn))
    (dolist (v `(("M-D" . ,(helm-mpd-run-remove-playlists conn))
                 ("C-c d" . ,(helm-mpd-run-remove-playlists-persistent conn))))
      (define-key m (kbd (car v)) (cdr v)))
    m))

(defun helm-mpd-build-existing-playlist-source (conn)
  "Build sources for existing playlists."
  (helm-mpd-build-mpd-source conn "Playlists"
                             :candidates (helm-mpd-playlist-candidates conn)
                             :action (helm-mpd-playlist-actions conn)
                             :keymap (helm-mpd-playlist-map conn)))

(defun helm-mpd-build-new-playlist-source (conn)
  "Build sources for new playlists."
  (helm-build-dummy-source "Create playlist"
    :action (helm-mpd-new-playlist-actions conn)
    :keymap (helm-mpd-map conn)))

(defun helm-mpd-build-playlist-source (conn)
  "Build sources for `helm-mpd-playlist'."
  (list (helm-mpd-build-existing-playlist-source conn)
        (helm-mpd-build-new-playlist-source conn)))

;;;###autoload
(defun helm-mpd-playlist (conn)
  "Helm for MPD playlists."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (helm-mpd-build-playlist-source conn)
        :buffer "*helm-mpd-playlist*"))

;; ----------------------------------------------------------------
;; Put together
;; ----------------------------------------------------------------

;;;###autoload
(defun helm-mpd (conn)
  "Helm for MPD.

This is a mixture of `helm-mpd-current-playlist', `helm-mpd-library' and
`helm-mpd-playlist'."
  (interactive (list (helm-mpd-read-host-and-port)))
  (helm :sources (concatenate 'list
                              (list (helm-mpd-build-current-playlist-source conn))
                              (helm-mpd-build-library-source conn)
                              (helm-mpd-build-playlist-source conn))
        :buffer "*helm-mpd*"))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
