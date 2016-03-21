;;; helm-mpd.el - Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; License: GPL3
;; Time-stamp: <2016-03-21 21:28:27 tuemura>
;;
;;; Code:

(require 'helm)
(require 'helm-mpdlib)
(eval-when-compile (require 'cl))

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

(defun helm-mpd-interactive-host-and-port ()
  "Read host and port.

If the current prefix argument is non-nil, read them from input.
Otherwise returns `helm-mpd-host' and `helm-mpd-port'."
  (if current-prefix-arg
      (list (read-string "Host: " nil nil helm-mpd-host)
            (read-number "Port: " helm-mpd-port))
    (list helm-mpd-host helm-mpd-port)))

(cl-defun helm-mpd-send (str &optional callback cbarg
                             &key (output-buffer "*helm-mpd-default-output*"))
  "Run `helm-mpdlib-send' with `helm-mpd-host' and `helm-mpd-port'."
  (helm-mpdlib-send helm-mpd-host helm-mpd-port
                    str callback cbarg :output-buffer output-buffer))

(defun helm-mpd-action (fun &optional on-marked command)
  "Make a helm action.

FUN must be a function with one parameter.

If ON-MARKED is nil, call FUN with the selected candidate.
If ON-MRAKED is non-nil, call FUN with the list of the marked candidates.

If COMMAND is non-nil, make an interactive function
which is called in a helm session.
If COMMAND is the simbol `persistent', the function does not exit helm session."
  (lexical-let* ((fun fun)
                 (g (if on-marked
                        (lambda (_ignore)
                          (funcall fun (helm-marked-candidates)))
                      fun)))
    (case command
      ((nil) g)
      ((persistent)
       (lambda ()
         (interactive)
         (with-helm-alive-p
           (helm-attrset 'mpd-persistent-action (cons g 'never-split))
           (helm-execute-persistent-action 'mpd-persistent-action))))
      (otherwise
       (lambda ()
         (interactive)
         (with-helm-alive-p
           (helm-exit-and-execute-action g)))))))

(defun helm-mpd-show-raw-data (candidates)
  (switch-to-buffer "*helm-mpd-raw-data*")
  (erase-buffer)
  (dolist (c candidates)
    (insert (format "%S" c) "\n")))

;;;; Songs

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
                 (propertize (or (cdr (assq (car x) song)) "") 'face (cdr x)))
               '((Artist . helm-mpd-artist-face)
                 (Title . helm-mpd-title-face)
                 (Album . helm-mpd-album-face))
               " "))
  "Function to format a song."
  :group 'helm-mpd
  :type 'function)

(defun helm-mpd-song--match-pattern (p song)
  (let ((mfn (if helm-migemo-mode
                 #'helm-mm-migemo-string-match
               #'string-match)))
    (if (string-match "^%\\(.\\)\\(.*\\)$" p)
        (let ((lead (match-string 1 p))
              (q (match-string 2 p))
              (key nil))
          (cond ((equal lead "f")
                 (setq key 'file))
                ((equal lead "a")
                 (setq key 'Artist))
                ((equal lead "t")
                 (setq key 'Title))
                ((equal lead "b")
                 (setq key 'Album))
                ((equal lead "y")
                 (setq key 'Date))
                ((equal lead "n")
                 (setq key 'Track))
                ((equal lead "g")
                 (setq key 'Genre)))
          (when (and key (assq key song))
            (funcall mfn q (cdr (assq key song)))))
      (cl-loop for v in song
               thereis (funcall mfn p (cdr v))))))

(defun helm-mpd-songs-match-function (candidate)
  (let ((song (get-text-property 0 :real-value candidate)))
    (cl-loop with pattern = helm-pattern
             for p in (split-string pattern " ")
             always (helm-mpd-song--match-pattern p song))))

(defun helm-mpd-display-song (song)
  (propertize (funcall helm-mpd-song-format song)
              :real-value song))

(defclass helm-source-mpd-songs (helm-source)
  ((match :initform '(helm-mpd-songs-match-function))
   (real-to-display :initform 'helm-mpd-display-song)))

(defun helm-mpd-songs-enqueue (songs)
  (let ((paths (apply #'append
                      (mapcar (lambda (song)
                                (when (assq 'file song)
                                  (list (cdr (assq 'file song)))))
                              songs))))
    (helm-mpd-send (mapcar (lambda (path)
                             (helm-mpdlib-make-command 'add path))
                           paths))))

;;;; Sources

(eval-when-compile
  (defmacro defsource (name command form class &rest args)
    (let* ((candidates (intern (format "helm-mpd-%s-candidates" name)))
           (source (intern (format "helm-mpd-%s-source" name)))
           (retrieve (intern (format "helm-mpd-%s-retrieve" name)))
           (buf (format "*helm-mpd-%s-output*" name))
           (build-source (intern (format "helm-mpd-%s-build-source" name)))
           (source-name (capitalize (replace-regexp-in-string "-" " " (format "%s" name))))
           (buf-var (intern (format "%s:buffer" retrieve)))
           (name-var (intern (format "%s:name" build-source)))
           (args-var (intern (format "%s:args" build-source))))
      `(progn
         (defvar ,candidates (cons nil nil))
         (defvar ,source nil)
         (defun ,retrieve ()
           (let ((,buf-var ,buf))
             (helm-mpd-send (helm-mpdlib-make-command ',command)
                            (lambda ()
                              (while (helm-mpdlib-received-p)
                                (setq ,candidates
                                      (cons t ,form)))
                              (with-helm-buffer
                                (helm-update nil ,source)))
                            nil :output-buffer ,buf-var)))
         (defun ,build-source (&optional ,name-var &rest ,args-var)
           (setq ,name-var (or ,name-var ,source-name))
           (apply #'helm-make-source ,name-var ',class
                  :candidates (lambda ()
                                (if (car ,candidates)
                                    (setcar ,candidates nil)
                                  (,retrieve))
                                (cdr ,candidates))
                  ,@args
                  ,args-var)))))

  (defmacro run-helm (sources &rest args)
    (let ((let-vars (mapcar (lambda (s)
                              (unless (consp s)
                                (setq s (list s)))
                              (list (intern (format "helm-mpd-run-helm:%s-source" (car s)))
                                    (cons (intern (format "helm-mpd-%s-build-source" (car s)))
                                          (cdr s))))
                            sources))
           (args0 (apply #'append
                         (mapcar (lambda (s)
                                   (when (consp s)
                                     (setq s (car s)))
                                   (list (intern (format ":mpd-%s-source" s))
                                         (intern (format "helm-mpd-run-helm:%s-source" s))))
                                 sources))))
      `(let ,let-vars
         (helm :sources (list ,@(mapcar #'car let-vars))
               ,@args0
               ,@args)))))

;;;;; Current playlist

(defun helm-mpd-current-playlist-play (song)
  (let ((pos (cdr (assq 'Pos song))))
    (when pos
      (helm-mpd-send (helm-mpdlib-make-command 'play pos)))))

(defun helm-mpd-current-playlist-delete (songs)
  (let ((poss (apply #'append
                     (mapcar (lambda (song)
                               (let ((p (assq 'Pos song)))
                                 (when p
                                   (list (string-to-number (cdr p))))))
                             songs))))
    (helm-mpd-send (mapcar (lambda (pos)
                             (helm-mpdlib-make-command 'delete pos))
                           (seq-uniq (seq-sort '> poss))))))

(defun helm-mpd-current-playlist-move (n)
  (interactive (list (if current-prefix-arg
                         (cond ((listp current-prefix-arg)
                                (car current-prefix-arg))
                               (t current-prefix-arg))
                       (read-number "Move to: "))))
  (with-helm-alive-p
    (let ((c (helm-get-selection)))
      (when c
        (let ((pos (cdr (assq 'Pos c))))
          (when pos
            (helm-mpd-send (helm-mpdlib-make-command 'move pos n))))))))
(put 'helm-mpd-current-playlist-move 'helm-only t)

(defvar helm-mpd-current-playlist-actions
  (helm-make-actions
   "Play song" (helm-mpd-action 'helm-mpd-current-playlist-play)
   "Delete song(s)" (helm-mpd-action 'helm-mpd-current-playlist-delete t)
   "Show raw data(s)" (helm-mpd-action 'helm-mpd-show-raw-data t))
  "Actions for `helm-mpd-current-playlist'.")

(defvar helm-mpd-current-playlist-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v `(("M-D" . ,(helm-mpd-action 'helm-mpd-current-playlist-delete t t))
                 ("C-c d" . ,(helm-mpd-action 'helm-mpd-current-playlist-delete t 'persistent))
                 ("C-c RET" . ,(helm-mpd-action 'helm-mpd-show-raw-data t t))
                 ("C-c C-j" . ,(helm-mpd-action 'helm-mpd-show-raw-data t 'persistent))
                 ("M-g g" . helm-mpd-current-playlist-move)))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-mpd-current-playlist'.")

(defsource current-playlist playlistinfo
  (helm-mpdlib-read-objects '(file))
  helm-source-mpd-songs
  :action 'helm-mpd-current-playlist-actions
  :keymap helm-mpd-current-playlist-map)

;;;###autoload
(defun helm-mpd-current-playlist (host port)
  "Helm for current playlist."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port))
    (run-helm (current-playlist)
              :buffer "*helm-mpd-current-playlist*")))

;;;;; Libraries

(defvar helm-mpd-library-actions
  (helm-make-actions
   "Enqueue song(s)" (helm-mpd-action 'helm-mpd-songs-enqueue t)
   "Show raw data(s)" (helm-mpd-action 'helm-mpd-show-raw-data t))
  "Actions for `helm-mpd-library'.")

(defvar helm-mpd-library-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v `(("C-c RET" . ,(helm-mpd-action 'helm-mpd-show-raw-data t t))
                 ("C-c C-j" . ,(helm-mpd-action 'helm-mpd-show-raw-data t 'persistent))))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-mpd-library'.")

(defsource library listallinfo
  (cl-loop for x in (helm-mpdlib-read-objects '(file directory playlist))
           when (assq 'file x)
           collect x)
  helm-source-mpd-songs
  :action 'helm-mpd-library-actions
  :keymap helm-mpd-library-map)

;;;###autoload
(defun helm-mpd-library (host port)
  "Helm for MPD library."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port))
    (run-helm (library)
              :buffer "*helm-mpd-library*")))

;;;;; Play lists

(defun helm-mpd-playlist-names (playlists)
  (apply #'append
         (mapcar (lambda (x)
                   (when (assq 'playlist x)
                     (list (cdr (assq 'playlist x)))))
                 playlists)))

(defun helm-mpd-playlist-load (playlists)
  (helm-mpd-send (mapcar (lambda (n)
                           (helm-mpdlib-make-command 'load n))
                         (helm-mpd-playlist-names playlists))))

(defun helm-mpd-playlist-remove (playlists)
  (helm-mpd-send (mapcar (lambda (n)
                           (helm-mpdlib-make-command 'rm n))
                         (helm-mpd-playlist-names playlists))))

(defun helm-mpd-playlist-rename (playlist)
  (let ((x (assq 'playlist playlist)))
    (when x
      (let ((name (completing-read "Rename to: "
                                   (mapcar (lambda (c)
                                             (cdr (assq 'playlist c)))
                                           helm-mpd-playlist-candidates))))
        (helm-mpd-send (helm-mpdlib-make-command 'rename (cdr x) name))))))

(defun helm-mpd-playlist-info (playlists)
  (let ((buf "*helm-mpd-playlist-info*"))
    (switch-to-buffer buf)
    (erase-buffer)
    (helm-mpd-send (mapcar (lambda (n)
                             (helm-mpdlib-make-command 'listplaylistinfo n))
                           (helm-mpd-playlist-names playlists))
                   #'ignore nil :output-buffer buf)))

(defun helm-mpd-playlist-list (playlists)
  (let ((buf "*helm-mpd-playlist-list*"))
    (switch-to-buffer buf)
    (erase-buffer)
    (helm-mpd-send (mapcar (lambda (n)
                             (helm-mpdlib-make-command 'listplaylist n))
                           (helm-mpd-playlist-names playlists))
                   #'ignore nil :output-buffer buf)))

(defvar helm-mpd-playlist-actions
  (helm-make-actions
   "Load playlist(s)" (helm-mpd-action 'helm-mpd-playlist-load t)
   "List playlist(s)" (helm-mpd-action 'helm-mpd-playlist-list t)
   "Show playlist(s) info" (helm-mpd-action 'helm-mpd-playlist-info t)
   "Remove playlist(s)" (helm-mpd-action 'helm-mpd-playlist-remove t)
   "Rename playlist" (helm-mpd-action 'helm-mpd-playlist-rename)
   "Show raw data(s)" (helm-mpd-action 'helm-mpd-show-raw-data t))
  "Actions for `helm-mpd-playlist'.")

(defvar helm-mpd-playlist-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m helm-map)
    (dolist (v `(("M-D" . ,(helm-mpd-action 'helm-mpd-playlist-remove t t))
                 ("C-c d" . ,(helm-mpd-action 'helm-mpd-playlist-remove t 'persistent))
                 ("M-L" . ,(helm-mpd-action 'helm-mpd-playlist-list t t))
                 ("C-c l" . ,(helm-mpd-action 'helm-mpd-playlist-list t 'persistent))
                 ("M-I" . ,(helm-mpd-action 'helm-mpd-playlist-info t t))
                 ("C-c i" . ,(helm-mpd-action 'helm-mpd-playlist-info t 'persistent))
                 ("M-R" . ,(helm-mpd-action 'helm-mpd-playlist-rename t t))
                 ("C-c RET" . ,(helm-mpd-action 'helm-mpd-show-raw-data t t))
                 ("C-c C-j" . ,(helm-mpd-action 'helm-mpd-show-raw-data t 'persistent))))
      (define-key m (kbd (car v)) (cdr v)))
    m)
  "Keymap for `helm-mpd-playlist'.")

(defsource playlist listplaylists
  (cl-loop for x in (helm-mpdlib-read-objects '(playlist))
           when (assq 'playlist x)
           collect x)
  helm-source
  :real-to-display (lambda (c)
                     (cdr (assq 'playlist c)))
  :action 'helm-mpd-playlist-actions
  :keymap helm-mpd-playlist-map)

;;;###autoload
(defun helm-mpd-playlist (host port)
  "Helm for MPD playlists."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port))
    (run-helm (playlist)
              :buffer "*helm-mpd-playlist*")))

(defun helm-mpd-new-playlist-save (name)
  (helm-mpd-send (helm-mpdlib-make-command 'save name)))

(defvar helm-mpd-new-playlist-actions
  (helm-make-actions
   "Save the current playlist" (helm-mpd-action 'helm-mpd-new-playlist-save))
  "Actions for `helm-mpd-new-playlist'.")

(defun helm-mpd-new-playlist-build-source (&optional name &rest args)
  (setq name (or name "Create playlist"))
  (apply #'helm-make-source name 'helm-source-dummy
         :action 'helm-mpd-new-playlist-actions
         args))

;;;###autoload
(defun helm-mpd-new-playlist (host port)
  "Helm for new MPD playlist."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port))
    (run-helm (new-playlist)
              :buffer "*helm-mpd-new-playlist*")))

;;;;; Put together

;;;###autoload
(defun helm-mpd (host port)
  "Helm for MPD.

This is a mixture of `helm-mpd-current-playlist', `helm-mpd-library',
`helm-mpd-playlist' and `helm-mpd-new-playlist'."
  (interactive (helm-mpd-interactive-host-and-port))
  (let ((helm-mpd-host host)
        (helm-mpd-port port))
    (run-helm (current-playlist library playlist new-playlist)
              :buffer "*helm-mpd*")))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
