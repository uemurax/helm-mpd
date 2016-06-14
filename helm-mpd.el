;;; helm-mpd.el --- Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Version: 0.1
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; Package-Requires: ((helm "1"))
;; License: GPL3
;;
;;; Code:

(defgroup helm-mpd ()
  "Predefined configurations for `helm-mpd'."
  :group 'helm)

(defclass helm-source-mpd-songs (helm-source-async)
  ((candidates-process :initform 'helm-mpd-songs-candidates-process)
   (candidate-transformer :initform 'helm-mpd-songs-candidate-transformer)
   (mpd-host :initarg :mpd-host)
   (mpd-port :initarg :mpd-port)
   (mpd-args :initarg :mpd-args)))

(defvar helm-mpd-song-attributes
  '(artist album albumartist comment composer date disc genre performer
           title track time file position mtime mdate))

(defvar helm-mpd-song-tag-begin "<%s>")
(defvar helm-mpd-song-tag-end "</%s>")

(defun helm-mpd-song-format ()
  (mapconcat (lambda (x)
               (concat (format helm-mpd-song-tag-begin x)
                       "%" (format "%s" x) "%"
                       (format helm-mpd-song-tag-end x)))
             helm-mpd-song-attributes
             ""))

(defun helm-mpd-parse-song (song-string)
  (do ((res nil res)
       (attrs helm-mpd-song-attributes (cdr attrs)))
      ((null attrs) res)
    (let ((attr (car attrs)))
      (when (string-match (concat (format helm-mpd-song-tag-begin attr)
                                  "\\(.*\\)"
                                  (format helm-mpd-song-tag-end attr))
                          song-string)
        (setq res (cons `(,attr . ,(match-string 1 song-string))
                        res))))))

(defun helm-mpd-display-song-default (song)
  (concat (cdr (assq 'artist song))
          " "
          (cdr (assq 'title song))
          " "
          (cdr (assq 'album song))))

(defcustom helm-mpd-display-song-function
  'helm-mpd-display-song-default
  "Function displaying a song."
  :group 'helm-mpd
  :type 'function)

(defun helm-mpd-songs-candidate-transformer (candidates)
  (mapcar (lambda (c)
            (let ((song (helm-mpd-parse-song c)))
              (cons (propertize (funcall helm-mpd-display-song-function
                                         song)
                                'mpd-song song)
                    song)))
          candidates))

(defun helm-mpd-songs-candidates-process ()
  (let* ((source (helm-get-current-source))
         (host (cdr (assq 'mpd-host source)))
         (port (cdr (assq 'mpd-port source)))
         (args (cdr (assq 'mpd-args source))))
    (make-process :name "helm-mpd-song-process"
                  :command (apply #'list
                                  "mpc"
                                  "-h" host
                                  "-p" (format "%s" port)
                                  "-f" (helm-mpd-song-format)
                                  (funcall args helm-pattern)))))

(defun helm-mpd-search-arguments (pattern)
  (list "search" "any"
        pattern))

(defun helm-mpd-current-playlist-arguments (pattern)
  (list "playlist"))

(defcustom helm-mpd-default-host "localhost"
  "Default host."
  :group 'helm-mpd
  :type 'string)

(defcustom helm-mpd-default-port 6600
  "Default port."
  :group 'helm-mpd
  :type 'number)

;;;###autoload
(defun helm-mpd (&optional host port)
  (interactive (if current-prefix-arg
                   (list (read-string "Host: " nil nil helm-mpd-default-host)
                         (read-number "Port: " helm-mpd-default-port))
                 (list helm-mpd-default-host helm-mpd-default-port)))
  (helm :sources (mapcar (lambda (xs)
                           (apply (lambda (name args)
                                    (helm-make-source name 'helm-source-mpd-songs
                                      :mpd-host host
                                      :mpd-port port
                                      :mpd-args args))
                                  xs))
                         '(("Current playlist" helm-mpd-current-playlist-arguments)
                           ("MPD songs" helm-mpd-search-arguments)))
        :buffer "*helm-mpd*"))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
