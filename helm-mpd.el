;;; helm-mpd.el --- Helm interface for MPD
;;
;; Copyright (C) 2016 Taichi Uemura
;;
;; Version: 0.1
;; Author: Taichi Uemura <t.uemura00@gmail.com>
;; Package-Requires: ((helm-core "1.9.7"))
;; License: GPL3
;;
;;; Code:

(require 'helm)

(defgroup helm-mpd ()
  "Predefined configurations for `helm-mpd'."
  :group 'helm)

(defcustom helm-mpd-host "localhost"
  "MPD host."
  :group 'helm-mpd
  :type 'string)

(defcustom helm-mpd-port 6600
  "MPD port."
  :group 'helm-mpd
  :type 'number)

(defun helm-mpd-retrieve-synchronously (cmd &rest args)
  (let* ((buf (generate-new-buffer "*helm-mpd-output*"))
         (proc (make-network-process :name (or (plist-get args :name)
                                               (format "helm-mpd:%s" cmd))
                                     :buffer buf
                                     :host (or (plist-get args :host)
                                               helm-mpd-host)
                                     :service (or (plist-get args :service)
                                                  helm-mpd-port)
                                     :family (plist-get args :family)
                                     :sentinel 'ignore)))
    (process-send-string proc (concat cmd "\n"))
    (process-send-eof proc)
    (while (process-live-p proc)
      (accept-process-output proc))
    buf))

(defun helm-mpd-send-command (cmd &rest args)
  (let ((proc (make-network-process :name (or (plist-get args :name)
                                              (format "helm-mpd:%s" cmd))
                                    :host (or (plist-get args :host)
                                              helm-mpd-host)
                                    :service (or (plist-get args :service)
                                                 helm-mpd-port)
                                    :family (plist-get args :family))))
    (process-send-string proc (concat cmd "\n"))
    (process-send-eof proc)
    proc))

(defvar helm-mpd-item-keywords '("file" "directory" "playlist"))

(defun helm-mpd-parse-response ()
  (let ((buf (generate-new-buffer "helm-mpd-temp-buffer")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "(())")
            (backward-char 2))
          (while (search-forward-regexp "^\\([^:\n]*\\): \\([^\n]*\\)\n" nil t)
            (let ((key (match-string 1))
                  (value (match-string 2)))
              (with-current-buffer buf
                (when (seq-position helm-mpd-item-keywords key 'equal)
                  (up-list)
                  (insert "()")
                  (backward-char 1))
                (insert (format "(%s . %S)" key value)))))
          (with-current-buffer buf
            (goto-char (point-min))
            (cdr (sexp-at-point))))
      (kill-buffer buf))))

(defvar helm-mpd-candidates-cache (make-hash-table :test 'equal))

(defun helm-mpd-candidates-synchronously (cmd &rest args)
  (let ((cache nil))
    (when (plist-get args :cache)
      (setq cache (gethash cmd helm-mpd-candidates-cache)))
    (or cache
        (let ((buf (apply 'helm-mpd-retrieve-synchronously cmd args)))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (puthash cmd (helm-mpd-parse-response)
                         helm-mpd-candidates-cache))
            (kill-buffer buf))))))

(defun helm-mpd-display-object-default (object)
  (or (cdr (assq 'directory object))
      (cdr (assq 'playlist object))
      (concat (cdr (assq 'Artist object))
              " "
              (cdr (assq 'Title object))
              " "
              (cdr (assq 'Album object)))))

(defcustom helm-mpd-display-object-function
  'helm-mpd-display-object-default
  "Function displaying an object."
  :group 'helm-mpd
  :type 'function)

(defun helm-mpd-filter-one-by-one (object)
  (cons (propertize (funcall helm-mpd-display-object-function
                             object)
                    'mpd-object object)
        object))

(defun helm-mpd-match-function-1 (pattern object &optional migemo)
  (let* ((mfn1 (if migemo
                   'helm-mm-migemo-string-match
                 'string-match))
         (mfn (cond ((string-match "^<\\([^<>]*\\)>\\(.*\\)$" pattern)
                     (let ((key (match-string 1 pattern))
                           (value (match-string 2 pattern)))
                       `(lambda (x)
                          (and (string-match ,key (format "%s" (car x)))
                               (funcall mfn1 ,value (cdr x))))))
                    (t
                     (lambda (x)
                       (funcall mfn1 pattern (cdr x)))))))
    (seq-filter mfn object)))

(defun helm-mpd-match-function (pattern object &optional migemo)
  (not (seq-filter (lambda (p)
                     (not (helm-mpd-match-function-1 p object migemo)))
                   (split-string pattern))))

(defun helm-mpd-match (candidate)
  (let ((object (get-text-property 0 'mpd-object candidate)))
    (helm-mpd-match-function helm-pattern object
                             (and (boundp 'helm-migemo-mode) helm-migemo-mode))))

(defvar helm-mpd--info-buffer "*helm-mpd-info*")

(defun helm-mpd-object-show (_ignore)
  (let ((buf (get-buffer-create helm-mpd--info-buffer)))
    (display-buffer buf)
    (with-current-buffer buf
      (view-mode)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert (format "%S" (helm-marked-candidates)))))))

(defcustom helm-mpd-object-action
  (helm-make-actions
   "Show object(s) information" 'helm-mpd-object-show)
  "Default action on objects."
  :group 'helm-mpd
  :type 'alist)

(defun helm-mpd-action-play (object)
  (helm-mpd-send-command (format "playid %s"
                                 (cdr (assq 'Id object)))))

(defun helm-mpd-action-delete (_ignore)
  (helm-mpd-send-command (concat "command_list_begin\n"
                                 (mapconcat (lambda (c)
                                              (format "deleteid %s"
                                                      (cdr (assq 'Id c))))
                                            (helm-marked-candidates)
                                            "\n")
                                 "\ncommand_list_end")))

(defun helm-mpd-action-transformer-current-playlist (actions object)
  (append actions
          (when (assq 'Id object)
            '(("Play song" . helm-mpd-action-play)
              ("Delete song(s)" . helm-mpd-action-delete)))))

(defun helm-mpd-action-add (_ignore)
  (helm-mpd-send-command (concat "command_list_begin\n"
                                 (mapconcat (lambda (c)
                                              (format "add %s"
                                                      (cdr (or (assq 'file c)
                                                               (assq 'directory c)))))
                                            (helm-marked-candidates)
                                            "\n")
                                 "\ncommand_list_end")))

(defun helm-mpd-action-transformer-song (actions object)
  (append actions
          (when (or (assq 'file object) (assq 'directory object))
            '(("Add song(s)" . helm-mpd-action-add)))))

(defun helm-mpd-action-load (_ignore)
  (helm-mpd-send-command (concat "command_list_begin\n"
                                 (mapconcat (lambda (c)
                                              (format "load %s"
                                                      (cdr (assq 'playlist c))))
                                            (helm-marked-candidates)
                                            "\n")
                                 "\ncommand_list_end")))

(defun helm-mpd-action-transformer-playlist (actions object)
  (append actions
          (when (assq 'playlist object)
            '(("Load playlist(s)" . helm-mpd-action-load)))))

(defclass helm-source-mpd-base (helm-source)
  ((candidates :initform (lambda ()
                           (helm-mpd-candidates-synchronously (helm-attr 'mpd-command)
                                                              :cache (helm-attr-defined 'mpd-cache))))
   (filter-one-by-one :initform 'helm-mpd-filter-one-by-one)
   (action :initform 'helm-mpd-object-action)
   (action-transformer :initform '(helm-mpd-action-transformer-current-playlist
                                   helm-mpd-action-transformer-song
                                   helm-mpd-action-transformer-playlist))
   (update :initform (lambda ()
                       (when (helm-attr-defined 'mpd-cache)
                         (helm-mpd-candidates-synchronously (helm-attr 'mpd-command)))))
   (match :initform '(helm-mpd-match))
   (mpd-command :initarg :mpd-command)
   (mpd-cache :initarg :mpd-cache
              :initform nil)))

(defvar helm-source-mpd-current-playlist
  (helm-make-source "Current playlist" 'helm-source-mpd-base
    :mpd-command "playlistinfo"))
(defvar helm-source-mpd-songs
  (helm-make-source "Songs" 'helm-source-mpd-base
    :mpd-command "listallinfo"
    :mpd-cache t))
(defvar helm-source-mpd-playlists
  (helm-make-source "Playlists" 'helm-source-mpd-base
    :mpd-command "listplaylists"))

(defvar helm-source-mpd
  '(helm-source-mpd-current-playlist
    helm-source-mpd-songs
    helm-source-mpd-playlists))

;;;###autoload
(defun helm-mpd ()
  (interactive)
  (helm :sources helm-source-mpd
        :buffer "*helm-mpd*"))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
