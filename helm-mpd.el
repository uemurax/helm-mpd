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

(defvar helm-mpd-tag-begin "<%s>")
(defvar helm-mpd-tag-end "</%s>")
(defvar helm-mpd-item-keywords '("file" "directory" "playlist"))
(defvar helm-mpd-known-tags nil)

(defvar-local helm-mpd-local-process nil)

(defun helm-mpd-make-process (host port args output-buffer)
  (let ((proc (make-network-process :name "helm-mpd"
                                    :buffer (generate-new-buffer "helm-mpd-process-temporary-output")
                                    :host host
                                    :service port
                                    :filter 'helm-mpd-process-filter
                                    :sentinel 'helm-mpd-process-sentinel)))
    (process-put proc 'helm-mpd-buffer output-buffer)
    (process-send-string proc
                         (concat (mapconcat 'identity args " ")
                                 "\n"))
    (process-send-eof proc)
    proc))

(defun helm-mpd-process-filter (proc string)
  (let ((tmp-buf (process-buffer proc))
        (out-buf (process-get proc 'helm-mpd-buffer)))
    (with-current-buffer tmp-buf
      (save-excursion
        (goto-char (point-max))
        (insert string))
      (helm-mpd-parse-response out-buf))))

(defun helm-mpd-process-sentinel (proc string)
  (cond ((string-match "^connection broken by remote peer" string)
         (kill-buffer (process-buffer proc)))))

(defun helm-mpd-parse-response (out-buf)
  (while (search-forward-regexp "^\\([^:\n]*\\): \\([^\n]*\\)\n" nil t)
    (let ((key (match-string 1))
          (value (match-string 2)))
      (add-to-list 'helm-mpd-known-tags (intern key))
      (with-current-buffer out-buf
        (when (seq-position helm-mpd-item-keywords key 'equal)
          (insert "\n"))
        (insert (format helm-mpd-tag-begin key)
                value
                (format helm-mpd-tag-end key))))))

(defun helm-mpd-parse-object (object-string)
  (do ((res nil res)
       (attrs helm-mpd-known-tags (cdr attrs)))
      ((null attrs) res)
    (let ((attr (car attrs)))
      (when (string-match (concat (format helm-mpd-tag-begin attr)
                                  "\\(.*\\)"
                                  (format helm-mpd-tag-end attr))
                          object-string)
        (setq res (cons `(,attr . ,(match-string 1 object-string))
                        res))))))

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

(defun helm-mpd-filtered-candidate-transformer (candidates source)
  (mapcar (lambda (c)
            (let ((object (helm-mpd-parse-object c)))
              (cons (propertize (funcall helm-mpd-display-object-function
                                         object)
                                'mpd-object object)
                    object)))
          (seq-filter (lambda (c)
                        (> (length c) 0))
                      candidates)))

(defun helm-mpd-search-function (regexp)
  (re-search-forward (mapconcat (lambda (tag)
                                  (concat (format helm-mpd-tag-begin tag)
                                          ".*" regexp ".*"
                                          (format helm-mpd-tag-end tag)))
                                helm-mpd-known-tags "\\|")
                     nil t))

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

(defclass helm-source-mpd-base (helm-source-in-buffer)
  ((filtered-candidate-transformer :initform '(helm-mpd-filtered-candidate-transformer))
   (search :initform '(helm-mpd-search-function))
   (action :initform 'helm-mpd-object-action)))

(defun helm-mpd-refresh-buffer (buffer args)
  (with-current-buffer buffer
    (erase-buffer)
    (when (process-live-p helm-mpd-local-process)
      (delete-process helm-mpd-local-process))
    (setq helm-mpd-local-process
          (helm-mpd-make-process helm-mpd-host helm-mpd-port args buffer))))

(defmacro helm-mpd-defclass (name args)
  (let ((cb-var (intern (format "helm-mpd-%s-candidate-buffer" name)))
        (cb-val (format "*helm-mpd-%s:candidates*" name))
        (cls (intern (format "helm-source-mpd-%s" name)))
        (a-var (intern (format "helm-mpd-%s-arguments" name))))
    `(progn
       (defvar ,cb-var ,cb-val)
       (defvar ,a-var ,args)
       (defclass ,cls (helm-source-mpd-base)
         ((init :initform (lambda ()
                            (let ((buf (helm-candidate-buffer (get-buffer-create ,cb-var))))
                              (when (with-current-buffer buf
                                      (= (length (buffer-string)) 0))
                                (helm-mpd-refresh-buffer buf ,a-var)))))
          (update :initform (lambda ()
                              (helm-mpd-refresh-buffer (get-buffer-create ,cb-var) ,a-var))))))))

(helm-mpd-defclass current-playlist '("playlistinfo"))
(helm-mpd-defclass songs '("listallinfo"))
(helm-mpd-defclass playlists '("listplaylists"))

;;;###autoload
(defun helm-mpd ()
  (interactive)
  (helm :sources (list (helm-make-source "Current playlist" 'helm-source-mpd-current-playlist)
                       (helm-make-source "Songs" 'helm-source-mpd-songs)
                       (helm-make-source "Playlists" 'helm-source-mpd-playlists))
        :buffer "*helm-mpd*"))

(provide 'helm-mpd)

;;; helm-mpd.el ends here.
