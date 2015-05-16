;;; -*- emacs-lisp -*-
;;; encoding: utf-8-unix

(require 'erc-youtube)

(defun erc-youtube/test/run-all ()
  "Should return t"
  (and
   (erc-youtube/test/title-1)))

(defun erc-youtube/test/title-1 ()
  (and
   ;; expected ;; actual
   (string= "Teens Won’t Stop Setting Themselves On Fire"
            (get-yttitle-sync "X1MBEAFLVLc"))
   (string= "Google Chrome : Hatsune Miku (初音ミク)"
            (get-yttitle-sync "MGt25mv4-2Q"))))


(defun get-yttitle-sync (video-id)
  "Query YouTube Data APIv3 for title of video with VIDEO-ID"
  (let* ((request-url (erc-youtube-make-request-url video-id))
         wkbuffer actual-title)
    (when request-url
      (setq wkbuffer (url-retrieve-synchronously request-url))
      (with-current-buffer wkbuffer
        (setq actual-title (erc-youtube--extract-title-from-response))))))

;;; ends here
