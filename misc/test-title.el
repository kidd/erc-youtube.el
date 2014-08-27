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
  (let* ((url (format "https://gdata.youtube.com/feeds/api/videos/%s"
                      video-id))
         (wkbuffer (url-retrieve-synchronously url)))
    (with-current-buffer wkbuffer
      (goto-char (point-min))
      (push-mark)
      (search-forward "\n\n")
      (set-buffer-multibyte t) ;;
      (kill-region (mark) (point))
      (car (xml-node-children
            (car (xml-get-children
                  (car (xml-parse-region))
                  'title)))))))
