;;; -*- emacs-lisp -*-

(require 'erc-youtube)

(defun erc-youtube/test/run-all ()
  "Should return t"
  (and
   (erc-youtube/test/matcher-1)
   (erc-youtube/test/matcher-2)))

(defun erc-youtube/test/matcher-1 ()
  (and
   ;; expected ;; actual
   (string=
    "I0kWZP9L9Kc"
    (replace-regexp-in-string
     erc-youtube-regex-extract-videoid "\\1"
     "https://www.youtube.com/watch?v=I0kWZP9L9Kc" nil nil))
   (string=
    "I0kWZP9L9Kc"
    (replace-regexp-in-string
     erc-youtube-regex-extract-videoid "\\1"
     "https://www.youtube.com/watch?v=I0kWZP9L9Kc&list=PL2VAYZE_4wRKKr5pJzfYD1w4tKCXARs5y&index=2" nil nil))
   (string=
    "asa_LlO6-6E"
    (replace-regexp-in-string
     erc-youtube-regex-extract-videoid "\\1"
     "http://www.youtube.com/v/asa_LlO6-6E" nil nil))
   (string=
    "asa_LlO6-6E"
    (replace-regexp-in-string
     erc-youtube-regex-extract-videoid "\\1"
     "http://www.youtube.com/v/asa_LlO6-6E&f=gdata_videos&c=ytapi-my-clientID&d=nGF83uyVrg8eD4rfEkk22mDOl3qUImVMV6ramM" nil nil))))


(defun erc-youtube/test/matcher-2 ()
  "Should return t"
  (and
   ;; expected ;; actual
   (string= "I0kWZP9L9Kc" (erc-youtube-id "https://www.youtube.com/watch?v=I0kWZP9L9Kc&list=PL2VAYZE_4wRKKr5pJzfYD1w4tKCXARs5y&index=2"))
   (string= "GLJbQ-mMusI" (erc-youtube-id "https://www.youtube.com/watch?v=GLJbQ-mMusI"))
   (string= "8yEQ3ZHGUvA" (erc-youtube-id "https://www.youtube.com/watch?v=8yEQ3ZHGUvA&list=UUU9pX8hKcrx06XfOB-VQLdw"))
   (string= "X1MBEAFLVLc" (erc-youtube-id "http://youtu.be/X1MBEAFLVLc"))
   (string= "8yEQ3ZHGUvA" (erc-youtube-id "http://youtu.be/8yEQ3ZHGUvA?t=22m9s"))))
