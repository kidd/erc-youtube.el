;;; erc-youtube.el --- Show received youtube urls in the ERC buffer

;; Copyright (C) 2014  Raimon Grau Cuscó

;; Author: Raimon Grau Cuscó <raimonster@gmail.com>
;; Version: 0.1
;; Package-Requires: ((erc "5.3"))
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Show inlined info about youtube links in erc buffers.  Requires
;; Emacs 24.2
;;
;; (require 'erc-youtube)
;; (add-to-list 'erc-modules 'youtube)
;; (erc-update-modules)
;;
;; Or `(require 'erc-youtube)` and  `M-x customize-option erc-modules RET`
;;
;; This plugin subscribes to hooks `erc-insert-modify-hook' and
;; `erc-send-modify-hook' to download and show youtubes.  In this early
;; version it's doing this synchronously.
;;
;;; Code:


(require 'erc)
(require 'xml)
(require 'url-queue)

(defgroup erc-youtube nil
  "Enable youtube."
  :group 'erc)

(defcustom erc-youtube-regex "^https?://\\(www.\\)?youtube.com/watch\\?v=\\([a-zA-Z0-9]+\\)"
  "Regex to mach URLs to be downloaded"
  :group 'erc-youtube
  :type '(regexp :tag "Regex"))

(defun erc-youtube (status marker)
  (interactive)
  (goto-char (point-min))
	(push-mark)
  (search-forward "

")
  (kill-region (mark) (point))


	(let ((video-title 	(car (xml-node-children
											(car (xml-get-children
														(car (xml-parse-region))
														'title)))) ))
		(with-current-buffer (marker-buffer marker)
			(save-excursion
				(let ((inhibit-read-only t))
					(goto-char (marker-position marker))
					(insert-before-markers
					 (erc-strip-tags
						(with-temp-buffer
							(insert "[youtube] -  " video-title "
")
							(buffer-string))))
					(put-text-property (point-min) (point-max) 'read-only t)))))
	)

(defun erc-youtube-id (url)
	(replace-regexp-in-string ".*youtube.com/watch\\?v=\\([a-zA-Z0-9]+\\)" "\\1" url))

(defun erc-youtube-show-info ()
  (interactive)
  (goto-char (point-min))
  (search-forward "http" nil t)
  (let ((url (thing-at-point 'url)))
    (when (and url (string-match erc-youtube-regex url))
      (goto-char (point-max))
      (url-queue-retrieve
			 (format "https://gdata.youtube.com/feeds/api/videos/%s" (erc-youtube-id url))
			 ;			 (or (match-string 2 url) (match-string 1 url))
			 'erc-youtube
			 (list
				(point-marker))
			 t))))


;;;###autoload
(eval-after-load 'erc
  '(define-erc-module youtube nil
     "Display inlined info about youtube links in ERC buffer"
     ((add-hook 'erc-insert-modify-hook 'erc-youtube-show-info t)
      (add-hook 'erc-send-modify-hook 'erc-youtube-show-info t))
     ((remove-hook 'erc-insert-modify-hook 'erc-youtube-show-info)
      (remove-hook 'erc-send-modify-hook 'erc-youtube-show-info))
     t))

(provide 'erc-youtube)

;;; erc-youtube.el ends here
