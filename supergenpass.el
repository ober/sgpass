;;; supergenpass.el --- SuperGenPass for Emacs

;; Copyright (C) 2012  Jaime Fournier <jaimef@linbsd.org>

;; Author: Jaime Fournier <jaimef@linbsd.org>
;; Keywords: SuperGenPass
;; Version: 0.1

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
;; Wanted Supergenpass available in Emacs :P
;; Based heavily off the Ruby implementation by rstacruz
;; https://github.com/rstacruz/sgpass.git
;;
;;
;; Many thanks to #emacs on Freenode for assistance.

;;; Code:

(defun b64_md5 (pickle)
  "Encrypt the string given to us as Base64 encoded Md5 byte stream"
  (replace-regexp-in-string "=" "A" (replace-regexp-in-string "+" "9" (replace-regexp-in-string "/" "8" (base64-encode-string (secure-hash 'md5 pickle nil nil t))))))

(defun supergenpass (password domain)
  "Create a unique password for a given domain and master password"
  (setq i 0)
  (setq result (format "%s:%s" password domain))
  (loop until
        (and (> i 10) (secure-enough result 10))
        do
        (setq result (b64_md5 result))
        (setq i (+ i 1))
        )
  (kill-new (format "%s" (substring result 0 10))))

(defun secure-enough (results len)
  "Ensure the password we have is sufficiently secure"
  (if
      (and (> (length results) len) (let ((case-fold-search nil)) (string-match "[0-9]" (substring results 0 10)) (string-match "[A-Z]" (substring results 0 10)) (string-match "^[a-z]" (substring results 0 10))))
      t
    nil
    ))

(provide 'supergenpass)

;; supergenpass.el ends here
