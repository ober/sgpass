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

;; Many thanks to #emacs on Freenode for assistance.

;;; Code:

;; Customizations

;; (defcustom spg-domains nil
;;   "List of SuperGenPass Domains"
;;   :tag "SGP Domains"
;;   :group 'sgp
;;   :type 'data)

(defun b64_md5 (pickle)
  "Encrypt the string given to us as Base64 encoded Md5 byte stream"
  (replace-regexp-in-string "=" "A" (replace-regexp-in-string "+" "9" (replace-regexp-in-string "/" "8" (base64-encode-string (secure-hash 'md5 pickle nil nil t))))))

(defun sgp_prompt_pass () (interactive)
  "Interactive function to prompt for domain and password and populate paste buffer with new password"
  (setq domain (read-from-minibuffer "SuperGenPass: Domain? "))
  (setq password (read-passwd "SuperGenPass: Password? "))
  (kill-new (format "%s" (sgp_generate password domain))))

(defun supergenpass (password domain)
  (kill-new (format "%s" (sgp_generate password domain))))

(defun sgp_generate (password domain)
  "Create a unique password for a given domain and master password"
  (setq i 0)
  (setq results (format "%s:%s" password domain))

  (loop until
        (and (> i 10) (secure-enough results 10))
        do
        (message "%s %s %s" i results (secure-enough results 10))
        (setq results (b64_md5 results))
        (setq i (+ i 1))
        )
  (message "%s %s %s" i results (secure-enough results 10))
  (substring results 0 10))

(defun secure-enough (results len)
  "Ensure the password we have is sufficiently secure"
  (let
      ((case-fold-search nil))
    (if
        (and
         (> (length results) 10)
         (string-match "[0-9]" (substring results 0 10))
         (string-match "[A-Z]" (substring results 0 10))
         (string-match "^[a-z]" (substring results 0 10)))
        t
      nil
      )))

(defun sgp_test_compat () (interactive)
  (setq test1
        (if
            (and
             (equal "ubRPZrdfu5" (sgp_generate "abcdefg" "cnn.com"))
             (equal "xaIlcL02A3" (sgp_generate "TheSeasonedSchemer" "slashdot.org"))
             )
            (message "All Passed")
          (message "Failures!")
          )))


(provide 'supergenpass)

;; supergenpass.el ends here
