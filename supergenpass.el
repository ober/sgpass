;;; supergenpass.el --- SuperGenPass for Emacs

;; Copyright (C) 2013  Jaime Fournier <jaimef@linbsd.org>

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

(defun b64-md5 (pickle)
  "Encrypt the string given to us as Base64 encoded Md5 byte stream"
  (replace-regexp-in-string "=" "A" (replace-regexp-in-string "+" "9" (replace-regexp-in-string "/" "8" (base64-encode-string (secure-hash 'md5 pickle nil nil t))))))

(defun sgp-prompt-pass (domain password)
  "Interactive function to prompt for domain and password and populate paste buffer with new password"
  (interactive (list (read-string "SuperGenPass: Domain: ") (read-passwd "SuperGenPass Password: ")))
  (kill-new (format "%s" (sgp-generate password domain))))

(defun sgp (domain)
  "Interactive function to prompt for domain and use the master password loaded from secrets"
  (interactive (list (read-string "SuperGenPass: Domain: ")))
  (kill-new (format "%s" (sgp-generate sgp-master-password domain))))

(defun supergenpass (password domain)
  (kill-new (format "%s" (sgp-generate password domain))))

(defun sgp-generate (password domain)
  "Create a unique password for a given domain and master password"
  (let ((i 0) (results (format "%s:%s" password domain)))
    (setq results (format "%s:%s" password domain))
    (while
        (not (and (> i 9) (secure-enough results 10)))
      (setq results (b64-md5 results))
      (setq i (1+ i)))
    (substring results 0 10)))

(defun s-pwgen () (interactive)
  (message (format "%s" (sgp-generate (random 1000000) (random 1000000)))))

(defun secure-enough (results len)
  "Ensure the password we have is sufficiently secure"
  (let
      ((case-fold-search nil))
    (and
     (> (length results) len)
     (string-match "[0-9]" (substring results 0 len))
     (string-match "[A-Z]" (substring results 0 len))
     (string-match "^[a-z]" (substring results 0 len)))))

(defun sgp-test-compat () (interactive)
  (if
      (and
       (equal "yzqAqlHt9f" (sgp-generate "aZ6hsFkQAB" "facebook.com"))
       (equal "hseXb5T8l4" (sgp-generate "et8U4PacDy" "twitter.com"))
       (equal "vGj89k995X" (sgp-generate "fB0JxUJk3F" "cnn.com"))
       (equal "ru8ZwQk0TD" (sgp-generate "fWeQm8cIm4" "slashdot.org"))
       (equal "hDdkCn4SYm" (sgp-generate "fmXLipc0a9" "tumblr.com"))
       (equal "j80eUtBGJN" (sgp-generate "fssqka0uhR" "twitter.com"))
       (equal "e9EG96c6fR" (sgp-generate "hUyoJKKb8X" "cnn.com"))
       (equal "z8ZI0szNvT" (sgp-generate "hxJch45dWd" "slashdot.org"))
       (equal "y7l8pErqDC" (sgp-generate "iSRqWL1NIv" "tumblr.com"))
       (equal "i0Yd5d88IR" (sgp-generate "j4fdVHliBW" "twitter.com"))
       (equal "lK8C0UvAPb" (sgp-generate "lonPN3Slmu" "cnn.com"))
       (equal "bD4zdZNfZa" (sgp-generate "m0x8y8pQd0" "slashdot.org"))
       (equal "kntCXxO1O8" (sgp-generate "oP9jePVJEt" "tumblr.com"))
       (equal "ek7KukV0b8" (sgp-generate "odCS62yxVF" "twitter.com"))
       (equal "n79c3ZL3fC" (sgp-generate "pC37b4Ed8U" "cnn.com"))
       (equal "qlT5M5HVwN" (sgp-generate "pLBe2YlyTe" "slashdot.org"))
       (equal "o6b4JfG7Y3" (sgp-generate "qIW3jgzAVY" "tumblr.com"))
       (equal "ulN2ntOT3d" (sgp-generate "r04PRY4gAq" "twitter.com"))
       (equal "cpWNqiqKH1" (sgp-generate "rDL8WCOsmK" "cnn.com"))
       (equal "bcZa4CZN5D" (sgp-generate "sh8kMDPyhF" "slashdot.org"))
       (equal "g8IBowWjUm" (sgp-generate "t8HQVPPSwA" "tumblr.com"))
       (equal "zw1efoqJCx" (sgp-generate "tbUkLl20QC" "twitter.com"))
       (equal "y9udF9AKzQ" (sgp-generate "u2262a1TEg" "cnn.com"))
       (equal "ajzQB3StiY" (sgp-generate "unxdaQdxQ4" "slashdot.org"))
       (equal "vs8nD21Cqj" (sgp-generate "uvphI9ofTX" "tumblr.com"))
       (equal "i64lFGXZ7W" (sgp-generate "vGkM2qMik7" "twitter.com"))
       (equal "i8Qn4iQloB" (sgp-generate "vvdsM88uvl" "cnn.com"))
       (equal "l7CK1Kq3BD" (sgp-generate "wHwt3lz6hr" "slashdot.org"))
       (equal "yahu9B8xii" (sgp-generate "wsjPWFc2wr" "tumblr.com"))
       (equal "sZFp0t91ku" (sgp-generate "wwV3EPLpJp" "twitter.com"))
       (equal "cbxYw9UxKg" (sgp-generate "xP9Ut9HIVB" "cnn.com"))
       (equal "qg2tWhKFRj" (sgp-generate "xZj9ucltUq" "slashdot.org"))
       (equal "oX0ExINYjb" (sgp-generate "xwz5j3mnM1" "tumblr.com"))
       (equal "yGqiWI9Kg8" (sgp-generate "y5kDzKGHw5" "twitter.com"))
       (equal "xNQP09Z94u" (sgp-generate "y8h9oZquqc" "cnn.com"))
       (equal "ubRPZrdfu5" (sgp-generate "abcdefg" "cnn.com"))
       (equal "xaIlcL02A3" (sgp-generate "TheSeasonedSchemer" "slashdot.org"))
       (equal "xXuB5haYik" (sgp-generate "ColdAndFluRelief" "flowdock.com"))
       (equal "oZ8YbHxSKg" (sgp-generate "LiveLongAndProper" "lisp.org")))
      (message "All Passed")
    (message "Failures!")))

(provide 'supergenpass)

;; supergenpass.el ends here
