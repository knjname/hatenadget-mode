; -*-coding:utf-8-*-
(require 'url)
(require 'url-cookie)

(defvar *hatena-home* "http://d.hatena.ne.jp")
(defvar *hatena-login* "https://www.hatena.ne.jp/login")
(defvar *hatena-username*)
(defvar *hatena-password*)

(defun urlfy (url-data)
  (mapconcat
   (lambda (arg)
	 (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
   url-data "&"))

(defun hatena-timestamp ()
  "20130122131110")

(defun base64-md5 (str)
  (let*
	  ((hexstr (md5 str))
	   (bytelist (mapcar #'(lambda (byte) (string-to-number byte 16))
						 (loop for i from 2 to (length hexstr) by 2
							   collect (substring hexstr (- i 2) i)
							   )))
	   (bytestring (apply #'string bytelist)))
	(replace-regexp-in-string "=+$" "" (base64-encode-string bytestring))
	))


(defun hatena-rkm ()
  (let* ((cookie url-cookie-storage)
		 (cookie-list (cdr (assoc ".hatena.ne.jp" cookie)))
		 (result))
	(loop for cook in cookie-list do
		  (when (equal "rk" (elt cook 1))
			(setq result (base64-md5 (elt cook 2)))))
	result
	)
  )

(defun hatena-login ()
  (let ((user-diary (concat *hatena-home* "/" *hatena-username* "/?")))
	(let* ((url-request-method "POST")
		   (url-request-extra-headers
			'(("Content-Type" . "application/x-www-form-urlencoded")))
		   (url-request-data (urlfy `(("name" . *hatena-username**)
									  ("password" . ,*hatena-password*)
									  ("persistent" . "1")
									  ("location" . "http://d.hatena.ne.jp/")
									  ))))
	  (url-retrieve *hatena-login*
					(lambda (status)
					  (print status)
					  (write-file "~/test.html")
					  )))
	))


