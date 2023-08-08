;;; -*- lexical-binding: t -*-
(use-package async)
(use-package consult)

;; MISSING FEATURES
;; - load next items when user came to the end of the list
;; - handle multiple words separated by whitespaces
;;     in these cases select on eto send the request to AWS and use the others to filter in emacs
;; - load next items when filter found less than 10 results
;; Check these alternatives to be able to implement the MISSING FEATURES
;; https://github.com/minad/consult
;; https://github.com/minad/vertico
(defgroup aws nil
  "An interactive AWS environment for emacs."
  :tag "AWS")

(defgroup aws-ssm-faces nil
  "AWS SSM Faces."
  :group 'aws)

(defface aws-ssm-value-font
  '((((background  dark)) :foreground "green")
    (((background light)) :foreground "black"))
  "AWS SSM value font."
  :group 'aws-ssm-faces)

;; (format "%-9s => Value" "aaaaaaaaa")

;; (consult--read `(,(format "key => %s" (propertize "value" 'face 'aws-ssm-value-font))
;; 				 ,(format "keyaaaaaaaaaaaaaaaaaaaaaaa => %s" (propertize "value" 'face 'aws-ssm-value-font))
;; 				 )
;; 			   :prompt "SSM: "
;; 			   :lookup (lambda (selected candidates input narrow)
;; 						 (let ((start-value-pos (next-property-change 0 selected)))
;; 						   (substring-no-properties selected start-value-pos)))
;; 			   ;; :lookup (lambda (selected candidates input narrow)
;; 			   ;; 			 (aws-ssm--get-parameter-value selected))
;; 			   )

(defvar parameters-cache nil
  "Cache to store the parameters candidates")

(defun aws-ssm ()
  (interactive)
  (consult--read parameters-cache
				 :prompt "SSM: "
				 :lookup (lambda (selected candidates input narrow)
						   (let ((start-value-pos (next-property-change 0 selected)))
							 (substring-no-properties selected start-value-pos)))
				 ;; :lookup (lambda (selected candidates input narrow)
				 ;; 			 (aws-ssm--get-parameter-value selected))
				 ))

(defun aws-ssm--load-data ()
  (message "AWS SSM updating parameters cache ...")
  (async-start
   (aws-ssm--get-parameters-names-async "aws ssm describe-parameters --max-items 2")
   (lambda (names)
	 (let ((get-parameters-command (format "aws ssm get-parameters --names %s" (string-join names " "))))
	   (async-start
		(aws-ssm--get-parameters-values-async get-parameters-command)
		(lambda (values)
		  (setq parameters-cache values)
		  (message "AWS SSM parameters cache updated")))))))

(defun aws-ssm--get-parameters-names-async (command)
  (lambda ()
	(defun execute-command ()
	  (json-parse-string (shell-command-to-string command)))

	(defun parse-describe-parameters-resp (command-result-json)
	  (mapcar (lambda (x)
				(gethash "Name" x))
			  (gethash "Parameters" command-result-json)))
	
	(parse-describe-parameters-resp (execute-command))))

(defun aws-ssm--get-parameters-values-async (command)
  (lambda ()
	(defun execute-command ()
	  (json-parse-string (shell-command-to-string command)))

	(defun parse-get-parameters-resp (command-result-json)
	  (mapcar (lambda (x)
				(format "%s => %s"
						(gethash "Name" x)
						(propertize (gethash "Value" x) 'face 'aws-ssm-value-font)))
			  (gethash "Parameters" command-result-json)))
	
	(setq parameters-cache (parse-get-parameters-resp (execute-command)))))
