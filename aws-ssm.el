;;; -*- lexical-binding: t -*-
(use-package async)
(use-package consult)

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
						   (message (s-trim (nth 0 (s-split "=>" selected)))))
				 ;; :lookup (lambda (selected candidates input narrow)
				 ;; 		   (let ((start-value-pos (next-property-change 0 selected)))
				 ;; 			 (substring-no-properties selected start-value-pos)))
				 ;; :lookup (lambda (selected candidates input narrow)
				 ;; 			 (aws-ssm--get-parameter-value selected))
				 ))

(defun aws-ssm-reload-cache ()
  (interactive)
  (message "AWS SSM updating parameters cache ...")
  (async-start
   (aws-ssm--get-parameters-names-async "aws ssm describe-parameters --max-items 10")
   (lambda (names)
	 (let ((get-parameters-command (format "aws ssm get-parameters --names %s" (string-join names " "))))
	   (async-start
		(aws-ssm--get-parameters-values-async get-parameters-command)
		(lambda (values)
		  (setq parameters-cache (mapcar
								  (lambda (elt)
									(format "%-80s => %s"
											(nth 0 elt)
											(propertize (aws-ssm--truncate (nth 1 elt) 80)
														'face 'aws-ssm-value-font)))
								  values))
		  (message "AWS SSM parameters cache updated")))))))

(defun aws-ssm--truncate (value limit)
  (let* ((break-line-pos (string-search "\n" value))
		 (first-line (substring value 0 break-line-pos)))
	(format "%s%s"
			(aws-ssm--truncate-single-line first-line limit (if (not break-line-pos) "..."))
			(or (if break-line-pos "...") ""))))

(defun aws-ssm--truncate-single-line (value limit &optional ellipsis)
  (if (<= (length value) limit)
	  value
	(format "%s%s" (substring value 0 limit) (or ellipsis ""))))

;; TODO => Get should show the complete value, if not truncated at minibuffer, open buffer otherwise

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
				`(,(gethash "Name" x)
				  ,(gethash "Value" x)))
			  (gethash "Parameters" command-result-json)))
	
	(setq parameters-cache (parse-get-parameters-resp (execute-command)))))
