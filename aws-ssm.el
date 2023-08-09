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

(defconst aws-ssm--value-limit-length 80
  "Constant to define the limit length that values displayed in the aws-ssm list can have. values with a value larger than this limit should be truncated.")

(defvar aws-ssm--parameters-model nil
  "Cache to store the parameters candidates model")

(defvar aws-ssm--parameters-view nil
  "Cache to store the parameters candidates view")

;; TODO => Add comments
;; TODO => Remove max-items parameter
;; TODO => Serialize and deserialize the cache variables

(defun aws-ssm ()
  (interactive)
  (consult--read aws-ssm--parameters-view
				 :prompt "SSM: "
				 :lookup (lambda (selected candidates input narrow)
						   (let ((value (gethash (s-trim (nth 0 (s-split "=>" selected)))
												 aws-ssm--parameters-model)))
							 (if (aws-ssm--truncate? value aws-ssm--value-limit-length)
								 (let ((buffer (get-buffer-create "*aws-ssm*")))
								   (with-current-buffer buffer
									 (erase-buffer)
									 (insert value)
									 (switch-to-buffer buffer)))
							   (message value))))))

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
		  (let ((ht (make-hash-table :test 'equal)))
			(seq-do (lambda (elt)
					  (puthash (nth 0 elt) (nth 1 elt) ht))
					values)
			(setq aws-ssm--parameters-model ht))
		  (setq aws-ssm--parameters-view (mapcar
										  (lambda (elt)
											(format "%-80s => %s"
													(nth 0 elt)
													(propertize (aws-ssm--truncate (nth 1 elt) aws-ssm--value-limit-length)
																'face 'aws-ssm-value-font)))
										  values))
		  (message "AWS SSM parameters cache updated")))))))

(defun aws-ssm--truncate (value limit)
  (let* ((break-line-pos (aws-ssm--break-line-pos value))
		 (first-line (substring value 0 break-line-pos)))
	(format "%s%s"
			(aws-ssm--truncate-single-line first-line limit (if (not break-line-pos) "..."))
			(or (if break-line-pos "...") ""))))

(defun aws-ssm--truncate? (value limit)
  (or
   (not (null (aws-ssm--break-line-pos value)))
   (aws-ssm--truncate-single-line? value limit)))

(defun aws-ssm--break-line-pos (value)
  (string-search "\n" value))

(defun aws-ssm--truncate-single-line (value limit &optional ellipsis)
  (if (aws-ssm--truncate-single-line? value limit)
	  (format "%s%s" (substring value 0 limit) (or ellipsis ""))
	value))

(defun aws-ssm--truncate-single-line? (value limit)
  (> (length value) limit))

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
    
	(parse-get-parameters-resp (execute-command))))
