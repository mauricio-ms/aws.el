;;; -*- lexical-binding: t -*-
(use-package async)
(use-package consult)
(use-package pcache)

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

;; TODO => Add comments
(defun aws-ssm ()
  (interactive)
  (let ((parameters-model (aws-ssm--read-data "aws-ssm-model-repo")))
	(consult--read (aws-ssm--get-parameters-view parameters-model)
				   :prompt "SSM: "
				   :lookup (lambda (selected candidates input narrow)
							 (let ((value (gethash (s-trim (nth 0 (s-split "=>" selected)))
												   parameters-model)))
							   (if (aws-ssm--truncate? value aws-ssm--value-limit-length)
								   (let ((buffer (get-buffer-create "*aws-ssm*")))
									 (with-current-buffer buffer
									   (erase-buffer)
									   (insert value)
									   (switch-to-buffer buffer)
									   (forward-line 1)))
								 (message value)))))))

(defun aws-ssm--get-parameters-view (parameters-model)
  (let ((parameters-view))
	(maphash
	 (lambda (k v)
	   (push
		(format "%-80s => %s"
				k
				(propertize (aws-ssm--truncate v aws-ssm--value-limit-length)
							'face 'aws-ssm-value-font))
		parameters-view))
	 parameters-model)
	parameters-view))

;; load cache
(defun aws-ssm-update-cache ()
  (interactive)
  (message "AWS SSM updating cache ...")
  (async-start
   (aws-ssm--get-parameters-async)
   (lambda (parameters)
	 (let ((ht (make-hash-table :test 'equal)))
	   (seq-do (lambda (elt)
				 (puthash (nth 0 elt) (nth 1 elt) ht))
			   parameters)
	   (aws-ssm--write-data "aws-ssm-model-repo" ht))
	 
	 (message "AWS SSM cache updated"))))

(defun aws-ssm--get-parameters-async ()
  (lambda ()
	(defun execute-command ()
	  (json-parse-string (shell-command-to-string "aws ssm get-parameters-by-path --path / --recursive --query \"Parameters[*].{Name:Name,Value:Value}\"")))

	(defun parse-get-parameters-resp (command-result-json)
	  (mapcar (lambda (x)
				`(,(gethash "Name" x)
				  ,(gethash "Value" x)))
			  command-result-json))
	
	(parse-get-parameters-resp (execute-command))))

;; display values
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

;; data serialization
(defun aws-ssm--read-data (filename)
  (let ((repo (pcache-repository filename)))
	(pcache-get repo 'model)))

(defun aws-ssm--write-data (filename data)
  (let ((repo (pcache-repository filename)))
	(pcache-put repo 'model data)))
