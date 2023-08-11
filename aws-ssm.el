;;; aws-ssm.el --- AWS SSM loader for Emacs. -*- lexical-binding: t -*-

;;; Commentary:

;;; aws-ssm provides a nice way to find AWS SSM parameters
;;; by using the consult package to enable fuzzy search.

;;; WARNING: aws-ssm is fast because all parameters are cached.
;;; IN CASE THIS IS A PROBLEM TO YOU, DON'T USE IT.

;;; To reload the cache you simply need to run the interactive
;;; function aws-ssm-update-cache.
;;; For the first use, this is loaded during the package initialization.

;;; Code:

(require 'async)
(require 'consult)
(require 'pcache)

(defgroup aws-ssm nil
  "An interactive AWS SSM environment for emacs."
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
  "Constant to define the limit length that values displayed in the aws-ssm list can have.  Values with a value larger than this limit should be truncated.")

(defun aws-ssm ()
  "Load AWS SSM parameters using AWS CLI."
  (interactive)
  (let ((parameters-model (aws-ssm--read-data)))
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
									   (goto-line 1)))
								 (message value)))))))

(defun aws-ssm--get-parameters-view (parameters-model)
  "Convert PARAMETERS-MODEL to PARAMETERS-VIEW to be displayed in the aws-ssm function."
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
  "Update the AWS SSM cache."
  (interactive)
  (if (y-or-n-p "AWS SSM will load all parameters, do you confirm? ")
	  (progn
		(message "AWS SSM updating cache ...")
		(async-start
		 (aws-ssm--get-parameters-async)
		 (lambda (parameters)
		   (let ((ht (make-hash-table :test 'equal)))
			 (seq-do (lambda (elt)
					   (puthash (nth 0 elt) (nth 1 elt) ht))
					 parameters)
			 (aws-ssm--write-data ht))
		   
		   (message "AWS SSM cache updated"))))))

(defun aws-ssm--get-parameters-async ()
  "Load AWS SSM parameters asynchronously."
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
  "Truncate VALUE to not have a length greater than LIMIT."
  (let* ((break-line-pos (aws-ssm--break-line-pos value))
		 (first-line (substring value 0 break-line-pos)))
	(format "%s%s"
			(aws-ssm--truncate-single-line first-line limit (if (not break-line-pos) "..."))
			(or (if break-line-pos "...") ""))))

(defun aws-ssm--truncate? (value limit)
  "Should VALUE to be truncated for LIMIT?"
  (or
   (not (null (aws-ssm--break-line-pos value)))
   (aws-ssm--truncate-single-line? value limit)))

(defun aws-ssm--break-line-pos (value)
  "Index of the break line in the VALUE."
  (string-search "\n" value))

(defun aws-ssm--truncate-single-line (value limit &optional ellipsis)
  "Truncate using the first line of VALUE to not have a length greater than LIMIT, using ELLIPSIS to indicate truncation or empty string if absent."
  (if (aws-ssm--truncate-single-line? value limit)
	  (format "%s%s" (substring value 0 limit) (or ellipsis ""))
	value))

(defun aws-ssm--truncate-single-line? (value limit)
  "Should first line of the VALUE to be truncated for LIMIT?"
  (> (length value) limit))

;; data serialization
(defun aws-ssm--read-data ()
  "Read data from pcache repository."
  (let ((repo (pcache-repository "aws-ssm-repository")))
	(pcache-get repo 'model)))

(defun aws-ssm--write-data (data)
  "Write DATA to pcache repository."
  (let ((repository (pcache-repository "aws-ssm-repository")))
	(pcache-put repository 'model data)))

;; ensure first load
(if (not (aws-ssm--read-data))
	(aws-ssm-update-cache))

(provide 'aws-ssm)
;;; aws-ssm.el ends here
