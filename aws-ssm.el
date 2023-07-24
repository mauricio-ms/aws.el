;;; -*- lexical-binding: t -*-
(use-package async)

(progn
  (setq params nil)
  (aws-ssm--load-data nil)
  (setq timer nil)
  (ivy-read "SSM: " (lambda (filter y z)
		      (if (not (string-empty-p filter))
			  (progn
			    (if timer
				(cancel-timer timer))
			    (setq timer
				  (run-at-time 0.25 nil (lambda () (aws-ssm--load-data filter))))))
		      params)
	    :dynamic-collection t))

(defun aws-ssm--load-data (filter)
  (async-start
   (aws-ssm--run-async filter)
   (lambda (result)
     (setq params result)
     (ivy-update-candidates result))))

(defun aws-ssm--command (filter)
  (format "aws ssm describe-parameters %s --max-items 10" (aws-ssm--filters filter)))

(defun aws-ssm--filters (filter)
  (if (not (string-empty-p (s-trim (or filter ""))))
      (format "--parameter-filters \"Key=Name,Option=Contains,Values=%s\"" filter)
    ""))

(defun aws-ssm--run-async (filter)
  (let ((command (aws-ssm--command filter)))
    (lambda ()
      (defun aws-ssm--parse-params (command-result-json)
	(mapcar (lambda (x)
		  (gethash "Name" x))
		(gethash "Parameters" command-result-json)))
      (defun aws-ssm--command-to-json (command-result)
	(json-parse-string command-result))

      (aws-ssm--parse-params (aws-ssm--command-to-json (shell-command-to-string command))))))
