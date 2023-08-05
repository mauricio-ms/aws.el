;;; -*- lexical-binding: t -*-
(use-package async)

;; MISSING FEATURES
;; - load next items when user came to the end of the list
;; - handle multiple words separated by whitespaces
;;     in these cases select on eto send the request to AWS and use the others to filter in emacs
;; - load next items when filter found less than 10 results
;; Check these alternatives to be able to implement the MISSING FEATURES
;; https://github.com/minad/consult
;; https://github.com/minad/vertico

(progn
  (setq params nil)
  
  (setq timer nil)
  (setq cursor 0)

  (aws-ssm--load-data nil)
  
  (ivy-read "SSM: " (lambda (filter y z)
		      (if (not (string-empty-p filter))
			  (progn
			    (if timer
				(cancel-timer timer))
			    (setq timer
				  (run-at-time 0.25 nil (lambda () (aws-ssm--load-data filter))))))
		      params)
	    :update-fn (lambda ()
			 (message "x: %s" ivy-history))
	    :dynamic-collection t))

(defun aws-ssm--load-data (filter)
  (async-start
   (aws-ssm--run-async filter)
   (lambda (result)
     (setq cursor 0)
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
