;;; copilot-company.el --- Company backend for copilot.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 copilot-emacs maintainers

;; Author: Copilot Emacs contributors
;; Keywords: convenience, completion

;;; Commentary:

;; Optional Company backend for copilot.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'copilot)

(declare-function company-begin-backend "company")

(defgroup copilot-company nil
  "Company backend for Copilot."
  :group 'copilot
  :prefix "copilot-company-")

(defcustom copilot-company-trigger-kind 1
  "Trigger kind used for Company requests.
Use 1 for manual invocation semantics and 2 for automatic semantics."
  :type '(choice (const :tag "Manual" 1)
                 (const :tag "Automatic" 2))
  :group 'copilot-company)

(defconst copilot-company--command-property 'copilot-company-command
  "Text property used to keep Copilot command payload on candidates.")

(defun copilot-company--candidate-from-item (item)
  "Convert Copilot completion ITEM to a Company candidate string."
  (let ((insert-text (plist-get item :insertText))
        (command (plist-get item :command)))
    (when (and (stringp insert-text)
               (not (string-empty-p insert-text)))
      (propertize insert-text
                  copilot-company--command-property command))))

(defun copilot-company--candidates-from-response (response)
  "Build Company candidates from Copilot RESPONSE."
  (let ((items (copilot--normalize-completion-response response))
        (candidates nil))
    (dolist (item items (nreverse candidates))
      (when-let ((candidate (copilot-company--candidate-from-item item)))
        (push candidate candidates)))))

(defun copilot-company--fetch-candidates (callback)
  "Fetch Copilot candidates and call CALLBACK with candidate list."
  (copilot--get-completion
   (lambda (response)
     (funcall callback (copilot-company--candidates-from-response response)))
   copilot-company-trigger-kind))

(defun copilot-company--execute-command (payload)
  "Execute Copilot command PAYLOAD for accepted completion."
  (copilot--async-request 'workspace/executeCommand payload))

;;;###autoload
(defun company-copilot (command &optional arg &rest _ignored)
  "Company backend that serves Copilot completions.
COMMAND, ARG and _IGNORED follow the Company backend protocol."
  (interactive (list 'interactive))
  (pcase command
    ('interactive
     (if (fboundp 'company-begin-backend)
         (company-begin-backend 'company-copilot)
       (user-error "company-mode is not available")))
    ('prefix
     (and (bound-and-true-p copilot-mode) ""))
    ('candidates
     (cons :async
           (lambda (callback)
             (copilot-company--fetch-candidates callback))))
    ('post-completion
     (when-let ((payload (get-text-property 0 copilot-company--command-property arg)))
       (copilot-company--execute-command payload)))
    ('sorted t)
    ('no-cache t)))

(defalias 'copilot-company-backend #'company-copilot)

(provide 'copilot-company)
;;; copilot-company.el ends here
