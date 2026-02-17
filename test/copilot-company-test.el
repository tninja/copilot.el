;;; copilot-company-test.el --- Tests for copilot-company.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for copilot-company.el.

;;; Code:

(require 'buttercup)
(require 'copilot-company)

(describe "company-copilot"
  (describe "candidates"
    (it "returns async candidates from Copilot response"
      (let (trigger-kind received)
        (spy-on 'copilot--get-completion
                :and-call-fake
                (lambda (callback &optional kind)
                  (setq trigger-kind kind)
                  (funcall callback
                           (list :items [(:insertText "alpha"
                                           :command (:command "cmd-1"))
                                         (:insertText "beta")]))))
        (let* ((result (company-copilot 'candidates ""))
               (fetcher (cdr result)))
          (expect (car result) :to-equal :async)
          (funcall fetcher (lambda (candidates)
                             (setq received candidates)))
          (expect trigger-kind :to-equal 1)
          (expect (mapcar #'substring-no-properties received)
                  :to-equal '("alpha" "beta"))
          (expect (get-text-property 0 'copilot-company-command (car received))
                  :to-equal '(:command "cmd-1"))))))

  (describe "post-completion"
    (it "executes completion command telemetry when available"
      (let ((payload nil)
            (candidate (propertize "alpha"
                                   'copilot-company-command
                                   '(:command "cmd-1"))))
        (spy-on 'copilot-company--execute-command
                :and-call-fake
                (lambda (p)
                  (setq payload p)))
        (company-copilot 'post-completion candidate)
        (expect payload :to-equal '(:command "cmd-1"))))))

;;; copilot-company-test.el ends here
