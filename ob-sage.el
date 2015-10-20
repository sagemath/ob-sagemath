;;; ob-sage.el --- org-babel functions for SageMath -*- lexical-binding: t -*-

;; Package-Requires: ((sage-shell-mode "0.0.8") (s "1.10.0"))
;; Version: 0.1
;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'org)
(require 'sage-shell-mode)
(require 'ob-python)
(require 's)
(add-to-list 'org-babel-tangle-lang-exts '("sage" . "sage"))
(defvar org-babel-default-header-args:sage '((:session . t)
                                             (:exports . "both")
                                             (:results . "output")))
;;; Do not evaluate code when exporting.
(setq org-export-babel-evaluate nil)
(defvar org-babel-sage--last-res-var "_emacs_ob_sage_var")

;;;###autoload
(defun org-babel-sage-ctrl-c-ctrl-c ()
  "Execute current src code block."
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (language (car info))
         (body (nth 1 info))
         (params (nth 2 info)))
    (if (member language '("sage" "sage-shell"))
        (org-babel-sage-execute1 body params)
      (call-interactively #'org-ctrl-c-ctrl-c))))

(defun org-babel-sage--init (session)
  (cond ((string= session "none")
         (error "ob-sage currently only supports evaluation using a session.
Make sure your src block has a :session param."))
        ((stringp session)
         (setq sage-shell:process-buffer
               (sage-shell:run "sage" nil 'no-switch
                               (format "*Sage<%s>*" session))))
        (t (setq sage-shell:process-buffer
                 (sage-shell:run "sage" nil 'no-switch))))

  (org-babel-remove-result)
  (message "Evaluating code block ..."))

(defun org-babel-sage-execute1 (body params)
  (let* (
         ;; (file (cdr (assoc :file params)))
         (session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params)))
         (code (org-babel-expand-body:generic
                (encode-coding-string body 'utf-8)
                params (org-babel-variable-assignments:python params)))
         (pt (point))
         (buf (current-buffer))
         (marker (make-marker))
         (marker (set-marker marker pt)))

    (org-babel-sage--init session)

    (with-current-buffer sage-shell:process-buffer
      (sage-shell:after-output-finished
        (sage-shell:send-command
         (format "%s = None" org-babel-sage--last-res-var) nil nil t)

        (let ((output-call-back
               (sage-shell:send-command
                (format "%s = %s(\"%s\")"
                        org-babel-sage--last-res-var
                        (sage-shell:py-mod-func "ip.run_cell")
                        (s-replace-all (list (cons (rx "\n") "\\\\n")
                                             (cons (rx "\"") "\\\\\"")) code))))
              (proc-buf sage-shell:process-buffer))
          (sage-shell:after-redirect-finished
            (let ((output (funcall output-call-back)))
              (defun org-babel-execute:sage (_body _params)
                (let ((result (if (eq result-type 'output)
                                  output
                                (ob-sage--create-output-buffer output)
                                (sage-shell:send-command-to-string
                                 org-babel-sage--last-res-var
                                 proc-buf))))
                  (org-babel-result-cond (cdr (assoc :result-params params))
                    result
                    (org-babel-sage-table-or-string (s-trim result)))))
              (fset 'org-babel-execute:sage-shell
                    (symbol-function 'org-babel-execute:sage)))
            (save-excursion
              (with-current-buffer buf
                (goto-char marker)
                (call-interactively #'org-babel-execute-src-block)))))))))


(defun ob-sage--create-output-buffer (output)
  (unless (s-blank? output)
    (save-excursion
      (let ((buf (get-buffer-create "*ob-sage-output*")))
        (with-current-buffer buf
          (special-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output)
            (goto-char (point-min))))
        (pop-to-buffer buf)))))

(defun org-babel-sage-table-or-string (res)
  (with-temp-buffer
    (insert res)
    (goto-char (point-min))
    (cond ((looking-at (rx (or "((" "([" "[(" "[[")))
           (forward-char 1)
           (with-syntax-table sage-shell-mode-syntax-table
             (cl-loop while (and (re-search-forward (rx (or "(" "[")) nil t)
                                 (progn (forward-char -1)
                                        (not (nth 3 (syntax-ppss)))))
                      collect
                      (org-babel-sage-table-or-string--1
                       (1+ (point))
                       (progn (forward-list) (1- (point)))))))
          (t res))))


(defun org-babel-sage-table-or-string--1 (beg end)
  (let ((start beg))
    (goto-char beg)
    (append
     (cl-loop while (and (re-search-forward "," end t)
                         (not (nth 3 (syntax-ppss))))
              collect (prog1
                          (s-trim
                           (buffer-substring-no-properties
                            start (- (point) 1)))
                        (setq start (point))))
     (list (s-trim (buffer-substring-no-properties start end))))))

(provide 'ob-sage)
;;; ob-sage.el ends here
