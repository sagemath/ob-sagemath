;;; ob-sagemath.el --- org-babel functions for SageMath -*- lexical-binding: t -*-

;; Package-Requires: ((sage-shell-mode "0.0.8") (s "1.8.0"))
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
(require 'ob-exp)
(add-to-list 'org-babel-tangle-lang-exts '("sage" . "sage"))
(defvar org-babel-default-header-args:sage '((:session . t)
                                             (:exports . "both")
                                             (:results . "output")))
;;; Do not evaluate code when exporting.
(setq org-export-babel-evaluate nil)



(defvar ob-sagemath--python-script-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(defvar ob-sagemath--script-name "_emacs_ob_sagemath")

(defvar ob-sagemath--imported-p nil)
(make-variable-buffer-local 'ob-sagemath--imported-p)

(defun ob-sagemath--python-name (f)
  (format "%s.%s" ob-sagemath--script-name f))

(defun ob-sagemath--import-script ()
  "Assumes `sage-shell:process-buffer' is already set."
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (unless ob-sagemath--imported-p
      (sage-shell:send-command
       (s-join "; "
               (list
                (format "sys.path.append('%s')"
                        ob-sagemath--python-script-dir)
                (format "import emacs_ob_sagemath as %s"
                        ob-sagemath--script-name)))
       nil nil t)
      (setq ob-sagemath--imported-p t))))

(defun ob-sagemath--get-success ()
  (with-current-buffer sage-shell:process-buffer
    (let ((res (s-trim (sage-shell:send-command-to-string
                        (ob-sagemath--python-name "last_state.success")))))
      (cond ((string= res "True") t)
            ((string= res "False") nil)
            (t (error "Invalid value of the last state."))))))

;;;###autoload
(defun org-babel-sage-ctrl-c-ctrl-c (arg)
  "Execute current src code block. With prefix argument, evaluate all code in a
buffer."
  (interactive "p")
  (case arg
    (1 (org-babel-sage-ctrl-c-ctrl-c-1))
    (4 (ob-sagemath-execute-buffer))))

(defun org-babel-sage-ctrl-c-ctrl-c-1 ()
  (let* ((info (org-babel-get-src-block-info))
           (language (car info))
           (body (nth 1 info))
           (params (nth 2 info)))
      (if (member language '("sage" "sage-shell"))
          (org-babel-sage-execute1 body params)
        (call-interactively #'org-ctrl-c-ctrl-c))))

(defun org-babel-sage--init (session)
  (cond ((string= session "none")
         (error "ob-sagemath currently only supports evaluation using a session.
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
  (let* ((session (cdr (assoc :session params)))
         (raw-code (org-babel-expand-body:generic
                    (encode-coding-string body 'utf-8)
                    params (org-babel-variable-assignments:python params)))
         (pt (point))
         (buf (current-buffer))
         (marker (make-marker))
         (marker (set-marker marker pt)))

    (org-babel-sage--init session)

    (with-current-buffer sage-shell:process-buffer
      (sage-shell:after-output-finished
        ;; Import a Python script if necessary.
        (ob-sagemath--import-script)

        (let ((output-call-back
               (sage-shell:send-command (ob-sagemath--code raw-code params buf)))
              (res-params (cdr (assoc :result-params params))))
          (sage-shell:change-mode-line-process t "eval")
          (sage-shell:after-redirect-finished
            (sage-shell:change-mode-line-process nil)
            (let ((output (funcall output-call-back))
                  (success-p (ob-sagemath--get-success)))
              (defun org-babel-execute:sage (_body _params)
                (cond (success-p
                       (let ((result output))
                         (cond ((member "file" res-params)
                                (assoc-default :file res-params))
                               ((member "table" res-params)
                                (org-babel-sage-table-or-string (s-trim result) params))
                               (t result))))
                      ;; Return the empty string when it fails.
                      (t "")))
              (fset 'org-babel-execute:sage-shell
                    (symbol-function 'org-babel-execute:sage))
              (with-current-buffer buf
                (save-excursion
                  (goto-char marker)
                  (call-interactively #'org-babel-execute-src-block)
                  (unless success-p
                    (ob-sagemath--failure-callback output)))))))))))

(defvar ob-sagemath-error-buffer-name "*Ob-SageMath-Error*")
(defvar ob-sagemath--error-regexp
  (rx symbol-start
      (or "ArithmeticError" "AssertionError" "AttributeError"
          "BaseException" "BufferError" "BytesWarning" "DeprecationWarning"
          "EOFError" "EnvironmentError" "Exception" "FloatingPointError"
          "FutureWarning" "GeneratorExit" "IOError" "ImportError"
          "ImportWarning" "IndentationError" "IndexError" "KeyError"
          "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
          "NotImplementedError" "OSError" "OverflowError"
          "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
          "RuntimeWarning" "StandardError" "StopIteration" "SyntaxError"
          "SyntaxWarning" "SystemError" "SystemExit" "TabError" "TypeError"
          "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
          "UnicodeError" "UnicodeTranslateError" "UnicodeWarning"
          "UserWarning" "ValueError" "Warning" "ZeroDivisionError")
      symbol-end))

(defvar ob-sagemath--error-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "w" table)
    (modify-syntax-entry ?\' "w" table)
    table))

(define-derived-mode ob-sagemath-error-mode nil "ObSageMathError"
  "Major mode for display errors."
  (set-syntax-table ob-sagemath--error-syntax-table)
  (setq font-lock-defaults
        (list (list (cons ob-sagemath--error-regexp 'font-lock-warning-face))
              nil nil nil 'beginning-of-line)))

(defun ob-sagemath--failure-callback (output)
  (let ((inhibit-read-only t)
        (view-read-only nil)
        (buf (get-buffer-create ob-sagemath-error-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert output)
      (ob-sagemath-error-mode)
      (unless view-mode (view-mode)))
    (pop-to-buffer buf)
    (message "An error raised in the SageMath process.")))


(defun ob-sagemath--code (raw-code params buf)
  (let ((code (s-replace-all (list (cons (rx "\"") "\\\\\"")
                                   (cons (rx "\n") "\\\\n"))
                             (s-replace "\\" "\\\\" raw-code))))
    (format "%s(\"%s\", filename=%s)"
            (ob-sagemath--python-name "run_cell_babel")
            code
            (sage-shell:aif (assoc-default :file params)
                (format "\"%s\""
                        (with-current-buffer buf
                          (expand-file-name it default-directory)))
              "None"))))


(defun ob-sagemath--create-output-buffer (output)
  (unless (s-blank? output)
    (save-excursion
      (let ((buf (get-buffer-create "*ob-sagemath-output*")))
        (with-current-buffer buf
          (special-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output)
            (goto-char (point-min))))
        (pop-to-buffer buf)))))

(defun org-babel-sage-table-or-string (res params)
  (with-temp-buffer
    (insert res)
    (goto-char (point-min))
    (cond ((looking-at (rx (or "((" "([" "[(" "[[")))
           (forward-char 1)
           (let ((res (with-syntax-table sage-shell-mode-syntax-table
                        (cl-loop while (re-search-forward (rx (or "(" "[")) nil t)
                                 when (save-excursion (forward-char -1)
                                                      (not (nth 3 (syntax-ppss))))
                                 collect
                                 (org-babel-sage-table-or-string--1
                                  (point)
                                  (progn (forward-char -1)
                                         (forward-list) (1- (point))))))))
             (cond ((equal (cdr (assoc :colnames params)) "yes")
                    (append (list (car res) 'hline) (cdr res)))
                   (t res))))
          (t res))))

(defun org-babel-sage-table-or-string--1 (beg end)
  (let ((start beg))
    (goto-char beg)
    (append
     (cl-loop while (and (re-search-forward "," end t)
                         (not (nth 3 (syntax-ppss))))
              collect (prog1
                          (org-babel-sage--string-unqote
                           (s-trim
                            (buffer-substring-no-properties
                             start (- (point) 1))))
                        (setq start (point))))
     (list (org-babel-sage--string-unqote
            (s-trim (buffer-substring-no-properties start end)))))))

(defun org-babel-sage--string-unqote (s)
  (sage-shell:->>
   (cond ((string-match (rx bol (group (or (1+ "'") (1+ "\""))) (1+ nonl))
                        s)
          (let ((ln (length (match-string 1 s))))
            (substring s ln (- (length s) ln))))
         (t s))
   (s-replace "\\\\" "\\")))

(defun ob-sagemath--code-block-markers ()
  (let ((markers nil)
        (mrkr nil))
    (org-save-outline-visibility t
      (org-babel-map-executables nil
        (setq mrkr (make-marker))
        (set-marker mrkr (save-excursion (forward-line 1) (point)))
        (push mrkr markers)))
    (reverse markers)))

(defun ob-sagemath-execute-buffer ()
  (interactive)
  (let ((markers (ob-sagemath--code-block-markers))
        (buf (current-buffer)))
    (save-excursion
      ;; Remove all results in current buffer
      (dolist (p markers)
        (goto-char p)
        (org-babel-remove-result))
      (ob-sagemath--execute-makers markers buf))))


(defun ob-sagemath--execute-makers (markers buf)
  (cond ((null markers)
         (message "Every code block in this buffer has been evaluated."))
        (t (with-current-buffer buf
             (save-excursion
               (goto-char (car markers))
               (org-babel-sage-ctrl-c-ctrl-c-1))
             (sage-shell:after-output-finished
               (sage-shell:after-redirect-finished
                 (ob-sagemath--execute-makers (cdr markers) buf)))))))

(provide 'ob-sagemath)
;;; ob-sagemath.el ends here
