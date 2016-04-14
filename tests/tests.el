(require 'f)
(require 'ert)
(require 'ob-sagemath)

(setq ob-sagemath-output-display-function #'ignore)

(setq org-babel-default-header-args:sage '((:session . t)
                                           (:results . "output")
                                           (:latexnewline . "\\\\\n")
                                           (:latexwrap . ("\\(" . "\\)"))))

(defun ob-sagemath-test-exec (params body)
  (org-babel-execute:sage
   body
   (append params org-babel-default-header-args:sage)))

(when (executable-find "sage")

  (ert-deftest ob-sagemth-latex-code-block ()
    (should (string= (ob-sagemath-test-exec
                      '((:tolatex . "yes")
                        (:latexwrap . nil))
                      "sqrt(x)")
                     "\\sqrt{x}"))
    (should (string= (ob-sagemath-test-exec
                      nil
                      "show(sqrt(x))")
                     "\\(\\sqrt{x}\\)"))
    (should (string= (ob-sagemath-test-exec
                      '((:tolatex . "yes")
                        (:latexwrap . ("\\begin{gather*}\n" . "\n\\end{gather*}")))
                      "sqrt(x)\nsqrt(x)")
                     "\\begin{gather*}
\\sqrt{x}\\\\
\\sqrt{x}
\\end{gather*}")))

  (ert-deftest ob-sagemath-results-value ()
    (should (string= (ob-sagemath-test-exec
                      '((:result-params "value"))
                      "print 1\n2")
                     "2")))

  (ert-deftest ob-sagemath-image ()
    (let ((file (make-temp-file "ob-sage" nil ".png")))
      (should (null (ob-sagemath-test-exec
                     `((:file . ,file))
                     "plot(sin, (0, 2*pi))")))
      (should
       (> (f-size file) 0))
      (delete-file file))))

(ert-deftest ob-sagemath--latex-arg-test ()
  (should (and (string= (ob-sagemath--latex-arg '(())) "False")
               (string= (ob-sagemath--latex-arg '((:tolatex))) "True")
               (string= (ob-sagemath--latex-arg '((:tolatex . "yes"))) "True")
               (string= (ob-sagemath--latex-arg '((:tolatex . "no"))) "False")
               (string= (ob-sagemath--latex-arg '((:tolatex . "foo.bar"))) "True"))))
