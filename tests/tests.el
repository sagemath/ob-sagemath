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
      (delete-file file))
    (let ((file (ob-sagemath-test-exec
                 '((:result-params "file"))
                 "plot(sin, (0, 2*pi))")))
      (should (stringp file))
      (should (f-exists? file))
      (should (> (f-size file) 0))))

  (ert-deftest ob-sagemath-table ()
    (let ((l '(("0" "2") ("1" "3") ("2" "5") ("3" "3^2") ("4" "17"))))
      (should (equal (ob-sagemath-test-exec
                      '((:result-params "table"))
                      "[(a, factor(2^a + 1)) for a in range(5)]")
                     l))
      (should (equal (ob-sagemath-test-exec
                      '((:result-params "table")
                        (:colnames "foo" "bar"))
                      "[(a, factor(2^a + 1)) for a in range(5)]")
                     `(("foo" "bar") hline ,@l)))))

  (ert-deftest ob-sagemath-misc ()
    (should (string= (ob-sagemath-test-exec
                      nil
                      "factor(factorial(10))")
                     "2^8 * 3^4 * 5^2 * 7"))))

(ert-deftest ob-sagemath-table-or-string ()
  (should (equal (ob-sagemath-table-or-string
                  "[[[1, 2, 3], 'a'], ('[[a, b]], c', d), [[4, 5, 6], c]]" nil)
                 '(("[1, 2, 3]" "a") ("[[a, b]], c" "d") ("[4, 5, 6]" "c")))))

(ert-deftest ob-sagemath--latex-arg-test ()
  (should (string= (ob-sagemath--latex-arg '(())) "False"))
  (should (string= (ob-sagemath--latex-arg '((:tolatex))) "True"))
  (should (string= (ob-sagemath--latex-arg '((:tolatex . "yes"))) "True"))
  (should (string= (ob-sagemath--latex-arg '((:tolatex . "no"))) "False"))
  (should (string= (ob-sagemath--latex-arg '((:tolatex . "foo.bar"))) "True")))

(ert-deftest ob-sagemath--escape-code-test ()
  (should (string= (ob-sagemath--escape-code "print 'foobar\\nbar\\nbaz'
print \"foo\\nbar\"
print r\"\\bar\"
print r'\\foo\\n\\bar'")
                   "print 'foobar\\\\nbar\\\\nbaz'\\nprint \\\"foo\\\\nbar\\\"\\nprint r\\\"\\\\bar\\\"\\nprint r'\\\\foo\\\\n\\\\bar'")))
