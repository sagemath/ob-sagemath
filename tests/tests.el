(require 'ert)
(require 'ob-sagemath)


(ert-deftest ob-sagemath--latex-arg-test ()
  (should (and (string= (ob-sagemath--latex-arg '(())) "False")
               (string= (ob-sagemath--latex-arg '((:tolatex))) "True")
               (string= (ob-sagemath--latex-arg '((:tolatex . "yes"))) "True")
               (string= (ob-sagemath--latex-arg '((:tolatex . "no"))) "False")
               (string= (ob-sagemath--latex-arg '((:tolatex . "foo.bar"))) "True"))))
