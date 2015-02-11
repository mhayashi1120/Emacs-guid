(require 'guid)

(defun guid-test--file-strings (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) nil t)))

(ert-deftest predicate-0001 ()
  :tags '(guid)
  (should (guid-string-p (guid-generate-string)))
  (should (guid-string-p (guid-generate-string t)))
  (should (guid-string-p (guid-generate-string t 'uuid-4)))
  ;; (should (guid-string-p (concat "{" (guid-generate-string) "}")))
  )

(ert-deftest recursive-0001 ()
  :tags '(guid)
  (random "")
  (unwind-protect
      (progn
        (guid-update-directory "tests")
        (should (equal (guid-test--file-strings "tests/guid1.txt")
                       '("c0d7fe41-4517-7206-926e-0cfb787c7e5a"
                         "4e0c0ad2-53e1-9ef7-1995-e2d172a69389"
                         "d9bec132-6b73-36a0-9c45-8481de4b1c93"
                         "c0d7fe41-4517-7206-926e-0cfb787c7e5a")))
        (should (equal (guid-test--file-strings "tests/guid2.txt")
                       '("d9bec132-6b73-36a0-9c45-8481de4b1c93"))))
    (random t)))
