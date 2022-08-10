;;; test-speed-type.el --- Tests for speed-type  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'speed-type)

(ert-deftest speed-type--stats-test ()
  (should (= 3 (speed-type--seconds-to-minutes 180)))
  (should (= 30 (speed-type--gross-wpm 450 180)))
  (should (= 15 (speed-type--net-wpm 450 45 180)))
  (should (= 85 (speed-type--accuracy 100 90 5)))
  (should (string= "Beginner" (speed-type--skill 10)))
  (should (string= "Pro" (speed-type--skill 45)))
  (should (string= "Racer" (speed-type--skill 400))))

(ert-deftest speed-type--url-test ()
  (should (string= "https://www.gutenberg.org/cache/epub/1/pg1.txt"
                   (speed-type--gb-url 1))))

(ert-deftest speed-type--charfun-tests ()
  (should (speed-type--check-same 0 "\nfoo\n" "\nfoo\n"))
  (should (speed-type--check-same 1 "\nfoo\s" "\nfoo\n"))
  (should (speed-type--check-same 4 "\nfoo\s" "\nfoo\t"))
  (should (not (speed-type--check-same 2 "\nfoo\s" "\nfxo\n"))))

(ert-deftest speed-type--trim-tests ()
  (should (string= "foo\n\t\sbar"
                   (speed-type--trim "\n\nfoo\n\t\sbar\n\n\n")))
  (should (string= "\tfoo\n\t\sbar"
                   (speed-type--trim "\n\tfoo\n\t\sbar\n\n\n\n"))))

(ert-deftest speed-type-mode-test()
  (let* ((hook-executed nil))
    (defun speed-type-mode-test-hook ()
      (setq hook-executed t))
    (unwind-protect
        (progn
          (add-hook 'speed-type-mode-hook 'speed-type-mode-test-hook)
          (with-temp-buffer
            (speed-type-mode)
            (kill-current-buffer))
          (should (equal hook-executed t)))
      (remove-hook 'speed-type-mode-hook 'speed-type-mode-test-hook))))

(defun speed-type--retrieve-non-existant-file-environment (filename-expected test)
  (let ((speed-type-gb-dir "/tmp"))
    (unwind-protect
        (progn
          (delete-file filename-expected)
          (funcall test))
      (delete-file filename-expected))))

(defun speed-type--retrieve-existant-file-environment (filename-expected test)
  (let ((speed-type-gb-dir "/tmp"))
    (unwind-protect
        (progn
          (delete-file filename-expected)
          (with-temp-buffer
            (write-file filename-expected))
          (funcall test))
      (delete-file filename-expected))))

(ert-deftest speed-type--retrieve-test ()
  (let ((filename "speed-type--retrieve-test-file")
        (filename-expected "/tmp/speed-type--retrieve-test-file.txt")
        (url "https://www.google.com"))

    (speed-type--retrieve-non-existant-file-environment
     filename-expected
     (lambda ()
       (let ((filename-response (speed-type--retrieve filename url)))
         (should (stringp filename-response))
         (should (string= filename-response filename-expected))
         (should (file-exists-p filename-expected))
         (should (file-readable-p filename-expected)))))

    (speed-type--retrieve-existant-file-environment
     filename-expected
     (lambda ()
       (let ((filename-response (speed-type--retrieve filename url)))
         (should (stringp filename-response))
         (should (string= filename-response filename-expected))
         (should (file-exists-p filename-expected))
         (should (file-readable-p filename-expected))))))

  (let ((filename "speed-type--retrieve-test-file")
        (filename-expected "/tmp/speed-type--retrieve-test-file.txt")
        (url "https://www.google.com/nonexitanresource"))

    (speed-type--retrieve-non-existant-file-environment
     filename-expected
     (lambda ()
       (let ((filename-response (speed-type--retrieve filename url)))
         (should (null filename-response))
         (should (not (file-exists-p filename-expected)))
         (should (not (file-readable-p filename-expected))))))))
