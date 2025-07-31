;;; test-speed-type.el --- Tests for speed-type  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'speed-type)

;; gutenberg
;; azquotes
;; :lang lang
;; :n-words n
;; :go-next-fn go-next-fn
;; :add-extra-word-content-fn
;; :replay-fn
;; (speed-type--setup buf
;; 	       (buffer-substring-no-properties start end)
;; 	       :author (user-full-name)
;; 	       :title title
;; 	       :replay-fn #'speed-type--get-replay-fn))))

(ert-deftest speed-type-test/feeling-better? ()
  "Checks if it's a good day to program."
  (should (= 1 1)))

(ert-deftest speed-type-test/general-region ()
  "Do a general test with `speed-type-region' with fundamental mode and a prog-mode, checking content, overlays, point and point-motion, buffer-variables and statistic file."
  (let ((content "abcde")
	(mode (nth (random 2) '(fundamental-mode emacs-lisp-mode))))
    (with-temp-buffer
      (insert content)
      (funcall mode)
      (let ((buf (speed-type-region (point-min) (point-max)))
	    (content-buf speed-type--content-buffer))
	(unwind-protect
	    (with-current-buffer buf
	      (insert "a")
	      (insert "b")
	      (insert "a")
	      (insert "a")
	      (funcall (keymap-lookup nil "DEL") 1)
	      (funcall (keymap-lookup nil "DEL") 1)
	      (insert "c")
	      (insert "!")
	      (insert "!")
	      ;	(should (= speed-type--start-time 1753299414.2124302))
	      (should (string= speed-type--orig-text content))
	      (should (eq speed-type--buffer (current-buffer)))
              ; (should (eq speed-type--content-buffer (get-buffer "*speed-type-content-buffer*")))
	      (should (= speed-type--entries 5))
	      (should (= speed-type--errors 4))
	      (should (= speed-type--non-consecutive-errors 2))
	      (should (= speed-type--remaining 0))
	      (should (string= speed-type--mod-str "\1\1\1\2\2"))
	      (should (= speed-type--corrections 1))
              ; (should (string= speed-type--title (buffer-name)))
	      (should (string= speed-type--author (user-full-name)))
	      (should (eq speed-type--lang nil))
	      (should (eq speed-type--n-words nil))
	      (should (eq speed-type--add-extra-word-content-fn nil))
	      (should (eq speed-type--extra-words-animation-time nil))
	      (should (eq speed-type--extra-word-quote nil))
	      (should (eq speed-type--go-next-fn nil))
	      (should (eq speed-type--replay-fn 'speed-type--get-replay-fn))
	      (should (eq speed-type--extra-word-quote nil))
	      (dotimes (i 3)
		(should (eq (overlay-get (car (overlays-at (1+ i))) 'face) 'speed-type-correct-face)))
	      (should (eq (overlay-get (car (overlays-at 4)) 'face) 'speed-type-error-face))
	      (should (eq (overlay-get (car (overlays-at 5)) 'face) 'speed-type-consecutive-error-face))
	      )
	  (kill-buffer buf)
	  (should (eq (buffer-live-p content-buf) nil)))))))

(ert-deftest speed-type-test/general-file-ref ()
  "Do a general test with `speed-type-region' and different modes, checking content, overlays, point and point-motion, buffer-variables and statistic file."
  (let ((content "abcde")
	(mode (nth 0 '(hexl-mode))))
    (with-temp-buffer
      (insert content)
      (write-file "test.bin")
      (funcall mode)
      (let ((buf (speed-type-region (point-min) (point-max))))
	(unwind-protect
	    (with-current-buffer buf
	      (insert "0")
	      (insert "0")
	      (insert "a")
	      (insert "b")
	      (funcall (keymap-lookup nil "DEL") 1)
	      (funcall (keymap-lookup nil "DEL") 1)
	      (insert "0")
	      (insert "!")
	      (insert "!")
	      ;	(should (= speed-type--start-time 1753299414.2124302))
	      (should (string= speed-type--orig-text "00000000: 6162 6364 65                             abcde"))
	      (should (eq speed-type--buffer (current-buffer)))
              ; (should (eq speed-type--content-buffer (get-buffer "*speed-type-content-buffer*")))
	      (should (= speed-type--entries 5))
	      (should (= speed-type--errors 4))
	      (should (= speed-type--non-consecutive-errors 2))
	      (should (= speed-type--remaining 51))
	      (should (string= speed-type--mod-str "\1\1\1\2\2\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"))
	      (should (= speed-type--corrections 1))
              ; (should (string= speed-type--title (buffer-name)))
	      (should (string= speed-type--author (user-full-name)))
	      (should (eq speed-type--lang nil))
	      (should (eq speed-type--n-words nil))
	      (should (eq speed-type--add-extra-word-content-fn nil))
	      (should (eq speed-type--extra-words-animation-time nil))
	      (should (eq speed-type--extra-word-quote nil))
	      (should (eq speed-type--go-next-fn nil))
	      (should (eq speed-type--replay-fn 'speed-type--get-replay-fn))
	      (should (eq speed-type--extra-word-quote nil))
	      (dotimes (i 3)
		(should (eq (overlay-get (car (overlays-at (1+ i))) 'face) 'speed-type-correct-face)))
	      (should (eq (overlay-get (car (overlays-at 4)) 'face) 'speed-type-error-face))
	      (should (eq (overlay-get (car (overlays-at 5)) 'face) 'speed-type-consecutive-error-face))
	      (let ((offset 5))
		(dotimes (i (- 51 offset))
		  (should (eq (car (overlays-at (+ (1+ offset) i))) nil))))
	      (speed-type-complete))
	  (kill-buffer buf))))))


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
