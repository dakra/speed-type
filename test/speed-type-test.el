;;; test-speed-type.el --- Tests for speed-type  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Gunther Hagleitner

;; Author: Gunther Hagleitner
;; Maintainer: Daniel Kraus <daniel@kraus.my>
;;      lordnik22
;; Version: 1.4.0
;; Keywords: games
;; URL: https://github.com/dakra/speed-type
;; Package-Requires: ((emacs "26.1") (compat "29.1.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file contains all the tests that ensure the functionality of speed-type

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

(ert-deftest speed-type-test/stop-words-p-supply-garbage ()
  (should-error (speed-type--stop-word-p nil))
  (should-error (speed-type--stop-word-p 1))
  (should-error (speed-type--stop-word-p 'symbol))
  (should-error (speed-type--stop-word-p '("asdf" "asdf")))
  (setq speed-type-stop-words 'symbol)
  (should-error (speed-type--stop-word-p "word"))
  (setq speed-type-stop-words 1)
  (should-error (speed-type--stop-word-p "word")))

(ert-deftest speed-type-test/stop-words-p-is-t-when-found-in-list ()
  (setq speed-type-stop-words '("word"))
  (should (string= (speed-type--stop-word-p "word") "word")))

(ert-deftest speed-type-test/stop-words-p-is-t-when-found-in-file ()
  (setq speed-type-stop-words (concat (temporary-file-directory) "/stop-words.txt"))
  (let ((buffer (find-file-noselect speed-type-stop-words)))
    (unwind-protect
	(progn (with-current-buffer buffer
		 (insert "word\n")
		 (insert "otherWord\n")
		 (save-buffer))
	       (should (string= (speed-type--stop-word-p "word") "word")))
      (delete-file (buffer-file-name buffer))
      (kill-buffer buffer))))

(ert-deftest speed-type-test/find-last-continue-at-point-in-stats-supply-garbage ()
  (should-error (speed-type--find-last-continue-at-point-in-stats 'symbol))
  (should-error (speed-type--find-last-continue-at-point-in-stats 1))
  (should-error (speed-type--find-last-continue-at-point-in-stats '("aqsf" "asdf"))))

(ert-deftest speed-type-test/find-last-continue-at-point-in-stats ()
  (let ((speed-type-statistic-filename (concat (temporary-file-directory) "speed-type-statistic.el")))
    (delete-file speed-type-statistic-filename)
    (should (eq (speed-type--find-last-continue-at-point-in-stats nil) nil))
    (should (null (speed-type--find-last-continue-at-point-in-stats "/tmp/101.txt")))
    (speed-type-save-stats
     speed-type-statistic-filename
     '((speed-type--continue-at-point . 2)
       (speed-type--file-name . "/tmp/101.txt")))
    (should (= (speed-type--find-last-continue-at-point-in-stats "/tmp/101.txt") 2))
    (speed-type-save-stats
     speed-type-statistic-filename
     '((speed-type--continue-at-point . 12)
       (speed-type--file-name . "/tmp/101.txt")))
    (speed-type-save-stats
     speed-type-statistic-filename
     '((speed-type--continue-at-point . 9)
       (speed-type--file-name . "/tmp/404.txt")))
    (should (= (speed-type--find-last-continue-at-point-in-stats "/tmp/101.txt") 12))
    (speed-type-save-stats
     speed-type-statistic-filename
     '((speed-type--continue-at-point . 22)
       (speed-type--file-name . "/tmp/101.txt")))
    (should (= (speed-type--find-last-continue-at-point-in-stats "/tmp/101.txt") 22))
    (should (= (speed-type--find-last-continue-at-point-in-stats "/tmp/404.txt") 9))
    (should (null (speed-type--find-last-continue-at-point-in-stats "/tmp/909.txt")))))

(ert-deftest speed-type-test/calc-median-supply-garbage ()
  (should-error (speed-type--calc-median "" '()))
  (should-error (speed-type--calc-median 1 '()))
  (should-error (speed-type--calc-median '() '()))
  (should-error (speed-type--calc-median 'a ""))
  (should-error (speed-type--calc-median 'a 'a))
  (should-error (speed-type--calc-median "" "")))

(ert-deftest speed-type-test/feeling-better? ()
  "Checks if it's a good day to program."
  (should (= 1 1)))

(defun speed-type-test-buffer (text test-in-buf)
  "Setup a speed-type-region for testing.

TEST-IN-BUF is a lambda which is executed within the speed-type-buffer."
  (let ((content text)
	(b-speed-type-save-buffer-option speed-type-save-buffer-option)
	(speed-type-statistic-filename (concat (temporary-file-directory) "speed-type-statistic.el")))
    (with-temp-buffer
      (insert content)
      (funcall 'fundamental-mode)
      (setq speed-type-save-buffer-option 'never)
      (let ((buf (speed-type-buffer nil)))
	(unwind-protect
	    (with-current-buffer buf
	      (funcall test-in-buf))
	  (setq speed-type-save-buffer-option b-speed-type-save-buffer-option)
	  (kill-buffer buf))))))

(defun speed-type-test-region (test-in-buf)
  "Setup a speed-type-region for testing.

TEST-IN-BUF is a lambda which is executed within the speed-type-buffer."
  (let ((content "abcde")
	(mode (nth (random 2) '(fundamental-mode emacs-lisp-mode)))
	(speed-type-statistic-filename (concat (temporary-file-directory) "speed-type-statistic.el")))
    (with-temp-buffer
      (insert content)
      (funcall mode)
      (let ((buf (speed-type-region (point-min) (point-max))))
	(unwind-protect
	    (with-current-buffer buf
	      (funcall test-in-buf))
	  (kill-buffer buf))))))

(ert-deftest speed-type-test/times-is-empty-when-no-input ()
  "Test the time-register-variable is empty for flow: session-start -> complete."
  (speed-type-test-region
   (lambda ()
     (should (length= speed-type--time-register 0))
     (speed-type-complete)
     (should (length= speed-type--time-register 0)))))

(ert-deftest speed-type-test/times-stay-length-if-duplicate-call ()
  "Test the time-register-variable is empty for flow: session-start -> complete."
  (speed-type-test-region
   (lambda ()
     (should (length= speed-type--time-register 0))
     (insert "a")
     (dotimes (i 3)
       (speed-type--resume)
       (should (length= speed-type--time-register 1)))))
  (speed-type-test-region
   (lambda ()
     (should (length= speed-type--time-register 0))
     (insert "a")
     (dotimes (i 3)
       (speed-type-pause)
       (should (length= speed-type--time-register 2))))))

(ert-deftest speed-type-test/times-are-pushed-correctly-standard-flow ()
  "Test the time-register-variable in a standard flow: session-start -> first-input -> pause -> resume -> complete. After each state change there should be a new time pushed to the register."
  (speed-type-test-region
   (lambda ()
     (should (length= speed-type--time-register 0))
     (insert "a")
     (should (length= speed-type--time-register 1))
     (speed-type-pause)
     (should (length= speed-type--time-register 2))
     (insert "b")
     (should (length= speed-type--time-register 3))
     (speed-type-complete)
     (should (length= speed-type--time-register 4)))))

(ert-deftest speed-type-test/calculate-elasped-time ()
  "Test if calculation of duration of the typing-session."
  (should (= (speed-type--elapsed-time '()) 0))
  (should (eq (speed-type--elapsed-time '(1)) 'uneven))
  (should (= (speed-type--elapsed-time '(1 2)) 1))
  (should (= (speed-type--elapsed-time '(2 1)) -1))
  (should (eq (speed-type--elapsed-time '(1 2 3)) 'uneven))
  (should (= (speed-type--elapsed-time '(1 2 6 7)) 2))
  (should (=  (speed-type--elapsed-time '(1 2 7 6 8 9)) 1)))

(ert-deftest speed-type-test/point-motion-stay ()
  "Test if points stays and error are counted correctly."
  (let ((b-speed-type-point-motion-on-error speed-type-point-motion-on-error))
    (unwind-protect
	(progn
	  (setq speed-type-point-motion-on-error 'point-stay)
	  (speed-type-test-region
	   (lambda ()
	     (should (= (point) 1))
	     (should (= speed-type--errors 0))
	     (should (= speed-type--non-consecutive-errors 0))
	     (insert "b")
	     (should (= speed-type--errors 1))
	     (should (= speed-type--non-consecutive-errors 1))
	     (should (= (point) 1))
	     (should (= speed-type--remaining (1+ (- (length (buffer-string)) (point)))))
	     (insert "c")
	     (should (= speed-type--errors 2))
	     (should (= speed-type--non-consecutive-errors 1))
	     (should (= (point) 1))
	     (should (= speed-type--remaining (1+ (- (length (buffer-string)) (point)))))
	     (insert "a")
	     (should (= speed-type--errors 2))
	     (should (= speed-type--non-consecutive-errors 1))
	     (should (= (point) 2))
	     (should (= speed-type--remaining (1+ (- (length (buffer-string)) (point)))))
	     (funcall (keymap-lookup nil "DEL") 1)
	     (should (= (point) 1))
	     (should (= speed-type--remaining (1+ (- (length (buffer-string)) (point)))))
	     (insert "a")
	     (should (= speed-type--errors 2))
	     (should (= speed-type--corrections 1))
	     (should (= speed-type--non-consecutive-errors 1))
	     (should (= (point) 2))
	     (should (= speed-type--remaining (1+ (- (length (buffer-string)) (point))))))))
      (setq speed-type-point-motion-on-error b-speed-type-point-motion-on-error))))

(ert-deftest speed-type-test/point-motion-move ()
  "Test if points move and error are counted correctly."
  (let ((b-speed-type-point-motion-on-error speed-type-point-motion-on-error))
    (unwind-protect
	(progn
	  (setq speed-type-point-motion-on-error 'point-move)
	  (speed-type-test-region
	   (lambda ()
	     (should (= (point) 1))
	     (should (= speed-type--remaining (length (buffer-string))))
	     (should (= speed-type--errors 0))
	     (should (= speed-type--non-consecutive-errors 0))
	     (insert "b")
	     (should (= speed-type--errors 1))
	     (should (= speed-type--non-consecutive-errors 1))
	     (should (= (point) 2))
	     (should (= speed-type--remaining (1+ (- (length (buffer-string)) (point)))))
	     (insert "c")
	     (should (= speed-type--errors 2))
	     (should (= speed-type--non-consecutive-errors 1))
	     (should (= (point) 3))
	     (should (= speed-type--remaining (1+ (- (length (buffer-string)) (point)))))
	     (funcall (keymap-lookup nil "DEL") 1)
	     (should (= (point) 2))
	     (should (= speed-type--remaining (1+ (- (length (buffer-string)) (point)))))
	     (insert "b")
	     (should (= speed-type--errors 2))
	     (should (= speed-type--corrections 1))
	     (should (= speed-type--non-consecutive-errors 1))
	     (should (= (point) 3))
	     (should (= speed-type--remaining (1+ (- (length (buffer-string)) (point))))))))
      (setq speed-type-point-motion-on-error b-speed-type-point-motion-on-error))))


(ert-deftest speed-type-test/test-chars-downcased ()
  "Test if text is downcased when speed-type-downcase is t.

Also assure when that added words are downcased too."
  (let ((b-speed-type-downcase speed-type-downcase)
	(b-speed-type-add-extra-words-on-error speed-type-add-extra-words-on-error)
	(b-speed-type-add-extra-words-on-non-consecutive-errors speed-type-add-extra-words-on-non-consecutive-errors))
    (setq speed-type-downcase t
	  speed-type-add-extra-words-on-error 1
	  speed-type-add-extra-words-on-non-consecutive-errors 0)
    (unwind-protect
	(speed-type-test-buffer
	 "ASDF"
	 (lambda ()
	   (should (string= "asdf" (buffer-string)))
	   (insert "b")
	   (sleep-for 1)
	   (should (string= "asdf asdf" (buffer-string)))
	   (with-current-buffer speed-type--content-buffer
	     (should (string= "ASDF" (buffer-string))))))
      (setq speed-type-downcase b-speed-type-downcase
	    speed-type-add-extra-words-on-error b-speed-type-add-extra-words-on-error
	    speed-type-add-extra-words-on-non-consecutive-errors b-speed-type-add-extra-words-on-non-consecutive-errors))))

;; assure preview buffer in general region
;; test continue feature
;;; user calls speed-type-continue on a *scratch* buffer
;;; user calls speed-type-continue on a book
;;; user types the whole buffer, he is prompted if wants to restart
;;; user completes with cursor at (point-max)
;;; user completes with cursor at (point-min)
;;; user completes with cursor at mid
;;; test with 3 consecutive continues and same length
;;; test with 3 continue and one random text sample on the same file
;; test top word iterator/calculation
;; test top word file and source file is written

(ert-deftest speed-type-test/general-region ()
  "Do a general test with `speed-type-region' with fundamental mode and a prog-mode, checking content, overlays, point and point-motion, buffer-variables and statistic file."
  (let ((content "abcde")
	(mode (nth (random 2) '(fundamental-mode emacs-lisp-mode)))
	(speed-type-statistic-filename (concat (temporary-file-directory) "speed-type-statistic.el")))
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


(ert-deftest speed-type-test/general-region ()
  "Do a general test with `speed-type-region' with fundamental mode and a prog-mode, checking content, overlays, point and point-motion, buffer-variables and statistic file."
  (let ((content "abcde")
	(mode (nth (random 2) '(fundamental-mode emacs-lisp-mode)))
	(speed-type-statistic-filename (concat (temporary-file-directory) "speed-type-statistic.el")))
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
	(mode (nth 0 '(hexl-mode)))
	(speed-type-statistic-filename (concat (temporary-file-directory) "speed-type-statistic.el")))
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
					;(should (string= speed-type--orig-text "00000000: 6162 6364 65                             abcde"))
	      (should (eq speed-type--buffer (current-buffer)))
					; (should (eq speed-type--content-buffer (get-buffer "*speed-type-content-buffer*")))
	      (should (= speed-type--entries 5))
	      (should (= speed-type--errors 4))
	      (should (= speed-type--non-consecutive-errors 2))
	      (should (= speed-type--remaining 19))
					;(should (string= speed-type--mod-str "\1\1\1\2\2\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"))
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
  (let ((speed-type-directory (temporary-file-directory)))
    (unwind-protect
        (progn
	  (kill-buffer (get-file-buffer filename-expected))
          (delete-file filename-expected)
          (funcall test))
      (kill-buffer (get-file-buffer filename-expected))
      (delete-file filename-expected))))

(defun speed-type--retrieve-existant-file-environment (filename-expected test)
  (let ((speed-type-directory (temporary-file-directory)))
    (unwind-protect
        (progn
	  (kill-buffer (get-file-buffer filename-expected))
          (delete-file filename-expected)
          (with-temp-buffer
            (write-file filename-expected))
          (funcall test))
      (kill-buffer (get-file-buffer filename-expected))
      (delete-file filename-expected))))

;; (ert-deftest speed-type--retrieve-test ()
;;   (let ((filename "speed-type--retrieve-test-file")
;;         (filename-expected "/tmp/speed-type--retrieve-test-file.txt")
;;         (url "https://www.google.com"))

;;     (speed-type--retrieve-non-existant-file-environment
;;      filename-expected
;;      (lambda ()
;;        (let ((filename-response (speed-type--retrieve filename url)))
;;          (should (bufferp filename-response))
;;          (should (string= (buffer-file-name filename-response) filename-expected))
;;          (should (file-exists-p filename-expected))
;;          (should (file-readable-p filename-expected)))))

;;     (speed-type--retrieve-existant-file-environment
;;      filename-expected
;;      (lambda ()
;;        (let ((filename-response (speed-type--retrieve filename url)))
;;          (should (bufferp filename-response))
;;          (should (string= (buffer-file-name filename-response) filename-expected))
;;          (should (file-exists-p filename-expected))
;;          (should (file-readable-p filename-expected))))))

;;   (let ((filename "speed-type--retrieve-test-file")
;;         (filename-expected "/tmp/speed-type--retrieve-test-file.txt")
;;         (url "https://www.google.com/nonexitanresource"))

;;     (speed-type--retrieve-non-existant-file-environment
;;      filename-expected
;;      (lambda ()
;;        (let ((filename-response (speed-type--retrieve filename url)))
;;          (should (null filename-response))
;;          (should (not (file-exists-p filename-expected)))
;;          (should (not (file-readable-p filename-expected))))))))
;;; test-speed-type.el ends here
