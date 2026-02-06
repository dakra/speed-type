;;; speed-type-test.el --- Tests for speed-type  -*- lexical-binding: t; -*-

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
;;             (buffer-substring-no-properties start end)
;;             :author (user-full-name)
;;             :title title
;;             :replay-fn #'speed-type--get-replay-fn))))

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
        (speed-type-save-statistic-option-b speed-type-save-statistic-option)
        (speed-type-statistic-filename (concat (temporary-file-directory) "speed-type-statistic.el"))
        (speed-type-randomize-b speed-type-randomize))
    (with-temp-buffer
      (insert content)
      (funcall 'fundamental-mode)
      (setq speed-type-save-statistic-option 'never
            speed-type-randomize t)
      (let ((buf (speed-type-buffer nil)))
        (unwind-protect
            (with-current-buffer buf
              (funcall test-in-buf))
          (setq speed-type-save-statistic-option speed-type-save-statistic-option-b
                speed-type-randomize speed-type-randomize-b)
          (kill-buffer buf))))))

(defun speed-type-test-region (test-in-buf)
  "Setup a speed-type-region for testing.

TEST-IN-BUF is a lambda which is executed within the speed-type-buffer."
  (let ((content "abcde")
        (mode (nth (random 2) '(fundamental-mode emacs-lisp-mode)))
        (speed-type-provide-preview-option t)
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
     (speed-type-test-delayed-insert "a")
     (dotimes (i 3)
       (speed-type--resume)
       (should (length= speed-type--time-register 1)))))
  (speed-type-test-region
   (lambda ()
     (should (length= speed-type--time-register 0))
     (speed-type-test-delayed-insert "a")
     (dotimes (i 3)
       (speed-type-pause)
       (should (length= speed-type--time-register 2))))))

(defun speed-type-test-delayed-insert (str)
  "Insert STR and sleep shortly.

The track-change uses timers which requires us to wait for a little tiny
bit to make sure the tracker has finished."
  (insert str)
  (sleep-for 0.01)
  nil)

(ert-deftest speed-type-test/times-are-pushed-correctly-standard-flow ()
  "Test the time-register-variable in a standard flow: session-start -> first-input -> pause -> resume -> complete. After each state change there should be a new time pushed to the register."
  (speed-type-test-region
   (lambda ()
     (should (length= speed-type--time-register 0))
     (speed-type-test-delayed-insert "a")
     (should (length= speed-type--time-register 1))
     (speed-type-pause)
     (should (length= speed-type--time-register 2))
     (speed-type-test-delayed-insert "b")
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
             (speed-type-test-delayed-insert "b")
             (should (= speed-type--errors 1))
             (should (= speed-type--non-consecutive-errors 1))
             (should (= (point) 1))
             (speed-type-test-delayed-insert "c")
             (should (= speed-type--errors 2))
             (should (= speed-type--non-consecutive-errors 1))
             (should (= (point) 1))
             (speed-type-test-delayed-insert "a")
             (should (= speed-type--errors 2))
             (should (= speed-type--non-consecutive-errors 1))
             (should (= (point) 2))
             (funcall (keymap-lookup nil "DEL") 1)
             (should (= (point) 1))
             (speed-type-test-delayed-insert "a")
             (should (= speed-type--errors 2))
             (should (= speed-type--corrections 1))
             (should (= speed-type--non-consecutive-errors 1))
             (should (= (point) 2)))))
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
             (should (= speed-type--errors 0))
             (should (= speed-type--non-consecutive-errors 0))
             (speed-type-test-delayed-insert "b")
             (should (= speed-type--errors 1))
             (should (= speed-type--non-consecutive-errors 1))
             (should (= (point) 2))
             (speed-type-test-delayed-insert "c")
             (should (= speed-type--errors 2))
             (should (= speed-type--non-consecutive-errors 1))
             (should (= (point) 3))
             (funcall (keymap-lookup nil "DEL") 1)
             (should (= (point) 2))
             (speed-type-test-delayed-insert "b")
             (should (= speed-type--errors 2))
             (should (= speed-type--corrections 1))
             (should (= speed-type--non-consecutive-errors 1))
             (should (= (point) 3)))))
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
           (speed-type-test-delayed-insert "b")
           (should (string= "asdf asdf" (buffer-string)))
           (with-current-buffer speed-type--content-buffer
             (should (string= "ASDF" (buffer-string))))))
      (setq speed-type-downcase b-speed-type-downcase
            speed-type-add-extra-words-on-error b-speed-type-add-extra-words-on-error
            speed-type-add-extra-words-on-non-consecutive-errors b-speed-type-add-extra-words-on-non-consecutive-errors))))

                                        ; assure preview buffer in general region
                                        ; test continue feature
;; complete a typing session and restart the same example
;; test variation with random
                                        ; test top word iterator/calculation
                                        ; test top word file and source file is written
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
              (speed-type-test-delayed-insert "a")
              (speed-type-test-delayed-insert "b")
              (speed-type-test-delayed-insert "a")
              (speed-type-test-delayed-insert "a")
              (funcall (keymap-lookup nil "DEL") 1)
              (funcall (keymap-lookup nil "DEL") 1)
              (speed-type-test-delayed-insert "c")
              (speed-type-test-delayed-insert "!")
              (speed-type-test-delayed-insert "!")
                                        ;       (should (= speed-type--start-time 1753299414.2124302))
              (should (eq speed-type--buffer (current-buffer)))
                                        ; (should (eq speed-type--content-buffer (get-buffer "*speed-type-content-buffer*")))
              (should (= speed-type--entries 5))
              (should (= speed-type--errors 4))
              (should (= speed-type--non-consecutive-errors 2))
              (should (= speed-type--corrections 1))
                                        ; (should (string= speed-type--title (buffer-name)))
              (should (string= speed-type--author (user-full-name)))
              (should (eq speed-type--lang nil))
              (should (eq speed-type--n-words nil))
              (should (eq speed-type--add-extra-word-content-fn nil))
              (should (eq speed-type--extra-words-animation-timer nil))
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
              (speed-type-test-delayed-insert "0")
              (speed-type-test-delayed-insert "0")
              (speed-type-test-delayed-insert "a")
              (speed-type-test-delayed-insert "b")
              (funcall (keymap-lookup nil "DEL") 1)
              (funcall (keymap-lookup nil "DEL") 1)
              (speed-type-test-delayed-insert "0")
              (speed-type-test-delayed-insert "!")
              (speed-type-test-delayed-insert "!")
              (should (eq speed-type--buffer (current-buffer)))
              (should (= speed-type--entries 5))
              (should (= speed-type--errors 4))
              (should (= speed-type--non-consecutive-errors 2))
              (should (= speed-type--corrections 1))
              (should (string= speed-type--author (user-full-name)))
              (should (eq speed-type--lang nil))
              (should (eq speed-type--n-words nil))
              (should (eq speed-type--add-extra-word-content-fn nil))
              (should (eq speed-type--extra-words-animation-timer nil))
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
  (should (= 15 (speed-type--net-wpm 450 50 5 180)))
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
  ;; in case of code we want to keep the indentation
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

(ert-deftest speed-type-test/pick-continue-error-cases ()
  "Should error if custom vars contain garbage or buffer is empty."
   ;; min or max contain something else than a number
  (should-error (speed-type--pick-continue-text-bounds "0" 0 1 100 0 nil))
  (should-error (speed-type--pick-continue-text-bounds 0 "0" 1 100 0 nil))
  (should-error (speed-type--pick-continue-text-bounds 0 0 "1" 100 0 nil))
  (should-error (speed-type--pick-continue-text-bounds 0 0 1 "100" 0 nil))
  (should-error (speed-type--pick-continue-text-bounds 0 0 1 100 "0" nil))
  (should-error (speed-type--pick-continue-text-bounds -1 0 1 100 0 nil))
  (should-error (speed-type--pick-continue-text-bounds 0 -1 1 100 0 nil))
  (should-error (speed-type--pick-continue-text-bounds 0 0 -1 100 0 nil))
  (should-error (speed-type--pick-continue-text-bounds 0 0 1 -100 0 nil))
  (should-error (speed-type--pick-continue-text-bounds 0 0 1 100 -1 nil)))

(ert-deftest speed-type-test/pick-continue-text-empty-cases ()
  "Cases in which the function should return empty string."
  (with-temp-buffer
    (insert "asdf")
    (should (string-empty-p (save-excursion (apply #'buffer-substring (speed-type--pick-continue-text-bounds 0 0 0 0 0 nil)))))
    (should (string-empty-p (save-excursion (apply #'buffer-substring (speed-type--pick-continue-text-bounds 0 0 1 100 0 nil)))))
    (should (string-empty-p (save-excursion (apply #'buffer-substring (speed-type--pick-continue-text-bounds 1 5 0 0 0 nil))))))
  (with-temp-buffer
    (should (string-empty-p (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 1 100 0 nil))))))

(ert-deftest speed-type-test/normal-case-min-is-considered ()
  (with-temp-buffer
    (insert "The quick brown fox jumps over the lazy dog.")
    (should (string-prefix-p "The quick brown" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 10 20 5 t))))))

(ert-deftest speed-type-test/special-case-very-long-word-max-+-tolerance-is-not-exceeded ()
  (with-temp-buffer
    (insert "Antidisestablishmentarianism is a very long word.")
    (should (string= "Antidisestablishmentarianism" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 5 10 20 t))))))

(ert-deftest speed-type-test/special-case-very-very-long-word-split-to-not-exceed-max-+-tolerance ()
  (with-temp-buffer
    (insert "Supercalifragilisticexpialidocious phenomenon.")
    (should (string= "Supercalifragil" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 5 10 5 t))))))

(ert-deftest speed-type-test/special-case-end-is-reached-before-min-can-be-achieved ()
  (with-temp-buffer
    (insert "Short.")
    (should (string= "Short." (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 20 30 10 t))))))

(ert-deftest speed-type-test/whitespace-case-whitespace-considered ()
  (with-temp-buffer
    (insert "This    text

has     lots
   of     whitespace.")
    (should (string-prefix-p "This    text" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 10 20 5 nil))))))

(ert-deftest speed-type-test/special-case-minimal-length-zero-tolerance ()
  (with-temp-buffer
    (insert "asdf")
    (should (string= "s" (apply #'buffer-substring (speed-type--pick-continue-text-bounds 2 3 1 1 0 t))))))

(ert-deftest speed-type-test/special-case-minimal-length-one-tolerance ()
  (with-temp-buffer
    (insert "asdf")
    (should (string= "as" (apply #'buffer-substring (speed-type--pick-continue-text-bounds 1 3 1 1 1 t))))))


(ert-deftest speed-type-test/special-case-tolerance-0-expect-exact-boundary-behavior ()
  (with-temp-buffer
    (insert "One two three four five six seven eight nine ten.")
    (should (string= "One two three f" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 15 15 0 nil))))))

(ert-deftest speed-type-test/multiple-runs-result-in-different-target-lengths ()
  (with-temp-buffer
    (insert "Lorem ipsum dolor sit amet consectetur adipiscing elit.")
    (let ((min 10)
          (max 40)
          (tolerance 5))
      (dotimes (_ 5)
        (let ((content (save-excursion (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) min max tolerance nil)))))
          (should (string-prefix-p "Lorem ipsum" content))
          (should (> (length content) min))
          (should (<= (length content) (+ max tolerance))))))))

(ert-deftest speed-type-test/special-case-leading-whitespace-at-the-start ()
  (with-temp-buffer
    (insert "      Leading whitespace should not break logic.")
    (should (string-prefix-p "      Leading whitespace" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 10 25 5 t))))))

(ert-deftest speed-type-test/51-words-expect-repeated-call-will-not-include-previous-chars ()
  (with-temp-buffer
    (dotimes (i 50)
      (insert (concat "Word" (number-to-string i) " ")))
    (insert "Word50.")
    (goto-char (point-min))
    ;; 51 words are 347 characters
    ;; min is 50 which leads to a maximum times of 7 (347 / 50= 6.94)
    ;; ensure function moves always forward and never takes characters from the previous call
    (let ((previous-last-char "")
          (current-first-word nil)
          (min 50)
          (max 200)
          (tolerance 10))
      (dotimes (i 7)
        (let ((previous-point (point))
              (content (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point) (point-max) min max tolerance nil))))
          (message "Speed-type-test: Content is %s" content)
          (should (string= (buffer-substring previous-point (point)) content))

          (should (<= (length content) (+ max tolerance)))
          (unless (eobp)
            (should (> (point) (* (1+ i) min)))
            (should (> (length content) min))
            (when (string= previous-last-char (substring content 0 1))
              (message "Previous point was %d. Current point is %d. The same character is %s" previous-point (point) (substring content 0 1)))
            (should (not (string= previous-last-char (substring content 0 1))))
            (setq previous-last-char (substring content (- (length content) 1)))))))))

(ert-deftest speed-type-test/special-case-asdfasdfasdfasdfasdf1-excceds-max-but-tolerance-allows-longer-word ()
  (with-temp-buffer
    (insert "asdfasdfasdfasdfasdf1")
    (should (string= "asdfasdfasdfasdfasdf1" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 20 20 20 t))))))

(ert-deftest speed-type-test/special-case-1-included-even-though-it-exceeds-max-because-word-ends-within-tolerance ()
  (with-temp-buffer
    (insert "asdf asdf asdf asdf asdf1")
    (should (string= "asdf asdf asdf asdf asdf1" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 20 20 20 t))))))

(ert-deftest speed-type-test/special-case-1234bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb--second-last-word-and-must-be-split-because-tolerance-exceeded ()
  (with-temp-buffer
    (insert "asdf asdf 1234bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb asdf")
    (should (string= "asdf asdf 1234bbbbbbbbbbbbbbbbbbbbbbbbbb" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 20 20 20 nil))))))

(ert-deftest speed-type-test/special-case-1234aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-is-the-last-word-split-because-tolerance-exceeded ()
  (with-temp-buffer
    (insert "asdf asdf asdf asdf 1234aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    (should (string= "asdf asdf asdf asdf 1234aaaaaaaaaaaaaaaa" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 20 20 20 nil))))))

(ert-deftest speed-type-test/special-case-1234aaaaaaaaaaaaa-is-a-whole-word-and-following-word-not-split-because-within-tolerance ()
  (with-temp-buffer
    (insert "asdf asdf asdf asdf 1234aaaaaaaaaaaaa asdf")
    (should (string= "asdf asdf asdf asdf 1234aaaaaaaaaaaaa" (apply #'buffer-substring (speed-type--pick-continue-text-bounds (point-min) (point-max) 20 20 20 t))))))

;; TODO For some reason it deletes a random buffer which is not intended
;; (ert-deftest speed-type--retrieve-test ()
;;   (let ((speed-type-directory-b speed-type-directory))
;;     (unwind-protect
;;         (progn (setq speed-type-directory "/tmp/")
;;                (let ((filename "speed-type--retrieve-test-file")
;;                      (filename-expected "/tmp/speed-type--retrieve-test-file.txt")
;;                      (url "https://www.google.com"))
;;                  (speed-type--retrieve-non-existant-file-environment
;;                   filename-expected
;;                   (lambda ()
;;                     (let ((response (speed-type--retrieve filename url)))
;;                       (should (bufferp response))
;;                       (should (string= (buffer-file-name response) filename-expected))
;;                       (should (file-exists-p filename-expected))
;;                       (should (file-readable-p filename-expected)))))
;;                  (speed-type--retrieve-existant-file-environment
;;                   filename-expected
;;                   (lambda ()
;;                     (let ((response (speed-type--retrieve filename url)))
;;                       (should (bufferp response))
;;                       (should (string= (buffer-file-name response) filename-expected))
;;                       (should (file-exists-p filename-expected))
;;                       (should (file-readable-p filename-expected))))))
;;                (let ((filename "speed-type--retrieve-test-file")
;;                      (filename-expected "/tmp/speed-type--retrieve-test-file.txt")
;;                      (url "https://www.google.com/nonexitanresource"))
;;                  (speed-type--retrieve-non-existant-file-environment
;;                   filename-expected
;;                   (lambda () (should-error (speed-type--retrieve filename url))))))
;;       (setq speed-type-directory speed-type-directory-b))))
;;; speed-type-test.el ends here
