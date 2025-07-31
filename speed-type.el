;;; speed-type.el --- Practice touch and speed typing -*- lexical-binding: t -*-

;; Copyright (C) 2015 Gunther Hagleitner

;; Author: Gunther Hagleitner
;; Maintainer: Daniel Kraus <daniel@kraus.my>, lordnik22
;; Version: 1.4
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
;; Speed-type allows you to practice your touch typing skills.  You can
;; test yourself by typing snippets from online books or use any piece
;; of text or code you have in Emacs.  Speed-type keeps track of your
;; stats (WPM, CPM, accuracy) while you are typing.

;;; Code:
(require 'cl-lib)
(require 'cl-seq)
(require 'compat)
(require 'url)
(require 'url-handlers)
(require 'url-http)
(require 'thingatpt)
(require 'dom)

(defgroup speed-type nil
  "Practice touch-typing in Emacs."
  :group 'games)

(defcustom speed-type-buffer-name "*speed-type*"
  "Name of buffer in which the user completes his typing session."
  :type 'string)

(defcustom speed-type-content-buffer-name "*speed-type-content-buffer*"
  "Name of buffer consisting of the content-source for the speed-type buffer."
  :type 'string)

(defcustom speed-type-min-chars 200
  "The minimum number of chars to type required when the text is picked randomly."
  :type 'integer)

(defcustom speed-type-max-chars 450
  "The maximum number of chars to type required when the text is picked randomly."
  :type 'integer)

(defcustom speed-type-gb-book-list
  '(1342 11 1952 1661 74 1232 23 135 5200 2591 844 84 98 2701 1400 16328 174
         46 4300 345 1080 2500 829 1260 6130 1184 768 32032 521 1399 55)
  "List of book numbers to use from the gutenberg web site.

Book numbers can be picked from https://www.gutenberg.org, when looking at
a book url.  E.G, https://www.gutenberg.org/ebooks/14577."
  :type '(repeat integer))

(defcustom speed-type-gb-dir (locate-user-emacs-file "speed-type")
  "Directory in which the gutenberg books will be saved."
  :type 'directory)

(defcustom speed-type-wordlist-urls
  '((English . "http://web.archive.org/web/20170227200416/http://wortschatz.uni-leipzig.de/Papers/top10000en.txt")
    (German . "http://web.archive.org/web/20170227200416/http://wortschatz.uni-leipzig.de/Papers/top10000de.txt")
    (French . "http://web.archive.org/web/20170227200416/http://wortschatz.uni-leipzig.de/Papers/top10000fr.txt")
    (Dutch . "http://web.archive.org/web/20170227200416/http://wortschatz.uni-leipzig.de/Papers/top10000nl.txt"))
  "Alist of language name as key and a URL where to download a wordlist for it."
  :type '(alist :key-type symbol :value-type string))

(defcustom speed-type-quote-urls
  '((johnVonNeumann . "https://www.azquotes.com/author/10753-John_von_Neumann")
    (happiness . "https://www.azquotes.com/quotes/topics/happiness.html")
    (alanTuring . "https://www.azquotes.com/author/14856-Alan_Turing"))
  "list of name as key and an URL to azquotes which will be downloaded and parsed."
  :type '(alist :key-type symbol :value-type string))

(defcustom speed-type-wordlist-transform nil
  "Function to transform wordlist before starting the exercise.
The function should take the `buffer-string' as argument and return
the transformed string that is used for the speed type exercise.

E.g. if you always want lowercase words, set:
`speed-type-wordlist-transform' to `downcase'."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Transform function"))
  :group 'speed-type)

(defcustom speed-type-default-lang nil
  "Default language for training wordlists.  Ask when NIL."
  :type '(choice (const :tag "None" nil)
                 (const :tag "English" English)
                 (const :tag "German" German)
                 (const :tag "French" French)
                 (const :tag "Dutch" Dutch))
  :group 'speed-type)

(defcustom speed-type-replace-strings '(("“" . "\"") ("”" . "\"") ("‘" . "'") ("’" . "'"))
  "Alist of strings to replace and their replacement, in the form:
`(bad-string . good-string)'
To remove without replacement, use the form: `(bad-string . \"\")'"
  :type '(alist :key-type string :value-type string)
  :group 'speed-type)

(defcustom speed-type-point-motion-on-error 'point-move
  "Define the behavior of point when mistyping a character.

when point-move (default), moves the point one character further.

when point-stay, stays at the current position until correct character is typed."
  :type 'symbol
  :group 'speed-type)

(defcustom speed-type-add-extra-words-on-error 0
  "How many new words should be added on error.
When 0 or less, no words are added. The typing-session will only
be complete when these extra words are typed too. Recommanded is
something between 1 and 7.

Similar to `speed-type-add-extra-words-on-non-consecutive-errors'
they accumulate each other if both variables are set."
  :type 'integer)

(defcustom speed-type-add-extra-words-on-non-consecutive-errors 0
  "How many new words should be added on a non-consecutive error.
A non-consecutive error is a mistyped character where the previous
one was correctly typed.
When 0 or less, no words are added.  The typing-session will only
be complete when these extra words are typed too.  Recommended is
something between 1 and 7.

Similar to `speed-type-add-extra-words-on-error',
they accumulate each other if both variables are set."
  :type 'integer)

(defcustom speed-type-save-statistic-option 'always
  "Save the stats for the play or not."
  :type '(choice (const :tag "Always" always)
		         (const :tag "Never" never)
		         (const :tag "Ask" ask)))

(defcustom speed-type-statistic-filename (concat speed-type-gb-dir "/" "speed-type-statistic.el")
  "Name of file for general stats."
  :type 'string)

(defcustom speed-type-max-num-records 10000
  "Maximum number of saved records."
  :type '(natnum :tag "None negative number." ))

(defcustom speed-type-code-modes '(prog-mode yaml-mode xml-mode html-mode)
  "Define which modes should be handled as code.

These modes will have syntax highlighting and NO `fill-region' will be called."
  :type '(repeat symbol)
  :group 'speed-type)

(defface speed-type-default
  '()
  "Default face for `speed-type'.")

(defface speed-type-correct-face
  '((t :inherit success :weight normal))
  "Face for correctly typed characters.")

(defface speed-type-consecutive-error-face
  '((t :inherit error :underline t))
  "Face for incorrectly typed characters where the previous is already an error.")

(defface speed-type-error-face
  '((t :inherit error :underline t))
  "Face for incorrectly typed characters.")

;; internal variables

(defvar speed-type--gb-url-format
  "https://www.gutenberg.org/cache/epub/%d/pg%d.txt")

(defvar speed-type-explaining-message "
Gross wpm/cpm ignore uncorrected errors and indicate raw speed.
Net wpm/cpm take uncorrected errors into account and are a measure
of effective or net speed.")

(defvar speed-type-stats-format "\n
Skill:                        %s
Net WPM:                      %d
Net CPM:                      %d
Gross WPM:                    %d
Gross CPM:                    %d
Accuracy:                     %.2f%%
Total time:                   %s
Total chars:                  %d
Corrections:                  %d
Total errors:                 %d
Total non-consecutive errors: %d
%s")

(defvar speed-type-previous-saved-stats-format  "\n
Num of records:                %d
Note: 'nil' values are excluded from the median calculations.
Median Skill:                  %s
Median Net WPM:                %d
Median Net CPM:                %d
Median Gross WPM:              %d
Median Gross CPM:              %d
Median Accuracy:               %.2f%%
Median Total time:             %d
Median Total chars:            %d
Median Corrections:            %d
Median Total errors:           %d
Median Non-consecutive errors: %d
Median Remaining:              %d")

(defvar speed-type--completed-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'speed-type--quit)
    (define-key map (kbd "d") 'speed-type--display-statistic)
    (define-key map (kbd "r") 'speed-type--replay)
    (define-key map (kbd "n") 'speed-type--play-next)
    map))

(defvar speed-type-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-k")  #'speed-type-complete)
    (define-key keymap (kbd "M-q") #'speed-type-fill-paragraph)
    keymap)
  "Keymap for `speed-type-mode'.")

(define-derived-mode speed-type-mode fundamental-mode "SpeedType"
  "Major mode for practicing touch typing."
  :group "speed-type")

;; buffer local internal variables

(defvar-local speed-type--start-time nil)
(defvar-local speed-type--orig-text nil)
(defvar-local speed-type--buffer nil)
(defvar-local speed-type--content-buffer nil)
(defvar-local speed-type--entries 0)
(defvar-local speed-type--errors 0)
(defvar-local speed-type--non-consecutive-errors 0)
(defvar-local speed-type--remaining 0)
(defvar-local speed-type--mod-str nil)
(defvar-local speed-type--corrections 0)
(defvar-local speed-type--title nil)
(defvar-local speed-type--author nil)
(defvar-local speed-type--lang nil)
(defvar-local speed-type--n-words nil)
(defvar-local speed-type--add-extra-word-content-fn nil)
(defvar-local speed-type--extra-words-animation-time nil)
(defvar-local speed-type--extra-words-queue '())
(defvar-local speed-type--go-next-fn nil)
(defvar-local speed-type--replay-fn #'speed-type--setup)
(defvar-local speed-type--extra-word-quote nil)


(defun speed-type--/ (number divisor)
  "Divide NUMBER by DIVISOR when DIVISOR is not null.
Otherwise return 0."
  (if (zerop divisor)
      0
    (/ number divisor)))

(defun speed-type--seconds-to-minutes (seconds)
  "Return minutes in float for SECONDS."
  (/ seconds 60.0))

(defun speed-type--gross-wpm (entries seconds)
  "Return gross words-per-minute.

Computes words-per-minute as (ENTRIES/5) / (SECONDS/60)."
  (round (speed-type--/ (/ entries 5.0)
              (speed-type--seconds-to-minutes seconds))))

(defun speed-type--gross-cpm (entries seconds)
  "Return gross characters-per-minute.

Computes characters-per-minute as ENTRIES / (SECONDS/60)."
  (round (speed-type--/ entries (speed-type--seconds-to-minutes seconds))))

(defun speed-type--net-wpm (entries uncorrected-errors seconds)
  "Return net words-per-minute.

Computes net words-per-minute as:
  ((ENTRIES/5) - UNCORRECTED-ERRORS) / (SECONDS/60)."
  (let ((net-wpm (round (- (speed-type--gross-wpm entries seconds)
                           (speed-type--/ uncorrected-errors
                                (speed-type--seconds-to-minutes seconds))))))
    (if (> 0 net-wpm) 0 net-wpm)))

(defun speed-type--net-cpm (entries uncorrected-errors seconds)
  "Return net characters-per-minute.

Computes net characters-per-minute as:
  (ENTRIES - UNCORRECTED-ERRORS) / (SECONDS/60)."
  (let ((net-cpm (round (- (speed-type--gross-cpm entries seconds)
                           (speed-type--/ uncorrected-errors
                                (speed-type--seconds-to-minutes seconds))))))
    (if (> 0 net-cpm) 0 net-cpm)))

(defun speed-type--accuracy (total-entries correct-entries corrections)
  "Return accuracy.

Accuracy is computed as (CORRECT-ENTRIES - CORRECTIONS) / TOTAL-ENTRIES."
  (let* ((correct-entries (- correct-entries corrections))
         (correct-entries (if (> correct-entries 0) correct-entries 0)))
    (* (round (* (speed-type--/ correct-entries (float total-entries)) 100.0) 0.01) 0.01)))

(defun speed-type--skill (wpm)
  "Return skill for WPM."
  (cond
   ((null wpm) "Zero or Infinity")
   ((< wpm 25) "Beginner")
   ((< wpm 30) "Intermediate")
   ((< wpm 40) "Average")
   ((< wpm 55) "Pro")
   ((< wpm 80) "Master")
   (t          "Racer")))

(defvar speed-type-coding-system 'utf-8-unix
  "The coding system speed-type uses for saving the stats.
Changing this value while Emacs is running is supported, but considered
unwise, unless you know what you are doing.")

(defconst speed-type-file-format-version 1
  "The current version of the format used by speed-type statistic files.
You should never need to change this.")

(defun speed-type-statistic-variables ()
  "Define the structure of raw-data used for calculating the median-stats.

If the structure is changed, SPEED-TYPE-FILE-FORMAT-VERSION must
be incremented and a migration must be coded in
SPEED-TYPE-MAYBE-UPGRADE-FILE-FORMAT."
  (let ((entries speed-type--entries)
	(errors speed-type--errors)
	(corrections speed-type--corrections)
	(remaining speed-type--remaining)
	(seconds (speed-type--elapsed-time)))
    (list (cons 'speed-type--title speed-type--title)
	  (cons 'speed-type--remaining remaining)
	  (cons 'speed-type--author speed-type--author)
	  (cons 'speed-type--lang speed-type--lang)
	  (cons 'speed-type--n-words speed-type--n-words)
	  (cons 'speed-type--entries entries)
	  (cons 'speed-type--errors errors)
	  (cons 'speed-type--non-consecutive-errors speed-type--non-consecutive-errors)
	  (cons 'speed-type--corrections corrections)
	  (cons 'speed-type--elapsed-time seconds)
	  (cons 'speed-type--gross-wpm (speed-type--gross-wpm entries seconds))
	  (cons 'speed-type--gross-cpm (speed-type--gross-cpm entries seconds))
	  (cons 'speed-type--net-wpm (speed-type--net-wpm entries errors seconds))
	  (cons 'speed-type--net-cpm (speed-type--net-cpm entries errors seconds))
	  (cons 'speed-type--accuracy (speed-type--accuracy entries (- entries errors) corrections)))))

(defun speed-type-grok-file-format-version ()
  "Integer which indicates the file-format version of speed-type statistic file.
Expects to be called from `point-min' in a speed-type statistic file."
  (if (looking-at "^;;;;")
      (save-excursion
        (save-match-data
          (re-search-forward "[0-9]")
          (forward-char -1)
          (read (current-buffer))))
    ;; Else this is format version 0, the original one, which didn't
    ;; even have version stamps.
    0))

(defun speed-type--maybe-insert-newline ()
  "Move last closing parantheses to own line if not already.

We use ‘pp’ to write the buffer state to the statistic-file.
Since ‘pp’ works a bit differently on EMACS 30.1 for some
reason. It is necessary to check if the last closing parantheses
has it’s own line.

This function fixes this, because otherwise we fail to load the file."
  (save-excursion
    (when (null (progn (goto-char (point-max))
		       (re-search-backward "^)" nil t 1)))
      (re-search-backward ")" nil t 1)
      (insert "\n"))))

(defun speed-type-maybe-upgrade-file-format ()
  "Check the file-format version of current file.
If the version is not up-to-date, upgrade it automatically.
This expects to be called from `point-min' in a speed-type statistic file."
  (let ((version
         (with-suppressed-warnings ((obsolete speed-type-grok-file-format-version)) ;; we use the same mechanism as bookmark file
           (speed-type-grok-file-format-version))))
    (cond
     ((= version speed-type-file-format-version))
     ((= version 0) (speed-type--maybe-insert-newline))
     (t (error "Speed-type statistic file format version strangeness")))))

(defconst speed-type-end-of-version-stamp-marker
  "-*- End Of Speed Type File Format Version Stamp -*-\n"
  "This string marks the end of the version stamp in a speed-type statistic file.")

(defun speed-type-insert-file-format-version-stamp (coding)
  "Insert text indicating current version of speed-type statistic file format.
CODING is the symbol of the coding-system in which the file is encoded."
  (if (memq (coding-system-base coding) '(undecided prefer-utf-8))
      (setq coding 'utf-8-emacs))
  (insert
   (format
    ";;;; Emacs Speed-type statisitic Format Version %d\
;;;; -*- coding: %S; mode: lisp-data -*-\n"
    speed-type-file-format-version (coding-system-base coding)))
  (insert ";;; This format is meant to be slightly human-readable;\n"
          ";;; nevertheless, you probably don't want to edit it.\n"
          ";;; "
          speed-type-end-of-version-stamp-marker))

(defun speed-type-save-stats-when-customized ()
  "Check the custom variable SPEED-TYPE-SAVE-STATISTIC-OPTION and save stats."
  (when (not (eq speed-type-save-statistic-option 'never))
    (when (if (eq speed-type-save-statistic-option 'ask) (y-or-n-p "Save statistic?") t)
      (speed-type-save-stats speed-type-statistic-filename))))

(defun speed-type-save-stats (file &optional alt-msg)
  "Write stats of current speed-type session to FILE.

Non-nil ALT-MSG is a message format string to use in place of the
default, \"Saving statistics of current speed-type session to
file `%s'...\". The string must contain a `%s' construct, so that
it can be passed along with FILE to `format'. At the end,
\"done\" is appended to the message."
  (let ((msg                      (or alt-msg  "Saving statistics of current speed-type session to file `%s'..."))
        (coding-system-for-write  speed-type-coding-system)
        (print-length             nil)
        (print-level              nil)
        (existing-buf             (get-file-buffer file))
        (emacs-lisp-mode-hook     nil) ; Avoid inserting automatic file header if existing empty file, so
        (lisp-mode-hook           nil) ; better chance `speed-type-maybe-upgrade-file-format' signals error.
	    (speed-type-buffer (current-buffer))
        start end)
    (when (file-directory-p file) (error "`%s' is a directory, not a file" file))
    (message msg (abbreviate-file-name file))
    (with-current-buffer (let ((enable-local-variables ())) (find-file-noselect file))
      (goto-char (point-min))
      (if (file-exists-p file)
          (speed-type-maybe-upgrade-file-format)
	    (delete-region (point-min) (point-max)) ; In case a find-file hook inserted a header, etc.
	    (unless (boundp 'speed-type-coding-system)	; Emacs < 25.2.
	      (speed-type-insert-file-format-version-stamp coding-system-for-write))
        (insert "(\n)"))
      (setq start (and (file-exists-p file)
                       (or (save-excursion (goto-char (point-min))
                                           (search-forward (concat speed-type-end-of-version-stamp-marker "(")
                                                           nil t))
                           (error "Invalid %s" file)))
            end    (and start
                        (or (save-excursion (goto-char start) (and (looking-at ")") start))
                            (save-excursion (goto-char (point-max)) (re-search-backward "^)" nil t))
                            (error "Invalid %s" file))))
      (if (not start)			; New file, no header yet.
          (goto-char 2)
        ;;  Existing file - delete old entry unless max is not reached. Rolling.
        (when (> (/ (count-lines start end) (length (or (speed-type-statistic-variables) '(1)))) speed-type-max-num-records)
	      (save-excursion
	        (goto-char start)
	        (or (looking-at "(") (search-forward "(" nil t 1))
	        (let ((bounds (bounds-of-thing-at-point 'sexp)))
	          (kill-region (car bounds) (+ 1 (cdr bounds))))))
        (goto-char (and start
                        (or (save-excursion (goto-char start) (and (looking-at ")") start))
                            (save-excursion (goto-char (point-max)) (re-search-backward "^)" nil t))
                            (error "Invalid %s" file)))))
      (pp (with-current-buffer speed-type-buffer (speed-type-statistic-variables)) (current-buffer))
      (speed-type--maybe-insert-newline)
      (when (boundp 'speed-type-coding-system) ; Emacs 25.2+.  See bug #25365
        ;; Make sure specified encoding can encode the speed-type stats.  If not, suggest utf-8-emacs as default.
        (with-coding-priority '(utf-8-emacs)
          (setq coding-system-for-write (select-safe-coding-system (point-min) (point-max)
                                                                   (list t coding-system-for-write))))
        (when start (delete-region 1 (1- start))) ; Delete old header.
        (goto-char 1)
        (speed-type-insert-file-format-version-stamp coding-system-for-write))
      (let ((require-final-newline t)
            (errorp nil))
        (condition-case nil
            (write-file file)
          (file-error (setq errorp  t)
                      ;; Do NOT raise error.  (Need to be able to exit.)
                      (let ((msg  (format "CANNOT WRITE FILE `%s'" file)))
                        (if (fboundp 'display-warning)
                            (display-warning 'speed-type msg)
                          (message msg)
                          (sit-for 4)))))
        (when (boundp 'speed-type-coding-system) ; Emacs 25.2+
          (setq speed-type-coding-system  coding-system-for-write))
        (unless existing-buf (kill-buffer (current-buffer)))
        (unless errorp (message (concat msg "done") file))))))

(defun speed-type-stats-list-from-buffer ()
  "Read and return a speed-type stats list from the current buffer.
Point is irrelevant and unaffected."
  (let ((stats (save-excursion
                (goto-char (point-min))
                (if (search-forward speed-type-end-of-version-stamp-marker nil t)
                    (condition-case err
                        (read (current-buffer))
                      (error (error "Cannot read definitions in speed type statistic file:  %s"
                                    (error-message-string err))))
                   ;; Else we're dealing with format version 0
		  (error "Buffer is not in speed-type statistic format")))))
      stats))

(defun speed-type--calc-median (symbol stats)
  "Calculate the median of given SYMBOL in STATS."
  (let* ((numbers (sort (remove nil (mapcar (lambda (e) (cdr (assoc symbol e))) stats)) '<))
	 (num-of-records (length numbers))
	 (medians (if (eq (% num-of-records 2) 0)
		      (/ (+ (nth (- (/ num-of-records 2) 1) numbers)
			    (nth (/ num-of-records 2) numbers))
			 2)
		    (nth (/ num-of-records 2) numbers))))
    medians))

(defun speed-type--calc-stats (stats)
  "Calculate the median of each numerical value in STATS.
Additional provide length and skill-value."
  (let ((median-gross-wpm (speed-type--calc-median 'speed-type--gross-wpm stats)))
    (list
     (length stats)
     (speed-type--skill median-gross-wpm)
     (speed-type--calc-median 'speed-type--net-wpm stats)
     (speed-type--calc-median 'speed-type--net-cpm stats)
     median-gross-wpm
     (speed-type--calc-median 'speed-type--gross-cpm stats)
     (speed-type--calc-median 'speed-type--accuracy stats)
     (speed-type--calc-median 'speed-type--elapsed-time stats)
     (speed-type--calc-median 'speed-type--entries stats)
     (speed-type--calc-median 'speed-type--corrections stats)
     (speed-type--calc-median 'speed-type--errors stats)
     (speed-type--calc-median 'speed-type--non-consecutive-errors stats)
     (speed-type--calc-median 'speed-type--remaining stats))))

(defun speed-type-display-menu ()
  "Display and set controls the user can make in this speed-type session.
leave buffer in read-only mode."
  (read-only-mode -1)
  (insert "\n\n"
	  (format "    [%s]uit\n"
		  (propertize "q" 'face 'highlight))
	  (format "    [%s]eplay this sample\n"
		  (propertize "r" 'face 'highlight)))
  (when (not (eq 'never speed-type-save-statistic-option))
    (insert (format "    [%s]isplay statistic\n"
		    (propertize "d" 'face 'highlight))))
  (when speed-type--go-next-fn
    (insert (format "    [%s]ext random sample\n"
		    (propertize "n" 'face 'highlight))))
  (let ((this-scroll-margin
	 (min (max 0 scroll-margin)
	      (truncate (/ (window-body-height) 4.0)))))
    (recenter this-scroll-margin t))
  (let ((view-read-only nil))
    (read-only-mode))
  (use-local-map speed-type--completed-keymap))

(defun speed-type-load-last-stats (file)
  "Load speed-type stats from FILE (which must be in the standard format).
Return the list of stats read from FILE.

If you use `speed-type--load-stats' to load a file that does not contain a
proper speed-type stats list, then when speed-type stats are saved the current
speed-type stats file will likely become corrupted.  You should load only
speed-type files that were created using the speed-type functions."
  ;; Load.
  (setq file (abbreviate-file-name (expand-file-name file)))
  (when (file-directory-p file) (error "`%s' is a directory, not a file" file))
  (unless (file-readable-p file) (error "Cannot read speed-type stats file `%s'" file))
  (message "Loading speed-type stats from `%s'..." file)
  (let ((existing-buf (get-file-buffer file))
        blist)
    (with-current-buffer (let ((enable-local-variables ())) (find-file-noselect file))
      (goto-char (point-min))
      (speed-type-maybe-upgrade-file-format)
      (setq blist (speed-type-stats-list-from-buffer))
      (unless (listp blist) (error "Invalid speed-type stats list in `%s'" file))
      (when (boundp 'speed-type-coding-system)	; Emacs 25.2+
        (setq speed-type-coding-system  buffer-file-coding-system))
      (unless (eq existing-buf (current-buffer)) (kill-buffer (current-buffer))))
    (message "Speed-type stats in `%s' loaded" file)
    blist))

(defun speed-type--gb-url (book-num)
  "Return url for BOOK-NUM."
  (format speed-type--gb-url-format book-num book-num))

;; Just to silence the byte-compiler as url.el doesn't declare this variable
;; but it's defined in `url-http'.
(defvar url-http-end-of-headers)

(defun speed-type--retrieve (filename url)
  "Return buffer FILENAME content in it or download from URL if file doesn't exist."
  (let ((fn (expand-file-name (format "%s.txt" filename) speed-type-gb-dir))
        (url-request-method "GET"))
    (if (file-readable-p fn)
        fn
      (make-directory speed-type-gb-dir 'parents)
      (let ((buffer (url-retrieve-synchronously url nil nil 5)))
        (when (and buffer (= 200 (url-http-symbol-value-in-buffer
                                  'url-http-response-status
                                  buffer)))
          (with-current-buffer buffer
            (write-region url-http-end-of-headers (point-max) fn))
          (unless (kill-buffer buffer)
            (message "WARNING: Buffer is not closing properly"))
          (when (file-readable-p fn)
            (with-temp-file fn
              (insert-file-contents fn)
              (delete-trailing-whitespace)
              (decode-coding-region (point-min) (point-max) 'utf-8))
            fn))))))

(defun speed-type--gb-retrieve (book-num)
  "Return buffer with book number BOOK-NUM in it."
  (speed-type--retrieve book-num  (speed-type--gb-url book-num)))

(defun speed-type--wordlist-retrieve (lang)
  "Return buffer with wordlist for language LANG in it."
  (speed-type--retrieve lang (cdr (assoc lang speed-type-wordlist-urls))))

(defun speed-type--elapsed-time ()
  "Return float with the total time since start."
  (let ((end-time (float-time)))
    (if (not speed-type--start-time)
        0 (- end-time speed-type--start-time))))

(defun speed-type--check-same (pos a b)
  "Return non-nil if both A[POS] B[POS] are white space or if they are the same."
  (let ((q (aref a pos))
        (p (aref b pos)))
    (or (and (= (char-syntax p) ?\s)
             (= (char-syntax q) ?\s))
        (= p q))))

(defun speed-type--handle-del (start end)
  "Keep track of the statistics when a deletion occurs between START and END."
  (delete-region start end)
  (dotimes (i (- end start))
    (let* ((pos (+ (1- start) i))
           (q (aref speed-type--mod-str pos)))
      (cond ((= q 0) ())
            ((or (= q 1)
		         (= q 2))
	         (progn (cl-decf speed-type--entries)
                    (cl-incf speed-type--remaining)))))))

(defun speed-type--display-statistic ()
  "Display median values from current and past entries."
  (interactive)
  (with-current-buffer speed-type--buffer
    (let ((original-max (point-max)))
      (goto-char original-max)
      (read-only-mode -1)
      (insert (apply 'format speed-type-previous-saved-stats-format (speed-type--calc-stats (speed-type-load-last-stats speed-type-statistic-filename))))
      (speed-type-display-menu)
      (read-only-mode)
      (goto-char original-max))))

(defun speed-type--quit ()
  "Kill buffer of speed-type session.
Expects CURRENT-BUFFER to be buffer of speed-type session."
  (interactive)
  (kill-buffer speed-type--buffer))

(defun speed-type--replay ()
  "Replay a speed-type session."
  (interactive)
  (when speed-type--replay-fn
    (let ((fn speed-type--replay-fn)
	  (cb (current-buffer)))
      (funcall fn)
      (kill-buffer cb))))


(defun speed-type--play-next ()
  "Play a new speed-type session, based on the current one."
  (interactive)
  (when speed-type--go-next-fn
    (let ((fn speed-type--go-next-fn)
	  (cb (current-buffer)))
      (funcall fn)
      (kill-buffer cb))))

(defun speed-type--code-buffer-p (buf)
  "Check BUF if we should use code-with-highlighting or treat it as text."
  (with-current-buffer buf
    (cl-find-if #'derived-mode-p speed-type-code-modes)))

(defun speed-type--fill-region ()
  "`fill-region' and sync speed-type local buffer variables.

`fill-region' replaces double spaces with one and breaks lines with newlines.
To reflect the applied changes from `fill-region' we set `speed-type--orig-text'
again and recalculate `speed-type--remaining'."
  (let ((orig-length (length speed-type--orig-text))
	    (fill-regioned-text (progn (fill-region (point-min) (point-max) 'none t)
				                   (buffer-substring (point-min) (point-max)))))
    (setq speed-type--orig-text fill-regioned-text)
    (when (> orig-length (length fill-regioned-text))
      (setq speed-type--remaining (- speed-type--remaining (- orig-length (length fill-regioned-text)))))))

(defun speed-type-fill-paragraph ()
  "Override keybinding of FILL-PARAGRAPH with this to not destory session."
  (interactive)
  (message "Fill paragraph not available"))

(defun speed-type-generate-stats (entries errors non-consecutive-errors corrections seconds)
  "Generate string of statistic data for given arguments:
ENTRIES ERRORS CORRECTIONS SECONDS."
  (format speed-type-stats-format
          (speed-type--skill (speed-type--net-wpm entries errors seconds))
          (speed-type--net-wpm entries errors seconds)
          (speed-type--net-cpm entries errors seconds)
          (speed-type--gross-wpm entries seconds)
          (speed-type--gross-cpm entries seconds)
          (speed-type--accuracy entries (- entries errors) corrections)
          (format-seconds "%M %z%S" seconds)
          entries
          corrections
          errors
	  non-consecutive-errors
          speed-type-explaining-message))

(defun speed-type-complete ()
  "Remove typing hooks from the buffer and print statistics."
  (interactive)
  (remove-hook 'after-change-functions 'speed-type--change)
  (remove-hook 'first-change-hook 'speed-type--first-change)
  (speed-type-finish-animation speed-type--buffer)
  (goto-char (point-max))
  (with-current-buffer speed-type--buffer
    (when speed-type--title
      (insert "\n\n")
      (save-excursion
	(insert (propertize speed-type--title 'face 'italic))
	(when speed-type--author
	  (insert (propertize
		   (format ", by %s" speed-type--author)
		   'face 'italic)))
	(insert (speed-type-generate-stats
		 speed-type--entries
		 speed-type--errors
		 speed-type--non-consecutive-errors
		 speed-type--corrections
		 (speed-type--elapsed-time)))
	(speed-type-save-stats-when-customized)
	(speed-type-display-menu)))))

(defun speed-type--diff (orig new start end)
  "Synchronise local buffer state with buffer-content by comparing ORIG and NEW.
ORIG is the original text. NEW is the new text.
START is a point where the check starts to scan for diff.
END is a point where the check stops to scan for diff."
  (let ((start0 (1- start))
        (_end0 (1- end))
	(any-error nil))
    (dotimes (i (- end start))
      (let* ((correct nil)
	     (pos0 (+ start0 i))
             (pos (+ start i))
	     (non-consecutive-error-p (or (and (<= pos0 0) (= speed-type--non-consecutive-errors 0)) ;; first char is always a non-consecutive error if counter is 0
					  (or (and (eq speed-type-point-motion-on-error 'point-stay) (not (= (aref speed-type--mod-str pos0) 2))) ;; staying, no movement, check current
					      (and (> pos0 0) (eq speed-type-point-motion-on-error 'point-move) (= (aref speed-type--mod-str (1- pos0)) 1)))))) ;; moving, check previous
        (if (speed-type--check-same i orig new)
            (progn (setq correct t)
		   (when (= (aref speed-type--mod-str pos0) 2) (cl-incf speed-type--corrections))
                   (store-substring speed-type--mod-str pos0 1))
          (progn (cl-incf speed-type--errors)
		 (unless any-error (setq any-error t))
		 (when non-consecutive-error-p (cl-incf speed-type--non-consecutive-errors))
                 (store-substring speed-type--mod-str pos0 2)
		 (speed-type-add-extra-words (+ (or speed-type-add-extra-words-on-error 0)
				      (or (and non-consecutive-error-p speed-type-add-extra-words-on-non-consecutive-errors) 0)))))
        (cl-incf speed-type--entries)
        (cl-decf speed-type--remaining)
	(let ((overlay (or (cl-find-if
			    (lambda (ov) (member (overlay-get ov 'face) '(speed-type-correct-face speed-type-error-face speed-type-consecutive-error-face)))
			    (overlays-at pos))
			   (make-overlay pos (1+ pos)))))
	  (overlay-put overlay 'priority 1)
	  (overlay-put overlay 'face (if correct 'speed-type-correct-face (if non-consecutive-error-p 'speed-type-error-face 'speed-type-consecutive-error-face))))))
    (if (or (eq speed-type-point-motion-on-error 'point-move)
	    (equal new "")
	    (not any-error))
        (goto-char end)
      (goto-char (- end 1))
      (beep)
      (message "Wrong key"))))

(defun speed-type--change (start end length)
  "Handle buffer change between START and END.
LENGTH is ignored. Used for hook AFTER-CHANGE-FUNCTIONS.
Make sure that the contents don't actually change, but rather the contents
are color coded and stats are gathered about the typing performance."
  (let ((len (length speed-type--orig-text)))
    (when (<= start len)
      (let* ((end (if (> end (1+ len)) len end))
             (length (if (> (+ start length) len) (1+ (- len start)) length))
             (start0 (1- start))
             (end0 (1- end))
             (new-text (buffer-substring start end))
             (old-text (substring speed-type--orig-text
                                  start0 (+ start0 length)))
             (orig (substring speed-type--orig-text start0 end0)))
        (speed-type--handle-del start end)
	    (insert old-text)
	    (when-let* ((overlay (and (equal new-text "")
				                  (car (overlays-at end)))))
	      (move-overlay overlay (1- (overlay-end overlay)) (overlay-end overlay)) (current-buffer))
	    (speed-type--diff orig new-text start end)
        (when (= speed-type--remaining 0)
          (speed-type-complete))))))

(defun speed-type--first-change ()
  "Start the timer."
  (when (not speed-type--start-time)
    (setq speed-type--start-time (float-time))))

(defun speed-type--trim (str)
  "Trim leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any "\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun speed-type--clean-text (text)
  "Return TEXT with unwanted strings replaced.
Replacements are found in `speed-type-replace-strings'."
  (cl-reduce
   (lambda (acc-text string-pair)
     (string-replace
      (car string-pair)
      (cdr string-pair)
      acc-text))
   speed-type-replace-strings
   :initial-value text))

(cl-defun speed-type--setup
    (content-buffer text &key author title lang n-words add-extra-word-content-fn replay-fn go-next-fn callback)
  "Set up a new buffer for the typing exercise on TEXT.

AUTHOR and TITLE can be given, this happen when the text to type comes
from a gutenberg book.

LANG and N-WORDS are used when training random words where LANG is the
language symbol and N-WORDS is the top N words that should be trained.

CONTENT-BUFFER defines the source from which the typing session
get his content.

If specified, call ADD-EXTRA-WORD-CONTENT-FN which provides extra
words when user makes an error.

If specified, call REPLAY-FN after completion of a speed type session
and replay is selected.  REPLAY-FN should take one argument, a string
corresponding to the text just speed typed.  If REPLAY-FN is not
specified, replay behvaiour will default to playing the same snippet
again.

Similarly, GO-NEXT-FN is called after completion of a session if next
is selected, it should take no arguments.

CALLBACK is called when the setup process has been completed."
  (let ((text (speed-type--clean-text text)))
    (with-temp-buffer
      (insert text)
      (delete-trailing-whitespace)
      (setq text (speed-type--trim (buffer-string))))
    (let ((buf (generate-new-buffer speed-type-buffer-name))
          (len (length text)))
      (set-buffer buf)
      (speed-type-mode)
      (buffer-face-set 'speed-type-default)
      (setq speed-type--orig-text text
	    speed-type--mod-str (make-string len 0)
	    speed-type--remaining len
	    speed-type--author author
	    speed-type--title title
	    speed-type--lang lang
	    speed-type--n-words n-words
	    speed-type--add-extra-word-content-fn add-extra-word-content-fn
	    speed-type--go-next-fn go-next-fn)
      (when content-buffer
	(setq speed-type--content-buffer content-buffer)
	(setq-local speed-type--buffer buf))
      (with-current-buffer speed-type--content-buffer
	(setq-local speed-type--buffer buf)
	(when (null (boundp 'speed-type--extra-word-quote))
	  (setq-local speed-type--extra-word-quote nil)))
      (when replay-fn (setq speed-type--replay-fn replay-fn))
      (insert text)
      (unless (speed-type--code-buffer-p speed-type--content-buffer)
	(speed-type--fill-region))
      (set-buffer-modified-p nil)
      (switch-to-buffer buf)
      (goto-char 0)
      (add-hook 'after-change-functions 'speed-type--change nil t)
      (add-hook 'first-change-hook 'speed-type--first-change nil t)
      (add-hook 'kill-buffer-hook 'speed-type--kill-buffer-hook nil t)
      (setq-local post-self-insert-hook nil)
      (when callback (funcall callback))
      (message "Timer will start when you type the first character."))))

(defun speed-type-prepare-content-buffer-from-buffer (buffer &optional start end)
  "Prepare content buffer from existing BUFFER."
  (let ((buf (generate-new-buffer speed-type-content-buffer-name)))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook 'speed-type--kill-content-buffer-hook nil t)
      (insert-buffer-substring buffer start end)
      (when (speed-type--code-buffer-p buffer)
	(prog-mode)
	(set-syntax-table (with-current-buffer buffer (syntax-table)))
	(setq font-lock-defaults (with-current-buffer buffer font-lock-defaults))
        (ignore-errors (font-lock-ensure)))
      (setq buffer-read-only nil)
      (goto-char (point-min)))
    (get-buffer-create buf)))

(defun speed-type-prepare-content-buffer (file-path)
  "Prepare content buffer from FILE-PATH."
  (let ((buf (generate-new-buffer speed-type-content-buffer-name)))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook 'speed-type--kill-content-buffer-hook nil t)
      (insert-file-contents file-path)
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (get-buffer-create buf))))

(defun speed-type--kill-buffer-hook ()
  "Hook when speed-type buffer is killed."
  (when speed-type--extra-words-animation-time (cancel-timer speed-type--extra-words-animation-time))
  (when speed-type--content-buffer
    (let ((buf speed-type--content-buffer))
      (setq speed-type--content-buffer nil)
      (kill-buffer buf))))

(defun speed-type--kill-content-buffer-hook ()
  "Hook when content buffer is killed."
  (when speed-type--buffer
    (let ((buf speed-type--buffer))
      (setq speed-type--buffer nil)
      (kill-buffer buf))))

(defun speed-type--pick-text-to-type (&optional start end)
  "Return a random section of the buffer usable for playing.

START and END allow to limit to a buffer section - they default
to (point-min) and (point-max)"
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  (goto-char start)
  (forward-paragraph
   ;; count the paragraphs, and pick a random one
   (random (let ((nb 0))
             (while (< (point) end)
               (forward-paragraph)
               (setq nb (+ 1 nb)))
             (goto-char start)
             nb)))
  (mark-paragraph)
  ;; select more paragraphs until there are more than speed-type-min-chars
  ;; chars in the selection

  (while (and (< (mark) end)
              (< (- (mark) (point)) speed-type-min-chars))
    (mark-paragraph 1 t))
  (exchange-point-and-mark)
  ;; and remove sentences if we are above speed-type-max-chars

  (let ((continue t)
        (sentence-end-double-space nil)
        (fwd nil))
    (while (and (< (point) end)
                (> (- (point) (mark)) speed-type-max-chars)
                continue)
      (setq continue (re-search-backward (sentence-end) (mark) t))
      (when continue (setq fwd t)))
    (when fwd (forward-char)))
  (unless (speed-type--code-buffer-p (current-buffer)) (speed-type--fill-region))
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun speed-type--setup-code
    (content-buffer text title author &optional replay-fn go-next-fn syntax-table font-lock-df)
  "Speed type the code snippet TEXT which was extracted from CONTENT-BUFFER.

CONTENT-BUFFER will be linked to the SPEED-TYPE-BUFFER.

If specified, call REPLAY-FN after completion of a speed type session
and replay is selected.  Similarly call GO-NEXT-FN after completion of
a session if next is selected.

For code highlighting, a syntax table can be specified by SYNTAX-TABLE,
and font lock defaults by FONT-LOCK-DF."
  (cl-flet ((callback ()
                      (electric-pair-mode -1)
                      (local-set-key (kbd "TAB") 'speed-type--code-tab)
                      (local-set-key (kbd "RET") 'speed-type--code-ret)
                      (when syntax-table (set-syntax-table syntax-table))
                      (when font-lock-df
                        (let ((font-lock-defaults font-lock-df))
                          ;; Fontify buffer
                          (ignore-errors (font-lock-ensure))))))
    (speed-type--setup content-buffer
	     text
	     :author author
	     :title title
	     :replay-fn replay-fn
             :go-next-fn go-next-fn
             :callback #'callback)))

(defun speed-type--code-with-highlighting (content-buffer text title author &optional syntax-table font-lock-df go-next-fn)
  "Speed type TEXT with syntax highlight which was extracted from CONTENT-BUFFER.

CONTENT-BUFFER will be linked to the SPEED-TYPE-BUFFER..

Syntax highlighting data is given by SYNTAX-TABLE and
FONT-LOCK-DF (font lock defaults).

If GO-NEXT-FN is specified, call it when speed typing the text has
been completed."
  (speed-type--setup-code content-buffer
		text
		title
		author
		#'speed-type--get-replay-fn
		go-next-fn
		syntax-table
		font-lock-df))

(defun speed-type--get-replay-fn ()
  "Return a replay function which will use GO-NEXT-FN after completion."
  (remove-hook 'kill-buffer-hook 'speed-type--kill-buffer-hook t)
  (if (speed-type--code-buffer-p speed-type--content-buffer)
      (speed-type--code-with-highlighting
       speed-type--content-buffer
       speed-type--orig-text
       speed-type--title
       speed-type--author
       (with-current-buffer speed-type--content-buffer (syntax-table))
       (with-current-buffer speed-type--content-buffer font-lock-defaults)
       speed-type--go-next-fn)
    (speed-type--setup speed-type--content-buffer
	     speed-type--orig-text
	     :lang speed-type--lang
	     :author speed-type--author
	     :title speed-type--title
	     :n-words speed-type--n-words
	     :add-extra-word-content-fn speed-type--add-extra-word-content-fn
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn speed-type--go-next-fn)))

(defun speed-type--get-next-word (content-buffer)
  "Get next word from point in CONTENT-BUFFER."
  (with-current-buffer content-buffer
    (forward-word)
    (if (= (point-max) (point))
	(goto-char (point-min)))
    (or (word-at-point) "")))

(defun speed-type--get-separated-thing-at-random-line (content-buffer limit separator)
  "Get thing that is SEPARATOR at random line in CONTENT-BUFFER.

LIMIT is supplied to RANDOM-function."
  (with-current-buffer content-buffer
    (save-excursion
      (goto-char (point-min))
      (beginning-of-line (+ 1 (random limit)))
      (let ((seperated-things (split-string (or (thing-at-point 'line) "") separator)))
	(dotimes (_  (random (length seperated-things)))
	  (setq seperated-things (cdr seperated-things)))
	(car seperated-things)))))

(defun speed-type--get-random-word (content-buffer limit)
  "Get random word in CONTENT-BUFFER.
LIMIT is supplied to the random-function."
  (with-current-buffer content-buffer
    (save-excursion
      (goto-char (point-min))
      (beginning-of-line (+ 1 (random limit)))
      (or (word-at-point) ""))))

(defun speed-type-add-extra-words (x)
  "Add `X' extra words of text to be typed for the typing-session to be complete."
  (when (and (> x 0) speed-type--add-extra-word-content-fn)
    (let ((words '()))
      (dotimes (_ x)
	(let ((word (string-trim (funcall speed-type--add-extra-word-content-fn))))
	  (if (string-empty-p word)
	      (message "You got lucky! Extra word function resulted in empty string.")
	    (push word words))))
      (let ((words-as-string (concat " " (string-trim (mapconcat 'identity (nreverse words) " ")))))
	(setq speed-type--extra-words-queue (append speed-type--extra-words-queue (split-string words-as-string "" t))
	      speed-type--orig-text (concat speed-type--orig-text words-as-string)
	      speed-type--mod-str (concat speed-type--mod-str (make-string (+ 1 (length words-as-string)) 0))
	      speed-type--remaining (+ (length words-as-string) speed-type--remaining))))
    (when (not (timerp speed-type--extra-words-animation-time))
      (setq speed-type--extra-words-animation-time (run-at-time nil 0.01 'speed-type-animate-extra-word-inseration speed-type--buffer)))))

(defun speed-type-finish-animation (buf)
  "Insert all remaining characters in ‘speed-type--extra-words-queue’ to BUF."
  (save-excursion
    (with-current-buffer buf
      (remove-hook 'after-change-functions 'speed-type--change t)
      (when speed-type--extra-words-animation-time (cancel-timer speed-type--extra-words-animation-time))
      (setq speed-type--extra-words-animation-time nil)
      (when speed-type--extra-words-queue
	(goto-char (point-max))
	(insert (mapconcat 'identity speed-type--extra-words-queue ""))
	(unless (speed-type--code-buffer-p speed-type--content-buffer)
	  (speed-type--fill-region))
	(setq speed-type--extra-words-queue nil)))))

(defun speed-type-animate-extra-word-inseration (buf)
  "Add words of punishment-lines in animated fashion to ‘BUF’."
  (save-excursion
    (with-current-buffer buf
      (remove-hook 'after-change-functions 'speed-type--change t)
      (if (and speed-type--extra-words-queue)
	  (let ((token (pop speed-type--extra-words-queue)))
	    (goto-char (point-max))
	    (insert token))
	(unless (speed-type--code-buffer-p speed-type--content-buffer) (speed-type--fill-region))
	(cancel-timer speed-type--extra-words-animation-time)
	(setq speed-type--extra-words-animation-time nil))
      (add-hook 'after-change-functions 'speed-type--change nil t))))

(defun speed-type--code-tab ()
  "A command to be mapped to TAB when speed typing code."
  (interactive)
  (let ((start (point))
	(end (re-search-forward "[^\t ]" (line-end-position) t)))
    (goto-char start)
    (when end (insert (buffer-substring-no-properties start (1- end))))))

(defun speed-type--code-ret ()
  "A command to be mapped to RET when speed typing code."
  (interactive)
  (when (= (point) (line-end-position))
    (newline) (move-beginning-of-line nil) (speed-type--code-tab)))

;;;###autoload
(defun speed-type-top-x (n)
  "Speed type the N most common words."
  (interactive "nTrain X most common words: ")
  (let* ((lang (or speed-type-default-lang
                   (intern (completing-read "Language: " (mapcar 'car speed-type-wordlist-urls)))))
         (file-path (speed-type--wordlist-retrieve lang))
         (char-length (+ speed-type-min-chars
                         (random (- speed-type-max-chars speed-type-min-chars))))
         (words (with-temp-buffer
                  (insert-file-contents file-path)
                  (split-string (buffer-string) "\n" t)))
         (n (min n (length words)))
	 (buf (speed-type-prepare-content-buffer file-path))
         (title (format "Top %s %s words" n lang))
	 (text (with-temp-buffer
		 (while (< (buffer-size) char-length)
		   (insert (speed-type--get-random-word buf n))
                   (insert " "))
		 (speed-type--fill-region)
		 (if speed-type-wordlist-transform
                     (funcall speed-type-wordlist-transform (buffer-string))
                   (buffer-string))))
	 (add-extra-word-content-fn (lambda () (speed-type--get-random-word buf n)))
         (go-next-fn (lambda () (speed-type-top-x n))))
    (speed-type--setup buf
	     text
             :title title
	     :author "Uni Leipzig"
             :lang lang
             :n-words n
	     :add-extra-word-content-fn add-extra-word-content-fn
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn go-next-fn)))

;;;###autoload
(defun speed-type-top-100 ()
  "Speed type the top 100 most common words."
  (interactive)
  (speed-type-top-x 100))

;;;###autoload
(defun speed-type-top-1000 ()
  "Speed type the top 1000 most common words."
  (interactive)
  (speed-type-top-x 1000))

;;;###autoload
(defun speed-type-region (start end)
  "Open copy of [START,END] in a new buffer to speed type the text."
  (interactive "r")
  (let* ((buf (speed-type-prepare-content-buffer-from-buffer (current-buffer) start end))
	 (title (concat (buffer-name)
			(when (and start end) " Region: ")
			(when start (int-to-string start))
			(when end (concat ":" (int-to-string end)))))
         (text (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max)))))
    (if (speed-type--code-buffer-p buf)
        (speed-type--code-with-highlighting buf
				  text
				  title
				  (user-full-name)
				  (syntax-table)
				  font-lock-defaults)
      (speed-type--setup buf
	       (buffer-substring-no-properties start end)
	       :author (user-full-name)
	       :title title
	       :replay-fn #'speed-type--get-replay-fn))))

;;;###autoload
(defun speed-type-buffer (full)
  "Open copy of buffer contents in a new buffer to speed type the text.

If using a prefix while calling this function `C-u', then the FULL text
will be used.  Else some text will be picked randomly."
  (interactive "P")
  (if full
      (speed-type-region (point-min) (point-max))
    (let* ((buf (speed-type-prepare-content-buffer-from-buffer (current-buffer)))
           (text (with-current-buffer buf (speed-type--pick-text-to-type)))
	   (line-count (with-current-buffer buf (count-lines (point-min) (point-max))))
           (go-next-fn (lambda () (with-current-buffer buf (speed-type-buffer full)))))
      (if (speed-type--code-buffer-p buf)
          (speed-type--code-with-highlighting buf
				    text
				    (user-full-name)
				    (buffer-name)
                                    (syntax-table)
                                    font-lock-defaults
                                    go-next-fn)
        (speed-type--setup buf
		 text
		 :author (user-full-name)
		 :title (buffer-name)
		 :add-extra-word-content-fn (lambda () (speed-type--get-separated-thing-at-random-line buf line-count " "))
		 :replay-fn #'speed-type--get-replay-fn
		 :go-next-fn go-next-fn)))))

;;;###autoload
(defun speed-type-text ()
  "Setup a new text sample to practice touch or speed typing."
  (interactive)
  (let* ((book-num (nth (random (length speed-type-gb-book-list))
                        speed-type-gb-book-list))
         (fn (speed-type--gb-retrieve book-num))
	 (buf (speed-type-prepare-content-buffer fn))
	 (title (with-current-buffer buf
		  (save-excursion
		    (when (re-search-forward "^Title: " nil t)
		      (buffer-substring (point) (line-end-position))))))
	 (author (with-current-buffer buf
		   (save-excursion
		     (when (re-search-forward "^Author: " nil t)
		       (buffer-substring (point) (line-end-position))))))
	 (start (with-current-buffer buf
		  (when (re-search-forward "***.START.OF.\\(THIS\\|THE\\).PROJECT.GUTENBERG.EBOOK" nil t)
		    (end-of-line 1)
		    (forward-line 1)
		    (point))))
	 (end (with-current-buffer buf
		(when (re-search-forward "***.END.OF.\\(THIS\\|THE\\).PROJECT.GUTENBERG.EBOOK" nil t)
		  (beginning-of-line 1)
		  (forward-line -1)
		  (point))))
	 (text (with-current-buffer buf
		 (speed-type--pick-text-to-type start end))))
    (speed-type--setup buf
	     text
             :author author
             :title title
	     :add-extra-word-content-fn (lambda () (speed-type--get-next-word buf))
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn #'speed-type-text)))

;;;###autoload
(defun speed-type-quotes (&optional arg)
  "Setup a new quote to practice touch or speed typing.

If `ARG' is given will prompt for a specific quote-URL."
  (interactive "p")
  (let* ((quote-url (if (= arg 1)
			(nth (random (length speed-type-quote-urls)) speed-type-quote-urls)
		      (assoc (intern (completing-read "Choose a quote: " (mapcar 'car speed-type-quote-urls) 'symbolp t nil nil "johnVonNeumann")) speed-type-quote-urls)))
	 (buf (speed-type-prepare-content-buffer (speed-type--retrieve (car quote-url) (cdr quote-url))))
	 (title (with-current-buffer buf
		  (save-excursion (search-forward-regexp "<title>\\(.*\\)</title>")
				  (match-string 1))))
	 (dom-quotes (with-current-buffer buf
		       (dom-by-class (dom-by-class (libxml-parse-html-region (point-min) (point-max) nil) "list-quotes") "title")))
	 (random-quote (nth (random (length dom-quotes)) dom-quotes))
	 (author (dom-attr random-quote 'data-author))
	 (text (dom-text random-quote))
	 (add-extra-word-content-fn
	  (lambda ()
	    (with-current-buffer speed-type--content-buffer
	      (when (null speed-type--extra-word-quote)
		(let ((dom-quotes (dom-by-class (dom-by-class (libxml-parse-html-region (point-min) (point-max) nil) "list-quotes") "title")))
		  (setq-local speed-type--extra-word-quote (split-string (dom-text (nth (random (length dom-quotes)) dom-quotes)) " "))))
	      (let ((word (nth 0 speed-type--extra-word-quote)))
		(setq-local speed-type--extra-word-quote (cdr speed-type--extra-word-quote))
		word))))
	 (go-next-fn (lambda () (speed-type-quotes arg))))
    (speed-type--setup buf
	     text
	     :title title
	     :author author
	     :add-extra-word-content-fn add-extra-word-content-fn
	     :replay-fn #'speed-type--get-replay-fn
	     :go-next-fn go-next-fn)))

(provide 'speed-type)
;;; speed-type.el ends here
