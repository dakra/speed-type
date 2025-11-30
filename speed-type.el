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

(defcustom speed-type-preview-buffer-name "*speed-type-preview-buffer*"
  "Name of buffer consisting of the preview for the speed-type buffer."
  :type 'string)

(defcustom speed-type-min-chars 200
  "The minimum number of chars to type required when the text is picked randomly."
  :type 'integer)

(defcustom speed-type-max-chars 450
  "The maximum number of chars to type required when the text is picked randomly."
  :type 'integer)

(defcustom speed-type-pause-delay-seconds 5
  "Define after which idle delay it should pause the timer."
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

(defcustom speed-type-stop-words '()
  "List of stop words which will be excluded for top-x-commands.

Can be a list or a path to a file which contains words newline separated."
  :type '(choice
          (list :tag "List of words" string)
          (file :tag "Store list in a file\n" :value (concat speed-type-gb-dir "/stop-words.txt"))))

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
                 (const :tag "Afrikaans" af)
                 (const :tag "Aleut" ale)
                 (const :tag "Arabic" ar)
                 (const :tag "Arapaho" arp)
                 (const :tag "Bodo" brx)
                 (const :tag "Breton" br)
                 (const :tag "Bulgarian" bg)
                 (const :tag "Caló" rmq)
                 (const :tag "Catalan" ca)
                 (const :tag "Cebuano" ceb)
                 (const :tag "Chinese" zh)
                 (const :tag "Czech" cs)
                 (const :tag "Danish" da)
                 (const :tag "Dutch" nl)
                 (const :tag "English" en)
                 (const :tag "Esperanto" eo)
                 (const :tag "Estonian" et)
                 (const :tag "Farsi" fa)
                 (const :tag "Finnish" fi)
                 (const :tag "French" fr)
                 (const :tag "Frisian" fy)
                 (const :tag "Friulian" fur)
                 (const :tag "Gaelic Scottish" gla)
                 (const :tag "Galician" gl)
                 (const :tag "Gamilaraay" kld)
                 (const :tag "German" de)
                 (const :tag "Greek" el)
                 (const :tag "Greek Ancient" grc)
                 (const :tag "Hebrew" he)
                 (const :tag "Hungarian" hu)
                 (const :tag "Icelandic" is)
                 (const :tag "Iloko" ilo)
                 (const :tag "Interlingua" ia)
                 (const :tag "Inuktitut" iu)
                 (const :tag "Irish" ga)
                 (const :tag "Italian" it)
                 (const :tag "Japanese" ja)
                 (const :tag "Kashubian" csb)
                 (const :tag "Khasi" kha)
                 (const :tag "Korean" ko)
                 (const :tag "Latin" la)
                 (const :tag "Maori" mi)
                 (const :tag "Mayan Languages" myn)
                 (const :tag "Middle English" enm)
                 (const :tag "Nahuatl" nah)
                 (const :tag "Napoletano-Calabrese" nap)
                 (const :tag "Navajo" nav)
                 (const :tag "North American Indian" nai)
                 (const :tag "Norwegian" no)
                 (const :tag "Occitan" oc)
                 (const :tag "Ojibwa" oji)
                 (const :tag "Old English" ang)
                 (const :tag "Polish" pl)
                 (const :tag "Portuguese" pt)
                 (const :tag "Romanian" ro)
                 (const :tag "Russian" ru)
                 (const :tag "Sanskrit" sa)
                 (const :tag "Serbian" sr)
                 (const :tag "Slovenian" sl)
                 (const :tag "Spanish" es)
                 (const :tag "Swedish" sv)
                 (const :tag "Tagabawa" bgs)
                 (const :tag "Tagalog" tl)
                 (const :tag "Telugu" te)
                 (const :tag "Welsh" cy))
  :group 'speed-type)

(defcustom speed-type-replace-strings '(("“" . "\"") ("”" . "\"") ("‘" . "'") ("’" . "'") ("—" . "-") ("–" . "-") ("Æ" . "Ae") ("æ" . "ae") ("»" . "\"") ("«" . "\""))
  "Alist of strings to replace and their replacement, in the form:
`(bad-string . good-string)'
To remove without replacement, use the form: `(bad-string . \"\")'"
  :type '(alist :key-type string :value-type string)
  :group 'speed-type)

(defcustom speed-type-randomize t
  "Affects the text-picker when starting speed-type-buffer or speed-type-text.

When non-nil it picks a random portion, otherwise it checks for existing
records to start from. If nothing found will take text-portion from the
beginning."
  :type 'boolean
  :group 'speed-type)

(defcustom speed-type-downcase nil
  "Toggle downcasing of mistyped words."
  :type 'boolean)

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

(defcustom speed-type-provide-preview-option nil
  "If a separate buffer should display the actual typed characters."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Hidden" hidden)
                 (const :tag "No" nil)))

(defcustom speed-type-max-num-records 10000
  "Maximum number of saved records."
  :type '(natnum :tag "None negative number." ))

(defcustom speed-type-code-modes '(prog-mode yaml-mode xml-mode html-mode)
  "Define which modes should be handled as code.

These modes will have syntax highlighting and NO `fill-region' will be called."
  :type '(repeat symbol))

(defcustom speed-type-ignore-whitespace-for-complete t
  "Defines if whitespace should be ignored for speed-type-session to be complete.

When non-nil, the completion of the speed-type-session is triggered even when
there are untyped blank-characters.

If nil, the completion is only triggered if all characters are typed."
  :type 'boolean)

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

(defface speed-type-info-face
  '((t :inherit font-lock-comment-face :underline t))
  "Face for point-movement in preview buffer.")

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
Median Non-consecutive errors: %d")

(defvar speed-type--completed-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'speed-type--quit)
    (define-key map (kbd "d") 'speed-type--display-statistic)
    (define-key map (kbd "r") 'speed-type--replay)
    (define-key map (kbd "n") 'speed-type--play-next)
    (define-key map (kbd "c") 'speed-type--continue)
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

(defvar-local speed-type--preview-buffer nil)
(defvar-local speed-type--last-position 0)
(defvar-local speed-type--randomize nil)
(defvar-local speed-type--continue-at-point nil)
(defvar-local speed-type--file-name nil)
(defvar-local speed-type--max-point-on-complete nil)
(defvar-local speed-type--time-register nil)
(defvar-local speed-type--last-changed-text nil)
(defvar-local speed-type--buffer nil)
(defvar-local speed-type--content-buffer nil)
(defvar-local speed-type--entries 0)
(defvar-local speed-type--errors 0)
(defvar-local speed-type--non-consecutive-errors 0)
(defvar-local speed-type--corrections 0)
(defvar-local speed-type--title nil)
(defvar-local speed-type--author nil)
(defvar-local speed-type--lang nil)
(defvar-local speed-type--n-words nil)
(defvar-local speed-type--add-extra-word-content-fn nil)
(defvar-local speed-type--extra-words-animation-time nil)
(defvar-local speed-type--extra-words-queue '())
(defvar-local speed-type--go-next-fn nil)
(defvar-local speed-type--continue-fn nil)
(defvar-local speed-type--replay-fn #'speed-type--setup)
(defvar-local speed-type--extra-word-quote nil)
(defvar-local speed-type--idle-pause-timer nil)

(defun speed-type--resume ()
  "Resume the current typing session.

Adding the idle timer again, and pushing the newest time to stack."
  (unless speed-type--idle-pause-timer
    (setq speed-type--idle-pause-timer (run-with-idle-timer speed-type-pause-delay-seconds nil #'speed-type-pause)
          speed-type--time-register (append speed-type--time-register (list (float-time))))))

(defun speed-type-pause ()
  "Pushes the current time to the start-time variable.

The list of times is used to calculate the overall active typing time."
  (interactive)
  (message "Speed-type session is paused. Resume will be triggered on buffer-change.")
  (when speed-type--idle-pause-timer
    (setq speed-type--idle-pause-timer nil
          speed-type--time-register (append speed-type--time-register (list (float-time))))))

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
        (seconds (speed-type--elapsed-time speed-type--time-register)))
    (list (cons 'speed-type--title speed-type--title)
          (cons 'speed-type--author speed-type--author)
          (cons 'speed-type--lang speed-type--lang)
          (cons 'speed-type--n-words speed-type--n-words)
          (cons 'speed-type--entries entries)
          (cons 'speed-type--errors errors)
          (cons 'speed-type--non-consecutive-errors speed-type--non-consecutive-errors)
          (cons 'speed-type--corrections corrections)
          (cons 'speed-type--elapsed-time seconds)
          (cons 'speed-type--time-register speed-type--time-register)
          (cons 'speed-type--gross-wpm (speed-type--gross-wpm entries seconds))
          (cons 'speed-type--gross-cpm (speed-type--gross-cpm entries seconds))
          (cons 'speed-type--net-wpm (speed-type--net-wpm entries errors seconds))
          (cons 'speed-type--net-cpm (speed-type--net-cpm entries errors seconds))
          (cons 'speed-type--accuracy (speed-type--accuracy entries (- entries errors) corrections))
          (cons 'speed-type--continue-at-point (unless speed-type--randomize (speed-type--get-continue-point)))
          (cons 'speed-type--file-name speed-type--file-name))))

(defun speed-type--stop-word-p (word)
  "Return given WORD when it is a stop-word.

What a stop-word is, is defined by ‘speed-type-stop-words’."
  (cond ((not (stringp word)) (error "Given ‘WORD’(%s) must be a string" word))
        ((listp speed-type-stop-words) (car (member word speed-type-stop-words)))
        ((not (stringp speed-type-stop-words)) (error "Custom variable SPEED-TYPE-STOP-WORDS(%s) must be a list or filename" speed-type-stop-words))
        ((file-readable-p (expand-file-name speed-type-stop-words speed-type-gb-dir))
         (with-current-buffer (find-file-noselect (expand-file-name speed-type-stop-words speed-type-gb-dir) t)
           (when (save-excursion (goto-char (point-min)) (re-search-forward (concat "^" word "$") nil t 1)) word)))
        (t (user-error "Custom variable ‘speed-type-stop-words’ must be a list or a filename in ‘speed-type-directory’"))))

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

(defun speed-type--find-last-continue-at-point-in-stats (file-name)
  "Find if any the last continue-at-point-in-stats."
  (if file-name
      (let ((last (cl-find-if (lambda (e) (and (string-equal-ignore-case (or (cdr (assoc 'speed-type--file-name e)) "") file-name)
                                               (cdr (assoc 'speed-type--continue-at-point e))))
                              (nreverse (speed-type-load-last-stats speed-type-statistic-filename)))))
        (cdr (assoc 'speed-type--continue-at-point last)))
    nil))

(defun speed-type-save-stats-when-customized ()
  "Check the custom variable SPEED-TYPE-SAVE-STATISTIC-OPTION and save stats."
  (when (and (not (= 0 speed-type--entries)) (not (eq speed-type-save-statistic-option 'never)))
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
        (unless (boundp 'speed-type-coding-system)      ; Emacs < 25.2.
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
      (if (not start)                   ; New file, no header yet.
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
  (unless (symbolp symbol) (error "Given SYMBOL(%s) is not a symbol" symbol))
  (unless (listp stats) (error "Given STATS(%s) is not an list" stats))
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
     (speed-type--calc-median 'speed-type--non-consecutive-errors stats))))

(defun speed-type-display-menu ()
  "Display and set controls the user can make in this speed-type session.
leave buffer in read-only mode."
  (read-only-mode -1)
  (insert "\n\n"
          (format "    [%s]uit\n"
                  (propertize "q" 'face 'highlight))
          (format "    [%s]eplay this sample\n"
                  (propertize "r" 'face 'highlight)))
  (when speed-type--continue-fn
    (insert (format "    [%s]ontinue on this sample\n"
                    (propertize "c" 'face 'highlight))))
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
      (when (boundp 'speed-type-coding-system)  ; Emacs 25.2+
        (setq speed-type-coding-system  buffer-file-coding-system))
      (unless (eq existing-buf (current-buffer)) (kill-buffer (current-buffer))))
    (message "Speed-type stats in `%s' loaded" file)
    blist))

(defun speed-type--convert-url (str)
  "Convert STR to a posix standard compatible form and use hash to make it more unique."
  (concat
   (let* ((hash-func (or (car (member 'sha1 (secure-hash-algorithms)))
                         (car (member 'md5 (secure-hash-algorithms)))))
          (hash-str (or (when hash-func (funcall hash-func str)) "no-hash"))
          (short-hash-str (substring hash-str 0 (min 7 (length hash-str)))))
     short-hash-str)
   "-"
   (let ((posix-str (replace-regexp-in-string "-\\." "." (replace-regexp-in-string "-+" "-" (replace-regexp-in-string "[^A-Za-z0-9-]" "-" str)))))
     (substring posix-str 0 (min 64 (length posix-str))))))

(defconst speed-type-pandoc-request-header "\"User-Agent:Emacs: speed-type/1.4 https://github.com/dakra/speed-type\""
  "This const is used when pandoc is retrieving content from an url.")

(defun speed-type--pandoc-top-filename (url)
  "Return name of frequency list of a given book identified by BOOK-NUM."
  (expand-file-name (replace-regexp-in-string "-\\." "." (replace-regexp-in-string "-+" "-" (format "%s-top-x.txt" (speed-type--convert-url url)))) speed-type-gb-dir))

(defun speed-type-retrieve-pandoc (url)
  "Retrieve URL and process it through pandoc.

If the file is already retrieved, will return file-location."
  (unless (executable-find "pandoc") (error "pandoc executable not installed"))
  (let* ((default-directory speed-type-gb-dir)
         (fn (expand-file-name (format "%s.txt" (speed-type--convert-url url)) speed-type-gb-dir))
         (cmd "pandoc")
         (url-opts (format "-s -r html \"%s\"" url))
         (request-header (format "--request-header %s" speed-type-pandoc-request-header))
         (text-opts "-t plain")
         (output (format "-o %s" fn))
         (full-pandoc-cmd (mapconcat 'identity (list cmd url-opts request-header text-opts output) " ")))
    (when (or (> (shell-command full-pandoc-cmd) 0) (not (file-readable-p fn)))
      (message "Check executed pandoc command: %s" full-pandoc-cmd)
      (error "Error while retrieving url with pandoc see *Messages* for details"))
    (find-file-noselect fn t)))

(defun speed-type-retrieve-pandoc-top-retrieve (url)
  "Return buffer containing a frequency list of given book identified by BOOK-NUM."
  (let* ((fn (speed-type--pandoc-top-filename url)))
    (if (file-readable-p fn)
        (find-file-noselect fn t)
      (with-current-buffer (speed-type-retrieve-pandoc url)
        (with-current-buffer
            (speed-type--calculate-word-frequency
             (current-buffer)
             (save-excursion
               (goto-char (point-min))
               (when (re-search-forward "***.START.OF.\\(THIS\\|THE\\).PROJECT.GUTENBERG.EBOOK" nil t)
                 (end-of-line 1)
                 (forward-line 1))
               (point))
             (save-excursion
               (goto-char (point-max))
               (when (re-search-backward "***.END.OF.\\(THIS\\|THE\\).PROJECT.GUTENBERG.EBOOK" nil t)
                 (beginning-of-line 1)
                 (forward-line -1))
               (point)))
          (write-region (point-min) (point-max) fn nil t nil 'excl)
          (rename-buffer (file-name-nondirectory (buffer-file-name)))
          (current-buffer))))))

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
        (find-file-noselect fn t)
      (make-directory speed-type-gb-dir 'parents)
      (let ((buffer (url-retrieve-synchronously url nil nil 5)))
        (when (and buffer (= 200 (url-http-symbol-value-in-buffer
                                  'url-http-response-status
                                  buffer)))
          (with-current-buffer buffer
            (set-buffer-file-coding-system speed-type-coding-system)
            (recode-region url-http-end-of-headers (point-max) 'utf-8-dos speed-type-coding-system)
            (write-region url-http-end-of-headers (point-max) fn))
          (unless (kill-buffer buffer)
            (message "WARNING: Buffer is not closing properly"))
          (find-file-noselect fn t))))))

(defun speed-type--calculate-word-frequency (buffer start end)
  "Create a new buffer containing the words of given BUFFER ordered by frequency.

Only the words which are in the boundaries START and END are considered."
  (let ((word-counts (make-hash-table :test 'equal)))
    (with-current-buffer buffer
      ;; Step 1: Count word frequencies using generator
      (save-excursion
        (goto-char start)
        (let ((word-start nil))
          (while (< (point) end)
            (let ((ch (char-after)))
              (if (and ch (speed-type--char-word-syntax-p ch))
                  (unless word-start (setq word-start (point)))
                (when word-start
                  (let ((word (downcase (buffer-substring-no-properties word-start (point)))))
                    (puthash word (1+ (gethash word word-counts 0)) word-counts))
                  (setq word-start nil))))
            (forward-char 1))
          ;; Final word at end of buffer
          (when word-start
            (let ((word (downcase (buffer-substring-no-properties word-start (point)))))
              (puthash word (1+ (gethash word word-counts 0)) word-counts)))))

      ;; Step 2: Transfer hash map table to file
      (let ((top-words '()))
        (maphash (lambda (k v)
                   ;; Step 2.1: Transfer hash map table to a sortable list
                   (push (cons k v) top-words))
                 word-counts)
        (setq top-words (sort top-words (lambda (a b) (> (cdr a) (cdr b)))))
        (with-current-buffer (generate-new-buffer "*temp*" t)
          (dolist (pair top-words)
            (insert (format "%s\n" (car pair))))
          (current-buffer))))))

(defun speed-type--char-word-syntax-p (ch)
  "Return non-nil if CH is a word-constituent character."
  (eq (char-syntax ch) ?w))

(defun speed-type--gb-retrieve (book-num)
  "Return buffer with book number BOOK-NUM in it."
  (speed-type--retrieve book-num (speed-type--gb-url book-num)))

(defun speed-type--gb-top-filename (book-num)
  "Return name of frequency list of a given book identified by BOOK-NUM."
  (expand-file-name (concat (number-to-string book-num) "-top-x.txt") speed-type-gb-dir))

(defun speed-type--gb-top-retrieve (book-num)
  "Return buffer containing a frequency list of given book identified by BOOK-NUM."
  (let* ((fn (speed-type--gb-top-filename book-num)))
    (if (file-readable-p fn)
        (find-file-noselect fn t)
      (with-current-buffer (speed-type--retrieve book-num (speed-type--gb-url book-num))
        (with-current-buffer
            (speed-type--calculate-word-frequency
             (current-buffer)
             (save-excursion
               (goto-char (point-min))
               (when (re-search-forward "***.START.OF.\\(THIS\\|THE\\).PROJECT.GUTENBERG.EBOOK" nil t)
                 (end-of-line 1)
                 (forward-line 1))
               (point))
             (save-excursion
               (goto-char (point-max))
               (when (re-search-backward "***.END.OF.\\(THIS\\|THE\\).PROJECT.GUTENBERG.EBOOK" nil t)
                 (beginning-of-line 1)
                 (forward-line -1))
               (point)))
          (write-region (point-min) (point-max) fn nil t nil 'excl)
          (rename-buffer (file-name-nondirectory (buffer-file-name)))
          (current-buffer))))))

(defun speed-type--retrieve-random-book-num (lang)
  "Get a random book-num which has the given LANG."
  (let ((lang-book-list (with-current-buffer (speed-type--retrieve (concat lang ".html")
                                                         (concat "https://www.gutenberg.org/ebooks/search/?query=l." lang))
                          (mapcar (lambda (h) (substring (dom-attr h 'href) 8))
                                  (mapcar (lambda (c) (dom-by-tag c 'a))
                                          (dom-by-class (libxml-parse-html-region (point-min) (point-max) nil) "booklink"))))))
    (string-to-number (nth (random (length lang-book-list)) lang-book-list))))

(defun speed-type--wordlist-retrieve (lang)
  "Return buffer with wordlist for language LANG in it."
  (let ((legacy-lang
         (cond ((string= "de" lang) 'German)
               ((string= "en" lang) 'English)
               ((string= "fr" lang) 'French)
               ((string= "nl" lang) 'Dutch)
               ((member lang (mapcar 'car speed-type-wordlist-urls)) lang)
               (t (user-error "Language (%s) not found" lang)))))
    (speed-type--retrieve
     (symbol-name legacy-lang)
     (cdr (assoc legacy-lang speed-type-wordlist-urls)))))

(defun speed-type--list-to-alist-safe (lst)
  "Convert flat list LST into an alist.
If the list length is odd, the last element is kept as (key . nil)."
  (cond
   ((null lst) nil)
   ((null (cdr lst)) (list (cons (car lst) nil)))
   (t (cons (cons (car lst) (cadr lst))
            (speed-type--list-to-alist-safe (cddr lst))))))

(defun speed-type--elapsed-time (time-register)
  "Return float with the total time since start.

TIME-REGISTER is a list of time-floats. Must be of length 0 or a even number. The elements are paired, between the pairs the difference calculated and summed.

If the length is 0 will return 0.

If the length is uneven will return symbol 'uneven."
  (if (= (% (length time-register) 2) 0)
      (if time-register
          (apply #'+ (mapcar (lambda (time-pair) (- (cdr time-pair) (car time-pair)))
                             (speed-type--list-to-alist-safe time-register)))
        0)
    'uneven))

(defconst speed-type--whitespace-table
  (let ((tbl (make-vector 256 nil)))
    (dolist (c '(?\s ?\t ?\n ?\r ?\f))
      (aset tbl c t))
    tbl)
  "Lookup table for ASCII whitespace characters.")

(defun speed-type--check-same (pos a b)
  "Return non-nil if A[POS] and B[POS] are identical or both whitespace.

Whitespace is determined using `char-syntax`."
  (let ((ca (aref a pos))
        (cb (aref b pos)))
    (or (= ca cb)
        (and (eq (char-syntax ca) ?\s)
             (eq (char-syntax cb) ?\s)))))

(defun speed-type--handle-del (start end)
  "Keep track of the statistics when a deletion occurs between START and END."
  (delete-region start end)
  (setq start (if (<= (point-max) start) (point-max) start))
  (setq end (if (<= (point-max) end) (point-max) end))
  (dotimes (i (- end start))
    (let* ((pos (+ (1- start) i))
           (q (get-text-property (1+ pos) 'speed-type-char-status)))
      (cond ((not q) ())
            ((or (eq q 'correct)
                 (eq q 'error))
             (cl-decf speed-type--entries))))))

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

(defun speed-type--continue ()
  "Play a new speed-type-session continuing the content where point is currently at."
  (interactive)
  (when speed-type--continue-fn
    (let ((fn speed-type--continue-fn)
          (cb (current-buffer)))
      (funcall fn)
      (kill-buffer cb))))

(defun speed-type--play-next ()
  "Play a new speed-type session with random content, based on the current one."
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

(defun speed-type--before-change (start end)
  "Store the region between START and END which is going to be modified."
  (setq speed-type--last-changed-text (buffer-substring start end)))

(defun speed-type--fill-region ()
  (fill-region (point-min) (point-max) 'none t))

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
  (remove-hook 'before-change-functions #'speed-type--before-change t)
  (remove-hook 'after-change-functions #'speed-type--change t)
  (speed-type-finish-animation speed-type--buffer)
  (goto-char (point-max))
  (with-current-buffer speed-type--buffer
    (setq speed-type--max-point-on-complete (point-max))
    (when speed-type--idle-pause-timer ;; if session is started
      (cancel-timer speed-type--idle-pause-timer)
      (setq speed-type--time-register (append speed-type--time-register (list (float-time)))))
    (speed-type-save-stats-when-customized)
    (insert "\n\n")
    (save-excursion
      (when speed-type--title (insert (propertize speed-type--title 'face 'italic)))
      (when speed-type--author (insert (propertize (format ", by %s" speed-type--author) 'face 'italic)))
      (insert (speed-type-generate-stats
               speed-type--entries
               speed-type--errors
               speed-type--non-consecutive-errors
               speed-type--corrections
               (speed-type--elapsed-time speed-type--time-register)))
      (speed-type-display-menu))))

(defun speed-type--diff (orig new start end)
  "Synchronise local buffer state with buffer-content by comparing ORIG and NEW.
ORIG is the original text. NEW is the new text.
START is a point where the check starts to scan for diff.
END is a point where the check stops to scan for diff."
  (let ((start0 (1- start))
        (_end0 (1- end))
        (any-error nil))
    (remove-overlays start end 'face 'speed-type-correct-face)
    (remove-overlays start end 'face 'speed-type-error-face)
    (remove-overlays start end 'face 'speed-type-consecutive-error-face)
    (dotimes (i (- end start))
      (let* ((correct nil)
             (pos0 (+ start0 i))
             (pos (+ start i))
             (non-consecutive-error-p (or (and (<= pos0 0) (= speed-type--non-consecutive-errors 0)) ;; first char is always a non-consecutive error if counter is 0
                                          (or (and (eq speed-type-point-motion-on-error 'point-stay) (not (eq (get-text-property (1+ pos0) 'speed-type-char-status) 'error))) ;; staying, no movement, check current
                                              (and (> pos0 0) (eq speed-type-point-motion-on-error 'point-move) (not (eq (get-text-property pos0 'speed-type-char-status) 'error))))))) ;; moving, check previous
        (if (speed-type--check-same i orig new)
            (progn (setq correct t)
                   (when (eq (get-text-property (1+ pos0) 'speed-type-char-status) 'error) (cl-incf speed-type--corrections))
                   (add-text-properties pos (1+ pos) '(speed-type-char-status correct)))
          (progn (cl-incf speed-type--errors)
                 (unless any-error (setq any-error t))
                 (when non-consecutive-error-p (cl-incf speed-type--non-consecutive-errors))
                 (add-text-properties pos (1+ pos) '(speed-type-char-status error))
                 (speed-type-add-extra-words (+ (or speed-type-add-extra-words-on-error 0)
                                      (or (and non-consecutive-error-p speed-type-add-extra-words-on-non-consecutive-errors) 0)))))
        (cl-incf speed-type--entries)
        (let ((overlay (make-overlay pos (1+ pos))))
          (overlay-put overlay 'priority 1)
          (overlay-put overlay 'face (if correct 'speed-type-correct-face (if non-consecutive-error-p 'speed-type-error-face 'speed-type-consecutive-error-face))))))
    (if (or (eq speed-type-point-motion-on-error 'point-move)
            (equal new "")
            (not any-error))
        (goto-char end)
      (goto-char (- end 1))
      (beep)
      (message "Wrong key"))
    (not any-error)))

(defun speed-type--change (start end length)
  "Handle buffer change between START and END.
LENGTH is ignored. Used for hook AFTER-CHANGE-FUNCTIONS.
Make sure that the contents don't actually change, but rather the contents
are color coded and stats are gathered about the typing performance."
  (unless speed-type--idle-pause-timer (speed-type--resume))
  (let ((new-text (buffer-substring start end))
        (old-text speed-type--last-changed-text))
    (speed-type--handle-del start end)
    (insert old-text)
    (if (< start (point-max))
        (let* ((end (if (> end (point-max)) (point-max) end))
               (orig (buffer-substring start end)))
          (when speed-type--preview-buffer
            (let ((new-last-pos start))
              (with-current-buffer speed-type--preview-buffer
                (unwind-protect
                    (save-excursion
                      (goto-char (point-max))
                      (when-let* ((win (get-buffer-window (current-buffer))))
                        (set-window-point win (point)))
                      (read-only-mode -1)
                      (when (and (not (= speed-type--last-position 0))
                                 (> (abs (- new-last-pos speed-type--last-position)) 2))
                        (let ((point-movement-str (concat "[ " (symbol-name last-command) "(" (number-to-string speed-type--last-position) ") → (" (number-to-string (1- new-last-pos)) ") ]")))
                          (insert point-movement-str)
                          (let ((overlay (make-overlay (- (point) (length point-movement-str)) (point))))
                            (overlay-put overlay 'priority 1)
                            (overlay-put overlay 'face 'speed-type-info-face))))
                      (insert (cond ((eq this-command (key-binding (kbd "<deletechar>"))) "⌦")
                                    ((eq this-command (key-binding (kbd "DEL"))) "⌫")
                                    (t (string-replace "\t" "⇥" (string-replace " " "·" (string-replace "\n" "⏎" new-text))))))
                      (setq-local speed-type--last-position new-last-pos))
                  (read-only-mode)))))
          (when speed-type-ignore-whitespace-for-complete ;; add the ignore status again to deleted blank-chars
            (save-excursion
              (goto-char start)
              (while (search-forward-regexp "[[:blank:]\n]+" (+ end length) t 1)
                (add-text-properties (match-beginning 0) (match-end 0) '(speed-type-char-status ignore)))))
          (when-let* ((overlay (and (equal new-text "")
                                    (car (overlays-at end)))))
            (move-overlay overlay (1- (overlay-end overlay)) (overlay-end overlay)) (current-buffer))
          (speed-type--diff orig new-text start end)
          (when (and (not (save-excursion (text-property-search-forward 'speed-type-char-status 'nil t)))
                     (not (save-excursion (text-property-search-backward 'speed-type-char-status 'nil t)))
                     (not (text-property-any (point-min) (point-max) 'speed-type-char-status 'nil)))
            (speed-type-complete)))
      (beep)
      (message "End of buffer"))))

(defun speed-type--trim (str)
  "Trim leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any "\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun speed-type--replace-map-adjust-properties (map property)
  "Replace each FROM/TO pair in MAP while adjusting PROPERTY regions.
MAP is a list of (FROM . TO) string pairs.
PROPERTY is the text property to preserve/adjust around the replaced region."
  (save-excursion
    (dolist (pair map)
      (let ((from (car pair))
            (to   (cdr pair)))
        (goto-char (point-min))
        (while (search-forward from nil t)
          (let* ((start (match-beginning 0))
                 (end   (match-end 0))
                 (prop-start (previous-single-property-change start property speed-type--buffer (point-min)))
                 (prop-end   (next-single-property-change end property speed-type--buffer (point-max)))
                 (len-from (length from))
                 (len-to   (length to))
                 (old-property-value (get-text-property start property)))
            (replace-match to t t)

            (cond
             ;; TO longer → extend property region
             ((> len-to len-from)
              (put-text-property
               prop-start (+ prop-end (- len-to len-from))
               property old-property-value))

             ;; FROM longer → shrink property region
             ((< len-to len-from)
              (let ((new-prop-end (- prop-end (- len-from len-to))))
                (when (> new-prop-end prop-start)
                  (put-text-property
                   prop-start new-prop-end
                   property old-property-value))))
             (t (put-text-property prop-start prop-end property old-property-value)))))))))

(cl-defun speed-type--setup
    (content-buffer text &key file-name author title lang n-words randomize continue-fn add-extra-word-content-fn replay-fn go-next-fn callback)
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
  (let ((buf (generate-new-buffer speed-type-buffer-name)))
    (set-buffer buf)
    (speed-type-mode)
    (buffer-face-set 'speed-type-default)
    (setq speed-type--file-name file-name
          speed-type--author author
          speed-type--title title
          speed-type--lang lang
          speed-type--n-words n-words
          speed-type--randomize randomize
          speed-type--add-extra-word-content-fn add-extra-word-content-fn
          speed-type--continue-fn continue-fn
          speed-type--go-next-fn go-next-fn)
    (when content-buffer
      (setq speed-type--content-buffer content-buffer)
      (setq-local speed-type--buffer buf))
    (with-current-buffer speed-type--content-buffer
      (setq-local speed-type--buffer buf)
      (when (null (boundp 'speed-type--extra-word-quote))
        (setq-local speed-type--extra-word-quote nil)))
    (let ((pbuf (when speed-type-provide-preview-option
                  (setq speed-type--preview-buffer (generate-new-buffer speed-type-preview-buffer-name))
                  (with-current-buffer speed-type--preview-buffer
                    (setq-local speed-type--buffer buf
                                speed-type--last-position 0
                                truncate-lines nil)
                    (speed-type-mode)
                    (add-hook 'kill-buffer-hook 'speed-type--kill-preview-buffer-hook nil t)
                    (read-only-mode))
                  speed-type--preview-buffer))
          (cbuf speed-type--content-buffer))
      (with-current-buffer speed-type--content-buffer
        (setq-local speed-type--preview-buffer pbuf))
      (when speed-type--preview-buffer
        (with-current-buffer speed-type--preview-buffer
          (setq-local speed-type--content-buffer cbuf))))
    (when replay-fn (setq speed-type--replay-fn replay-fn))
    (let ((b-inhibit-read-only inhibit-read-only)
          (b-buffer-undo-list buffer-undo-list)
          (b-inhibit-modification-hooks inhibit-modification-hooks)
          (b-inhibit-field-text-motion inhibit-field-text-motion))
      (unwind-protect
          (progn
            (setq-local inhibit-read-only t
                        buffer-undo-list t
                        inhibit-modification-hooks t
                        inhibit-field-text-motion t)
            (insert (speed-type--trim text))
            (speed-type--replace-map-adjust-properties speed-type-replace-strings 'speed-type-orig-pos)
            (when speed-type-downcase (downcase-region (point-min) (point-max)))
            (unless (speed-type--code-buffer-p speed-type--content-buffer)
              (speed-type--fill-region))
            (when speed-type-ignore-whitespace-for-complete
              (save-excursion
                (goto-char (point-min))
                (while (search-forward-regexp "[[:blank:]\n]+" nil t 1)
                  (add-text-properties (match-beginning 0) (match-end 0) '(speed-type-char-status ignore))))))
        (setq-local inhibit-read-only b-inhibit-read-only
                    buffer-undo-list b-buffer-undo-list
                    inhibit-modification-hooks b-inhibit-modification-hooks
                    inhibit-field-text-motion b-inhibit-field-text-motion)))
    (set-buffer-modified-p nil)
    (switch-to-buffer buf)
    (when (eq speed-type-provide-preview-option t)
      (let ((sw (selected-window))
            (pw (split-window nil 5 'above)))
        (set-window-buffer sw speed-type--preview-buffer)
        (set-window-buffer pw buf)
        (select-window pw)))
    (goto-char 0)
    (add-hook 'before-change-functions #'speed-type--before-change nil t)
    (add-hook 'after-change-functions 'speed-type--change nil t)
    (add-hook 'kill-buffer-hook 'speed-type--kill-buffer-hook nil t)
    (setq-local post-self-insert-hook nil)
    (when callback (funcall callback))
    (message "Timer will start when you type the first character.")))

(defun speed-type-prepare-content-buffer-from-buffer (buffer &optional start end)
  "Prepare content-buffer from existing BUFFER."
  (let ((buf (generate-new-buffer speed-type-content-buffer-name)))
    (with-current-buffer buf
      (setq-local inhibit-read-only t
                  buffer-undo-list t
                  inhibit-modification-hooks t
                  inhibit-field-text-motion t)
      (add-hook 'kill-buffer-hook 'speed-type--kill-content-buffer-hook nil t)
      (insert-buffer-substring buffer start end)
      (when (speed-type--code-buffer-p buffer)
        (prog-mode)
        (set-syntax-table (with-current-buffer buffer (syntax-table)))
        (setq font-lock-defaults (with-current-buffer buffer font-lock-defaults))
        (ignore-errors (font-lock-ensure)))
      (goto-char (point-min)))
    (get-buffer-create buf)))

(defun speed-type--kill-buffer-hook ()
  "Hook when speed-type buffer is killed."
  (when speed-type--idle-pause-timer (cancel-timer speed-type--idle-pause-timer))
  (when speed-type--extra-words-animation-time (cancel-timer speed-type--extra-words-animation-time))
  (when speed-type--content-buffer
    (let ((buf speed-type--content-buffer))
      (setq-local speed-type--content-buffer nil)
      (kill-buffer buf)))
  (when speed-type--preview-buffer
    (let ((pbuf speed-type--preview-buffer))
      (setq-local speed-type--preview-buffer nil)
      (kill-buffer pbuf))))

(defun speed-type--kill-content-buffer-hook ()
  "Hook when content buffer is killed."
  (when speed-type--buffer
    (let ((buf speed-type--buffer))
      (setq-local speed-type--buffer nil)
      (kill-buffer buf)))
  (when speed-type--preview-buffer
    (let ((pbuf speed-type--preview-buffer))
      (setq-local speed-type--preview-buffer nil)
      (kill-buffer pbuf))))

(defun speed-type--kill-preview-buffer-hook ()
  "Hook when preview buffer is killed."
  (when (get-buffer-window (current-buffer))
    (delete-window (get-buffer-window speed-type--preview-buffer)))
  (when speed-type--buffer
    (let ((buf speed-type--buffer))
      (setq-local speed-type--buffer nil)
      (kill-buffer buf)))
  (when speed-type--content-buffer
    (let ((cbuf speed-type--content-buffer))
      (setq-local speed-type--content-buffer nil)
      (kill-buffer cbuf))))

(defun speed-type--pick-continue-text-to-type (start end)
  "Pick text of size between speed-type-min and speed-type-max continuing at START.

If END is reached, will ignore speed-type-min.

Expects to be in the content-buffer."
  (goto-char start)
  (while (and (< (point) end)
              (< (- (point) start) speed-type-min-chars))
    (forward-paragraph 1))
  (let ((continue t)
        (sentence-end-double-space nil)
        (fwd nil))
    (while (and (< (point) end)
                (> (- (point) start) speed-type-max-chars)
                continue)
      (setq continue (re-search-backward (sentence-end) start t 1))
      (when continue (setq fwd t)))
    (when fwd (forward-char)))
  (unless (speed-type--code-buffer-p (current-buffer)) (speed-type--fill-region))
  (save-excursion (speed-type--put-text-property-orig-pos start (point)))
  (buffer-substring start (point)))

(defun speed-type--put-text-property-orig-pos (start end)
  "Propertize the given region from START to END with orig-pos."
  (goto-char start)
  (while (< (point) end)
    (let* ((before (point))
           (skipped (progn (skip-chars-forward " \t\r\n" (point-max))))
           (start (point))
           (inv-skipped (progn (skip-chars-forward "^ \t\r\n" (point-max)) (point))))
      (when (< before start)
        (put-text-property before start 'speed-type-orig-pos (cons before start)))
      (put-text-property start inv-skipped 'speed-type-orig-pos (cons start inv-skipped)))))

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
  (save-excursion (speed-type--put-text-property-orig-pos (region-beginning) (region-end)))
  (buffer-substring (region-beginning) (region-end)))

(defun speed-type--setup-code
    (content-buffer text file-name title author randomize &optional replay-fn go-next-fn continue-fn syntax-table font-lock-df)
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
             :file-name file-name
             :author author
             :title title
             :randomize randomize
             :replay-fn replay-fn
             :go-next-fn go-next-fn
             :continue-fn continue-fn
             :callback #'callback)))

(defun speed-type--code-with-highlighting (content-buffer text file-name title author randomize &optional syntax-table font-lock-df go-next-fn continue-fn)
  "Speed type TEXT with syntax highlight which was extracted from CONTENT-BUFFER.

CONTENT-BUFFER will be linked to the SPEED-TYPE-BUFFER..

Syntax highlighting data is given by SYNTAX-TABLE and
FONT-LOCK-DF (font lock defaults).

If GO-NEXT-FN is specified, call it when speed typing the text has
been completed."
  (speed-type--setup-code content-buffer
                text
                file-name
                title
                author
                randomize
                #'speed-type--get-replay-fn
                go-next-fn
                continue-fn
                syntax-table
                font-lock-df))

(defun speed-type--get-continue-point ()
  (let ((continue-point (save-excursion
                          (goto-char (point-min))
                          (text-property-search-forward 'speed-type-char-status 'nil t)
                          (text-property-search-backward 'speed-type-char-status 'nil t)
                          (point))))
    (if (= (point-min) continue-point)
        (car (get-text-property continue-point 'speed-type-orig-pos))
      (cdr (get-text-property (1- continue-point) 'speed-type-orig-pos)))))

(defun speed-type--get-continue-fn (end)
  "Return a replay function which will use GO-NEXT-FN after completion."
  (let* ((content-buffer speed-type--content-buffer)
         (start (speed-type--get-continue-point))
         (text (with-current-buffer content-buffer
                 (speed-type--pick-continue-text-to-type start (point-max)))))
    (when (and speed-type--preview-buffer (get-buffer-window speed-type--preview-buffer))
      (delete-window (get-buffer-window speed-type--preview-buffer)))
    (when speed-type--preview-buffer
      (with-current-buffer speed-type--preview-buffer
        (setq-local speed-type--content-buffer nil)))
    (setq-local speed-type--content-buffer nil)
    (if (speed-type--code-buffer-p content-buffer)
        (speed-type--code-with-highlighting
         content-buffer
         text
         speed-type--file-name
         speed-type--title
         speed-type--author
         speed-type--randomize
         (with-current-buffer content-buffer (syntax-table))
         (with-current-buffer content-buffer font-lock-defaults)
         speed-type--go-next-fn
         (lambda () (speed-type--get-continue-fn end)))
      (speed-type--setup content-buffer
               text
               :file-name speed-type--file-name
               :author speed-type--author
               :title speed-type--title
               :randomize speed-type--randomize
               :add-extra-word-content-fn (lambda () (speed-type--get-next-word content-buffer))
               :replay-fn #'speed-type--get-replay-fn
               :continue-fn (lambda () (speed-type--get-continue-fn end))
               :go-next-fn speed-type--go-next-fn))))

(defun speed-type--get-next-top-fn (x)
  "Generates itself a function which will create a new typing session as next."
  (let* ((content-buffer speed-type--content-buffer)
         (char-length (+ speed-type-min-chars (random (- speed-type-max-chars speed-type-min-chars))))
         (n (min x (with-current-buffer speed-type--content-buffer (count-words (point-min) (point-max)))))
         (text (with-temp-buffer
                 (while (< (buffer-size) char-length)
                   (let ((random-word (speed-type--get-random-word content-buffer n)))
                     (unless (or (string-blank-p random-word) (speed-type--stop-word-p random-word)) (insert random-word " "))))
                 (speed-type--fill-region)
                 (if speed-type-wordlist-transform
                     (funcall speed-type-wordlist-transform (buffer-string))
                   (buffer-string)))))
    (when (and speed-type--preview-buffer (get-buffer-window speed-type--preview-buffer))
      (delete-window (get-buffer-window speed-type--preview-buffer)))
    (setq-local speed-type--content-buffer nil)
    (when speed-type--preview-buffer
      (with-current-buffer speed-type--preview-buffer
        (setq-local speed-type--content-buffer nil)))
    (speed-type--setup content-buffer
             text
             :file-name speed-type--file-name
             :title speed-type--title
             :n-words n
             :add-extra-word-content-fn speed-type--add-extra-word-content-fn
             :replay-fn speed-type--replay-fn
             :go-next-fn speed-type--go-next-fn)))

(defun speed-type--get-replay-fn ()
  "Return a replay function which will use GO-NEXT-FN after completion."
  (let ((content-buffer speed-type--content-buffer))
    (when (and speed-type--preview-buffer (get-buffer-window speed-type--preview-buffer))
      (delete-window (get-buffer-window speed-type--preview-buffer)))
    (setq-local speed-type--content-buffer nil)
    (when speed-type--preview-buffer
      (with-current-buffer speed-type--preview-buffer
        (setq-local speed-type--content-buffer nil)))
    (if (speed-type--code-buffer-p content-buffer)
        (speed-type--code-with-highlighting
         content-buffer
         (progn
           (read-only-mode -1)
           (remove-text-properties (point-min) speed-type--max-point-on-complete '(speed-type-char-status nil))
           (buffer-substring (point-min) speed-type--max-point-on-complete))
         speed-type--file-name
         speed-type--title
         speed-type--author
         speed-type--randomize
         (with-current-buffer content-buffer (syntax-table))
         (with-current-buffer content-buffer font-lock-defaults)
         speed-type--go-next-fn
         speed-type--continue-fn)
      (speed-type--setup content-buffer
               (progn
                 (read-only-mode -1)
                 (remove-text-properties (point-min) speed-type--max-point-on-complete '(speed-type-char-status nil))
                 (buffer-substring (point-min) speed-type--max-point-on-complete))
               :lang speed-type--lang
               :file-name speed-type--file-name
               :author speed-type--author
               :title speed-type--title
               :n-words speed-type--n-words
               :randomize speed-type--randomize
               :add-extra-word-content-fn speed-type--add-extra-word-content-fn
               :replay-fn #'speed-type--get-replay-fn
               :continue-fn speed-type--continue-fn
               :go-next-fn speed-type--go-next-fn))))

(defun speed-type--get-next-word (content-buffer)
  "Get next word from point in CONTENT-BUFFER."
  (with-current-buffer content-buffer
    (forward-word)
    (if (= (point-max) (point))
        (goto-char (point-min)))
    (let ((word-bound (bounds-of-thing-at-point 'word)))
      (save-excursion (speed-type--put-text-property-orig-pos (car word-bound) (cdr word-bound))))
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
      (let ((word (word-at-point)))

        (or (unless (and word (speed-type--stop-word-p word)) word) ""))
      )))

(defun speed-type-add-extra-words (x)
  "Add `X' extra words of text to be typed for the typing-session to be complete."
  (when (and (> x 0) speed-type--add-extra-word-content-fn)
    (let ((words '()))
      (dotimes (_ x)
        (let ((word (string-trim (funcall speed-type--add-extra-word-content-fn))))
          (if (string-blank-p word)
              (message "You got lucky! Extra word function resulted in empty string.")
            (push word words))))
      (when words
        (let ((words-as-string
               (concat (propertize " " 'speed-type-char-status (when speed-type-ignore-whitespace-for-complete 'ignore))
                       (string-trim (mapconcat (if speed-type-downcase 'downcase 'identity) (nreverse words)
                                               (propertize " " 'speed-type-char-status (when speed-type-ignore-whitespace-for-complete 'ignore)))))))
          (setq speed-type--extra-words-queue (append speed-type--extra-words-queue (split-string words-as-string "" t))))
        (when (not (timerp speed-type--extra-words-animation-time))
          (setq speed-type--extra-words-animation-time (run-at-time nil 0.01 'speed-type-animate-extra-word-inseration speed-type--buffer)))))))

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
      (remove-hook 'before-change-functions #'speed-type--before-change t)
      (remove-hook 'after-change-functions 'speed-type--change t)
      (if (and speed-type--extra-words-queue)
          (let ((token (pop speed-type--extra-words-queue)))
            (goto-char (point-max))
            (insert token))
        (unless (speed-type--code-buffer-p speed-type--content-buffer) (speed-type--fill-region))
        (cancel-timer speed-type--extra-words-animation-time)
        (setq speed-type--extra-words-animation-time nil))
      (add-hook 'before-change-functions #'speed-type--before-change nil t)
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
(defun speed-type-text-top-x (x)
  "Speed type the N most common words."
  (interactive "nTrain X most common words: ")
  (let* ((book-num (if speed-type-default-lang
                       (speed-type--retrieve-random-book-num speed-type-default-lang)
                     (nth (random (length speed-type-gb-book-list)) speed-type-gb-book-list)))
         (buffer (speed-type--gb-top-retrieve book-num))
         (fn (buffer-file-name buffer))
         (char-length (+ speed-type-min-chars (random (- speed-type-max-chars speed-type-min-chars))))
         (n (min x (with-current-buffer buffer (count-words (point-min) (point-max)))))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
         (title (format "Top %s words of book %s" n book-num))
         (text (with-temp-buffer
                 (while (< (buffer-size) char-length)
                   (let ((random-word (speed-type--get-random-word buf n)))
                     (unless (or (string-blank-p random-word) (speed-type--stop-word-p random-word)) (insert random-word " "))))
                 (speed-type--fill-region)
                 (if speed-type-wordlist-transform
                     (funcall speed-type-wordlist-transform (buffer-string))
                   (buffer-string))))
         (add-extra-word-content-fn (lambda () (speed-type--get-random-word buf n)))
         (go-next-fn (lambda () (speed-type--get-next-top-fn x))))
    (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    (speed-type--setup buf
             text
             :file-name fn
             :title title
             :author "To be extracted from gb-book"
             :n-words n
             :add-extra-word-content-fn add-extra-word-content-fn
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn go-next-fn)))


;;;###autoload
(defun speed-type-top-x (x)
  "Speed type the N most common words."
  (interactive "nTrain X most common words: ")
  (let* ((lang (or speed-type-default-lang
                   (intern (completing-read "Language: " (mapcar 'car speed-type-wordlist-urls)))))
         (buffer (speed-type--wordlist-retrieve lang))
         (char-length (+ speed-type-min-chars
                         (random (- speed-type-max-chars speed-type-min-chars))))
         (n (min x (with-current-buffer buffer (count-words (point-min) (point-max)))))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
         (fn (buffer-file-name buffer))
         (title (format "Top %s %s words" n lang))
         (text (with-temp-buffer
                 (while (< (buffer-size) char-length)
                   (let ((random-word (speed-type--get-random-word buf n)))
                     (unless (or (string-blank-p random-word) (speed-type--stop-word-p random-word)) (insert random-word " "))))
                 (speed-type--fill-region)
                 (when speed-type-downcase (downcase-region (point-min) (point-max)))
                 (if speed-type-wordlist-transform
                     (funcall speed-type-wordlist-transform (buffer-string))
                   (buffer-string))))
         (add-extra-word-content-fn (lambda () (speed-type--get-random-word buf n)))
         (go-next-fn (lambda () (speed-type--get-next-top-fn x))))
    (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    (speed-type--setup buf
             text
             :file-name fn
             :author "Uni Leipzig"
             :title title
             :lang lang
             :n-words n
             :randomize t
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
         (fn (buffer-file-name (current-buffer)))
         (title (concat (buffer-name)
                        (when (and start end) " Region: ")
                        (when start (int-to-string start))
                        (when end (concat ":" (int-to-string end)))))
         (text (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max)))))
    (if (speed-type--code-buffer-p buf)
        (speed-type--code-with-highlighting buf
                                  text
                                  fn
                                  title
                                  (user-full-name)
                                  (syntax-table)
                                  font-lock-defaults)
      (speed-type--setup buf
               (buffer-substring-no-properties start end)
               :file-name fn
               :randomize t
               :author (user-full-name)
               :title title
               :replay-fn #'speed-type--get-replay-fn))))

;;;###autoload
(defun speed-type-buffer-top-x (x)
  "Calculate a frequency list of CURRENT-BUFFER and assemble typing-text from top-X-words."
  (interactive "nTrain X most common words: ")
  (let* ((buffer (speed-type--calculate-word-frequency (current-buffer) (point-min) (point-max)))
         (fn (buffer-file-name (current-buffer)))
         (char-length (+ speed-type-min-chars (random (- speed-type-max-chars speed-type-min-chars))))
         (n (min x (with-current-buffer buffer (count-words (point-min) (point-max)))))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
         (title (format "Top %s words of buffer %s" n (buffer-name (current-buffer))))
         (text (with-temp-buffer
                 (while (< (buffer-size) char-length)
                   (let ((random-word (speed-type--get-random-word buf n)))
                     (unless (or (string-blank-p random-word) (speed-type--stop-word-p random-word)) (insert random-word " "))))
                 (speed-type--fill-region)
                 (if speed-type-wordlist-transform
                     (funcall speed-type-wordlist-transform (buffer-string))
                   (buffer-string))))
         (add-extra-word-content-fn (lambda () (speed-type--get-random-word buf n))))
    (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    (speed-type--setup buf
             text
             :file-name fn
             :title title
             :n-words n
             :add-extra-word-content-fn add-extra-word-content-fn
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn (lambda () (speed-type--get-next-top-fn x)))))

;;;###autoload
(defun speed-type-buffer (full)
  "Open copy of buffer contents in a new buffer to speed type the text.

If using a prefix while calling this function `C-u', then the FULL text
will be used.  Else some text will be picked randomly."
  (interactive "P")
  (if full
      (speed-type-region (point-min) (point-max))
    (if speed-type-randomize
        (let* ((buf (speed-type-prepare-content-buffer-from-buffer (current-buffer)))
               (text (with-current-buffer buf (speed-type--pick-text-to-type)))
               (line-count (with-current-buffer buf (count-lines (point-min) (point-max))))
               (go-next-fn (lambda () (with-current-buffer buf (speed-type-buffer full)))))
          (if (speed-type--code-buffer-p buf)
              (speed-type--code-with-highlighting buf
                                        text
                                        (buffer-file-name (current-buffer))
                                        (buffer-name)
                                        (user-full-name)
                                        t
                                        (syntax-table)
                                        font-lock-defaults
                                        go-next-fn)
            (speed-type--setup buf
                     text
                     :file-name (buffer-file-name (current-buffer))
                     :author (user-full-name)
                     :title (buffer-name)
                     :randomize t
                     :add-extra-word-content-fn (lambda () (speed-type--get-separated-thing-at-random-line buf line-count " "))
                     :replay-fn #'speed-type--get-replay-fn
                     :go-next-fn go-next-fn)))
      (speed-type-continue))))

;;;###autoload
(defun speed-type-text ()
  "Setup a new text sample to practice touch or speed typing."
  (interactive)
  (let* ((book-num (if speed-type-default-lang
                       (speed-type--retrieve-random-book-num speed-type-default-lang)
                     (nth (random (length speed-type-gb-book-list)) speed-type-gb-book-list)))
         (buffer (speed-type--gb-retrieve book-num)))
    (if speed-type-randomize
        (let* ((buf (speed-type-prepare-content-buffer-from-buffer buffer))
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
                   :file-name (buffer-file-name buffer)
                   :author author
                   :title title
                   :randomize t
                   :add-extra-word-content-fn (lambda () (speed-type--get-next-word buf))
                   :replay-fn #'speed-type--get-replay-fn
                   :continue-fn (lambda () (speed-type--get-continue-fn end))
                   :go-next-fn #'speed-type-text))
      (speed-type-continue (buffer-file-name buffer)))
    (kill-buffer buffer)))

;;;###autoload
(defun speed-type-quotes (&optional arg)
  "Setup a new quote to practice touch or speed typing.

If `ARG' is given will prompt for a specific quote-URL."
  (interactive "p")
  (let* ((quote-url (if (= arg 1)
                        (nth (random (length speed-type-quote-urls)) speed-type-quote-urls)
                      (assoc (intern (completing-read "Choose a quote: " (mapcar 'car speed-type-quote-urls) 'symbolp t nil nil "johnVonNeumann")) speed-type-quote-urls)))
         (buffer (speed-type--retrieve (car quote-url) (cdr quote-url)))
         (fn (buffer-file-name buffer))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
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
    (kill-buffer buffer)
    (speed-type--setup buf
             text
             :file-name fn
             :title title
             :author author
             :randomize t
             :add-extra-word-content-fn add-extra-word-content-fn
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn go-next-fn)))

;;;###autoload
(defun speed-type-continue (&optional file-name)
  "Searches the last position of the file opened by this buffer and setup a speed-type-session continuing at that last found position. If nothing is found, will begin at 0.

If FILE-NAME is given, will continue with that given file-name.

If FILE-NAME is not found, will throw a user-error.

If FILE-NAME is nil, will use file-name of CURRENT-BUFFER."
  (interactive "P")
  (if (eq speed-type-save-statistic-option 'never)
      (user-error "To use continue the variable speed-type-save-statistic-option can't be never")
    (let* ((buffer (cond ((equal '(4) file-name)
                          (find-file-noselect (read-file-name "Pick your file:" speed-type-gb-dir)))
                         ((stringp file-name) (find-file-noselect file-name))
                         (t (current-buffer))))
           (buf (speed-type-prepare-content-buffer-from-buffer buffer)))
      (with-current-buffer buf
        (let* ((title (save-excursion
                        (or (when (re-search-forward "^Title: " nil t)
                              (buffer-substring (point) (line-end-position)))
                            (buffer-name buffer))))
               (author (save-excursion
                         (when (re-search-forward "^Author: " nil t)
                           (buffer-substring (point) (line-end-position)))))
               (fn (with-current-buffer buffer
                     (progn
                       (unless (buffer-file-name buffer)
                         (let ((r-fn (read-file-name "To save progress, choose a file-location for buffer:" speed-type-gb-dir)))
                           (when (> (or (speed-type--find-last-continue-at-point-in-stats r-fn) 0) (point-max)) (user-error "Can not continue because file already has saved progress which exceeds buffer length"))
                           (write-file r-fn)))
                       (buffer-file-name (current-buffer)))))
               (start (or (speed-type--find-last-continue-at-point-in-stats (buffer-file-name buffer))
                          (when (re-search-forward "***.START.OF.\\(THIS\\|THE\\).PROJECT.GUTENBERG.EBOOK" nil t)
                            (end-of-line 1)
                            (forward-line 1)
                            (point))
                          (point-min)))
               (end (or (when (re-search-forward "***.END.OF.\\(THIS\\|THE\\).PROJECT.GUTENBERG.EBOOK" nil t)
                          (beginning-of-line 1)
                          (forward-line -1)
                          (point))
                        (point-max)))
               (text (speed-type--pick-continue-text-to-type
                      (if (>= start (1- end))
                          (if (y-or-n-p "You completed this file, would you start over again?")
                              (or (save-excursion
                                    (goto-char (point-min))
                                    (when (re-search-forward "***.START.OF.\\(THIS\\|THE\\).PROJECT.GUTENBERG.EBOOK" nil t)
                                      (end-of-line 1)
                                      (forward-line 1)
                                      (point)))
                                  (point-min))
                            (user-error "Aborted speed-type-continue because file completed"))
                        start)
                      end)))
          (if (speed-type--code-buffer-p buf)
              (speed-type--code-with-highlighting
               buf
               text
               fn
               title
               author
               nil
               (syntax-table)
               font-lock-defaults
               speed-type--go-next-fn
               (lambda () (speed-type--get-continue-fn end)))
            (speed-type--setup buf
                     text
                     :file-name fn
                     :randomize nil
                     :author author
                     :title title
                     :add-extra-word-content-fn (lambda () (speed-type--get-next-word buf))
                     :replay-fn #'speed-type--get-replay-fn
                     :continue-fn (lambda () (speed-type--get-continue-fn end))
                     :go-next-fn #'speed-type-text)))))))

(defun speed-type-pandoc (url)
  "Start a typing-session with the content of a given retrieved URL.

If the url already is retrieved will reuse the stored content in ‘speed-type-gb-dir’.

The url is converted to a unique file-name."
  (interactive "sURL: ")
  (let* ((buffer (speed-type-retrieve-pandoc url))
         (fn (buffer-file-name buffer)))
    (if speed-type-randomize
        (let* ((buf (speed-type-prepare-content-buffer-from-buffer buffer))
               (title (buffer-name))
               (start (with-current-buffer buf (point-min)))
               (end (with-current-buffer buf (point-max)))
               (text (with-current-buffer buf (speed-type--pick-text-to-type start end))))
          (speed-type--setup buf
                   text
                   :file-name fn
                   :title title
                   :add-extra-word-content-fn (lambda () (speed-type--get-next-word buf))
                   :replay-fn #'speed-type--get-replay-fn
                   :continue-fn (lambda () (speed-type--get-continue-fn end))
                   :go-next-fn #'speed-type-text))
      (speed-type-continue fn))
    (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    ))

(defun speed-type-pandoc-top-x (url &optional n)
  "Start a typing-session with the content of a given retrieved URL.

If the url already is retrieved will reuse the stored content in ‘speed-type-gb-dir’.

The url is converted to a unique file-name."
  (interactive "sURL: ")
  (let* ((x (string-to-number (completing-read "Train X most common words: " obarray 'numberp nil "100")))
         (buffer (speed-type-retrieve-pandoc-top-retrieve url))
         (fn (buffer-file-name buffer))
         (char-length (+ speed-type-min-chars (random (- speed-type-max-chars speed-type-min-chars))))
         (n (min x (with-current-buffer buffer (count-words (point-min) (point-max)))))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
         (title (format "Top %s words of url %s" n url))
         (text (with-temp-buffer
                 (while (< (buffer-size) char-length)
                   (let ((random-word (speed-type--get-random-word buf n)))
                     (unless (or (string-blank-p random-word) (speed-type--stop-word-p random-word)) (insert random-word " "))))
                 (speed-type--fill-region)
                 (if speed-type-wordlist-transform
                     (funcall speed-type-wordlist-transform (buffer-string))
                   (buffer-string))))
         (add-extra-word-content-fn (lambda () (speed-type--get-random-word buf n))))
    (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    (speed-type--setup buf
             text
             :file-name fn
             :title title
             :n-words n
             :add-extra-word-content-fn add-extra-word-content-fn
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn (lambda () (speed-type--get-next-top-fn x)))))

(provide 'speed-type)
;;; speed-type.el ends here
