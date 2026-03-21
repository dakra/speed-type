;;; speed-type.el --- Practice touch and speed typing -*- lexical-binding: t -*-

;; Copyright (C) 2015 Gunther Hagleitner

;; Author: Gunther Hagleitner
;; Maintainer: Daniel Kraus <daniel@kraus.my>, lordnik22
;; Version: 1.6
;; Keywords: games
;; URL: https://github.com/dakra/speed-type
;; Package-Requires: ((emacs "27.1") (compat "29.1.3"))
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
(require 'text-property-search)
(require 'cl-lib)
(require 'cl-macs)
(require 'compat)
(require 'url)
(require 'url-handlers)
(require 'url-http)
(require 'thingatpt)
(require 'dom)

(when (version< emacs-version "29.1")
  (eval-and-compile
    (defmacro with-undo-amalgamate (&rest body)
      "Like `progn' but perform BODY with amalgamated undo barriers.

This allows multiple operations to be undone in a single step.
When undo is disabled this behaves like `progn'."
      (declare (indent 0) (debug t))
      (let ((handle (make-symbol "--change-group-handle--")))
        `(let ((,handle (prepare-change-group))
               ;; Don't truncate any undo data in the middle of this,
               ;; otherwise Emacs might truncate part of the resulting
               ;; undo step: we want to mimic the behavior we'd get if the
               ;; undo-boundaries were never added in the first place.
               (undo-outer-limit nil)
               (undo-limit most-positive-fixnum)
               (undo-strong-limit most-positive-fixnum))
           (unwind-protect
               (progn
                 (activate-change-group ,handle)
                 ,@body)
             (progn
               (accept-change-group ,handle)
               (undo-amalgamate-change-group ,handle))))))))

(defgroup speed-type nil
  "Practice touch-typing in Emacs."
  :group 'games)

(defcustom speed-type-buffer-name "*speed-type*"
  "Name of buffer in which the user completes his typing session."
  :type 'string)

(defcustom speed-type-content-buffer-name "*speed-type-content*"
  "Name of buffer consisting of the content-source for the speed-type buffer."
  :type 'string)

(defcustom speed-type-preview-buffer-name "*speed-type-preview*"
  "Name of buffer consisting of the preview for the speed-type buffer."
  :type 'string)

(defcustom speed-type-min-chars 200
  "The minimum number of chars to type required when the text is picked randomly."
  :type 'integer)

(defcustom speed-type-max-chars 450
  "The maximum number of chars to type required when the text is picked randomly."
  :type 'integer)

(defcustom speed-type-text-picker-tolerance 20
  "Char-count allowed to exceed MAX if text-picker would cut of word otherwise."
  :type 'integer)

(defcustom speed-type-pause-delay-seconds 5
  "Define after which idle delay it should pause the timer."
  :type 'integer)

(defcustom speed-type-gb-book-list
  '(1342 11 1952 1661 74 1232 23 135 5200 2591 844 84 98 2701 1400 16328 174
         46 4300 345 1080 2500 829 1260 6130 1184 768 32032 521 1399 55)
  "List of book numbers to use from the gutenberg web site.

+Book numbers can be picked from https://www.gutenberg.org, when looking
+at a book url (e.g for url https://www.gutenberg.org/ebooks/14577,
+14577 is the book number)."
  :type '(repeat integer))

(defcustom speed-type-directory (locate-user-emacs-file "speed-type")
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
          (file :tag "Store list in a file\n" :value (concat speed-type-directory "/stop-words.txt"))))

(defcustom speed-type-quote-urls
  '((johnVonNeumann . "https://www.azquotes.com/author/10753-John_von_Neumann")
    (happiness . "https://www.azquotes.com/quotes/topics/happiness.html")
    (alanTuring . "https://www.azquotes.com/author/14856-Alan_Turing"))
  "List of name as key and an URL to azquotes which will be downloaded and parsed."
  :type '(alist :key-type symbol :value-type string))

(defcustom speed-type-wordlist-transform nil
  "OBSOLETE: Better use `speed-type-transform-hook'.

Function to transform wordlist before starting the exercise.
The function should take the `buffer-string' as argument and return
the transformed string that is used for the speed type exercise.

E.g. if you always want lowercase words, set:
`speed-type-wordlist-transform' to `downcase'."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Transform function")))

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
                 (const :tag "Welsh" cy)))

(defcustom speed-type-replace-regexs '()
  "Alist of regex to replace and their replacement, in the form:
`(bad-regex . good-regex)'
To remove without replacement, use the form: `(bad-regex . \"\")'.

It's possible to reference capture groups in good-regex.

Keep in mind the escaping hell."
  :type '(alist :key-type string :value-type string))

(defcustom speed-type-replace-strings '(("“" . "\"") ("”" . "\"") ("‘" . "'") ("’" . "'") ("—" . "-") ("–" . "-") ("Æ" . "Ae") ("æ" . "ae") ("»" . "\"") ("«" . "\"") ("„" . "\"") ("…" . "..."))
  "Alist of literal strings to replace and their replacement, in the form:
`(bad-string . good-string)'
To remove without replacement, use the form: `(bad-string . \"\")'"
  :type '(alist :key-type string :value-type string))

(defcustom speed-type-randomize t
  "Affects the text-picker when starting speed-type-buffer or speed-type-text.

When non-nil it picks a random portion, otherwise it checks for existing
records to start from. If nothing found will take text-portion from the
beginning."
  :type 'boolean)

(defcustom speed-type-downcase nil
  "If t will downcase content."
  :type 'boolean)

(defcustom speed-type-point-motion-on-error 'point-move
  "Define the behavior of point when mistyping a character.

when point-move (default), moves the point one character further.

when point-stay, stays at the current position until correct character is typed."
  :type 'symbol)

(defcustom speed-type-complete-all-correct nil
  "This flag controls the behaviour of triggering complete in speed-type session.

When non-nil, will complete if all characters are typed correctly (have
status correct or ignore)

When nil, will complete when all characters have a status (any non-nil
status value)."
  :type 'boolean)

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

(defcustom speed-type-statistic-filename (concat speed-type-directory "/" "speed-type-statistic.el")
  "This filename points to the speed-type stats file.

The file contains all relevant buffer-local variables for each
speed-type session completed.

Stats are stored using a rolling strategy.

See also `speed-type-max-num-records'."
  :type 'string)

(defcustom speed-type-provide-preview-option nil
  "When t will open a separate window where typed characters are inserted.

It tracks the speed-type session.

When symbol `hidden' is used will track the input but won't open a new
window.

When nil won't track the speed-type session."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "Hidden" hidden)
                 (const :tag "No" nil)))

(defcustom speed-type-max-num-records 10000
  "Maximum number of saved records a stats-file is going to hold."
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

(defcustom speed-type-transform-hook
  '(speed-type--replace-string-hook
    speed-type--replace-regex-hook
    speed-type--filter-stop-word-hook
    speed-type--left-align-hook
    speed-type--delete-trailing-whitespace-hook
    speed-type--downcase-hook
    speed-type--fill-region-hook)
  "Hook run when text is inserted to speed-type-buffer."
  :type 'hook)

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
(defvar speed-type--gb-url-format "https://www.gutenberg.org/cache/epub/%d/pg%d.txt")

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
APM:                          %d
APC:                          %.3f
Accuracy:                     %.2f%%
Total time:                   %s
Total chars:                  %d
Corrections:                  %d
Best correct streak:          %d
Total errors:                 %d
Total non-consecutive errors: %d
%s")

(defvar speed-type-stats-analysis-format  "\n
Num of records: %d
From--To: <%s>--<%s>
Note: 'nil' values are excluded from the calculations.
|                         | Median  | Avg     | SD      | Min     | Max     |
| Skill:                  | %7s | %7s | ------- | %7s | %7s |
| Net WPM:                | %7d | %7d | %7d | %7d | %7d |
| Net CPM:                | %7d | %7d | %7d | %7d | %7d |
| Gross WPM:              | %7d | %7d | %7d | %7d | %7d |
| Gross CPM:              | %7d | %7d | %7d | %7d | %7d |
| APM:                    | %7d | %7d | %7d | %7d | %7d |
| APC:                    | %7.3f | %7.3f | %7.3f | %7.3f | %7.3f |
| Accuracy:               | %6.2f%% | %6.2f%% | %6.2f%% | %6.2f%% | %6.2f%% |
| Total time:             | %6.1fs | %6.1fs | %6.1fs | %6.1fs | %6.1fs |
| Total chars:            | %7d | %7d | %7d | %7d | %7d |
| Corrections:            | %7d | %7d | %7d | %7d | %7d |
| Best correct streak:    | %7d | %7d | %7d | %7d | %7d |
| Total errors:           | %7d | %7d | %7d | %7d | %7d |
| Non-consecutive errors: | %7d | %7d | %7d | %7d | %7d |")

(defvar-keymap speed-type-mode-completed-map
  :doc "Key when speed-type session is completed (menu)."
  "q" #'speed-type-quit
  "d" #'speed-type--display-statistic
  "r" #'speed-type-replay
  "n" #'speed-type-play-next
  "c" #'speed-type-play-continue
  "t" #'speed-type-toggle-preview)

(defvar-keymap speed-type-mode-map
  :doc "Keymap for `speed-type-mode'."
  "C-c C-k" #'speed-type-complete
  "C-c C-f" #'speed-type-finish-animation
  "TAB" #'speed-type-code-tab
  "RET" #'speed-type-code-ret)

(define-derived-mode speed-type-mode fundamental-mode "SpeedType"
  "Major mode for practicing touch typing."
  :group "speed-type")

;; buffer local internal variables
(defvar-local speed-type--text-type nil
  "Symbol indicating what text-picker is used.

It can be one of following symbol:
- continue-text-section
- random-text-section
- random-wordlist
- transform-text
- quote")
(defvar-local speed-type--preview-buffer nil)
(defvar-local speed-type--last-position 0
  "Used in preview-buffer to detect unusual point-movement in speed-type-buffer.")
(defvar-local speed-type--randomize nil
  "Used to determine if continue-at-point should be stored.")
(defvar-local speed-type--continue-at-point nil
  "Used to determine at which point in content-buffer to continue.")
(defvar-local speed-type--file-name nil
  "Used as identification when storing the progress for a book or buffer.")
(defvar-local speed-type--max-point-on-complete nil
  "Used for replay marking the end of the content to type.

It's the point within speed-type-buffer.")
(defvar-local speed-type--time-register nil
  "Holds timestamps and used to calculate duration of a speed-type session.")
(defvar-local speed-type--last-modified-tick nil
  "Detect property-only-changes between before- and after-functions.

It's a property-only-change when modified-tick is the same in before and after.")
(defvar-local speed-type--last-changed-text nil
  "Store characters which are going be compared against actual.

It's used in the before-change-hook.")
(defvar-local speed-type--buffer nil)
(defvar-local speed-type--content-buffer nil)
(defvar-local speed-type--entries 0 "Count the number of inserted characters typed (but each only ones).")
(defvar-local speed-type--actions 0 "Count the number of key typed (which are bound to a command).")

(defvar-local speed-type--errors 0 "Counts mistyped characters.")
(defvar-local speed-type--current-correct-streak 0 "Tracks the correct streak since last error or beginning.")
(defvar-local speed-type--best-correct-streak 0 "The highest count of consecutively correct typed characters.")
(defvar-local speed-type--non-consecutive-errors 0 "Counts mistyped characters but only if previous was correct.")
(defvar-local speed-type--corrections 0
  "Counts the speed-type-status transition of characters from error to correct.")
(defvar-local speed-type--title nil
  "Holds the title of the speed-type session.")
(defvar-local speed-type--author nil
  "Holds the author of the speed-type session.")
(defvar-local speed-type--lang nil
  "Holds the language of the typing content.")
(defvar-local speed-type--n-words nil
  "Specifies the limit for picking a random word from a word frequency list.")
(defvar-local speed-type--add-extra-word-content-fn nil
  "Generate word which is  going to be added to the speed-type session on error.")
(defvar-local speed-type--extra-words-animation-timer nil
  "Timer which inserts characters one by one for a fancy insertion-animation.")
(defvar-local speed-type--extra-words-queue '()
  "Holds characters which are inserted by speed-type--extra-words-animation-timer.")
(defvar-local speed-type--go-next-fn nil
  "Procecures how to call setup for the next-action.")
(defvar-local speed-type--continue-fn nil
  "Procecures how to call setup for the continue-action.")
(defvar-local speed-type--replay-fn nil
  "Procecures how to call setup for the replay-action.")
(defvar-local speed-type--extra-word-quote nil
  "Used for \"quote boundary\" for `speed-type--add-extra-word-content-fn'.")
(defvar-local speed-type--idle-pause-timer nil
  "Trigger code if speed-type session stays untouched for a certain duration.")

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
  (message "Speed-type: session is paused. Resume will be triggered on buffer-change.")
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

(defun speed-type--gross-Xpm (entries seconds)
  "Return gross characters-per-minute.

Computes characters-per-minute as ENTRIES / (SECONDS/60)."
  (round (speed-type--/ entries (speed-type--seconds-to-minutes seconds))))

(defun speed-type--net-wpm (entries errors corrections seconds)
  "Return net words-per-minute.

Computes net words-per-minute as:
UNCORRECTED-ERRORS = ERRORS - CORRECTIONS
  ((ENTRIES/5) - UNCORRECTED-ERRORS) / (SECONDS/60)."
  (let ((net-wpm (round (- (speed-type--gross-wpm entries seconds)
                           (speed-type--/ (- errors corrections)
                                (speed-type--seconds-to-minutes seconds))))))
    (if (> 0 net-wpm) 0 net-wpm)))

(defun speed-type--net-cpm (entries errors corrections seconds)
  "Return net characters-per-minute.

Computes net characters-per-minute as:
UNCORRECTED-ERRORS = ERRORS - CORRECTIONS
  (ENTRIES - UNCORRECTED-ERRORS) / (SECONDS/60)."
  (let ((net-cpm (round (- (speed-type--gross-Xpm entries seconds)
                           (speed-type--/ (- errors corrections)
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
   ((< wpm 25) "Rookie")
   ((< wpm 30) "Novice")
   ((< wpm 40) "Adept")
   ((< wpm 55) "Expert")
   ((< wpm 80) "Master")
   (t          "Legend")))

(defvar speed-type-coding-system 'utf-8-unix
  "The coding system speed-type uses for saving the stats.
Changing this value while Emacs is running is supported, but considered
unwise, unless you know what you are doing.")

(defconst speed-type-file-format-version 1
  "The current version of the format used by speed-type statistic files.
You should never need to change this.
- 0 = initial version.
- 1 = fix by maybe adding a newline")

(defun speed-type-statistic-variables ()
  "Define the structure of raw-data used for calculating the median-stats.

If the structure is changed, SPEED-TYPE-FILE-FORMAT-VERSION must
be incremented and a migration must be coded in
SPEED-TYPE-MAYBE-UPGRADE-FILE-FORMAT."
  (let ((entries speed-type--entries)
        (actions speed-type--actions)
        (errors speed-type--errors)
        (corrections speed-type--corrections)
        (seconds (speed-type--elapsed-time speed-type--time-register)))
    (list (cons 'speed-type--create-time (decode-time (float-time) (current-time-zone)))
          (cons 'speed-type--title speed-type--title)
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
          (cons 'speed-type--gross-cpm (speed-type--gross-Xpm entries seconds))
          (cons 'speed-type--gross-apm (speed-type--gross-Xpm actions seconds))
          (cons 'speed-type--apc (speed-type--/ (float actions) entries))
          (cons 'speed-type--net-wpm (speed-type--net-wpm entries errors corrections seconds))
          (cons 'speed-type--net-cpm (speed-type--net-cpm entries errors corrections seconds))
          (cons 'speed-type--accuracy (speed-type--accuracy entries (- entries errors) corrections))
          (cons 'speed-type--continue-at-point (unless speed-type--randomize (speed-type--get-continue-point)))
          (cons 'speed-type--file-name speed-type--file-name)
          (cons 'speed-type--best-correct-streak speed-type--best-correct-streak))))

(defun speed-type--stop-word-p (word)
  "Return given WORD when it is a stop-word.

What a stop-word is, is defined by `speed-type-stop-words'."
  (cond ((not (stringp word)) (error "Given WORD(%s) must be a string" word))
        ((listp speed-type-stop-words) (car (member word speed-type-stop-words)))
        ((not (stringp speed-type-stop-words)) (error "Custom variable SPEED-TYPE-STOP-WORDS(%s) must be a list or filename" speed-type-stop-words))
        ((file-readable-p (expand-file-name speed-type-stop-words speed-type-directory))
         (with-current-buffer (find-file-noselect (expand-file-name speed-type-stop-words speed-type-directory) t)
           (when (save-excursion (goto-char (point-min)) (re-search-forward (concat "^" word "$") nil t 1)) word)))
        (t (user-error "Custom variable `speed-type-stop-words' must be a list or a filename in `speed-type-directory'"))))

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

We use `pp' to write the buffer state to the statistic-file.
Since `pp' works a bit differently on EMACS 30.1 for some
reason. It is necessary to check if the last closing parantheses
has it's own line.

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
  "Search for last speed-type--continue-at-point in given FILE-NAME.

When given FILE-NAME is nil will return nil.

If SPEED-TYPE-STATISTIC-FILE-NAME does not exists yet, will return nil.

If no entries are found with FILE-NAME, will return nil.

Expects FILE-NAME to be a speed-type-stats-file.

Create stats-file with function `speed-type-save-stats'."
  (unless (or (null file-name)
              (stringp file-name))
    (error "Wrong type of argument FILE-NAME(%s)" file-name))
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
      (speed-type-save-stats speed-type-statistic-filename (with-current-buffer speed-type--buffer (speed-type-statistic-variables))))))

(defun speed-type-save-stats (file session-stats &optional alt-msg)
  "Write given SESSION-STATS to FILE.

SESSIONS-STATS is a alist containing the local-variables of the typing-session:
- `((speed-type--entries . 2) ...)

See detailed alist SPEED-TYPE-STATISTIC-VARIABLES.

This function is heavily inspired by bookmark+-save-stats.

Non-nil ALT-MSG is a message format string to use in place of the
default, \"Saving statistics of current speed-type session to
file `%s'...\". The string must contain a `%s' construct, so that
it can be passed along with FILE to `format'. At the end,
\"done\" is appended to the message."
  (unless (listp session-stats) (error "Given SESSION-STATS(%s) is not an alist" session-stats))
  (unless (stringp file) (error "Given FILE(%s) is not a string" file))
  (let ((msg                      (or alt-msg  "Saving statistics of current speed-type session to file `%s'..."))
        (coding-system-for-write  speed-type-coding-system)
        (print-length             nil)
        (print-level              nil)
        (existing-buf             (get-file-buffer file))
        (emacs-lisp-mode-hook     nil) ; Avoid inserting automatic file header if existing empty file, so
        (lisp-mode-hook           nil) ; better chance `speed-type-maybe-upgrade-file-format' signals error.
        start end)
    (when (file-directory-p file) (error "FILE(%s) is a directory, not a file" file))
    (message msg (abbreviate-file-name file))
    (with-current-buffer (let ((enable-local-variables ())) (find-file-noselect file))
      (goto-char (point-min))
      (if (file-exists-p file)
          (speed-type-maybe-upgrade-file-format)
        (delete-region (point-min) (point-max)) ; In case a find-file hook inserted a header, etc.
        (unless (boundp 'speed-type-coding-system)        ; Emacs < 25.2.
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
      (pp session-stats (current-buffer))
      (speed-type--maybe-insert-newline)
      (when (boundp 'speed-type-coding-system)  ; Emacs 25.2+.  See bug #25365
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
                      (let ((msg  (format "CANNOT WRITE FILE(%s)" file)))
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

(defun speed-type--calc-standard-deviation (symbol stats)
  (unless (symbolp symbol) (error "Given SYMBOL(%s) is not a symbol" symbol))
  (unless (listp stats) (error "Given STATS(%s) is not an list" stats))
  (or (when-let* ((numbers (sort (remove nil (mapcar (lambda (e) (cdr (assoc symbol e))) stats)) '<))
                  (avg (speed-type--calc-avg symbol stats))
                  (sum-of-variance (apply '+ (mapcar (lambda (n) (expt (- n avg) 2)) numbers)))
                  (num-of-records (length numbers))
                  (standard-deviation (sqrt (/ sum-of-variance num-of-records))))
        standard-deviation)
      0))

(defun speed-type--calc-max (symbol stats &optional comparator-fn)
  (unless (symbolp symbol) (error "Given SYMBOL(%s) is not a symbol" symbol))
  (unless (listp stats) (error "Given STATS(%s) is not an list" stats))
  (or (when-let* ((numbers (sort (remove nil (mapcar (lambda (e) (cdr (assoc symbol e))) stats)) (or comparator-fn '>=)))
                  (max (nth 0 numbers)))
        max)
      0))

(defun speed-type--calc-min (symbol stats &optional comparator-fn)
  (unless (symbolp symbol) (error "Given SYMBOL(%s) is not a symbol" symbol))
  (unless (listp stats) (error "Given STATS(%s) is not an list" stats))
  (or (when-let* ((numbers (sort (remove nil (mapcar (lambda (e) (cdr (assoc symbol e))) stats)) (or comparator-fn '<)))
                  (min (nth 0 numbers)))
        min)
      0))

(defun speed-type--calc-avg (symbol stats)
  "Calculate the average of given SYMBOL in STATS."
  (unless (symbolp symbol) (error "Given SYMBOL(%s) is not a symbol" symbol))
  (unless (listp stats) (error "Given STATS(%s) is not an list" stats))
  (or (when-let* ((numbers (remove nil (mapcar (lambda (e) (cdr (assoc symbol e))) stats)))
                  (sum-of-records (apply '+ numbers))
                  (num-of-records (length numbers))
                  (avg (/ sum-of-records num-of-records)))
        avg)
      0))

(defun speed-type--calc-median (symbol stats)
  "Calculate the median of given SYMBOL in STATS."
  (unless (and (not (eq t symbol)) (not (null symbol)) (symbolp symbol)) (error "Given SYMBOL(%s) is not a symbol" symbol))
  (unless (listp stats) (error "Given STATS(%s) is not an list" stats))
  (or (when-let* ((numbers (sort (remove nil (mapcar (lambda (e) (cdr (assoc symbol e))) stats)) '<))
                  (num-of-records (length numbers))
                  (medians (if (eq (% num-of-records 2) 0)
                               (/ (+ (nth (- (/ num-of-records 2) 1) numbers)
                                     (nth (/ num-of-records 2) numbers))
                                  2)
                             (nth (/ num-of-records 2) numbers))))
        medians)
      0))

(defun speed-type--calc-stats (stats)
  "Calculate the median of each numerical value in STATS.
Additional provide length and skill-value."
  (if stats
      (let ((median-gross-wpm (speed-type--calc-median 'speed-type--gross-wpm stats))
            (avg-gross-wpm (speed-type--calc-avg 'speed-type--gross-wpm stats))
            (min-gross-wpm (speed-type--calc-min 'speed-type--gross-wpm stats))
            (max-gross-wpm (speed-type--calc-max 'speed-type--gross-wpm stats)))
        (list
         (length stats) (format-time-string "%F %T" (encode-time (speed-type--calc-min 'speed-type--create-time stats (lambda (e1 e2) (time-less-p (encode-time e1) (encode-time e2)))))) (format-time-string "%F %T" (encode-time (speed-type--calc-max 'speed-type--create-time stats (lambda (e1 e2) (time-less-p (encode-time e2) (encode-time e1))))))
         (speed-type--skill median-gross-wpm) (speed-type--skill avg-gross-wpm) (speed-type--skill min-gross-wpm) (speed-type--skill max-gross-wpm)
         (speed-type--calc-median 'speed-type--net-wpm stats) (speed-type--calc-avg 'speed-type--net-wpm stats) (speed-type--calc-standard-deviation 'speed-type--net-wpm stats) (speed-type--calc-min 'speed-type--net-wpm stats) (speed-type--calc-max 'speed-type--net-wpm stats)
         (speed-type--calc-median 'speed-type--net-cpm stats) (speed-type--calc-avg 'speed-type--net-cpm stats) (speed-type--calc-standard-deviation 'speed-type--net-cpm stats) (speed-type--calc-min 'speed-type--net-cpm stats) (speed-type--calc-max 'speed-type--net-cpm stats)
         median-gross-wpm avg-gross-wpm (speed-type--calc-standard-deviation 'speed-type--gross-wpm stats) min-gross-wpm max-gross-wpm
         (speed-type--calc-median 'speed-type--gross-cpm stats) (speed-type--calc-avg 'speed-type--gross-cpm stats) (speed-type--calc-standard-deviation 'speed-type--gross-cpm stats) (speed-type--calc-min 'speed-type--gross-cpm stats) (speed-type--calc-max 'speed-type--gross-cpm stats)
         (speed-type--calc-median 'speed-type--gross-apm stats) (speed-type--calc-avg 'speed-type--gross-apm stats) (speed-type--calc-standard-deviation 'speed-type--gross-apm stats) (speed-type--calc-min 'speed-type--gross-apm stats) (speed-type--calc-max 'speed-type--gross-apm stats)
         (speed-type--calc-median 'speed-type--apc stats) (speed-type--calc-avg 'speed-type--apc stats) (speed-type--calc-standard-deviation 'speed-type--apc stats) (speed-type--calc-min 'speed-type--apc stats) (speed-type--calc-max 'speed-type--apc stats)
         (speed-type--calc-median 'speed-type--accuracy stats) (speed-type--calc-avg 'speed-type--accuracy stats) (speed-type--calc-standard-deviation 'speed-type--accuracy stats) (speed-type--calc-min 'speed-type--accuracy stats) (speed-type--calc-max 'speed-type--accuracy stats)
         (speed-type--calc-median 'speed-type--elapsed-time stats) (speed-type--calc-avg 'speed-type--elapsed-time stats) (speed-type--calc-standard-deviation 'speed-type--elapsed-time stats) (speed-type--calc-min 'speed-type--elapsed-time stats) (speed-type--calc-max 'speed-type--elapsed-time stats)
         (speed-type--calc-median 'speed-type--entries stats) (speed-type--calc-avg 'speed-type--entries stats) (speed-type--calc-standard-deviation 'speed-type--entries stats) (speed-type--calc-min 'speed-type--entries stats) (speed-type--calc-max 'speed-type--entries stats)
         (speed-type--calc-median 'speed-type--corrections stats) (speed-type--calc-avg 'speed-type--corrections stats) (speed-type--calc-standard-deviation 'speed-type--corrections stats) (speed-type--calc-min 'speed-type--corrections stats) (speed-type--calc-max 'speed-type--corrections stats)
         (speed-type--calc-median 'speed-type--best-correct-streak stats) (speed-type--calc-avg 'speed-type--best-correct-streak stats) (speed-type--calc-standard-deviation 'speed-type--best-correct-streak stats) (speed-type--calc-min 'speed-type--best-correct-streak stats) (speed-type--calc-max 'speed-type--best-correct-streak stats)
         (speed-type--calc-median 'speed-type--errors stats) (speed-type--calc-avg 'speed-type--errors stats) (speed-type--calc-standard-deviation 'speed-type--errors stats) (speed-type--calc-min 'speed-type--errors stats) (speed-type--calc-max 'speed-type--errors stats)
         (speed-type--calc-median 'speed-type--non-consecutive-errors stats) (speed-type--calc-avg 'speed-type--non-consecutive-errors stats) (speed-type--calc-standard-deviation 'speed-type--non-consecutive-errors stats) (speed-type--calc-min 'speed-type--non-consecutive-errors stats) (speed-type--calc-max 'speed-type--non-consecutive-errors stats)))
    '(0 "empty" "empty" "empty" "empty" "empty" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

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
  (when (not (null speed-type-provide-preview-option))
    (insert (format "    [%s]oggle preview\n"
                    (propertize "t" 'face 'highlight))))
  (when speed-type--go-next-fn
    (insert (format "    [%s]ext random sample\n"
                    (propertize "n" 'face 'highlight))))
  (let ((this-scroll-margin
         (min (max 0 scroll-margin)
              (truncate (/ (window-body-height) 4.0)))))
    (recenter this-scroll-margin t))
  (let ((view-read-only nil))
    (read-only-mode))
  (use-local-map speed-type-mode-completed-map))

(defun speed-type-load-last-stats (file)
  "Load speed-type stats from FILE (which must be in the standard format).
Return the list of stats read from FILE.

If you use `speed-type--load-stats' to load a file that does not contain a
proper speed-type stats list, then when speed-type stats are saved the current
speed-type stats file will likely become corrupted.  You should load only
speed-type files that were created using the speed-type functions."
  ;; Load.
  (when (null file) (error "While loading stats given FILE was nil"))
  (setq file (abbreviate-file-name (expand-file-name file)))
  (when (file-directory-p file) (error "FILE(%s) is a directory, not a file" file))
  (message "Loading speed-type stats from FILE(%s)..." file)
  (if (file-readable-p file)
      (let ((existing-buf (get-file-buffer file))
            blist)
        (with-current-buffer (let ((enable-local-variables ())) (find-file-noselect file))
          (goto-char (point-min))
          (speed-type-maybe-upgrade-file-format)
          (setq blist (speed-type-stats-list-from-buffer))
          (unless (listp blist) (error "Invalid speed-type stats list in FILE(%s)" file))
          (when (boundp 'speed-type-coding-system)        ; Emacs 25.2+
            (setq speed-type-coding-system  buffer-file-coding-system))
          (unless (eq existing-buf (current-buffer)) (kill-buffer (current-buffer))))
        (message "Speed-type stats in FILE(%s) loaded" file)
        blist)
    nil))

(defun speed-type--url-to-filename (url)
  "Convert URL to a POSIX-standard compatible form.

The return value has no file-extension, it has to be appended
separately. When URL contains a file-extension it will become part
of the filename.

Return value can be used as filename except URL is nil or blank-string
in such a case return URL as is.

URL with different whitespace at end and/or begin result in the same
return value.

A shorten hash is appended to make the filename more unique.

Therefore `secure-hash-algorithms' should provide sha1 or md5 else
\"no-hash\" is appended instead of a real hash."
  (if (or (null url) (string-blank-p url))
      url
    (let ((url (string-trim url)))
      (concat
       (let ((posix-str (replace-regexp-in-string "^-\\|-$" "" (replace-regexp-in-string "-\\." "." (replace-regexp-in-string "-+" "-" (replace-regexp-in-string "[^A-Za-z0-9-]" "-" url))))))
         (substring posix-str 0 (min 64 (length posix-str))))
       "-"
       (let* ((hash-func (or (car (member 'sha1 (secure-hash-algorithms)))
                             (car (member 'md5 (secure-hash-algorithms)))))
              (hash-str (or (when hash-func (funcall hash-func url)) "no-hash"))
              (short-hash-str (substring hash-str 0 (min 7 (length hash-str)))))
         short-hash-str)))))

(defconst speed-type-pandoc-request-header "\"User-Agent:Emacs: speed-type/1.4 https://github.com/dakra/speed-type\""
  "This const is used when pandoc is retrieving content from an url.")

(defun speed-type--pandoc-top-filename (url)
  "Create a filename using URL for a top-x-file."
  (expand-file-name (format "%s-top-x.txt" (speed-type--url-to-filename url)) speed-type-directory))

(defun speed-type-retrieve-pandoc (url)
  "Retrieve URL and process it through pandoc.

If the file is already retrieved, will return file-location."
  (unless (executable-find "pandoc") (error "Executable: pandoc not installed"))
  (let* ((fe (file-name-extension url))
         (fn (expand-file-name (format "%s.%s"
                                       (speed-type--url-to-filename (file-name-sans-extension url))
                                       (cond
                                        ((string= fe "html") "txt")
                                        ((not (null fe)) fe)
                                        (t "txt")))
                               speed-type-directory)))
    (unless (file-readable-p fn)
      (let* ((default-directory speed-type-directory)
             (cmd "pandoc")
             (url-opts (format "-s -r html \"%s\"" url))
             (request-header (format "--request-header %s" speed-type-pandoc-request-header))
             (text-opts "-t plain --wrap preserve")
             (output (format "-o %s" fn))
             (full-pandoc-cmd (mapconcat #'identity (list cmd url-opts request-header text-opts output) " ")))
        (message "Speed-type: Retrieving content with pandoc...")
        (when (or (> (call-process-shell-command full-pandoc-cmd) 0) (not (file-readable-p fn)))
          (error "Speed-type: Retrieving content with pandoc...FAILED, command was %s" full-pandoc-cmd))
        (message "Speed-type: Retrieving content with pandoc...DONE")))
    (find-file-noselect fn t)))

(defun speed-type-retrieve-pandoc-top-retrieve (url)
  "Retrieve URL via pandoc and get a buffer with the converted content."
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
  (or (let ((fn (expand-file-name (format "%s.txt" filename) speed-type-directory))
            (url-request-method "GET"))
        (if (file-readable-p fn)
            (progn (message "Speed-type: Using cached version at FN(%s) for URL(%s)" fn url)
                   (find-file-noselect fn t))
          (make-directory speed-type-directory 'parents)
          (message "Speed-type: Going to retrieve FN(%s) from URL(%s)" fn url)
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
              (find-file-noselect fn t)))))
      (error "Error retrieving book with URL(%s)" url)))

(defun speed-type--calculate-word-frequency (buffer start end)
  "Create a new buffer containing the words of given BUFFER ordered by frequency.

Only the words which are in the boundaries START and END are considered."
  (let ((word-counts (make-hash-table :test 'equal)))
    (with-current-buffer buffer
      ;; Step 1: Count word frequencies using hash-table
      (save-excursion
        (goto-char start)
        (let ((word-start nil)
              (first-word-in-sentence-p (bobp)))
          (while (< (point) end)
            (let ((ch (char-after)))
              (if (and ch (eq (char-syntax ch) ?w))
                  (unless word-start (setq word-start (point)))
                (when word-start
                  (let* ((word (buffer-substring-no-properties word-start (point)))
                         (key (downcase word))
                         (value (gethash key word-counts (list 0 nil)))
                         (rep (if first-word-in-sentence-p (cadr value) word)))
                    (puthash (downcase word) (list (1+ (car value)) rep) word-counts))
                  (setq word-start nil
                        first-word-in-sentence-p nil))
                (when (and (not first-word-in-sentence-p) (string-match-p (sentence-end) (char-to-string ch)))
                  (setq first-word-in-sentence-p t))))
            (forward-char 1))
          ;; Final word at end of buffer
          (when word-start
            (let* ((word (buffer-substring-no-properties word-start (point)))
                   (key (downcase word))
                   (value (gethash key word-counts (list 0 nil)))
                   (rep (if first-word-in-sentence-p (cadr value) word)))
              (puthash (downcase word) (list (1+ (car value)) rep) word-counts))
            (setq word-start nil
                  first-word-in-sentence-p nil))))

      (let ((top-words '()))
        ;; Step 2: Transfer hash map table to a sortable list
        (maphash (lambda (k v)

                   (push (cons k v) top-words))
                 word-counts)
        (setq top-words (sort top-words (lambda (a b) (> (cadr a) (cadr b)))))
        ;; Step 2: Transfer sorted list to file
        (with-current-buffer (generate-new-buffer "*temp*" t)
          (dolist (pair top-words)
            (insert (format "%s\n" (or (caddr pair) (car pair)))))
          (current-buffer))))))

(defun speed-type--gb-retrieve (book-num)
  "Return buffer with book number BOOK-NUM in it."
  (speed-type--retrieve book-num (speed-type--gb-url book-num)))

(defun speed-type--gb-top-filename (book-num)
  "Return name of frequency list of a given book identified by BOOK-NUM."
  (expand-file-name (concat (number-to-string book-num) "-top-x.txt") speed-type-directory))

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
  (let* ((lang-buffer (speed-type--retrieve (concat lang ".html")
                                  (concat "https://www.gutenberg.org/ebooks/search/?query=l." lang)))
         (lang-book-list (with-current-buffer lang-buffer
                           (mapcar (lambda (h) (substring (dom-attr h 'href) 8))
                                   (mapcar (lambda (c) (dom-by-tag c 'a))
                                           (dom-by-class (libxml-parse-html-region (point-min) (point-max) nil) "booklink"))))))
    (kill-buffer lang-buffer)
    (string-to-number (nth (random (length lang-book-list)) lang-book-list))))

(defun speed-type--wordlist-retrieve (lang)
  "Return buffer with wordlist for language LANG in it."
  (let ((legacy-lang
         (cond ((string= "de" lang) 'German)
               ((string= "en" lang) 'English)
               ((string= "fr" lang) 'French)
               ((string= "nl" lang) 'Dutch)
               ((member lang (mapcar #'car speed-type-wordlist-urls)) lang)
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

TIME-REGISTER is a list of time-floats. Must be of length 0 or a even
number. The elements are paired, between the pairs the difference
calculated and summed.

If the length is 0 will return 0.

If the length is uneven will return symbol `uneven'."
  (if (= (% (length time-register) 2) 0)
      (if time-register
          (apply #'+ (mapcar (lambda (time-pair) (- (cdr time-pair) (car time-pair)))
                             (speed-type--list-to-alist-safe time-register)))
        0)
    'uneven))

(defun speed-type--check-same-str (a b)
  "Return non-nil if A and B are identical or both whitespace.

Whitespace is determined using `char-syntax'."
  (when (not (= (length a) (length b))) (user-error "A(%d) and B(%d) must be the same" a b))
  (let ((still-correct t)
        (i 0)
        (length-a (length a)))
    (while (and (< i length-a) still-correct)
      (setq still-correct (speed-type--check-same i a b))
      (setq i (1+ i)))
    still-correct))

(defun speed-type--check-same (pos a b)
  "Return non-nil if A[POS] and B[POS] are identical or both whitespace.

Whitespace is determined using `char-syntax'."
  (let ((ca (aref a pos))
        (cb (aref b pos)))
    (or (= ca cb)
        (and (eq (char-syntax ca) ?\s)
             (eq (char-syntax cb) ?\s)))))

(defun speed-type--handle-del (start end)
  "Keep track of the statistics when a deletion occurs between START and END."
  (unless (and overwrite-mode ;; only insert old char, when not in overwrite-mode or del-key pressed
               (not (= start end))
               (not (eq this-command (key-binding (kbd "<deletechar>"))))
               (not (eq this-command (key-binding (kbd "DEL")))))
    (delete-region start end))
  (setq start (if (<= (point-max) start) (point-max) start))
  (setq end (if (<= (point-max) end) (point-max) end))
  (dotimes (i (- end start))
    (let* ((pos (+ (1- start) i))
           (q (get-text-property (1+ pos) 'speed-type-char-status)))
      (cond ((not q) ())
            ((or (eq q 'correct)
                 (eq q 'error))
             (cl-decf speed-type--entries))))))

(defun speed-type-toggle-preview ()
  "Toggle preview of speed-type session in `current-buffer'.

If preview is currently display in some window will delete that window.
Else if there is no such window, `split-window' and display the preview
in new window."
  (interactive)
  (unless (derived-mode-p 'speed-type-mode) (user-error "Not in a speed-type buffer: cannot open preview"))
  (unless (and (boundp 'speed-type--preview-buffer) speed-type--preview-buffer)
    (speed-type--connect-preview-buffer speed-type--buffer speed-type--content-buffer)
    (message "Preview-buffer connected to current speed-type session."))
  (let ((bw (get-buffer-window speed-type--preview-buffer)))
    (if bw (delete-window bw)
      (let ((sw (selected-window))
            (pw (split-window nil 5 'above))
            (buf (current-buffer)))
        (set-window-buffer sw speed-type--preview-buffer)
        (set-window-buffer pw buf)
        (select-window pw)))))

(defun speed-type--display-statistic ()
  "Display median values from current and past entries."
  (interactive)
  (unless (derived-mode-p 'speed-type-mode) (user-error "Not in a speed-type buffer: cannot execute action"))
  (unless speed-type--max-point-on-complete (speed-type-complete))
  (with-current-buffer speed-type--buffer
    (let ((original-max (point-max)))
      (goto-char original-max)
      (read-only-mode -1)
      (insert (apply #'format speed-type-stats-analysis-format (speed-type--calc-stats (speed-type-load-last-stats speed-type-statistic-filename))))
      (speed-type-display-menu)
      (read-only-mode)
      (goto-char original-max))))

(defun speed-type-quit ()
  "Kill buffer of speed-type session.
Expects CURRENT-BUFFER to be buffer of speed-type session."
  (interactive)
  (unless (derived-mode-p 'speed-type-mode) (user-error "Not in a speed-type buffer: cannot quit session"))
  (unless speed-type--max-point-on-complete (save-excursion (speed-type-complete)))
  (kill-buffer speed-type--buffer))

(defun speed-type--execute-action (action-fn)
  "ACTION-FN is a function which setups a new speed-type session."
  (unless (derived-mode-p 'speed-type-mode) (user-error "Not in a speed-type buffer: cannot execute action"))
  (if action-fn
      (let ((cb (current-buffer)))
        (unless speed-type--max-point-on-complete (save-excursion (speed-type-complete)))
        (let ((ab (funcall action-fn)))
          (kill-buffer cb)
          ab))
    (user-error "Action not available")))

(defun speed-type-replay ()
  "Replay a speed-type session."
  (interactive)
  (speed-type--execute-action speed-type--replay-fn))

(defun speed-type-play-continue ()
  "Play new speed-type-session continuing right where current session ended."
  (interactive)
  (speed-type--execute-action speed-type--continue-fn))

(defun speed-type-play-next ()
  "Play a new speed-type session with random content, based on the current one."
  (interactive)
  (speed-type--execute-action speed-type--go-next-fn))

(defun speed-type--code-buffer-p (mode)
  "Check BUF if we should use code-with-highlighting or treat it as text."
  (provided-mode-derived-p mode speed-type-code-modes))

(defun speed-type--before-change (start end)
  "Store the region between START and END which is going to be modified."
  (setq speed-type--last-changed-text (buffer-substring start end)
        speed-type--last-modified-tick (buffer-chars-modified-tick)))

(defun speed-type-format-stats (entries actions errors non-consecutive-errors corrections best-correct-streak seconds)
  "Format statistic data using given arguments:
ENTRIES ACTIONS ERRORS NON-CONSECUTIVE-ERRORS CORRECTIONS BEST-CORRECT-STREAK SECONDS."
  (format speed-type-stats-format
          (speed-type--skill (speed-type--net-wpm entries errors corrections seconds))
          (speed-type--net-wpm entries errors corrections seconds)
          (speed-type--net-cpm entries errors corrections seconds)
          (speed-type--gross-wpm entries seconds)
          (speed-type--gross-Xpm entries seconds)
          (speed-type--gross-Xpm actions seconds)
          (speed-type--/ (float actions) entries)
          (speed-type--accuracy entries (- entries errors) corrections)
          (format-seconds "%M %z%S" seconds)
          entries
          corrections
          best-correct-streak
          errors
          non-consecutive-errors
          speed-type-explaining-message))

(defun speed-type-complete ()
  "Remove typing hooks from the buffer and print statistics."
  (interactive)
  (unless (derived-mode-p 'speed-type-mode) (user-error "Not in a speed-type buffer: cannot complete session"))
  (remove-hook 'pre-command-hook #'speed-type--action-counter-hook t)
  (remove-hook 'post-command-hook #'speed-type-preview-logger t)
  (speed-type-finish-animation speed-type--buffer)
  (remove-hook 'before-change-functions #'speed-type--before-change t)
  (remove-hook 'after-change-functions #'speed-type--change t)
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
      (insert (speed-type-format-stats
               speed-type--entries
               speed-type--actions
               speed-type--errors
               speed-type--non-consecutive-errors
               speed-type--corrections
               speed-type--best-correct-streak
               (speed-type--elapsed-time speed-type--time-register)))
      (speed-type-display-menu))))

(defun speed-type-preview-logger ()
  "Logs the commands of a speed-type session."
  (unless (or
           (not (boundp 'speed-type--preview-buffer))
           (null speed-type--preview-buffer)
           (null speed-type--time-register)
           (null speed-type--idle-pause-timer)
           (member this-command '(self-insert-command speed-type-code-ret speed-type-code-tab))
           (null this-command))
    (let ((new-last-pos (point)))
      (with-current-buffer speed-type--preview-buffer
        (unwind-protect
            (save-excursion
              (goto-char (point-min))
              (end-of-line)
              (when-let* ((win (get-buffer-window (current-buffer))))
                (set-window-point win (point)))
              (read-only-mode -1)
              (let ((command-str
                     (cond ((eq this-command (key-binding (kbd "<deletechar>"))) "->")
                           ((eq this-command (key-binding (kbd "DEL"))) "<-")
                           ((eq this-command 'isearch-printing-char) (key-description (this-command-keys)))
                           (t (concat "[ " (symbol-name this-command) "(" (number-to-string speed-type--last-position) ") → (" (number-to-string (1- new-last-pos )) ") ]")))))
                (insert command-str)
                (let ((overlay (make-overlay (- (point) (length command-str)) (point))))
                  (overlay-put overlay 'priority 1)
                  (overlay-put overlay 'face 'speed-type-info-face)))
              (setq-local speed-type--last-position new-last-pos))
          (read-only-mode))))))

(defun speed-type-preview-buffer-insert (orig new new-last-pos face)
  "Insert NEW at the end of preview-buffer and set given FACE as overlay.

ORIG is inserted below, when it doesn't match NEW.

POS is used to access the focused char.

NEW-LAST-POS is `point' in `current-buffer', it's used to determine if
movement-commands were used since last insert.

When `current-buffer' has no variable `speed-type--preview-buffer' with non-nil
value return nil."
  (when (and (boundp 'speed-type--preview-buffer) speed-type--preview-buffer)
    (with-current-buffer speed-type--preview-buffer
      (unwind-protect
          (save-excursion
            (goto-char (point-min))
            (end-of-line)
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
            (insert (string-replace "\t" "⇥" (string-replace " " "·" (string-replace "\n" "⏎" new))))
            (let ((overlay (make-overlay (- (point) (length new)) (point))))
              (overlay-put overlay 'priority 1)
              (overlay-put overlay 'face face))
            (when (not (speed-type--check-same-str orig new))
              (let ((inhibit-message t))
                (end-of-line)
                (let ((cc (1- (current-column))))
                  (or (search-forward "\n" nil t 1) (insert "\n"))
                  (move-to-column cc t))
                (insert (string-replace "\t" "⇥" (string-replace " " "·" (string-replace "\n" "⏎" orig))))
                (let ((overlay (make-overlay (- (point) (length orig)) (point))))
                  (overlay-put overlay 'priority 1)
                  (overlay-put overlay 'face 'speed-type-correct-face))))
            (setq-local speed-type--last-position new-last-pos))
        (read-only-mode)))))

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
      (let* ((is-same nil)
             (pos0 (+ start0 i))
             (pos (+ start i))
             (non-consecutive-error-p (or (and (<= pos0 0) (= speed-type--non-consecutive-errors 0)) ;; first char is always a non-consecutive error if counter is 0
                                          (or (and (eq speed-type-point-motion-on-error 'point-stay) (not (eq (get-text-property (1+ pos0) 'speed-type-char-status) 'error))) ;; staying, no movement, check current
                                              (and (> pos0 0) (eq speed-type-point-motion-on-error 'point-move) (not (eq (get-text-property pos0 'speed-type-char-status) 'error))))))) ;; moving, check previous

        (message "new: %s old: %s" (char-to-string (aref new i)) (char-to-string (aref orig i)))

        (if (speed-type--check-same i orig new)
            (progn (setq is-same t)
                   (cl-incf speed-type--current-correct-streak)
                   (when (> speed-type--current-correct-streak speed-type--best-correct-streak)
                     (setq speed-type--best-correct-streak speed-type--current-correct-streak))
                   (let ((char-status (get-text-property i 'speed-type-char-status orig)))
                     (when (eq char-status 'error) (cl-incf speed-type--corrections))
                     (add-text-properties pos (1+ pos) '(speed-type-char-status correct))))
          (progn (unless any-error (setq any-error t))
                 (cl-incf speed-type--errors)
                 (setq speed-type--current-correct-streak 0)
                 (when non-consecutive-error-p (cl-incf speed-type--non-consecutive-errors))
                 (add-text-properties pos (1+ pos) '(speed-type-char-status error))
                 (when overwrite-mode
                   (add-text-properties pos (1+ pos) (list 'speed-type-orig-char (aref orig i))))
                 (speed-type-add-extra-words (+ (or speed-type-add-extra-words-on-error 0)
                                      (or (and non-consecutive-error-p speed-type-add-extra-words-on-non-consecutive-errors) 0)))))
        (cl-incf speed-type--entries)
        (let ((f (if is-same 'speed-type-correct-face (if non-consecutive-error-p 'speed-type-error-face 'speed-type-consecutive-error-face))))
          (let ((overlay (make-overlay pos (1+ pos))))
            (overlay-put overlay 'priority 1)
            (overlay-put overlay 'face f))
          (speed-type-preview-buffer-insert (char-to-string (aref orig i)) (char-to-string (aref new i)) pos f))))
    (if (or (eq speed-type-point-motion-on-error 'point-move)
            (string= new "")
            (not any-error))
        (goto-char (- end (if overwrite-mode 1 0)))
      (goto-char (- end (if overwrite-mode 2 1)))
      (beep)
      (message "Wrong key"))
    (not any-error)))

(defun speed-type--sync-after-undo (start end)
  "Sync overlay between START and END with whatever text-properties appear there.

Undo does not track overlay changes but it does update text-properties.

This function should be called after undo has finished it's job and the
region got new text-property values. This function should not modify the
buffer, else undo ends up in an infinite loop.

`buffer-undo-list' is per buffer which is why point is synced in
content-buffer manually if there is a add-extra-word-function."
  (when-let ((add-word-fn speed-type--add-extra-word-content-fn)
             (orig-end (cdr (get-text-property (1- (point-max)) 'speed-type-orig-pos))))
    (with-current-buffer speed-type--content-buffer (goto-char orig-end)))
  (remove-overlays start end 'face 'speed-type-correct-face)
  (remove-overlays start end 'face 'speed-type-error-face)
  (remove-overlays start end 'face 'speed-type-consecutive-error-face)
  (dotimes (i (- end start))
    (let* ((pos (+ start i))
           (pos0 (+ (1- start) i))
           (non-consecutive-error-p (or (and (<= pos0 0) (= speed-type--non-consecutive-errors 0)) ;; first char is always a non-consecutive error if counter is 0
                                        (or (and (eq speed-type-point-motion-on-error 'point-stay) (not (eq (get-text-property (1+ pos0) 'speed-type-char-status) 'error))) ;; staying, no movement, check current
                                            (and (> pos0 0) (eq speed-type-point-motion-on-error 'point-move) (not (eq (get-text-property pos0 'speed-type-char-status) 'error))))))
           (char-status (get-text-property pos 'speed-type-char-status)))
      (when-let (f (cond ((eq char-status 'correct) 'speed-type-correct-face)
                         ((eq char-status 'error)
                          (if non-consecutive-error-p 'speed-type-error-face 'speed-type-consecutive-error-face))
                         (t nil)))
        (let ((overlay (make-overlay pos (1+ pos))))
          (overlay-put overlay 'priority 1)
          (overlay-put overlay 'face f))))))

(defun speed-type-complete-p ()
  "Check if speed-type buffer is complete."
  (or speed-type--max-point-on-complete
      (and (not (save-excursion (text-property-search-forward 'speed-type-char-status 'nil t)))
           (not (save-excursion (text-property-search-backward 'speed-type-char-status 'nil t)))
           (null speed-type--extra-words-queue)
           (not (text-property-any (point-min) (point-max) 'speed-type-char-status 'nil))
           (or (not speed-type-complete-all-correct)
               (and speed-type-complete-all-correct
                    (not (save-excursion (text-property-search-forward 'speed-type-char-status 'error t)))
                    (not (save-excursion (text-property-search-backward 'speed-type-char-status 'error t)))
                    (not (text-property-any (point-min) (point-max) 'speed-type-char-status 'error)))))))

(defun speed-type--swap-orig-char (str)
  "Read any text-property: speed-type-oirg-char in STR and make a new string."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((m nil))
      (while (setq m (text-property-search-forward 'speed-type-orig-char nil (lambda (_ v) v)))
        (let* ((p-orig-char (apply 'propertize (char-to-string (get-text-property (prop-match-beginning m) 'speed-type-orig-char)) (text-properties-at (prop-match-beginning m))))
               (new-orig-char (char-after (prop-match-beginning m))))
          (delete-region (prop-match-beginning m) (prop-match-end m))
          (insert p-orig-char)
          (put-text-property (prop-match-beginning m) (prop-match-end m) 'speed-type-orig-char new-orig-char))))
    (buffer-string)))

(defun speed-type--change (start end _length)
  "Handle buffer change between START and END.
LENGTH is ignored. Used for hook AFTER-CHANGE-FUNCTIONS.
Make sure that the contents don't actually change, but rather the contents
are color coded and stats are gathered about the typing performance."
  (cond (undo-in-progress (speed-type--sync-after-undo start end))
        ((or (member this-command '(fill-paragraph))
             (= speed-type--last-modified-tick (buffer-chars-modified-tick)))
         nil)
        (t (progn
             (speed-type--resume)
             (let ((new-text (buffer-substring start end))
                   (old-text speed-type--last-changed-text))
               (speed-type--handle-del start end)
               (unless (and overwrite-mode ;; only insert old char, when not in overwrite-mode or del-key pressed
                            (not (eq this-command (key-binding (kbd "<deletechar>"))))
                            (not (eq this-command (key-binding (kbd "DEL")))))
                 (insert (speed-type--swap-orig-char old-text)))
               (if (< start (point-max))
                   (let* ((end (if (> end (point-max)) (point-max) end))
                          (orig (if overwrite-mode old-text (buffer-substring start end))))
                     (when-let* ((overlay (and (equal new-text "") (car (overlays-at end)))))
                       (move-overlay overlay (1- (overlay-end overlay)) (overlay-end overlay)) (current-buffer))
                     (unless (string-blank-p orig) (speed-type--diff (speed-type--swap-orig-char orig) new-text start end))
                     (when (speed-type-complete-p) (speed-type-complete)))
                 (beep)
                 (message "End of buffer")))))))

(defun speed-type-forward-replace-map-adjust-properties (map property end)
  "Replace each FROM/TO pair in MAP while adjusting PROPERTY regions.
MAP is a list of (FROM . TO) string pairs.
PROPERTY is the text property to preserve/adjust around the replaced region.

Bounds search to END.

Optional OBJECT can be a string. It will be inserted into a temp-buffer
and property adjusted. A new string is returned with adjusted
properties."
  (let ((preset (point)))
    (dolist (pair map)
      (let ((from (car pair))
            (to   (cdr pair)))
        (goto-char preset)
        (while (search-forward from end t 1)
          (let* ((mstart (match-beginning 0))
                 (mend   (match-end 0))
                 (prop-start (previous-single-property-change mstart property speed-type--buffer (point-min)))
                 (prop-end   (next-single-property-change mend property speed-type--buffer (point-max)))
                 (len-from (- mend mstart))
                 (len-to   (length to))
                 (old-property-value (get-text-property mstart property)))
            (replace-match to t t)

            (cond
             ;; TO longer → extend property region
             ((> len-to len-from)
              (setq end (+ end (- len-to len-from)))
              (put-text-property
               prop-start (+ prop-end (- len-to len-from))
               property old-property-value))
             ;; FROM longer → shrink property region
             ((< len-to len-from)
              (setq end (- end (- len-from len-to)))
              (let ((new-prop-end (- prop-end (- len-from len-to))))
                (when (and (> len-to 0) (> new-prop-end prop-start))
                  (put-text-property
                   prop-start new-prop-end
                   property old-property-value))))
             (t (put-text-property prop-start prop-end property old-property-value)))))))))

(defun speed-type-re-forward-replace-map-adjust-properties (map property end)
  "Replace each FROM/TO pair in MAP while adjusting PROPERTY regions.
MAP is a list of (FROM . TO) string pairs.
PROPERTY is the text property to preserve/adjust around the replaced region.

Bounds search to END.

Optional OBJECT can be a string. It will be inserted into a temp-buffer
and property adjusted. A new string is returned with adjusted
properties."
  (let ((preset (point)))
    (dolist (pair map)
      (let ((from (car pair))
            (to   (cdr pair)))
        (goto-char preset)
        (while (re-search-forward from end t 1)
          (let* ((mstart (match-beginning 0))
                 (mend   (match-end 0))
                 (prop-start (previous-single-property-change mstart property speed-type--buffer (point-min)))
                 (prop-end   (next-single-property-change mend property speed-type--buffer (point-max)))
                 (len-from (- mend mstart))
                 (replacement (match-substitute-replacement to t))
                 (len-to   (length replacement))
                 (old-property-value (get-text-property mstart property)))
            (replace-match replacement t t)

            (cond
             ;; TO longer → extend property region
             ((> len-to len-from)
              (setq end (+ end (- len-to len-from)))
              (put-text-property
               prop-start (+ prop-end (- len-to len-from))
               property old-property-value))

             ;; FROM longer → shrink property region
             ((< len-to len-from)
              (setq end (- end (- len-from len-to)))
              (let ((new-prop-end (- prop-end (- len-from len-to))))
                (when (and (> len-to 0) (> new-prop-end prop-start))
                  (put-text-property
                   prop-start new-prop-end
                   property old-property-value))))
             (t (put-text-property prop-start prop-end ',property old-property-value)))))))))

(defun speed-type--connect-preview-buffer (s-buf c-buf)
  "Create preview-buffer and connect it to S-BUF and C-BUF.

S-BUF is the speed-type buffer which runs the speed-type session.

C-BUF is the content buffer which holds the content from which the
speed-type session has been started.

With connect means a kill-hook is added which kills the preview-buffer
if one of the other buffers are killed and vice versa.

Returns the preview-buffer."
  (let ((p-buf (generate-new-buffer speed-type-preview-buffer-name)))
    (with-current-buffer s-buf
      (setq-local speed-type--preview-buffer p-buf))
    (with-current-buffer c-buf
      (setq-local speed-type--preview-buffer p-buf))
    (with-current-buffer p-buf
      (setq-local speed-type--buffer s-buf
                  speed-type--content-buffer c-buf
                  speed-type--last-position 0
                  truncate-lines t)
      (speed-type-mode)
      (add-hook 'kill-buffer-hook #'speed-type--kill-preview-buffer-hook nil t)
      (read-only-mode))
    p-buf))

(cl-defun speed-type--setup
    (content-buffer start end text-type &key file-name title author lang n-words randomize continue-fn add-extra-word-content-fn replay-fn go-next-fn syntax-table fldf)
  "Set up a new buffer for the typing exercise on TEXT.

TITLE and AUTHOR can be given, this happen when the text to type comes
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
    (setq-local speed-type--file-name file-name
                speed-type--title title
                speed-type--author author
                speed-type--lang lang
                speed-type--n-words n-words
                speed-type--randomize randomize
                speed-type--continue-fn continue-fn
                speed-type--go-next-fn go-next-fn
                speed-type--replay-fn replay-fn
                speed-type--text-type text-type)
    (when content-buffer
      (setq-local speed-type--content-buffer (or (with-current-buffer content-buffer (when (boundp 'speed-type--content-buffer) speed-type--content-buffer)) content-buffer)
                  speed-type--buffer buf))
    (unless (speed-type--code-buffer-p (with-current-buffer speed-type--content-buffer major-mode))
      (setq-local speed-type--add-extra-word-content-fn add-extra-word-content-fn))
    (with-current-buffer speed-type--content-buffer
      (setq-local speed-type--buffer buf)
      (when (null (boundp 'speed-type--extra-word-quote))
        (setq-local speed-type--extra-word-quote nil)))
    (when speed-type-provide-preview-option (speed-type--connect-preview-buffer buf speed-type--content-buffer))
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (inhibit-modification-hooks t)
          (inhibit-field-text-motion t)
          (transform-context (make-speed-type-transform-context :major-mode (with-current-buffer speed-type--content-buffer major-mode)
                                                                :text-type speed-type--text-type
                                                                :start nil
                                                                :end nil
                                                                :file-name speed-type--file-name
                                                                :entries speed-type--entries
                                                                :errors speed-type--errors
                                                                :non-consecutive-errors speed-type--non-consecutive-errors
                                                                :corrections speed-type--corrections
                                                                :best-correct-streak speed-type--best-correct-streak)))
      (cond ((eq text-type 'transform-text)
             (speed-type-insert-text content-buffer start end speed-type-ignore-whitespace-for-complete speed-type-transform-hook transform-context))
            ((eq text-type 'random-wordlist)
             (speed-type-insert-wordlist content-buffer start end speed-type-min-chars speed-type-max-chars speed-type-text-picker-tolerance speed-type-ignore-whitespace-for-complete speed-type-transform-hook transform-context))
            ((eq text-type 'random-text-section)
             (speed-type-insert-text-section
              content-buffer
              (with-current-buffer content-buffer
                (goto-char (random (+ start (- end start))))
                (skip-syntax-backward "^\s" start) (point))
              end speed-type-min-chars speed-type-max-chars speed-type-text-picker-tolerance speed-type-ignore-whitespace-for-complete speed-type-transform-hook transform-context))
            ((eq text-type 'quote)
             (speed-type-insert-quote content-buffer speed-type-min-chars speed-type-max-chars speed-type-ignore-whitespace-for-complete speed-type-transform-hook transform-context))
            ((eq text-type 'continue-text-section)
             (speed-type-insert-text-section content-buffer start end speed-type-min-chars speed-type-max-chars speed-type-text-picker-tolerance speed-type-ignore-whitespace-for-complete speed-type-transform-hook transform-context))))
    (set-buffer-modified-p nil)
    (switch-to-buffer buf)
    (when (eq speed-type-provide-preview-option t)
      (speed-type-toggle-preview))
    (goto-char 0)
    (add-hook 'before-change-functions #'speed-type--before-change nil t)
    (add-hook 'post-command-hook #'speed-type-preview-logger nil t)
    (add-hook 'pre-command-hook #'speed-type--action-counter-hook nil t)
    (add-hook 'after-change-functions #'speed-type--change nil t)
    (add-hook 'kill-buffer-hook #'speed-type--kill-buffer-hook nil t)
    (setq-local post-self-insert-hook nil)
    (when (speed-type--code-buffer-p (with-current-buffer speed-type--content-buffer major-mode))
      (electric-pair-mode -1)
      (when syntax-table (set-syntax-table syntax-table))
      (when fldf
        (let ((font-lock-defaults fldf)
              (major-mode-value (buffer-local-value 'major-mode speed-type--content-buffer)))
          (with-current-buffer speed-type--content-buffer
            (dolist (bl-var (seq-filter
                             (lambda (var) (string-prefix-p (symbol-name major-mode-value) (symbol-name (car var))))
                             (buffer-local-variables)))
              (with-current-buffer speed-type--buffer
                (set (make-local-variable (car bl-var)) (cdr bl-var)))))
          ;; Fontify buffer
          (ignore-errors (font-lock-ensure)))))
    (message "Timer will start when you type the first character.")
    (when (speed-type-complete-p)
      (speed-type-complete)
      (message "Speed-type: Completed speed-type session because buffer is empty"))
    buf))

(defun speed-type-prepare-content-buffer-from-buffer (buffer &optional start end)
  "Prepare content-buffer from existing BUFFER.

START and END are supplied to `insert-buffer-substring'."
  (let ((buf (generate-new-buffer speed-type-content-buffer-name)))
    (with-current-buffer buf
      (setq-local inhibit-read-only t
                  buffer-undo-list t
                  inhibit-modification-hooks t
                  inhibit-field-text-motion t)
      (add-hook 'kill-buffer-hook #'speed-type--kill-content-buffer-hook nil t)
      (insert-buffer-substring buffer start end)
      (when (speed-type--code-buffer-p (with-current-buffer buffer major-mode))
        (condition-case nil
            (funcall (buffer-local-value 'major-mode buffer))
          (prog-mode)
          (message "speed-type: could not call major-mode, fallback prog-mode"))
        (set-syntax-table (with-current-buffer buffer (syntax-table)))
        (setq font-lock-defaults (with-current-buffer buffer font-lock-defaults))
        (ignore-errors (font-lock-ensure)))
      (goto-char (point-min)))
    (get-buffer-create buf)))

(defun speed-type--kill-buffer-hook ()
  "Hook when speed-type buffer is killed."
  (when speed-type--idle-pause-timer (cancel-timer speed-type--idle-pause-timer))
  (when speed-type--extra-words-animation-timer (cancel-timer speed-type--extra-words-animation-timer))
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

(defun speed-type--action-counter-hook ()
  "Count actions on `pre-command-hook'."
  (unless (or (null speed-type--time-register) (null speed-type--idle-pause-timer) (null this-command))
    (setq speed-type--actions (+ speed-type--actions 1))))

(cl-defstruct speed-type-transform-context
  "Define what context-variable are present when running transform-hook.

See also `speed-type-transform-hook'."
  major-mode
  text-type
  start
  end
  file-name
  (entries 0)
  (errors 0)
  (non-consecutive-errors 0)
  (corrections 0)
  (best-correct-streak 0))

(defun speed-type--filter-stop-word-hook (transform-context &rest _)
  "Remove words from an random-wordlist which are on the stop-list.

TRANSFORM-CONTEXT is used to detect the text-type."
  (when (eq 'random-wordlist (speed-type-transform-context-text-type transform-context))
    (goto-char (point-min))
    (while (not (eobp))
      (skip-syntax-forward "\s")
      (let* ((word-start (point))
             (word-end (progn (skip-syntax-forward "^\s") (point)))
             (word (buffer-substring word-start word-end)))
        (when (or (string-blank-p word) (speed-type--stop-word-p word))
          ;; we need to delete the space with it so no double space are left
          (delete-region (- word-start (if (or (= 1 word-start) (bobp)) 0 1)) word-end))))))

(defun speed-type--delete-trailing-whitespace-hook (transform-context &rest _)
  "Deletes trailing whitespace if ignore is active."
  (when (and speed-type-ignore-whitespace-for-complete
             (<= (speed-type-transform-context-errors transform-context) 0) ;; add word triggered this hook, exclude
             )
    (delete-trailing-whitespace (point) (point-max))))

(defun speed-type--left-align-hook (transform-context &rest _)
  "Deletes any space that appears on the left side.

TRANSFORM-CONTEXT is used to determine if code buffer."
  (when (and (not (speed-type--code-buffer-p (speed-type-transform-context-major-mode transform-context)))
             (<= (speed-type-transform-context-errors transform-context) 0) ;; add word triggered this hook, exclude
             )
    (speed-type-re-forward-replace-map-adjust-properties '(("^[[:blank:]]+" . "")) 'speed-type-orig-pos (speed-type-transform-context-end transform-context))))

(defun speed-type--fill-region-hook (transform-context &rest _)
  "Apply `fill-region' to the whole text.

TRANSFORM-CONTEXT is used to determine if code buffer."
  (unless (speed-type--code-buffer-p (speed-type-transform-context-major-mode transform-context))
    (fill-region (speed-type-transform-context-start transform-context) (speed-type-transform-context-end transform-context) 'none t)))

(defun speed-type--downcase-hook (transform-context &rest _)
  "Make text downcase.

TRANSFORM-CONTEXT is used to specify the region."
  (when speed-type-downcase (downcase-region (speed-type-transform-context-start transform-context) (speed-type-transform-context-end transform-context))))

(defun speed-type--replace-regex-hook (transform-context &rest _)
  "Apply `speed-type-replace-strings' replacing regex with other.

TRANSFORM-CONTEXT is used to bound search to end.

See also `speed-type-replace-regexs'."
  (speed-type-re-forward-replace-map-adjust-properties speed-type-replace-regexs 'speed-type-orig-pos (speed-type-transform-context-end transform-context)))

(defun speed-type--replace-string-hook (transform-context &rest _)
  "Apply `speed-type-replace-strings' replacing strings with other.

TRANSFORM-CONTEXT is used to bound search to end.

See also `speed-type-replace-strings'"
  (speed-type-forward-replace-map-adjust-properties speed-type-replace-strings 'speed-type-orig-pos (speed-type-transform-context-end transform-context)))

(defun speed-type-prepare-string (str ignore trim transform-hook transform-context)
  "Prepare STR with `speed-type-prepare-buffer'.

IGNORE TRIM TRANSFORM-HOOK TRANSFORM-CONTEXT are supplied to
`speed-type-prepare-buffer'."
  (with-temp-buffer
    (let ((point-before-insert (point)))
      (insert str)
      (speed-type-prepare-buffer point-before-insert (point) trim ignore transform-hook transform-context))
    (buffer-string)))

(defun speed-type-prepare-buffer (start end trim ignore transform-hook transform-context)
  "Prepare `current-buffer' so it can be used for speed-type session.

It first applies all TRANSFORM-HOOK with TRANSFORM-CONTEXT. See also
`speed-type-transform-context'. After each hook-function point is moved back to
start. Due to TRANSFORM-HOOK can arbitrary function this function does
not guarantee what the resulting `buffer-size' will be. See also
`speed-type-transform-hook'.

If there are still wordlist-transform-functions they are executed
afterwards. They are obsolete and TRANSFORM-HOOK should be used.

If TRIM is non-nil, will trim the beginning and end of buffer.

If IGNORE is non-nil, will add text-property `(speed-type-char-status ignore) to
all whitespace characters from START to END.

To prevent point-movement use `save-excurison'.

This function returns a triple: `(START END IGNORE-COUNT). If IGNORE is nil,
IGNORE-COUNT will always be 0.

END may be modified if TRANSFORM-HOOK extended the region."
  (when (and (null transform-context) transform-hook)
    (error "TRANSFORM-CONTEXT can't be nil if TRANSFORM-HOOK supplied"))
  (when transform-context
    (setf (speed-type-transform-context-start transform-context) start
          (speed-type-transform-context-end transform-context) end))
  (when (and transform-hook (sequencep transform-hook))
    (dolist (fn transform-hook)
      (when (functionp fn)
        (goto-char start)
        (let ((before-point-max (point-max)))
          (funcall fn transform-context)
          (when (not (= before-point-max (point-max))) ;; extend or shrink end, if transform-function added/removed text
            (setq end
                  (if (< before-point-max (point-max))
                      (+ end (- (point-max) before-point-max))
                    (- end (- before-point-max (point-max)))))
            (setf (speed-type-transform-context-end transform-context) end))))))
  (when speed-type-wordlist-transform ;; obsolete, stay compatible, better us transform-hook instead
    (let ((wordlist-transform-result (funcall speed-type-wordlist-transform (buffer-string))))
      (delete-region (point-min) (point-max))
      (insert wordlist-transform-result)))
  (when trim
    (if (and transform-context (speed-type--code-buffer-p (speed-type-transform-context-major-mode transform-context)))
        (progn
          (goto-char start)
          (delete-region (point)
                         (progn (skip-chars-forward "\n") (point)))
          (goto-char end)
          (delete-region (save-excursion (skip-syntax-backward "\s") (point)) (point)))
      (goto-char start)
      (delete-region (point) (progn (skip-syntax-forward "\s") (point)))
      (goto-char end)
      (delete-region (save-excursion (skip-syntax-backward "\s") (point)) (point))))
  (let ((whitespace-count 0))
    (when ignore
      (goto-char start)
      (while (search-forward-regexp "[[:blank:]\n]+" end t 1)
        (let ((match-start (match-beginning 0))
              (match-end (match-end 0)))
          (add-text-properties match-start match-end '(speed-type-char-status ignore))
          (setq whitespace-count (+ whitespace-count (- match-end match-start))))))
    (goto-char end)
    (list start end whitespace-count)))

(defun speed-type-insert-wordlist (cbuf start end min max tolerance ignore &optional transform-hook transform-context)
  "Generate a random word-list with words between START and END in CBUF.

The resulting word-list will have a random-length between MIN and MAX.

The resulting length will not exceed MAX + TOLERANCE.

IGNORE TRANSFORM-HOOK TRANSFORM-CONTEXT are supplied to
`speed-type-prepare-buffer'."
  (unless (= end start)
    (let ((target (+ min (if (< min max) (random (- max min)) 0))))
      (let* ((bounds)
             (hard-upper-limit (+ max tolerance))
             (chars 0)
             (empty-content-escape-counter 0))
        (while (and (< chars target)
                    (< empty-content-escape-counter 25))
          (let ((point-before-insert (point)))
            (when (= (point) point-before-insert)
              (setq empty-content-escape-counter (1+ empty-content-escape-counter)))
            (while (< (- (point) point-before-insert) (- target chars))
              (setq bounds
                    (with-current-buffer cbuf
                      (goto-char (+ start (random (- end start))))
                      ;; we use skip-syntax-forward because we don't expect
                      ;; that the words are actually newline-separated
                      ;; we don't use bounds-of-thing-at-point because
                      ;; 'word doesn't include quotes (like e.g. don't => don)
                      (skip-syntax-backward "^\s") ;; if random-point is in the middle of a word, jump back at start of word
                      (skip-syntax-forward "\s") ;; if in the middle of whitespace, jump to start of next word
                      (list (point) (progn (skip-syntax-forward "^\s") (point)))))
              (unless (or (bobp) (= (car bounds) (cadr bounds))) (insert " "))
              (with-current-buffer cbuf (speed-type--put-text-property-orig-pos (car bounds) (cadr bounds)))
              (insert-buffer-substring cbuf (car bounds) (cadr bounds)))
            (let ((r (speed-type-prepare-buffer point-before-insert (point) nil ignore transform-hook transform-context)))
              (setq chars (+ chars (- (cadr r) (car r) (caddr r)))))))
        (when (> chars hard-upper-limit)
          (let ((back-chars (save-excursion (skip-syntax-backward "^\s" start))))
            (setq chars (+ chars back-chars)))
          (and-let* ((ignore-flag (not ignore))
                     (whitespace-back-chars (save-excursion (skip-syntax-backward "\s" start))))
            (setq chars (+ chars whitespace-back-chars)))
          (if (< chars min)
              (delete-char (- (- (point) (- (point) min)) (point)))
            (let ((back-chars (save-excursion (skip-syntax-backward "^\s" start))))
              (delete-char back-chars))
            (and-let* ((ignore-flag ignore)
                       (whitespace-back-chars (save-excursion (skip-syntax-backward "\s" start))))
              (delete-char whitespace-back-chars))))
        (when (> empty-content-escape-counter 20)
          (message "Speed-type: Found no fitting content after 20+ iterations"))))))

(defun speed-type-insert-text (cbuf start end ignore &optional transform-hook transform-context)
  "Insert text between START and END from CBUF into `current-buffer'.

The inserted text will have text-property
`(speed-type-orig-pos (WORD-START . WORD-END)) on all characters.

IGNORE TRANSFORM-HOOK TRANSFORM-CONTEXT are supplied to
`speed-type-prepare-buffer'."
  (with-current-buffer cbuf (speed-type--put-text-property-orig-pos start end))
  (let ((point-before-insert (point)))
    (insert-buffer-substring cbuf start end)
    (speed-type-prepare-buffer point-before-insert (point) nil ignore transform-hook transform-context)))

(defun speed-type-insert-quote (cbuf min max ignore &optional transform-hook transform-context)
  "Insert quotes from CBUF into `current-buffer'.

Expects CBUF to contain the html-result one of `speed-type-quote-urls'.

The random-target-length is picked between MIN and MAX.

Each call picks a random-target-length and inserts as many
quotes as possible that fit into the random-target-length.

IGNORE TRANSFORM-HOOK TRANSFORM-CONTEXT are supplied to
`speed-type-prepare-buffer'."
  (let ((target (+ min (if (< min max) (random (- max min)) 0)))
        (dom-quotes (with-current-buffer cbuf
                      (dom-by-class (dom-by-class (libxml-parse-html-region (point-min) (point-max) nil) "list-quotes") "title"))))
    (let* ((chars 0))
      (while (< chars target)
        (let ((point-before-insert (point))
              (random-quote (dom-text (nth (random (length dom-quotes)) dom-quotes))))
          (unless (bobp) (insert " "))
          (insert random-quote)
          (let ((r (speed-type-prepare-buffer point-before-insert (point) nil ignore transform-hook transform-context)))
            (setq chars (+ chars (- (cadr r) (car r) (caddr r))))))))))

(defun speed-type-insert-text-section (cbuf start end min max tolerance ignore &optional transform-hook transform-context)
  "Insert text-section into `current-buffer' where `point' is currently at.

CBUF defines the content-buffer from which to pick the text-section. To
prevent inconsistent behaviour it's discouraged to use `current-buffer'
as CBUF. MIN MAX START END MIN MAX TOLERANCE IGNORE are supplied to
`speed-type--pick-continue-text-bounds' and act on CBUF.

For each call this function uses a random-target-length which is between
MIN and MAX. In general it's undefined what the actual length will be.
You can supply no TRANSFORM-HOOK than length <= MAX + TOLERANCE.

TRANSFORM-HOOK are called with TRANSFORM-CONTEXT after picking text. If
the resulting text-section shrinks under the random-target-length, will
continue to pick text and apply the TRANSFORM-HOOK until
random-target-length or end is reached.

The picked text-section are prepared using `speed-type-prepare-buffer'."
  (setq start (if (= start 0) 1 start))
  (with-current-buffer cbuf (goto-char start))
  (let* ((bounds)
         (target (+ min (if (< min max) (random (- max min)) 0)))
         (chars 0)
         (point-before-insert (point)))
    (while (and (< chars target)
                (or (null bounds) (not (= (car bounds) (cadr bounds))))
                (not (= (max (- max chars) 0) 0))
                (not (with-current-buffer cbuf (eobp))))
      (let ((min-remaining (max (- min chars) 0))
            (max-remaining (max (- max chars) 0)))
        (setq bounds (with-current-buffer cbuf (speed-type--pick-continue-text-bounds (point) end min-remaining max-remaining tolerance ignore)))
        (with-current-buffer cbuf (speed-type--put-text-property-orig-pos (car bounds) (cadr bounds)))
        (insert-buffer-substring cbuf (car bounds) (cadr bounds))
        (let ((r (speed-type-prepare-buffer point-before-insert (point) t ignore transform-hook transform-context)))
          (setq chars (+ chars (- (cadr r) (car r) (caddr r)))))))))

(defun speed-type--pick-continue-text-bounds (start end min max tolerance ignore)
  "Pick bounds for text with an approximated length between MIN and MAX.

Moves point to START and begins picking text moving point forward until
text has approximate length or END is reached. The approximated length
is randomly picked between MIN and MAX.

This function moves point only forward, if END is reached, will halt and
ignore MIN and TOLERANCE.

The actual length of the picked text is random and can exceed MAX by
given TOLERANCE characters. How much exactly depends on the
word-constellation of the text but it will not exceed MAX + TOLERANCE.

If IGNORE is non-nil, will ignore whitespace which will not account to
the approximated length.

If START and END are the same and TOLERANCE is zero will just return
\(START END).

Use `save-excursion' to prevent point-movement."
  (dolist (arg `((START . ,start) (END . ,end) (MIN . ,min) (MAX . ,max) (TOLERANCE . ,tolerance)))
    (unless (integerp (cdr arg)) (error "%s must be an integer" (car arg)))
    (unless (>= (cdr arg) 0) (error "%s(%d) must be >= 0" (car arg) (cdr arg))))
  (when (or (< start (1- (point-min)))
            (> end (point-max))
            (> start end))
    (error "Invalid START(%d < %s)/END(%d > %d) range" start (point-min) end (point-max)))
  (when (> min max) (error "MIN(%d) must be <= MAX(%d)" min max))
  (setq start (if (= start 0) 1 start))
  (goto-char start)
  (cond ((and (= tolerance 0) (<= max 0)) (list start start))
        ((and (= tolerance 0) (= start end)) (list start end))
        (t (let ((target (+ min (if (< min max) (random (- max min)) 0)))
                 (hard-upper-limit (+ max tolerance))
                 (count 0))
             (while (and (< (point) end)
                         (< count target))
               (and-let* ((whitespace-count (skip-syntax-forward "\s" end))
                          (ignore-flag (not ignore)))
                 (setq count (+ count whitespace-count)))
               (setq count (+ count (skip-syntax-forward "^\s"  end))))
             (when (> count hard-upper-limit)
               (setq count (+ count (skip-syntax-backward "^\s" start)))
               (and-let* ((whitespace-count (skip-syntax-backward "\s" start))
                          (ignore-flag (not ignore)))
                 (setq count (+ count whitespace-count)))
               (setq count (+ count (skip-syntax-backward "^\s" start))))
             (when (< count min)
               (if (<= end (+ (point) (- min count)))
                   (goto-char end)
                 (forward-char (- hard-upper-limit count))))
             (list start (point))))))

(defun speed-type--put-text-property-orig-pos (start end)
  "Propertize the given region from START to END with orig-pos."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let* ((before (point))
             (start (progn (skip-syntax-forward "\s" end) (point)))
             (inv-skipped (progn (skip-syntax-forward "^\s" end) (point))))
        (when (< before start)
          (put-text-property before start 'speed-type-orig-pos (cons before start)))
        (put-text-property start inv-skipped 'speed-type-orig-pos (cons start inv-skipped))))))

(defun speed-type--pick-random-text-bounds (start end min max tolerance ignore)
  "Pick random bounds for text with an approximated length between MIN and MAX.

Will move to random `point' between START and END then move back to
previous space. From there call `speed-type--pick-continue-text-bounds'.

END MIN MAX TOLERANCE IGNORE are supplied to
`speed-type--pick-continue-text-bounds'."
  (goto-char (random (+ start (- end start))))
  (skip-syntax-backward "^\s" start)
  (speed-type--pick-continue-text-bounds (point) end min max tolerance ignore))

(defun speed-type--get-continue-point ()
  "Get speed-type-orig-pos of the last char in the first typed sequence."
  (save-excursion
    (goto-char (point-min))
    (or (text-property-search-forward 'speed-type-char-status 'nil t)
        (goto-char (point-max)))
    (text-property-search-backward 'speed-type-char-status 'nil t)
    (if (or (bolp)
            (eq (char-syntax (char-before (point))) ?\s))
        (car (get-text-property (point) 'speed-type-orig-pos))
      (cdr (get-text-property (1- (point)) 'speed-type-orig-pos)))))

(defun speed-type--get-continue-fn (end)
  "Return a function when evaled continues where current-speed-type session end.

END is where the content in content-buffer ends. It's not always
`point-max', e.g. in a gutenberg-buffer. This prevents continuing over
actual content."
  (let* ((content-buffer speed-type--content-buffer)
         (start (or (speed-type--get-continue-point) 1)))
    (when (and speed-type--preview-buffer (get-buffer-window speed-type--preview-buffer))
      (delete-window (get-buffer-window speed-type--preview-buffer)))
    (when speed-type--preview-buffer
      (with-current-buffer speed-type--preview-buffer
        (setq-local speed-type--content-buffer nil)))
    (setq-local speed-type--content-buffer nil)
    (speed-type--setup content-buffer
             start
             end
             'continue-text-section
             :file-name speed-type--file-name
             :title speed-type--title
             :author speed-type--author
             :randomize speed-type--randomize
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn speed-type--go-next-fn
             :continue-fn (lambda () (speed-type--get-continue-fn end))
             :add-extra-word-content-fn speed-type--add-extra-word-content-fn
             :syntax-table (with-current-buffer content-buffer (syntax-table))
             :fldf (with-current-buffer content-buffer font-lock-defaults))))

(defun speed-type--get-next-top-fn (x)
  "Return a function which create a new speed-type session picks random sample.

X is the user-picked limit for the random-function."
  (let* ((content-buffer speed-type--content-buffer)
         (n (min x (with-current-buffer speed-type--content-buffer (count-words (point-min) (point-max))))))
    (when (and speed-type--preview-buffer (get-buffer-window speed-type--preview-buffer))
      (delete-window (get-buffer-window speed-type--preview-buffer)))
    (setq-local speed-type--content-buffer nil)
    (when speed-type--preview-buffer
      (with-current-buffer speed-type--preview-buffer
        (setq-local speed-type--content-buffer nil)))
    (speed-type--setup content-buffer
             (point-min)
             (with-current-buffer content-buffer (save-excursion (goto-char (point-min)) (beginning-of-line (+ 1 n)) (point)))
             'random-wordlist
             :file-name speed-type--file-name
             :title speed-type--title
             :randomize speed-type--randomize
             :n-words n
             :replay-fn speed-type--replay-fn
             :go-next-fn speed-type--go-next-fn
             :continue-fn speed-type--continue-fn
             :add-extra-word-content-fn speed-type--add-extra-word-content-fn
             :syntax-table (with-current-buffer content-buffer (syntax-table))
             :fldf (with-current-buffer content-buffer font-lock-defaults))))

(defun speed-type--get-replay-fn ()
  "Return a replay function which will use GO-NEXT-FN after completion."
  (let ((content-buffer speed-type--content-buffer))
    (when (and speed-type--preview-buffer (get-buffer-window speed-type--preview-buffer))
      (delete-window (get-buffer-window speed-type--preview-buffer)))
    (when speed-type--preview-buffer
      (with-current-buffer speed-type--preview-buffer
        (setq-local speed-type--content-buffer nil)))
    (read-only-mode -1)
    (remove-text-properties (point-min) speed-type--max-point-on-complete '(speed-type-char-status nil))
    (let ((cb (current-buffer))
          (buf (speed-type--setup (current-buffer)
                        (point-min)
                        speed-type--max-point-on-complete
                        'transform-text
                        :lang speed-type--lang
                        :file-name speed-type--file-name
                        :title speed-type--title
                        :author speed-type--author
                        :n-words speed-type--n-words
                        :randomize speed-type--randomize
                        :replay-fn #'speed-type--get-replay-fn
                        :go-next-fn speed-type--go-next-fn
                        :continue-fn speed-type--continue-fn
                        :add-extra-word-content-fn speed-type--add-extra-word-content-fn
                        :syntax-table (with-current-buffer content-buffer (syntax-table))
                        :fldf (with-current-buffer content-buffer font-lock-defaults))))
      (with-current-buffer cb (setq-local speed-type--content-buffer nil))
      buf)))

(defun speed-type--get-next-word-rolling (content-buffer)
  "Get next word from point in CONTENT-BUFFER.

If at `point-max' move to `point-min'."
  (with-current-buffer content-buffer
    (when (eobp) (goto-char (point-min))))
  (let ((next-word (speed-type--get-next-word content-buffer))) next-word))

(defun speed-type--get-next-word (content-buffer)
  "Get next word from point in CONTENT-BUFFER."
  (with-current-buffer content-buffer
    (let* ((bounds (save-excursion (speed-type--pick-continue-text-bounds (point) (point-max) 20 20 speed-type-text-picker-tolerance t)))
           (text-section (progn (speed-type--put-text-property-orig-pos (car bounds) (cadr bounds))
                                (buffer-substring (car bounds) (cadr bounds))))
           (first-word (string-trim (or (car (split-string text-section "[ \t\r\n]" t)) ""))))
      (unless (string-blank-p first-word)
        (goto-char (cdr (get-text-property (1- (length first-word)) 'speed-type-orig-pos first-word))))
      first-word)))

(defun speed-type--get-random-word-rolling (content-buffer limit)
  "Get random word in CONTENT-BUFFER.
LIMIT is supplied to the random-function."
  (with-current-buffer content-buffer
    (let* ((bounds (save-excursion (speed-type--pick-random-text-bounds (point-min) (save-excursion (beginning-of-line (+ 1 limit)) (point)) 20 20 20 t)))
           (text-section (progn (speed-type--put-text-property-orig-pos (car bounds) (cadr bounds))
                                (buffer-substring (car bounds) (cadr bounds))))
           (words (split-string text-section "[ \t\r\n]" t))
           (word-count (length words))
           (random-word (string-trim (nth (1- word-count) words))))
      random-word)))

(defun speed-type-add-extra-words (x)
  "Add X extra words of text to be typed for the typing-session to be complete."
  (when (and (> x 0) speed-type--add-extra-word-content-fn)
    (let ((words nil))
      (dotimes (_ x)
        (let ((word (funcall speed-type--add-extra-word-content-fn)))
          (if (string-blank-p word)
              (message "You got lucky! Extra word function resulted in empty string.")
            (push word words))))
      (when words
        (let ((prepared-words-as-string
               (speed-type-prepare-string (concat " " (string-trim (mapconcat #'identity (nreverse words) " ")))
                                speed-type-ignore-whitespace-for-complete
                                nil
                                speed-type-transform-hook
                                (make-speed-type-transform-context :major-mode (with-current-buffer speed-type--content-buffer major-mode)
                                                                   :text-type speed-type--text-type
                                                                   :start nil
                                                                   :end nil
                                                                   :entries speed-type--entries
                                                                   :errors speed-type--errors
                                                                   :non-consecutive-errors speed-type--non-consecutive-errors
                                                                   :corrections speed-type--corrections
                                                                   :best-correct-streak speed-type--best-correct-streak))))
          (setq speed-type--extra-words-queue (append speed-type--extra-words-queue (split-string prepared-words-as-string "" t))))
        (when (not (timerp speed-type--extra-words-animation-timer))
          (setq speed-type--extra-words-animation-timer (run-at-time nil 0.01 #'speed-type-animate-extra-word-inseration speed-type--buffer)))))))

(defun speed-type-finish-animation (&optional buf)
  "Insert all remaining characters in `speed-type--extra-words-queue' to BUF."
  (interactive)
  (save-excursion
    (with-current-buffer (or buf speed-type--buffer)
      (remove-hook 'after-change-functions #'speed-type--change t)
      (when speed-type--extra-words-animation-timer (cancel-timer speed-type--extra-words-animation-timer))
      (setq speed-type--extra-words-animation-timer nil)
      (when speed-type--extra-words-queue
        (goto-char (point-max))
        (insert (mapconcat #'identity speed-type--extra-words-queue ""))
        (unless (speed-type--code-buffer-p (with-current-buffer  speed-type--content-buffer major-mode))
          (fill-region (point-min) (point-max) 'none t))
        (setq speed-type--extra-words-queue nil))
      (add-hook 'after-change-functions #'speed-type--change nil t))))

(defun speed-type-animate-extra-word-inseration (buf)
  "Add words of punishment-lines in animated fashion to BUF."
  (with-undo-amalgamate
    (save-excursion
      (with-current-buffer buf
        (remove-hook 'before-change-functions #'speed-type--before-change t)
        (remove-hook 'after-change-functions #'speed-type--change t)
        (if speed-type--extra-words-queue
            (let ((token (pop speed-type--extra-words-queue)))
              (goto-char (point-max))
              (insert token))
          (unless (speed-type--code-buffer-p (with-current-buffer speed-type--content-buffer major-mode))
            (fill-region (point-min) (point-max) 'none t))
          (cancel-timer speed-type--extra-words-animation-timer)
          (setq speed-type--extra-words-animation-timer nil))
        (add-hook 'before-change-functions #'speed-type--before-change nil t)
        (add-hook 'after-change-functions #'speed-type--change nil t)))
    (goto-char (point)) ;; this is for undo making the "jump-back" part of this undo-group
    ))

(defun speed-type-code-tab ()
  "A command to be mapped to TAB when speed typing code."
  (interactive)
  (let ((start (point))
        (end (re-search-forward "[^\t ]" (line-end-position) t)))
    (goto-char start)
    (when end
      (let ((fill-content (buffer-substring-no-properties start (1- end))))
        (if (string-empty-p fill-content)
            (insert "\t")
          (insert fill-content))))))

(defun speed-type-code-ret ()
  "A command to be mapped to RET when speed typing code."
  (interactive)
  (if (eolp)
      (progn (newline)
             (move-beginning-of-line nil)
             (let ((start (point))
                   (end (re-search-forward "[^\t ]" (line-end-position) t)))
               (goto-char start)
               (when end (insert (buffer-substring-no-properties start (1- end))))))
    (insert "\n")))

;;;###autoload
(defun speed-type-text-top-x (x)
  "Calculate a frequency list of a random gutenberg book.

Then assembles the text from the X most common words. Starting a
speed-type session with the assembled text.

The frequency list is stored at `speed-type-directory' ([book-num]-top-x.txt).

If `speed-type-default-lang' is set, will pick a random book of that language."
  (interactive "nTrain X most common words: ")
  (let* ((book-num (if speed-type-default-lang
                       (speed-type--retrieve-random-book-num speed-type-default-lang)
                     (nth (random (length speed-type-gb-book-list)) speed-type-gb-book-list)))
         (gb-buffer (speed-type--gb-retrieve book-num))
         (buffer (speed-type--gb-top-retrieve book-num))
         (fn (buffer-file-name buffer))
         (n (min x (with-current-buffer buffer (count-words (point-min) (point-max)))))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
         (title (format "Top %s words of book %s" n book-num))
         (author (with-current-buffer gb-buffer
                   (save-excursion
                     (when (re-search-forward "^Author: " nil t)
                       (buffer-substring (point) (line-end-position))))))
         (add-extra-word-content-fn (lambda () (speed-type--get-random-word-rolling buf n)))
         (go-next-fn (lambda () (speed-type--get-next-top-fn x))))
    (kill-buffer gb-buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    (speed-type--setup buf
             (point-min)
             (with-current-buffer buf (save-excursion (goto-char (point-min)) (beginning-of-line (+ 1 n)) (point)))
             'random-wordlist
             :file-name fn
             :title title
             :author author
             :randomize t
             :n-words n
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn go-next-fn
             :add-extra-word-content-fn add-extra-word-content-fn)))


;;;###autoload
(defun speed-type-top-x (x)
  "Speed type the X most common words.

The frequency list is stored at `speed-type-directory' ([lang].txt)."
  (interactive "nTrain X most common words: ")
  (let* ((lang (or speed-type-default-lang
                   (intern (completing-read "Language: " (mapcar #'car speed-type-wordlist-urls)))))
         (buffer (speed-type--wordlist-retrieve lang))
         (n (min x (with-current-buffer buffer (count-words (point-min) (point-max)))))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
         (fn (buffer-file-name buffer))
         (title (format "Top %s %s words" n lang))
         (add-extra-word-content-fn (lambda () (speed-type--get-random-word-rolling buf n)))
         (go-next-fn (lambda () (speed-type--get-next-top-fn x))))
    (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    (speed-type--setup buf
             (point-min)
             (with-current-buffer buf (save-excursion (goto-char (point-min)) (beginning-of-line (+ 1 n)) (point)))
             'random-wordlist
             :file-name fn
             :title title
             :author "Uni Leipzig"
             :lang lang
             :n-words n
             :randomize t
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn go-next-fn
             :add-extra-word-content-fn add-extra-word-content-fn)))

;;;###autoload
(defun speed-type-top-100 ()
  "Speed type the top 100 most common words.

More details see `speed-type-top-x'."
  (interactive)
  (speed-type-top-x 100))

;;;###autoload
(defun speed-type-top-1000 ()
  "Speed type the top 1000 most common words.

More details see `speed-type-top-x'."
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
         (start (with-current-buffer buf (point-min)))
         (end (with-current-buffer buf (point-max))))
    (speed-type--setup buf
             start
             end
             'transform-text
             :file-name fn
             :title title
             :author (user-full-name)
             :randomize t
             :replay-fn #'speed-type--get-replay-fn
             :syntax-table (with-current-buffer buf (syntax-table))
             :fldf (with-current-buffer buf font-lock-defaults))))

;;;###autoload
(defun speed-type-buffer-top-x (x)
  "Calculate a frequency list of `current-buffer'.

Then assembles the text from the X most common words. Starting a
speed-type session with the assembled text."
  (interactive "nTrain X most common words: ")
  (let* ((buffer (speed-type--calculate-word-frequency (current-buffer) (point-min) (point-max)))
         (fn (buffer-file-name (current-buffer)))
         (n (min x (with-current-buffer buffer (count-words (point-min) (point-max)))))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
         (title (format "Top %s words of buffer %s" n (buffer-name (current-buffer))))
         (add-extra-word-content-fn (lambda () (speed-type--get-random-word-rolling buf n))))
    (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    (speed-type--setup buf
             (point-min)
             (with-current-buffer buf (save-excursion (goto-char (point-min)) (beginning-of-line (+ 1 n)) (point)))
             'random-wordlist
             :file-name fn
             :title title
             :randomize t
             :n-words n
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn (lambda () (speed-type--get-next-top-fn x))
             :add-extra-word-content-fn add-extra-word-content-fn)))

;;;###autoload
(defun speed-type-buffer (full)
  "Open copy of buffer contents in a new buffer to speed type the text.

If using a prefix while calling this function `C-u', then the FULL text
will be used.  Else some text will be picked randomly."
  (interactive "P")
  (let ((cb (current-buffer)))
    (if full
        (speed-type-region (point-min) (point-max))
      (if speed-type-randomize
          (let* ((buf (speed-type-prepare-content-buffer-from-buffer (current-buffer)))
                 (start (with-current-buffer buf (point-min)))
                 (end (with-current-buffer buf (point-max)))
                 (go-next-fn (lambda () (with-current-buffer buf (speed-type-buffer full)))))
            (speed-type--setup buf
                     start
                     end
                     'random-text-section
                     :file-name (buffer-file-name (current-buffer))
                     :title (buffer-name)
                     :author (user-full-name)
                     :randomize t
                     :replay-fn #'speed-type--get-replay-fn
                     :go-next-fn go-next-fn
                     :add-extra-word-content-fn (lambda () (speed-type--get-next-word-rolling buf))
                     :syntax-table (syntax-table)
                     :fldf font-lock-defaults))
        (speed-type-continue (lambda () (with-current-buffer (speed-type-prepare-content-buffer-from-buffer cb) (speed-type-buffer full))))))))

;;;###autoload
(defun speed-type-text ()
  "Setup a new text sample to practice touch or speed typing."
  (interactive)
  (let* ((book-num (if speed-type-default-lang
                       (speed-type--retrieve-random-book-num speed-type-default-lang)
                     (nth (random (length speed-type-gb-book-list)) speed-type-gb-book-list)))
         (buffer (speed-type--gb-retrieve book-num)))
    (let ((stb (if speed-type-randomize
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
                                  (point)))))
                    (speed-type--setup buf
                             start
                             end
                             'random-text-section
                             :file-name (buffer-file-name buffer)
                             :title title
                             :author author
                             :randomize t
                             :replay-fn #'speed-type--get-replay-fn
                             :go-next-fn #'speed-type-text
                             :continue-fn (lambda () (speed-type--get-continue-fn end))
                             :add-extra-word-content-fn (lambda () (speed-type--get-next-word-rolling buf))))
                 (speed-type-continue #'speed-type-text (buffer-file-name buffer)))))
      (kill-buffer buffer)
      stb)))

;;;###autoload
(defun speed-type-quotes (&optional arg)
  "Setup a new quote to practice touch or speed typing.

If ARG is given will prompt for a specific quote-URL."
  (interactive "p")
  (let* ((quote-url (if (= arg 1)
                        (nth (random (length speed-type-quote-urls)) speed-type-quote-urls)
                      (assoc (intern (completing-read "Choose a quote: " (mapcar #'car speed-type-quote-urls) #'symbolp t nil nil "johnVonNeumann")) speed-type-quote-urls)))
         (buffer (speed-type--retrieve (car quote-url) (cdr quote-url)))
         (fn (buffer-file-name buffer))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
         (title (with-current-buffer buf
                  (save-excursion (search-forward-regexp "<title>\\(.*\\)</title>")
                                  (match-string 1))))
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
             (point-min)
             (point-max)
             'quote
             :file-name fn
             :title title
             :randomize t
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn go-next-fn
             :add-extra-word-content-fn add-extra-word-content-fn)))

;;;###autoload
(defun speed-type-continue (go-next-fn &optional file-name)
  "Will continue where user left of in given FILE-NAME.

GO-NEXT-FN is supplied to the setup-functions, to provide the
next-action (random) in the complete-menu.

Find last speed-type--continue-at-point of FILE-NAME and setup a speed-type
session continuing at that last found position. If nothing is found,
will begin at `point-min' or [GUTENBERG-START].

If an `universal-argument' (4) is used, will prompt user for FILE-NAME.

If FILE-NAME is given, will continue with that given file-name.

If FILE-NAME is not found, will throw a user-error.

If FILE-NAME is nil, will use file-name of CURRENT-BUFFER."
  (interactive "P")
  (if (eq speed-type-save-statistic-option 'never)
      (user-error "To use continue the variable speed-type-save-statistic-option can't be never")
    (let* ((buffer (cond ((equal '(4) file-name)
                          (find-file-noselect (read-file-name "Pick your file:" speed-type-directory)))
                         ((stringp file-name) (find-file-noselect file-name))
                         (t (current-buffer))))
           (fn (with-current-buffer buffer
                 (progn
                   (unless (buffer-file-name buffer)
                     (let ((r-fn (read-file-name "To save progress, choose a file-location for buffer:" speed-type-directory)))
                       (when (> (or (speed-type--find-last-continue-at-point-in-stats r-fn) 0) (point-max)) (user-error "Can not continue because file already has saved progress which exceeds buffer length"))
                       (write-file r-fn)))
                   (buffer-file-name (current-buffer)))))
           (buf (speed-type-prepare-content-buffer-from-buffer buffer)))
      (with-current-buffer buf
        (let* ((title (save-excursion
                        (or (when (re-search-forward "^Title: " nil t)
                              (buffer-substring (point) (line-end-position)))
                            (buffer-name buffer))))
               (author (save-excursion
                         (when (re-search-forward "^Author: " nil t)
                           (buffer-substring (point) (line-end-position)))))
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
                        (point-max))))
          (speed-type--setup buf
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
                   end
                   'continue-text-section
                   :file-name fn
                   :title title
                   :author author
                   :randomize nil
                   :replay-fn #'speed-type--get-replay-fn
                   :go-next-fn go-next-fn
                   :continue-fn (lambda () (speed-type--get-continue-fn end))
                   :add-extra-word-content-fn (lambda () (speed-type--get-next-word buf))
                   :syntax-table (syntax-table)
                   :fldf font-lock-defaults))))))
;;;###autoload
(defun speed-type-pandoc (url)
  "Use pandoc to start a speed-type session.

URL is retrieved and the content are stored in `speed-type-directory'.

If the URL is already retrieved will reuse the stored content in
`speed-type-directory'.

The file-name of the content is a converted form of URL."
  (interactive "sURL: ")
  (let* ((buffer (speed-type-retrieve-pandoc url))
         (fn (buffer-file-name buffer))
         (go-next-fn (lambda () (call-interactively #'speed-type-pandoc))))
    (let ((stb (if speed-type-randomize
        (let* ((buf (speed-type-prepare-content-buffer-from-buffer buffer))
               (title (format "Text section of url %s" url))
               (start (with-current-buffer buf (point-min)))
               (end (with-current-buffer buf (point-max))))
          (speed-type--setup buf
                   start
                   end
                   'random-text-section
                   :file-name fn
                   :title title
                   :randomize t
                   :replay-fn #'speed-type--get-replay-fn
                   :go-next-fn go-next-fn
                   :continue-fn (lambda () (speed-type--get-continue-fn end))
                   :add-extra-word-content-fn (lambda () (speed-type--get-next-word-rolling buf))
                   :syntax-table (with-current-buffer buf (syntax-table))
                   :fldf (with-current-buffer buf font-lock-defaults)))
        (speed-type-continue go-next-fn fn))))
      (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
      stb)
    ))

;;;###autoload
(defun speed-type-pandoc-top-x (url &optional x)
  "Calculate a frequency list using pandoc.

Then assembles the text from the X most common words. Starting a
speed-type session with the assembled text.

If the URL is already retrieved will reuse the stored content in
`speed-type-directory'.

The URL is converted to a unique file-name.

If the frequency list is already calculated for this URL will reuse the
stored content in `speed-type-directory'."
  (interactive "sURL: ")
  (let* ((x (if x x (string-to-number (completing-read "Train X most common words: " obarray 'numberp nil "100"))))
         (buffer (speed-type-retrieve-pandoc-top-retrieve url))
         (fn (buffer-file-name buffer))
         (n (min x (with-current-buffer buffer (count-words (point-min) (point-max)))))
         (buf (speed-type-prepare-content-buffer-from-buffer buffer))
         (title (format "Top %s words of url %s" n url))
         (add-extra-word-content-fn (lambda () (speed-type--get-random-word-rolling buf n))))
    (kill-buffer buffer) ;; buffer is retrieved, remove it again to not clutter the buffer-list
    (speed-type--setup buf
             (point-min)
             (with-current-buffer buf (save-excursion (goto-char (point-min)) (beginning-of-line (+ 1 n)) (point)))
             'random-wordlist
             :file-name fn
             :title title
             :n-words n
             :randomize t
             :replay-fn #'speed-type--get-replay-fn
             :go-next-fn (lambda () (speed-type--get-next-top-fn x))
             :add-extra-word-content-fn add-extra-word-content-fn)))

(provide 'speed-type)
;;; speed-type.el ends here
