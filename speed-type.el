;;; speed-type.el --- Practice touch and speed typing -*- lexical-binding: t -*-

;; Copyright (C) 2015 Gunther Hagleitner

;; Author: Gunther Hagleitner
;; Maintainer: Daniel Kraus <daniel@kraus.my>
;; Version: 1.3
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
(require 'compat)
(require 'url)
(require 'url-handlers)
(require 'url-http)
(require 'thingatpt)

(defgroup speed-type nil
  "Practice touch-typing in Emacs."
  :group 'games)

(defcustom speed-type-buffer-name "*speed-type*"
  "Name of buffer in which the user completes his typing session."
  :group 'speed-type
  :type 'string)

(defcustom speed-type-content-buffer-name "*speed-type-content-buffer*"
  "Name of buffer consisting of the content-source for the speed-type buffer."
  :group 'speed-type
  :type 'string)

(defcustom speed-type-min-chars 200
  "The minimum number of chars to type required when the text is picked randomly."
  :group 'speed-type
  :type 'integer)

(defcustom speed-type-max-chars 450
  "The maximum number of chars to type required when the text is picked randomly."
  :group 'speed-type
  :type 'integer)

(defcustom speed-type-gb-book-list
  '(1342 11 1952 1661 74 1232 23 135 5200 2591 844 84 98 2701 1400 16328 174
         46 4300 345 1080 2500 829 1260 6130 1184 768 32032 521 1399 55)
  "List of book numbers to use from the gutenberg web site.

Book numbers can be picked from https://www.gutenberg.org, when looking at
a book url.  E.G, https://www.gutenberg.org/ebooks/14577."
  :group 'speed-type
  :type '(repeat integer))

(defcustom speed-type-gb-dir (locate-user-emacs-file "speed-type")
  "Directory in which the gutenberg books will be saved."
  :group 'speed-type
  :type 'directory)

(defcustom speed-type-wordlist-urls
  '((English . "http://web.archive.org/web/20170227200416/http://wortschatz.uni-leipzig.de/Papers/top10000en.txt")
    (German . "http://web.archive.org/web/20170227200416/http://wortschatz.uni-leipzig.de/Papers/top10000de.txt")
    (French . "http://web.archive.org/web/20170227200416/http://wortschatz.uni-leipzig.de/Papers/top10000fr.txt")
    (Dutch . "http://web.archive.org/web/20170227200416/http://wortschatz.uni-leipzig.de/Papers/top10000nl.txt"))
  "Alist of language name as key and a URL where to download a wordlist for it."
  :type '(alist :key-type symbol :value-type string))

(defcustom speed-type-wordlist-transform nil
  "Function to transform wordlist before starting the exercise.
The function should take the `buffer-string' as argument and return
the transformed string that is used for the speed type exercise.

E.g. if you always want lowercase words, set:
`speed-type-wordlist-transform' to `downcase'."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Transform function")))

(defcustom speed-type-default-lang nil
  "Default language for training wordlists.  Ask when NIL."
  :type '(choice (const :tag "None" nil)
                 (const :tag "English" English)
                 (const :tag "German" German)
                 (const :tag "French" French)
                 (const :tag "Dutch" Dutch)))

(defcustom speed-type-replace-strings '(("“" . "\"") ("”" . "\"") ("‘" . "'") ("’" . "'"))
  "Alist of strings to replace and their replacement, in the form:
`(bad-string . good-string)'
To remove without replacement, use the form: `(bad-string . \"\")'"
  :type '(alist :key-type string :value-type string))

(defcustom speed-type-add-extra-words-on-mistake 0
  "How many new words should be added on mistake.
When 0 or less, no words are added. The typing-session will only
be complete when these extra words are typed too."
  :group 'speed-type
  :type 'integer)

(defface speed-type-default
  '()
  "Default face for `speed-type'."
  :group 'speed-type)

(defface speed-type-correct
  '((t :foreground "green"))
  "Face for correctly typed characters."
  :group 'speed-type)

(defface speed-type-mistake
  '((t :foreground "red" :underline "red"))
  "Face for incorrectly typed characters."
  :group 'speed-type)

;; internal variables

(defvar speed-type--gb-url-format
  "https://www.gutenberg.org/cache/epub/%d/pg%d.txt")

(defvar speed-type-explaining-message "
Gross wpm/cpm ignore uncorrected errors and indicate raw speed.
Net wpm/cpm take uncorrected errors into account and are a measure
of effective or net speed.")

(defvar speed-type-stats-format "\n
Skill:        %s
Net WPM:      %d
Net CPM:      %d
Gross WPM:    %d
Gross CPM:    %d
Accuracy:     %.2f%%
Total time:   %s
Total chars:  %d
Corrections:  %d
Total errors: %d
%s")

(defvar speed-type--completed-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-current-buffer)
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
  (cond ((< wpm 25) "Beginner")
        ((< wpm 30) "Intermediate")
        ((< wpm 40) "Average")
        ((< wpm 55) "Pro")
        ((< wpm 80) "Master")
        (t          "Racer")))

(defun speed-type--generate-stats (entries errors corrections seconds)
  "Return string of statistics."
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
          (+ errors corrections)
          speed-type-explaining-message))

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
            ((= q 1) (progn (cl-decf speed-type--entries)
                            (cl-incf speed-type--remaining)))
            ((= q 2) (progn (cl-decf speed-type--entries)
                            (cl-incf speed-type--remaining)
                            (cl-decf speed-type--errors)
                            (cl-incf speed-type--corrections))))
      (store-substring speed-type--mod-str pos 0))))

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

(defun speed-type-fill-paragraph ()
  "Override keybinding of fill-paragraph with this to not destory session."
  (interactive)
  (message "Fill paragraph not available"))

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
      (insert (propertize speed-type--title 'face 'italic))
      (when speed-type--author
	(insert (propertize
		 (format ", by %s" speed-type--author)
		 'face 'italic))))
    (insert (speed-type--generate-stats
             speed-type--entries
             speed-type--errors
             speed-type--corrections
             (speed-type--elapsed-time)))
    (insert "\n\n")
    (insert (format "    [%s]uit\n"
                    (propertize "q" 'face 'highlight)))
    (insert (format "    [%s]eplay this sample\n"
                    (propertize "r" 'face 'highlight)))
    (when speed-type--go-next-fn (insert (format "    [%s]ext random sample\n"
                                       (propertize "n" 'face 'highlight))))
    (let ((view-read-only nil))
      (read-only-mode))
    (use-local-map speed-type--completed-keymap)))

(defun speed-type--diff (orig new start end)
  "Update stats and buffer contents with result of changes in text."
  (let ((start0 (1- start))
        (_end0 (1- end))
        (correct nil))
    (dotimes (i (- end start))
      (let ((pos0 (+ start0 i))
            (pos (+ start i)))
        (if (speed-type--check-same i orig new)
            (progn (setq correct t)
                   (store-substring speed-type--mod-str pos0 1))
          (progn (cl-incf speed-type--errors)
                 (store-substring speed-type--mod-str pos0 2)
		 (when speed-type-add-extra-words-on-mistake (speed-type-add-extra-words))))
        (cl-incf speed-type--entries)
        (cl-decf speed-type--remaining)
	(let ((overlay (make-overlay pos (1+ pos))))
	  (overlay-put overlay 'face (if correct 'speed-type-correct
				       'speed-type-mistake)))))))

(defun speed-type--change (start end length)
  "Handle buffer change.

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
        (speed-type--diff orig new-text start end)
        (goto-char end)
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

If specified, call ADD-EXTRA-WORD-CONTENT-FN which provides an extra
line when user makes an mistake.

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
      (setq speed-type--orig-text text)
      (setq speed-type--mod-str (make-string len 0))
      (setq speed-type--remaining len)
      (setq speed-type--author author)
      (setq speed-type--title title)
      (setq speed-type--lang lang)
      (setq speed-type--n-words n-words)
      (setq speed-type--add-extra-word-content-fn add-extra-word-content-fn)
      (setq speed-type--go-next-fn go-next-fn)
      (when content-buffer
	(setq speed-type--content-buffer content-buffer)
	(setq-local speed-type--buffer buf))
      (with-current-buffer speed-type--content-buffer
	(setq-local speed-type--buffer buf))
      (when replay-fn
        (setq speed-type--replay-fn replay-fn))
      (insert text)
      (set-buffer-modified-p nil)
      (switch-to-buffer buf)
      (goto-char 0)
      (add-hook 'after-change-functions 'speed-type--change nil t)
      (add-hook 'first-change-hook 'speed-type--first-change nil t)
      (add-hook 'kill-buffer-hook 'speed-type--kill-buffer-hook nil t)
      (setq-local post-self-insert-hook nil)
      (when callback (funcall callback))
      (message "Timer will start when you type the first character."))))

(defun speed-type-prepare-content-buffer-from-buffer (buffer)
  "Prepare content buffer from existing BUFFER."
  (let ((buf (generate-new-buffer speed-type-content-buffer-name)))
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook 'speed-type--kill-content-buffer-hook nil t)
      (insert-buffer-substring buffer)
      (setq buffer-read-only nil)
      (funcall (buffer-local-value 'major-mode buffer))
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
  (unless (derived-mode-p 'prog-mode) (fill-region (region-beginning) (region-end)))
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun speed-type--setup-code
    (content-buffer text &optional replay-fn go-next-fn syntax-table font-lock-df)
  "Speed type the code snippet TEXT.

If specified, call REPLAY-FN after completion of a speed type session
and replay is selected.  Similarly call GO-NEXT-FN after completion of
a session if next is selected.

For code highlighting, a syntax table can be specified by SYNTAX-TABLE,
and font lock defaults by FONT-LOCK-DF."
  (cl-flet ((callback ()
                      (electric-pair-mode -1)
                      (local-set-key (kbd "TAB") 'speed-type-code-tab)
                      (local-set-key (kbd "RET") 'speed-type-code-ret)
                      (when syntax-table (set-syntax-table syntax-table))
                      (when font-lock-df
                        (let ((font-lock-defaults font-lock-df))
                          ;; Fontify buffer
                          (ignore-errors (font-lock-ensure))))))
    (speed-type--setup content-buffer
	     text
	     :replay-fn replay-fn
             :go-next-fn go-next-fn
             :callback #'callback)))

(defun speed-type--code-with-highlighting
    (content-buffer text &optional syntax-table font-lock-df go-next-fn)
  "Speed type TEXT with syntax highlighting.

Syntax highlighting data is given by SYNTAX-TABLE and
FONT-LOCK-DF (font lock defaults).

If GO-NEXT-FN is specified, call it when speed typing the text has
been completed."
  (speed-type--setup-code content-buffer
		text
		#'speed-type--get-replay-fn
		go-next-fn syntax-table font-lock-df))

(defun speed-type--get-replay-fn ()
  "Return a replay function which will use GO-NEXT-FN after completion."
  (remove-hook 'kill-buffer-hook 'speed-type--kill-buffer-hook t)
  (if (with-current-buffer speed-type--content-buffer
	(derived-mode-p 'prog-mode))
      (speed-type--code-with-highlighting
       speed-type--content-buffer
       speed-type--orig-text
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
	(beginning-of-buffer))
    (or (word-at-point) "")))

(defun speed-type--get-random-word (content-buffer limit)
  "Get random word in CONTENT-BUFFER.
LIMIT is supplied to the random-function."
  (with-current-buffer content-buffer
    (save-excursion
      (goto-char (point-min))
      (beginning-of-line (+ 1 (random limit)))
      (or (word-at-point) ""))))

(defun speed-type-add-extra-words ()
  "Add extra words of text to be typed for the typing-session to be complete."
  (when (and (> (or speed-type-add-extra-words-on-mistake 0) 0)
	     speed-type--add-extra-word-content-fn)
    (let ((words '()))
      (dotimes (_ speed-type-add-extra-words-on-mistake)
	(let ((word (funcall speed-type--add-extra-word-content-fn)))
	  (if (string-empty-p word)
	      (message "Extra word function resulted in empty string.")
	    (push word words))))
      (let ((words-as-string (concat " " (string-trim (mapconcat 'identity words " ")))))
	(setq speed-type--extra-words-queue (append speed-type--extra-words-queue (split-string words-as-string "" t)))
	(setq speed-type--orig-text (concat speed-type--orig-text words-as-string))
	(setq speed-type--mod-str (concat speed-type--mod-str (make-string (+ 1 (length words-as-string)) 0)))
	(setq speed-type--remaining (+ 1 (length words-as-string) speed-type--remaining))))
    (when (not (timerp speed-type--extra-words-animation-time))
      (setq speed-type--extra-words-animation-time (run-at-time nil 0.01 'speed-type-animate-extra-word-inseration speed-type--buffer)))))

(defun speed-type-finish-animation (buf)
  "Insert all remaining characters in ‘speed-type--extra-words-queue’."
  (save-excursion
    (with-current-buffer buf
      (remove-hook 'after-change-functions 'speed-type--change t)
      (cancel-timer speed-type--extra-words-animation-time)
      (setq speed-type--extra-words-animation-time nil)
      (when speed-type--extra-words-queue
	  (insert (mapconcat 'identity speed-type--extra-words-queue))
	  (fill-region (point-min) (point-max))))))

(defun speed-type-animate-extra-word-inseration (buf)
  "Add words of punishment-lines in animated fashion to ‘BUF’."
  (save-excursion
    (with-current-buffer buf
      (remove-hook 'after-change-functions 'speed-type--change t)
      (if speed-type--extra-words-queue
	  (let ((token (pop speed-type--extra-words-queue)))
	    (goto-char (point-max))
	    (when (not (string= token "\n"))
	      (end-of-line 1))
	    (insert token))
	(fill-region (point) (point-max))
	(cancel-timer speed-type--extra-words-animation-time)
	(setq speed-type--extra-words-animation-time nil))
      (add-hook 'after-change-functions 'speed-type--change nil t))))

(defun speed-type-code-tab ()
  "A command to be mapped to TAB when speed typing code."
  (interactive)
  (let ((start (point))
	(end (re-search-forward "[^\t ]" (line-end-position) t)))
    (goto-char start)
    (when end (insert (buffer-substring-no-properties start (1- end))))))

(defun speed-type-code-ret ()
  "A command to be mapped to RET when speed typing code."
  (interactive)
  (when (= (point) (line-end-position))
    (newline) (move-beginning-of-line nil) (speed-type-code-tab)))

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
		 (fill-region (point-min) (point-max))
		 (if speed-type-wordlist-transform
                     (funcall speed-type-wordlist-transform (buffer-string))
                   (buffer-string))))
	 (add-extra-word-content-fn (lambda () (speed-type--get-random-word buf n)))
         (go-next-fn (lambda () (speed-type-top-x n))))
    (speed-type--setup buf
	     text
             :title title
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
  (let* ((buf (speed-type-prepare-content-buffer-from-buffer (current-buffer)))
         (text (with-current-buffer buf
		 (buffer-substring-no-properties start end))))
    (if (with-current-buffer buf
	  (derived-mode-p 'prog-mode))
        (speed-type--code-with-highlighting buf
				  text
                                  (syntax-table)
                                  font-lock-defaults)
      (speed-type--setup buf
	       (buffer-substring-no-properties start end)
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
           (text (with-current-buffer buf
		   (speed-type--pick-text-to-type)))
           (go-next-fn (lambda ()
                         (with-current-buffer buf
                           (speed-type-buffer nil)))))
      (if (with-current-buffer buf
	    (derived-mode-p 'prog-mode))
          (speed-type--code-with-highlighting buf
				    text
                                    (syntax-table)
                                    font-lock-defaults
                                    go-next-fn)
        (speed-type--setup buf
		 text
		 :add-extra-word-content-fn (lambda () (speed-type--get-random-word buf))
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

(provide 'speed-type)

;;; speed-type.el ends here
