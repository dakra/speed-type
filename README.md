# speed-type [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

Practice touch/speed typing in GNU Emacs.

![Screenshot](https://raw.github.com/dakra/speed-type/master/speed-type-screen-shot.png)

## Installation

Install speed-type from [MELPA](melpa.org) with:

```
M-x package-install RET speed-type
```

Or via use-package and straight:
```emacs-lisp
(use-package speed-type
  :straight t)
```

Alternatively, you can clone this project and add it to the load-path
in your init-file:
```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/speed-type/speed-type.el")
(require 'speed-type)
```

## Running speed-type

Executing `M-x speed-type-text` will start the typing exercise. A new buffer will
open and a random text sample will appear. As you type the text it will change
color to show progress and highlight correct and incorrect entries. Timing
happens automatically, the clock starts on the first character typed and ends
with the last. Statistics like characters typed, words-per-minute, and total
time will be shown as soon as the last character is entered.

Random samples are taken from [Project Gutenberg](https://www.gutenberg.org/).
A small number of books will be downloaded on demand and stored in
`~/emacs.d/speed-type`. They will only be downloaded once.

You can configure a language (var `speed-type-default-lang`) which
will pick books of this language. Beaware that gutenberg does not have
hundreds of books for all supported languages.

If you'd like to type a book from start to finish while saving your
progress you can set `speed-type-randomize` to `nil`.

You can use any buffer or part of it to run speed-type. `M-x speed-type-region` and `M-x speed-type-buffer` will do the same thing
as `speed-type-text`, except they take the text sample you've picked.
This works for programming code buffers/regions as well.

`speed-type-buffer` by default will only take a random portion of the
buffer - If you want the whole buffer, use `C-u speed-type-buffer`.
You can also call `speed-type-buffer-top-x` which will calculate a
frequency list of the current buffer and assemble text from the top
words.

`speed-type-region` will start a speed-type session with the text from
the selected region.

`speed-type-top-x` (or -100/-1000) lets you practice the top X words
for the selected language. If your language isn't supported by this
command you can try using `speed-type-text-top-x` (setting
var `speed-type-default-lang` beforehand). Which will calculate a
frequency list of a gutenberg book.

`speed-type-quote` by default will take a random quote from a random
quote-url listed in `speed-type-quote-urls`. You can `C-u
speed-type-quote` to specify the url.

`speed-type-pandoc` will prompt you for a URL (e.g. wikipedia) which
will be downloaded. The contents will be used for setting up the text
to type. `speed-type-pandoc-top-x` is also included and works similar
to the other top-x-commands.

## Contribution

This package is certainly not bug-free. Since we can’t test every
variant of Emacs setups out there, it’s inevitable that some issues
slip through.

If you encounter anything odd while using this package, please open an
issue. We’ll do our best to fix it without regression.

If you’d prefer to fix the problem yourself, feel free to fork the
repository and open a pull request with your improvement. We love
pressing that merge button.

## Customization

See all custom variables of speed-type pressing:
```
M-x customize-group speed-type RET
```

### Customize cursor motion

By default the cursor moves forward with each typed character and
marks it green if correct or red otherwise.

If you'd like to stay on the character until it's correctly typed, you
can customize the `speed-type-point-motion-on-error` to
`point-stay`.

### Make consecutive errors appear in different color (yellow)

Speed-type differenciate between errors and consecutive-errors.

An error is a mistyped character. A consectuive-error is a mistyped
character where previous character was already an error.

You can make consecutive errors yellow/orange (warning-color) by customizing the face as follows:
```emacs-lisp
(face-spec-set 'speed-type-consecutive-error-face '((t :inherit warning :underline t)))
```

### Statistics

The default of `speed-type-save-statistic-option` is `always` which
means speed-type session data is stored to file
`speed-type-statistic-filename`. All values which you see at the end
of each speed-type session are stored there.

You can display the calculated medians from the data by pressing `d`
at the end of a speed-type session.

### Add words on error (typer-mode)

There exists two variable for this purpose:
1. `speed-type-add-extra-words-on-non-consecutive-errors`
2. `speed-type-add-extra-words-on-error`.

Both have the default `0` which means no addittional words are added.
If both are set they accumlate each other.

If you like to challange yourself, it's recommanded to set one of them
to a number between 1 and 7.

It adds random or next words from the content the speed-type session
was started from. If you replay the session, the added words will be
included.

[melpa-link]: https://melpa.org/#/speed-type
[melpa-stable-link]: https://stable.melpa.org/#/speed-type
[melpa-badge]: https://melpa.org/packages/speed-type-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/speed-type-badge.svg

## Documentation
The documentation describes what exists and should give new people a
basic understanding how speed-type works.

A little diagram because [uniline](https://github.com/tbanel/uniline) is fun:
```uniline
                          ╭────╮   ╭─────╮         ╭────────────╮
                          │undo│   │pause│         │median stats│
                          ╰┬──┬╯   ╰┬───┬╯         ╰┬─────────┬─╯
                           △  │     △   │   display △         │
                           │  │     │   │           │         │
                           │  ▽     │   ▽ resume    │         ▽
  ╭─────╮   ╭─────╮       ╭┴──┴─────┴───┴────╮    ╭─┴─────────┴─╮     ╭────╮
  │start├──▷┤setup├──────▷┤speed-type session├───▷┤complete/menu├────▷┤quit│
  ╰─────╯   ╰─┬───╯       ╰─┬──────────────┬─╯   │╰───┬───┬───┬─╯     ╰────╯
              △             │              △   save   │   │   △
              │             ▽              │          │   ▽   │
              │           ╭─┴──────────────┴─╮        │   ├───┴──────────╮
              │           │add words on error│        │   │toggle preview│
              │           ╰──────────────────╯        │   ╰──────────────╯
              │                                       │
              │          ╭────────────────────╮       │
              ╰──────────┤replay/next/continue├─-◁────╯
                         ╰────────────────────╯
```
### Start
The flow is started by calling one of the autoloaded commands:
- `speed-type-text`
- `speed-type-text-top-x`
- `speed-type-top-x`
- `speed-type-top-100`
- `speed-type-top-1000`
- `speed-type-buffer`
- `speed-type-buffer-top-x`
- `speed-type-region`
- `speed-type-quotes`
- `speed-type-pandoc`
- `speed-type-pandoc-top-x`
- `speed-type-continue`

### Setup

Every speed-type buffer is created via the function
`speed-type--setup`. This is a large, configurable function with many
optional parameters that define how the flow should behave. Through
these parameters you can control which buffer-local variables are
initialized, which menu entries appear, and how new words are
inserted. It also connects the various speed-type buffers and
initializes all buffer-local variables required for a consistent
typing session. In short, `speed-type--setup` is the blueprint for
every typing session.

#### Buffers:
- `speed-type-buffer`: The buffer in which typing takes places
- `speed-type-content-buffer`: Contains the original content from which the flow was started. It's used for adding words and "continue" the content.
- `speed-type-preview-buffer`: Buffer which "records" typed characters and "unusual" point movement


### Speed-type session

This phase is the core of speed-type. While a typing session is
active, hooks and timers run continuously. With every keystroke, the
user's input is compared to the buffer contents. Based on the
difference (match vs. mismatch), various buffer-local variables are
updated, and characters update their status and color accordingly.

Performance is crucial in this phase: hooks must remain lightweight so
that typing remains responsive and the user is not restricted in how
they move or what commands they use to complete the session.

#### Hooks:
- first-change-hook: Used to start the timer
- before-change-functions: Used to store characters which are going be compared against.
- after-change-functions: Used to compare the inserted characters with the actual characters

#### Overlay and Faces:
To color the characters a overlay is used:
- speed-type-correct-face
- speed-type-error-face
- speed-type-consecutive-error-face

#### Text Properties:
- speed-type-orig-pos: Used for "continue" and add new words
  - car: start of word
  - cdr: end of word
- speed-type-char-status: Used to ignore characters and determine complete
  - ignore
  - correct
  - error

### Undo
Undo already modifies the text-properties. We therefore only need to
sync the overlay with whaterever just has been changed by undo.
### Pause
An idle-timer (`speed-type--idle-pause-timer`) is used in case the
user leaves his speed-type session untouched for a configured delay
(`speed-type-pause-delay-seconds`). On pause the `(float-time)` is
pushed to a register (`speed-type--time-register`). On resume a
`(float-time)` is pushed again. The elapsed time is calculated by
`speed-type--elapsed-time` which expects an even length in the
register. It pairs them, calculates the diff an sums them.
### Add words on error

Adding words is animated using timers which means it run more or less
asynchronous to the typing itself. The words are picked using the
function `speed-type--add-extra-word-content-fn`. The words are pushed to the queue
`speed-type--extra-words-queue`. This queue is consumed by
`speed-type--extra-words-animation-timer` which calls
`speed-type-animate-extra-word-inseration`.

The function is called X-times depending on the two custom variables
`speed-type-add-extra-words-on-non-consecutive-errors` and
`speed-type-add-extra-words-on-error`. If neither are set, no words
are added if both are set they accumulate each other.

### Completion and Menu

A typing session is considered complete when every character in the
buffer has a `speed-type-char-status` property (see also
`speed-type-complete-all-correct`). Once all characters have such a
value, completion is triggered. It's also possible to call
`speed-type-complete` interactivly.

Before displaying the menu, the buffer-local-vars are stored into the
stats-file (when customized).

At this point, the `speed-type-buffer` becomes read-only and only the
menu-control keys remain active. The user has the choice to either
quit or trigger an action.

### Replay / Continue / Next

These actions are available from the menu. Based on the key the user
presses, the corresponding action is invoked. The actions are
processed in a similar way how a speed-type session was started. The
action calls `speed-type--setup` with mostly the same parameters
again, which starts a new session.

After setup is complete it kills the completed `speed-type-buffer` and
`speed-type-preview-buffer`. It may reuse the existing content-buffer
for the new speed-type session.

### Toggle Preview
Toggles the display of the preview buffer. It opens a little window
below the speed-type buffer.

If the preview buffer is already displayed, will delete the window.
The preview buffer stays open in the background until the speed-type
buffer is quit.

The preview buffer works with two lines:
+ On the first line every input is appended.
+ On the second line the corresponding correct-character is inserted
  below if the input was an error.

If a user types a newline (or whitespace in general) in the speed-type
buffer it's converted to a representation of that whitespace. This
makes sure the input is logged always on the first line.

A `post-command-hook` is used to log commands and show point-movement.

If the user makes no error the second line stays empty.

### Median Stats
Calculates and displays the median stats of various buffer-local vars
from current and previous speed-type sessions.

### Quit
Kills the `speed-type-buffer` and all related buffers (content-buffer,
preview-buffer, etc.).
