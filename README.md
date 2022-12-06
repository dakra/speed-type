# Fork of speed-type

Practice touch/speed typing in GNU Emacs.

![Screenshot](https://raw.github.com/parkouss/speed-type/master/speed-type-screen-shot.png)

## Installation

Because this is a Fork it’s not available on melpa.

You can use an `el-patch` with `straight`:
```emacs-lisp
(use-package speed-type
  :straight (speed-type :type git :host github :repo "lordnik22/speed-type"))
```

Alternatively, you can clone this project
```
mkdir -p ~/.emacs.d/elisp
cd ~/.emacs.d/elisp
git clone https://github.com/lordnik22/speed-type.git
```
...and add it to the load-path in your init-file:
```
(add-to-list 'load-path "~/.emacs.d/elisp/speed-type")
(require 'speed-type)
```

## Configuration

speed-type can be customized using:

```
M-x customize-group speed-type RET
```

## Why a Fork?

If you like this fork also give a star to the origin: https://github.com/dakra/speed-type

Main differences:
- content-buffer from which the speed-type-buffer retrieves it’s content
- integrated add-word-feature from typer-mode
- some bugfixes and more consistent state-handling
- deactivate keys which can break a typing session (e.g. fill-paragraph)
- refactored code at some places

## Running speed-type

Executing M-x speed-type-text will start the typing exercise. A new buffer will
open and a random text sample will appear. As you type the text it will change
color to show progress and higlight correct and incorrect entries. Timing
happens automatically, the clock starts on the first character typed and ends
with the last. Statistics like characters typed, words-per-minute, and total
time will be shown as soon as the last character is entered.

You can use any buffer or part of it to run speed-type. M-x speed-type-region
and M-x speed-type-buffer will do the same thing as speed-type-text, except they
take the text sample you've picked.

speed-type-buffer by default will only take a random portion of the buffer - If
you want the whole buffer, use C-u speed-type-buffer.

Random samples are taken from Project Gutenberg. A small number of books will be
downloaded on demand and stored in "~/emacs.d/speed-type". They will only be
downloaded once.

`speed-type-region` will start a speed-type session with the text from
the selected region.

`speed-type-top-x` (or -100/-1000) lets you practice the top X words
for the selected language.


[melpa-link]: https://melpa.org/#/speed-type
[melpa-stable-link]: https://stable.melpa.org/#/speed-type
[melpa-badge]: https://melpa.org/packages/speed-type-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/speed-type-badge.svg
