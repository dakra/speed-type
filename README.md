# speed-type [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

Practice touch/speed typing in GNU Emacs.

![Screenshot](https://raw.github.com/dakra/speed-type/master/speed-type-screen-shot.png)

## Installation

Install speed-type from [MELPA](melpa.org) with:

```
M-x package-install RET speed-type
```

Or via [use-package](https://github.com/jwiegley/use-package) and [straight](https://github.com/radian-software/straight.el):
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

You can use any buffer or part of it to run speed-type. `M-x speed-type-region`
and `M-x speed-type-buffer` will do the same thing as speed-type-text, except they
take the text sample you've picked.
This works for programming code buffers/regions as well.

`speed-type-buffer` by default will only take a random portion of the buffer - If
you want the whole buffer, use `C-u speed-type-buffer`.

Random samples are taken from Project Gutenberg. A small number of books will be
downloaded on demand and stored in "~/emacs.d/speed-type". They will only be
downloaded once.

`speed-type-region` will start a speed-type session with the text from
the selected region.

`speed-type-top-x` (or -100/-1000) lets you practice the top X words
for the selected language.

`speed-type-quote` by default will take a random quote from a random quote-url listed in `speed-type-quote-urls`. You can `C-u speed-type-quote` to specify the url.

## Contribution

This pacakge is certaintly not bug-free. This is because we don’t have
or take the time to test every change with all kinds of configs in
which emacs can operate. It happens more often than not that the
merge-button is pressed with a bit too much excitedness.

If you got something odd happening while using this package please
create an issue, we will fix them.

If you rather like to fix them by your own you can just fork this repo
and implement the fix by yourself. We really like pressing that
merge-button.

We tried to consolidate the typing programs available for emacs.

### Integration of [Emacs Monkeytype](https://github.com/jpablobr/emacs-monkeytype)

For a detailed list of migrated features see: https://github.com/jpablobr/emacs-monkeytype/issues/4

### Integration of [etype](https://github.com/larstvei/etype)

For a detailed list of migrated features see: https://github.com/larstvei/etype/issues/5

### Integration of [typing](https://github.com/kensanata/typing)

For a detailed list of migrated features see: https://github.com/kensanata/typing/issues/1

## Typing content and integration

This package integrates different sources to start a typing-session
with somewhat meaningfull content. The following chapters explain what
the benefits are and how to gain the most out of it.

### Integration of the [Gutenberg Project](https://www.gutenberg.org/about/)

The Project Gutenberg is an online library of more than 75'000 free
eBooks.

Gutenberg books are retrieved when using the commands
`speed-type-text` or `speed-type-text-top-x`.

Retrieving the book is relativly expensive hence we store the book at
`speed-type-directory`.

The `speed-tpye-text-top-x` calculates a frequency list from a book
and stores the words sorted decending alongside the book-file.
Calculating the frequency of words can be quite expensive hence we
store it too.

### Integration of [AzQuotes](https://www.azquotes.com/about.html)

AzQuotes provides a organized collection of quotes spanning many
different topics (authors, birthday, notebook). The quotes are only
available in english.

A random quote from the `speed-type-quote-urls` is retrieved when
using the command `speed-type-quotes`. The page with all the quotes
from a provided URL, is stored in the `speed-type-directory`.

### Integration of [Wortschatz Uni-Leibzig](https://wortschatz.uni-leipzig.de/en)

For the command `speed-type-top-x` we retrieve a frequency list from
the uni leibzig. Due to the age of the lists they are accesed via
webarchive. The frequency lists is created for the year 2012 and is available for the languages: german, english, dutch, french.

`speed-type-top-x` is an outdated command and it’s recommanded to use the
`speed-type-buffer-top-x` or `speed-type-text-top-x` commands.

### Pandoc

This relies on the external programm [pandoc](https://pandoc.org/).
It’s a conversion tool, which allows any text-format to be converted
to any other text-format (e.g. xml -> json). The command
`speed-type-save-text-from-url` simplifies the creation of
typing-content by downloading the given URL and running it through
pandoc. The converted file is stored in the `speed-type-directory`.

Beaware that this is a best effort solution, and doesn’t lead to
perfect results with every website on the internet.

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
Character where previous character was already an error.

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
