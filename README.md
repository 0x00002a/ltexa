# ltexa

## Introduction

ltexa parses the output of latex and displays warnings, errors, etc, along with
the exact line (if possible) and file where it occurred. 

Here is a sample of an error message produced by ltexa:
```
tex/testfile.tex:25: error: Undefined control sequence. (page 12)
At:
       \obscurenonexistantthingy
                             ~~^~~
column: 25
```
and the same error as produced by latex:
```
! Undefined control sequence.
l.25 \obscurenonexistantthingy
```
As you can see, the "raw" latex output lacks information about where the error
actually occurred and on what page in the output. It is also a lot easier to
miss in the log.


## Why?

Personal confession time, I cannot make head nor tail of most of the output
produced by latex. The [latexrun] parser and "pretty print" output solved this
issue but was mainly a build tool. This project was created for two main reasons:

1. To be a latex log parser, nothing more, nothing less 
2. To improve my knowledge of Haskell and Parsec

## Features

- Colour coded output (error is red, warning is purple, line numbers green)
- File and page numbers alongside errors & warnings (where possible)
- "Stack trace" alongside errors, showing the source(s) of the error (where
  possible)
- Support for concatenated log files produced by multiple runs (e.g. output of
  latexmk)

## Installation

Currently this project can only be built from source. It requires the Haskell
tool `stack` to be installed and an internet connection. Once you have both of
these it is as simple as:

```bash
cd <path to clone of repo> && stack install
```

## Usage

Running ltexa on a log file:
```bash
ltxa <path to log file>
```

Piping the contents of a log file:
```bash
latex -interaction=nonstopmode <file> | ltxa
```

## Special thanks to
[latexrun]. This project started as the
source code of latexrun minus everything except the LaTeX parser. The current
implementation of the parser is more or less a direct translation of the
original python and regex.

## Similar projects

- [latexrun]: Provides similar functionality to this project, but requires using
  _it_ to compile. 


## License

This project is licensed under the GNU GPLv3

> Copyright (C) 2020-2021 Natasha England-Elbro
>
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
>
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> GNU General Public License for more details.
>
> You should have received a copy of the GNU General Public License
> along with this program.  If not, see <https://www.gnu.org/licenses/>.




[latexrun]: https://github.com/aclements/latexrun
