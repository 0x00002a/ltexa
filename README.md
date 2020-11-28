# ltexa

## Introduction

ltexa parses the output of latex and displays warnings, errors, etc, along with
the exact line (if possible) and file where it occurred.


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




[latexrun]: https://github.com/aclements/latexrun
