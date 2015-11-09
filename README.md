# Flix: Programming with Fixpoints #

A new programming language for declarative fixpoint computations based on Datalog.

## Syntax Highlighting ##

### Intellij IDEA ###
- Go to `File > Settings > Editor > File Types`.
- Press the "green plus" icon on the right to add a new filetype.
- Enter the following:
    * Name: `Flix`. Description: `The Flix Language`.
    * Line comment: `//`.
    * Block comment start: `/*`. Block comment end: `*/`.
    * Check support `paired braces`, `paired parens`, and `paired brackets`.
    * Add the following keywords: 
        * `assert`, `case`, `def`, `else`, `enum`, `if`, `in`, `index`, `lat`, `let`, `match`,  `namespace`, `print`, `rel`, `val`, `with`.
- Press `OK`.

### Vim ###

Create the file `~/.vim/ftdetect/flix.vim` with the content:

```
au BufRead,BufNewFile *.flix set filetype=flix
```

Create the file `~/.vim/syntax/flix.vim` with the content:

```
if exists("b:current_syntax")
  finish
endif

syn keyword keywords assert case def else enum if in index lat
syn keyword keywords let match namespace print rel val with

let b:current_syntax = "flix"

hi def link keywords Statement
```

Ensure that your `~/.vimrc` file contains at least the following:

```
source ~/.vim/syntax/flix.vim
source ~/.vim/ftdetect/flix.vim
syntax on
```