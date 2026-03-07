led
===

The line editor

## Overview

led is a line-oriented text editor inspired by ed, the GOAT editor.

It is supposed to mostly work like ed but adds a lot of bloat, namely:

- Multiple and cross-document editing
- Expression substitution
- User functions
- Interim document
- Visual mode
- Smart replace
- Filter through shell
- Multiple undo and redo
- File management
- Dynamic prompt
- Script loading

Disclaimer: This editor is my hobby project and as such an eternal work in progress. Use at your own risk.

### Multiple and cross-document editing

The document list behaves just like a document with filenames for each opened file.

The list itself can be addressed with &, so &a myfile opens another document, &d. closes the current document, and so on.

Switching to another document is the matter of changing the current line in the document list. &+/&- switch to next/previous document, &/my/ switches to the first document with a filename matching "my", etc.

It is possible to open/close several documents at once using regular editing commands, so &e filelist closes all open documents and opens a given list of filenames instead. Just in case you need session management.

A range followed by a colon specifies a document range. A special range &* refers to all documents that have unsaved changes.

Therefore, a command can be run on several documents at once. ,:0r licence inserts the contents of the file licence into all opened documents.

Target range for t and m commands can likewise be a range in one or more documents other than the current one. 1:$t2,3:0 copies the last line of the first document to the beginning of documents 2 and 3.

### Expression substitution

A command in curly braces {} is run and expanded with anything it prints before running the command.

a {f} can be used to quickly append current file name to current file. By the way, a/c/i commands now have one line versions, everything after a space becomes the line that is appended, inserted or replaces current line or given range. It is finally possible to easily add a single dot on a line with a . in case you need to.

### Interim document

A special prefix @ can be used to address an interim document that can be used as you like.

Interim documents are also used to pass parameters to a user function (see below).

### User functions

You can give a name to any sequence of commands by defining a user function:

fn my {
command1
command2
etc.
}

Then you can run the commands by simply entering the name of the function.

You can put any number of space separated arguments after a function name, and they will be passed to it in its own interim document (one per line).

If you need to pass arguments containing spaces, you can use my/arg1/arg2 etc.

A separate interim document is created when the function is called, and populated with its parameters. You can use parameters any way you like with a caveat that parameters that expand to multiple lines shift the following parameters by the corresponding number of lines, so it's your responsibility to deal with it. A function can access the interim document of its caller with ^@, and its caller's caller with ^^@, all the way up to the toplevel interim document.

Function declaration also has an alternative syntax similar to G command.

### Visual mode

vi command toggles visual mode on and off. In visual mode line editing features are limited (nonvisual mode uses haskeline while for visual mode a homebrew solution has been implemented). Visual mode prints the results of a command in a scrollable screen, and also allows to preview the effect of adding, changing, inserting or deleting lines, as well as search and replace.

### Smart replace

If a search string (regular expression) is all lowercase, case insensitive search is performed. If at least one character is uppercase, the search becomes case sensitive.

If both the search and the replacement string are lowercase, the case of the replacement is preserved. s/cat/mouse turns Cat into Mouse, CAt into MOuse, CaT into MoUse, and CAT into MOUSE.

To turn this behaviour off and replace case insensitive matches with a literal replacement string use /i flag (for "imbecil replace", note that it is the opposite of what the i flag does in the GNU ed.

### Filter through shell

When ! command is given a range, it filters given range through a shell command and replaces it with the result, same as in GNU ed.

### Multiple undo and redo

Every command that modifies lines of a document creates an undo step, and this includes document list, so opening/closing documents is undoable (but be careful about external modifications that might render undo state inconsistent and produce errors).

Running u multiple times undoes changes step by step, running U redoes them. Undo history is linear, so running a command other than U destroys all redo steps that have been left.

### File management

A special prefix && produces side effects on files. &&a creates an empty file, &&d deletes a file, &&s/old/new/ performs a mass rename, etc.

### Dynamic prompt

Prompt can be set from inside the editor with P new_prompt command, and any expressions in curly braces {} are evaluated every time the prompt is printed.

### Script loading

im script.led reads the file script.led and runs each line as a command, allowing scripting. Startup scripts can be run by using -e switch on the command line (multiple scripts can be run this way).

A script can run any commands including im, and there is a special imd command that prints the directory of the current script, allowing relative imports with expression substitution: im {imd}/second.led. At toplevel imd prints current directory.

## Installation

The editor is written in Haskell (GHC2024) and built with cabal.

I am compiling it with Cabal 3.16 and GHC 9.14 on macos Sonoma 14.7.

It has not been tested on other platforms and is not guaranteed to work… Who am i kidding, it is not guaranteed to work even on my own computer.

This readme file along with some of the code has been proudly written using the editor itself.
