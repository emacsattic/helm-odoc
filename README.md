# What is `helm-odoc`?

## Short answer -- for who is familiar with [Org mode](http://orgmode.org) and
   [Helm](https://github.com/emacs-helm/helm):

Extend the idea & features of `helm-org` that comes with Helm: you can search a collection
of Org files for particular headlines, properties and tags.

## Long answer:

It is built on top of two excellent libraries:

- [Org mode](http://orgmode.org)

- [Helm](https://github.com/emacs-helm/helm)

The idea is:

- Documents such as help files, reference manuals, notes, etc. are written in Org
  format. Though, Org files have simple outline structure; the format, however, has
  extensive additional markups and the library has a rich feature set that makes Org mode
  is an excellent choice for this purpose. Entries can be tagged and they can have
  user-defined properties, etc. Hence, information retrieval is relatively simple &
  flexible.

- Helm, a successor of `anything.el`, provides an interactive & incremental completion for
  searching a certain piece of information from something such as documents.

`helm-odoc` is an attempt to make use of these two libraries to develop a simple help
system within Emacs. It comes with a practical example: `hod-latex.el` & a number
associated Org files. With `hod-latex`, the following help will be available at your
fingertips:

- Most LaTeX commands, parameters

- Common LaTeX environments,

- Some useful LaTeX packages,

- Furthermore, your own LaTeX reference documents, tips or notes are easily added to the
  system.

You can also search the documents by tag name or a combination of tags that have been
marked in the files. Common tags currently exist in the documents: font, math, symbol,
colours, etc. (N.B. prompt for tag is also auto-completed by Helm, so you don't need to
remember them!). You can also introduce your own tags -- just simply mark the documents
with your tags.

# Installation:

- Make sure you have Org mode and Helm installed.

- Basic setup:

   1. Download or clone the package from github:
   
      $ git clone https://github.com/trietlai/helm-odoc.git \
            ~/path/to/helm-odoc

   2. Edit HELM_DIR variable in Makefile & run make in this
      directory (This step is optional):

      $ cd ~/path/to/helm-odoc
      $ editor Makefile         # edit HELM_DIR variable
      $ make

   3. Add following lines in your .emacs startup:

      (add-to-list 'load-path "~/path/to/helm-odoc")
      (require 'hod-latex) ; Automatically load helm-odoc

      If you want to enable key bindings and "Doc" menu globally:

      (helm-odoc-global-mode 1)

# Usage

Typical steps you would do when using the hod-latex library:

- Invoke one of hod-latex commands (described later).  As a result, Helm shows two
  windows.  One displays matching candidates for completion & searching.  The other
  displays the section of document that corresponds to the current candidate.

- Search candidates in completion buffer by typing the keyword that you want to find in
  the mini-buffer.  Helm incrementally narrows down the search while you typing.

- If you move the point to the next (`M-n` or down-arrow key) or previous candidate (`M-p`
  or up-arrow key) in the completion buffer, then the document shown in the other window
  is synchronised accordingly.

- Press ENTER to select the candidate; hod-latex displays the document in the view mode.
  Once finishing with document you can type `q` to bury the Org buffer and return back to
  the previous buffer, which had been shown before you ran the hod-latex command.

- While the candidate buffer is displayed, you can press `C-g` to quit Helm and return
  back to the previous buffer.

* Find help sections for LaTeX commands:
  - Mini-buffer:  `M-x helm-c-latex-doc-cmd`
  - Key bindings: `\C-c \C-h lc`
  - Menu:         `Doc` -> `LaTeX` -> `LaTeX commands`

* Find help sections for LaTeX environments:
  - Mini-buffer:  `M-x helm-c-latex-doc-env`
  - Key bindings: `\C-c \C-h le`
  - Menu:         `Doc` -> `LaTeX` -> `LaTeX environments`

* Find help sections for LaTeX packages:
  - Mini-buffer:  `M-x helm-c-latex-doc-pkg`
  - Key bindings: `\C-c \C-h lp`
  - Menu:         `Doc` -> `LaTeX` -> `LaTeX packages`

* Find help sections for LaTeX parameters:
  - Mini-buffer:  `M-x helm-c-latex-doc-param`
  - Key bindings: `\C-c \C-h lm`
  - Menu:         `Doc` -> `LaTeX` -> `LaTeX parameters`
  
* Find help sections matching tags:
  - Mini-buffer:  `M-x helm-c-latex-doc`
  - Key bindings: `\C-c \C-h ld`
  - Menu:         `Doc` -> `LaTeX` -> `Search for tags`

  Completion buffer initially includes all known tags.  You can incrementally filter out
  the tags by typing tag name in the mini-buffer.  You can also combine multiple tags in
  the query, e.g.:

    math+symbol

  it means: match all entries, which are marked by both "math" and "symbol" tags.  For
  advanced usages, please check the "Match Syntax" section in Org mode user guide.

* Find help sections matching hierarchical headlines:
  - Mini-buffer:  `M-x helm-c-latex-doc-hln`
  - Key bindings: `\C-c \C-h lh`
  - Menu:         `Doc` -> `LaTeX` -> `Search for headlines`

Acknowledgements
================

LaTeX help documents are converted from:

   - LaTeX Reference of Texmaker program

   - LaTeX2e info written by Karl Berry
