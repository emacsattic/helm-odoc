;;; helm-odoc.el --- Finding headlines in Org documents.

;; Copyright (C) 2012-2013 Triet H. Lai
;; Author: Triet H. Lai <th_lai@yahoo.com.au>
;; Maintainer: Triet H. Lai <th_lai@yahoo.com.au>
;; Version: 0.2
;; Keywords: convenience, docs, help, outlines
;; Created: 2012

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
;; helm-odoc.el is built on top of Helm and Org mode.  It provides
;; some convenient functions to search collections of documents
;; written in Org format for particular headline, tags, and property.
;;
;; hod-latex.el is an example how to create a simple but practical
;; help system; where users can search reference documents, notes
;; interactively for different categories:
;;
;; - `helm-c-latex-doc':       finding headlines or tags in documents.
;;
;; - `helm-c-latex-doc-hln':   finding hierarchical headlines in documents.
;;
;; - `helm-c-latex-doc-cmd':   searching help document for LaTeX commands.
;;
;; - `helm-c-latex-doc-env':   searching help document for LaTeX environments.
;;
;; - `helm-c-latex-doc-pkg':   searching help document for LaTeX packages.
;;
;; - `helm-c-latex-doc-param': searching help document for LaTeX parameters.
;;
;; Prerequisite:
;; ============
;;
;; - Helm: https://github.com/emacs-helm/helm
;;
;; - Org mode: http://orgmode.org
;;
;; Installation:
;; ============
;;
;; * A. Make sure you have Org mode and Helm installed.
;;
;; * B. If you are only interested in using hod-latex.el to find help
;;      document for LaTeX commands, environments, etc.  you can ignore
;;      Section D.
;;
;; ** Basic setup:
;;
;;    1. Download or clone the package from github:
;;
;;       $ git clone https://github.com/trietlai/helm-odoc.git \
;;             ~/path/to/helm-odoc
;;
;;    2. Edit HELM_DIR variable in Makefile & run make in this
;;       directory (This step is optional):
;;
;;       $ cd ~/path/to/helm-odoc
;;       $ editor Makefile         # edit HELM_DIR variable
;;       $ make
;;
;;    3. Add following lines in your .emacs startup:
;;
;;       (add-to-list 'load-path "~/path/to/helm-odoc")
;;       (require 'hod-latex) ; Automatically load helm-odoc
;;
;;       If you want to enable key bindings and "Doc" menu globally:
;;
;;       (helm-odoc-global-mode 1)
;;
;; ** Advanced setup:
;;
;;    To enable helm-odoc minor mode in certain type of buffers, comment
;;    out the `helm-odoc-global-mode' and put `helm-odoc-mode' in some
;;    major-mode hook, like:
;;
;;      (add-hook 'LaTeX-mode-hook '(lambda () (helm-odoc-mode)))
;;
;;    Alternatively, do use `helm-odoc-global-mode' and create
;;    *exceptions* using the `helm-odoc-dont-activate' local variable,
;;    like this:
;;
;;      (add-hook 'c-mode-common-hook
;;                '(lambda () (setq helm-odoc-dont-activate t)))
;;
;; * C. Default key bindings in `helm-odoc-mode':
;;
;;    - Prefix key: "\C-c \C-h"
;;
;;      It is defined by `helm-odoc-prefix-key' variable, which you
;;      can customise.
;;
;;    - `helm-c-odoc-hln':          "<prefix-key> dh"   
;;
;;    - `helm-c-odoc-for-tags':     "<prefix-key> dt"   
;;
;;    - `helm-c-odoc-for-property': "<prefix-key> dp"   
;;
;;    - `helm-c-latex-doc':         "<prefix-key> ld"   
;;                                   
;;    - `helm-c-latex-doc-hln':     "<prefix-key> lh"    
;;                                   
;;    - `helm-c-latex-doc-cmd':     "<prefix-key> lc"
;;                                   
;;    - `helm-c-latex-doc-env':     "<prefix-key> le"
;;                                   
;;    - `helm-c-latex-doc-pkg':     "<prefix-key> lp"
;;
;;    - `helm-c-latex-doc-param':   "<prefix-key> lm"
;;
;; * D. If you'd like to extend helm-odoc or making use of your own Org
;;      files for similar purpose:
;;
;; ** Decide what PROPERTIES or TAGS in your Org files that may help
;;    to categorise headlines (i.e. sections or entries) in the
;;    documents.  Your commands may use these PROPERTIES and/or TAGS to
;;    filter candidates while constructing the completion buffer.  The
;;    'hod-latex.el' is an example shown how that is done for LaTeX
;;    reference documents.
;;
;; ** Add following lines in your .emacs startup:
;;
;;    (add-to-list 'load-path "~/path/to/helm-odoc")
;;    (require 'helm-odoc)
;;    ;; Set directories where Org files can be found:
;;    (setq helm-odoc-directories '("dir1" "dir2"))
;;    ;; load your code here
;;
;; Usage
;; =====
;;
;; Typical steps you would do when using the hod-latex library:
;;
;; - Invoke one of hod-latex commands (described later).  As a result,
;;   Helm shows two windows.  One displays matching candidates for
;;   completion & searching.  The other displays the section of
;;   document that corresponds to the current candidate.
;;
;; - Search candidates in completion buffer by typing the keyword that
;;   you want to find in the mini-buffer.  Helm incrementally narrows
;;   down the search while you typing.
;;
;; - If you move the point to the next (M-n or down-arrow key) or
;;   previous candidate (M-p or up-arrow key) in the completion
;;   buffer, then the document shown in the other window is
;;   synchronised accordingly.
;;
;; - Press ENTER to select the candidate; hod-latex displays the
;;   document in the view mode.  Once finishing with document you can
;;   type "q" to bury the Org buffer and return back to the previous
;;   buffer, which had been shown before you ran the hod-latex
;;   command.
;;
;; - While the candidate buffer is displayed, you can press C-g to
;;   quit Helm and return back to the previous buffer.
;;
;; * Find help sections for LaTeX commands:
;;   - Mini-buffer:  M-x helm-c-latex-doc-cmd
;;   - Key bindings: \C-c \C-h lc
;;   - Menu:         Doc -> LaTeX -> LaTeX commands
;;
;; * Find help sections for LaTeX environments:
;;   - Mini-buffer:  M-x helm-c-latex-doc-env
;;   - Key bindings: \C-c \C-h le
;;   - Menu:         Doc -> LaTeX -> LaTeX environments
;;
;; * Find help sections for LaTeX packages:
;;   - Mini-buffer:  M-x helm-c-latex-doc-pkg
;;   - Key bindings: \C-c \C-h lp
;;   - Menu:         Doc -> LaTeX -> LaTeX packages
;;
;; * Find help sections for LaTeX parameters:
;;   - Mini-buffer:  M-x helm-c-latex-doc-param
;;   - Key bindings: \C-c \C-h lm
;;   - Menu:         Doc -> LaTeX -> LaTeX parameters
;;
;; * Find help sections matching tags:
;;   - Mini-buffer:  M-x helm-c-latex-doc
;;   - Key bindings: \C-c \C-h ld
;;   - Menu:         Doc -> LaTeX -> Search for tags
;;
;;   Completion buffer initially includes all known tags.  You can
;;   incrementally filter out the tags by typing tag name in the
;;   mini-buffer.  You can also combine multiple tags in the query, e.g.:
;;
;;     math+symbol
;;
;;   it means: match all entries, which are marked by both "math" and
;;   "symbol" tags.  For advanced usages, please check the "Match
;;   Syntax" section in Org mode user guide.
;;
;; * Find help sections matching hierarchical headlines:
;;   - Mini-buffer:  M-x helm-c-latex-doc-hln
;;   - Key bindings: \C-c \C-h lh
;;   - Menu:         Doc -> LaTeX -> Search for headlines
;;
;; Acknowledgements
;; ================
;;
;; * LaTeX help documents are converted from:
;;
;;    - LaTeX Reference of Texmaker program
;;
;;    - LaTeX2e info written by Karl Berry
;; 

;;; Code:
(eval-when-compile (require 'cl))
(require 'easymenu)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-plugin)
(require 'org)
(require 'view)



(defvar helm-odoc-sub-keymap (make-sparse-keymap)
  "Sub-keymap contains key bindings for helm-odoc commands.")

(defvar helm-odoc-mode-map (make-sparse-keymap)
  "Keymap for helm-odoc mode.")



(defgroup helm-odoc nil
  "Finding headlines in Org documents."
  :prefix "helm-odoc-" :group 'helm)

(defcustom helm-odoc-show-inline-images t
  "Non-nil means showing inline images in the Org document buffer."
  :group 'helm-odoc
  :version "24.1"
  :type 'boolean)

;; TODO: Currently doesn't work
(defcustom helm-odoc-hide-emphasis-markers t
  "Non-nil means hiding emphasis marker characters.

Original text in Org documents may contain emphasis markers such
as *bold*, /italic/, _underlined_, =code=, and ~verbatim~. If the
option is non-nil (default), the text in Org document buffer is
displayed with usual faces but the markers become invisible."
  :group 'helm-odoc
  :type 'boolean)

(defcustom helm-odoc-directories "doc"
  "Directory or directories that contain(s) reference documents.

The top-level directory where reference documents written in Org
format are stored. This variable can also be a list of top-level
directories. In that case, please ensure these directories are
not overlapped, otherwise there will be some duplication in the
document content. The directories can be specified relative to
the path where this file resides.

To skip certain directory and its content, please create a hidden
file '.helm-odoc-skip' in that directory."
  :type '(choice (string :tag "Single directory (string)")
                 (repeat :args (string) :tag "List of directories (strings)"))
  :group 'helm-odoc)

(defcustom helm-odoc-candidate-limit 9999
  "Maximum number of candidates in the completion buffer."
  :type '(integer)
  :group 'helm-odoc)

(defcustom helm-odoc-prefix-key "\C-c\C-h"
  "Prefix key for helm-odoc commands.

This has to be a string representing a key sequence in a format
understood by the `kbd' macro.

Setting this variable directly does not take effect until
restart Emacs; Please use \\[customize]."
  :type '(string :tag "Key sequence")
  :initialize 'custom-initialize-default
  :set '(lambda (symbol value)
          (define-key helm-odoc-mode-map helm-odoc-prefix-key nil)
          (set-default symbol value)
          (when (and value (> (length value) 0))
            (define-key helm-odoc-mode-map value helm-odoc-sub-keymap)))
  :group 'helm-odoc)



;;; Internal variables & functions

(defconst helm-odoc--lib-dir (or (and load-file-name
                                      (file-name-directory load-file-name))
                                 default-directory)
  "Directory that helm-odoc.el is loaded from.

This directory is used as the base directory for
`helm-odoc--expand-dir-name' function to expand relative
directories, which store Org documents.")

(defun helm-odoc--expand-dir-name (directory)
  "Expand DIRECTORY into absolute path if it is relative.

Use the `helm-odoc--lib-dir' directory as the base for the
expansion."
  (if (file-name-absolute-p directory)
      directory
    (expand-file-name directory helm-odoc--lib-dir)))

(defun helm-odoc--find-files-1 (directory)
  "Recursively find all Org files in DIRECTORY.

Files are sorted in alphabetical order and the recursion uses
breadth-first traversal. As a result, headlines in files under
the top-most level DIRECTORY are displayed at the top of the
completion buffer.

Skip collecting all Org files in DIRECTORY nor descending into its
sub-directories if the hidden file '.helm-odoc-skip' found within
DIRECTORY."
  (let ((abs-dir (helm-odoc--expand-dir-name directory)))
    (cond
     ((not (file-exists-p abs-dir))
      (error "Directory %s is not found" abs-dir))
     ((not (file-directory-p abs-dir))
      (error "Parameter %s is not directory" abs-dir))
     ((not (file-accessible-directory-p abs-dir))
      (message "Directory %s is not accessible: ignored!" abs-dir))
     (t
      (unless (file-exists-p (concat abs-dir "/" ".helm-odoc-skip"))
        (let (files dirs)
          (dolist (file (directory-files abs-dir t))
            (if (file-directory-p file)
                (unless (string-match "^\\." (file-name-nondirectory file))
                  (setq dirs (cons file dirs)))
              (unless (or (not (string= "org" (file-name-extension file)))
                          (backup-file-name-p file)
                          (string-match "^#.*#$" (file-name-nondirectory file)))
                (setq files (cons file files)))))
          ;; Order of files in the list is important
          (apply 'append (nreverse files)
                 (when dirs
                   (mapcar 'helm-odoc--find-files-1 (nreverse dirs))))))))))

(defun helm-odoc--find-files (directories)
  "Recursively find all Org files in DIRECTORIES.

Order of directory in DIRECTORIES is significant; it determines
which Org file is displayed at the top of the content buffer.

While visitting a directory, skip reading all Org files and
descending into its sub-directories if the hidden file
'.helm-odoc-skip' found within the directory."
  (cond
   ((listp directories)
    (apply 'append (mapcar 'helm-odoc--find-files-1 directories)))
   (t
    (helm-odoc--find-files-1 directories))))

(defun helm-odoc--get-buffer-create (buffer-or-name directories)
  "Return a buffer specified by BUFFER-OR-NAME with document content.

Creating the new buffer if it does not exist. The content of the
buffer is accumulated by inserting all Org documents found in
DIRECTORIES recursively.

The buffer is put into `view-mode' and thus it should not be
made editable after its creation."
  (let ((buffer (get-buffer buffer-or-name)))
    (unless (buffer-live-p buffer)
      (let ((files (helm-odoc--find-files directories)))
        (when (null files)
          (error "No Org document is found in %s" directories))
        (setq buffer (get-buffer-create buffer-or-name))
        (with-current-buffer buffer
          (dolist (file files)
            (when (and (file-regular-p file)
                       (file-readable-p file))
	      (unless (or (bobp) (bolp))
                ;; make sure new file starts on separate line,
                ;; otherwise it messes up Org headlines.
		(insert "\n"))
              ;; Put content of each file into its own
              ;; *unconventional* block.  This could help us to
              ;; identify which file the text belongs to quickly.
              ;;
              ;; NOTE: The block is *unconventional* because
              ;; #+BEGIN_FILE and #+ENDF_FILE are not *real* Org
              ;; markup elements, however every #+BEGIN_ and its
              ;; corresponding #+END_ do mark a block.
              (insert (format "#+BEGIN_FILE %s\n"
                              (file-name-nondirectory file)))
	      (condition-case err
                  (insert-file-contents file)
		(error (message "Insert document %s failed: %s" file
				(error-message-string err))))
              (goto-char (point-max))
              (insert "\n#+END_FILE\n\n")))
          (let ((org-startup-with-inline-images helm-odoc-show-inline-images)
                ;; TODO: still having problem to hide emphasis markers.
                (org-hide-emphasis-markers helm-odoc-hide-emphasis-markers))
            (org-mode))
          (view-mode)))) 
    buffer))

(defconst helm-odoc--headline-regexp
  (mapcar
   (lambda (num)
     (format "^\\*\\{%d\\} \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
	     num))
   (number-sequence 1 8))
  "Regexp to match Org headlines")

(defun helm-odoc--headline-get-candidates (buffer)
  "Extract and build Org headline hierarchies from BUFFER.

An modification of `helm-headline-get-candidates' function in
`helm-org' so that it works with the given BUFFER instead of
`helm-current-buffer'.

Each candidate is a line of text that consists of hierarchical
headlines separated by '/' character. The left most component is
the level-1 headline and the right most component is the current
headline in Org document buffer."
  (with-current-buffer (get-buffer buffer)
    (save-excursion
      (goto-char (point-min))
      (let ((regexp helm-odoc--headline-regexp)
            hierarchy curhead)
        (flet ((matched ()
                        (cons (match-string-no-properties 1)
                              (match-beginning 1)))
               (hierarchies (headlines)
                            (1+ (loop for (_ . hierarchy) in headlines
                                      maximize hierarchy)))
               (vector-0-n (v n)
                           (loop for i from 0 to hierarchy
                                 collecting (aref curhead i)))
               (arrange (headlines)
                        (unless (null headlines) ; FIX headlines empty bug!
                          (loop with curhead = (make-vector (hierarchies headlines) "")
                                for ((str . pt) . hierarchy) in headlines
                                do (aset curhead hierarchy str)
                                collecting
                                (cons
                                 (format "H%d:%s" (1+ hierarchy)
                                         (mapconcat 'identity (vector-0-n curhead hierarchy) " / "))
                                 pt)))))
          (arrange
           (sort
            (loop for re in regexp
                  for hierarchy from 0
                  do (goto-char (point-min))
                  appending
                  (loop
                   while (re-search-forward re nil t)
                   collect (cons (matched) hierarchy)))
            (lambda (a b) (> (cdar b) (cdar a))))))))))

(defun helm-odoc--get-candidates-for-property (buffer property value &optional match)
  "Select candidates in BUFFER where PROPERTY equals to VALUE.

BUFFER is a Org buffer that contains document collected by
appending all Org files.

PROPERTY and VALUE is a key-value pair associated with an Org
entry. Please refer to 'Properties and columns' section in Org
mode user guide for full description.

MATCH is a search string with special syntax as described in
'Match syntax' section of Org mode documentation.

Each candidate corresponds to current headline in Org document
buffer."
  (with-current-buffer (get-buffer buffer)
    (save-excursion
      (goto-char (point-min))
      (delq nil
            (org-map-entries
             (lambda ()
               (when (equal value (org-entry-get nil property))
                 (cons (substring-no-properties (org-get-heading))
                       (point))))
             match)))))

(defun helm-odoc--get-candidates-for-tags (buffer match)
  "Select candidates in BUFFER that match the pattern MATCH.

BUFFER is a Org buffer that contains document collected by
appending all Org files.

MATCH is a search string with special syntax as described in
'Match syntax' section of Org mode documentation."
  (with-current-buffer (get-buffer buffer)
    (save-excursion
      (goto-char (point-min))
      (delq nil
            (org-map-entries
             (lambda ()
               (cons (substring-no-properties (org-get-heading))
                     (point)))
             match)))))

(defun helm-odoc--make-candidate-buffer (buffer-name directories selector &rest sel-args)
  "Create a buffer, which contains candidates for searching.

BUFFER-NAME is the name of Org buffer that contains reference
document, which is created by appending all Org files found under
DIRECTORIES.

SELECTOR is a predicate function, which filters out unwanted Org
entries. The Org buffer is passed to SELECTOR as the first
parameter. SEL-ARGS are the remaining parameters passed to
SELECTOR if it requires them.

There are three pre-defined SELECTORs available:

- `helm-odoc--headline-get-candidates': select all entries.

- `helm-odoc--get-candidates-for-property': select entries match
  given property and optionally match given tags.

- `helm-odoc--get-candidates-for-tags': select entries match
  given tags."
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((obuffer (helm-odoc--get-buffer-create buffer-name directories)))
      (loop for (content . pos) in (apply selector obuffer sel-args)
            do (insert
                (format "%5d:%s\n"
                        (with-current-buffer (get-buffer obuffer)
                          ;; TODO: avoid switching buffer
                          (line-number-at-pos pos))
                        content))))))

(defvar helm-odoc--buffer-name "*helm odoc*"
  "Name of buffer that contains documents in Org format.")

(defadvice helm-move-selection-common (after helm-odoc--move-selection)
  "Display corresponding entry after moving to different candidate.

The corresponding Org entry is an Org headline that associates
with the selected candidate in the current `helm-buffer'.

In order for this advice to work, the helm source must include:
'(persistent-action . helm-odoc--view-action)' and the helm
interactive function should call `helm-odoc--startup' and
`helm-odoc--cleanup' to active/deactive the advice properly."
  (when (eq (helm-buffer-get) helm-buffer)
    (helm-execute-persistent-action)))

(ad-deactivate-regexp "^helm-odoc")

(defun helm-odoc--startup ()
  "Initialise settings before invoking helm completion."
  (ad-activate-regexp "^helm-odoc"))

(defun helm-odoc--cleanup ()
  "Reset settings after exiting helm completion."
  (ad-deactivate-regexp "^helm-odoc"))

(defun helm-odoc--view-action (candidate)
  "Display corresponding Org entry in other window.

The corresponding Org entry is an Org headline that associates
with the selected candidate in the current `helm-buffer'.

This function should be used as a persistent-action so that
whenever the point in the `helm-buffer' moves to a different
candidate, `helm-odoc--move-selection' advice triggers it."
  (let* ((lineno-and-content
          (helm-c-display-to-real-numbered-line candidate))
         (lineno (car lineno-and-content))
         (content (cdr lineno-and-content)))
    (switch-to-buffer helm-odoc--buffer-name)
    (helm-goto-line lineno t)
    (recenter 0)))

(defun helm-odoc--default-action (candidate)
  "Helm default action that jumps to current Org entry.

The Org entry is an Org headline that associates with the current
candidate in the current `helm-buffer'."
  (let ((lineno
         (car (helm-c-display-to-real-numbered-line candidate))))
    (switch-to-buffer helm-odoc--buffer-name)
    (helm-goto-line lineno)
    (recenter)))



(defun helm-odoc-run (source cbuffer &optional obuffer odirs)
  "Invoke `helm' with SOURCE and CBUFFER.

SOURCE is `helm' source object, which is normally instantiated by
`helm-odoc-make-source'.

CBUFFER is name of buffer that contains candidates for
completion, i.e. the current `helm-buffer'.

OBUFFER is name of buffer that contains reference document in Org
format where CBUFFER is built upon.

ODIRS is a directory or a list of directories that contains Org
document files, which make up OBUFFER."
  (let ((helm-odoc--buffer-name (or obuffer helm-odoc--buffer-name))
        (helm-odoc-directories (or odirs helm-odoc-directories)))
    (helm-odoc--startup)
    (unwind-protect
        (helm-other-buffer source cbuffer)
      (helm-odoc--cleanup))))

(defun helm-odoc-make-source (name generator)
  "Create `helm' source with given NAME and GENERATOR function.

NAME is a title text, which is displayed at the top of the
current candidate buffer a.k.a `helm-buffer'.

GENERATOR is a function that creates candidates for the current
`helm-buffer'."
  `((name . ,name)
    (init . ,generator)
    (candidate-number-limit . ,helm-odoc-candidate-limit)
    (get-line . buffer-substring)
    (candidates-in-buffer)
    (action . helm-odoc--default-action)
    (persistent-action . helm-odoc--view-action)))

(defun helm-odoc-generator (&optional buffer directories)
  "Return function that generates candidate buffer.

BUFFER is the name of Org buffer that contains reference
document, which is created by appending all Org files found under
DIRECTORIES.

DIRECTORIES is a directory or a list of directories that contains
Org document files to build BUFFER."
  (let ((obuf (or buffer helm-odoc--buffer-name))
        (odirs (or directories helm-odoc-directories)))
    `(lambda ()
       (helm-odoc--make-candidate-buffer
        ,obuf ,odirs
        #'helm-odoc--headline-get-candidates))))

(defun helm-odoc-generator-for-tags (match &optional buffer directories)
  "Return function that generates candidate buffer.

The function returns only candidate entries that have tags
matching pattern MATCH.

BUFFER-NAME is the name of Org buffer that contains reference
document, which is created by appending all Org files found under
DIRECTORIES.

DIRECTORIES is a directory or a list of directories that contains
Org document files to build BUFFER."
  (let ((obuffer (or buffer helm-odoc--buffer-name))
        (odirs (or directories helm-odoc-directories)))
    `(lambda ()
       (helm-odoc--make-candidate-buffer
        ,obuffer ,odirs
        #'helm-odoc--get-candidates-for-tags
        ,match))))

(defun helm-odoc-generator-for-property (property value &optional buffer directories match)
  "Return function that generates candidate buffer.

The function returns only candidate entries that have PROPERTY
equal to VALUE.

BUFFER-NAME is the name of Org buffer that contains reference
document, which is created by appending all Org files found under
DIRECTORIES.

DIRECTORIES is a directory or a list of directories that contains
Org document files to build BUFFER.

Optionally MATCH can be specified to limit candidates that match
the given pattern."
  (let ((obuffer (or buffer helm-odoc--buffer-name))
        (odirs (or directories helm-odoc-directories)))
    `(lambda ()
       (helm-odoc--make-candidate-buffer
        ,obuffer ,odirs
        #'helm-odoc--get-candidates-for-property
        ,property ,value ,match))))

(defvar helm-odoc--current-tags-alist nil
  "Simple cache stores tags found in associated buffers.")

(defconst helm-odoc--match-all-tag "*ALL*"
  "Tag stands for matching everything.")

(defun helm-odoc-match-all-tag-p (tag)
  "Return non-nil if TAG matches everything."
  (cond
   ((stringp tag)
    (string= tag helm-odoc--match-all-tag))
   ((consp tag)
    (string= (car tag) helm-odoc--match-all-tag))
   (t nil)))

(defun helm-odoc-get-buffer-tags (buffer-name default-tags)
  "Return tags used in the BUFFER-NAME for completion purpose.

BUFFER-NAME must be an existing buffer, otherwise DEFAULT-TAGS is
returned instead."
  (let* ((buffer (get-buffer buffer-name))
         (elt (assoc buffer-name helm-odoc--current-tags-alist))
         (tags (cdr elt)))
    (if (buffer-live-p buffer)
        (unless tags
          (with-current-buffer buffer
            (setq tags (append helm-odoc--match-all-tag
                               (org-get-buffer-tags)))
            (add-to-list 'helm-odoc--current-tags-alist
                         (cons buffer-name tags))))
      (when elt
        (setq helm-odoc--current-tags-alist
              (delq elt helm-odoc--current-tags-alist)))
      (setq tags (append helm-odoc--match-all-tag default-tags)))
    tags))



(defvar helm-c-source-odoc-headline
  (let ((generator (helm-odoc-generator)))
    (helm-odoc-make-source "Org Documents" generator))
  "helm source for documents in Org format.

Candidates are displayed as hierarchical headlines without any
pre-filtering." )

;;;###autoload
(defun helm-c-odoc-hln ()
  "Preconfigured `helm' for Org document files.

Initially display all headlines."
  (interactive)
  (helm-odoc-run 'helm-c-source-odoc-headline
                 "*helm-odoc-headline*"))

;;;###autoload
(defun helm-c-odoc-for-tags (match)
  "Preconfigured `helm' for Org document files.

Display only candidates that have tags in headline matching MATCH
as described in 'Match syntax' section of Org mode
documentation."
  (interactive "sTags: ")
  (if (zerop (length match))
      (helm-c-odoc-hln)
    (let* ((generator (helm-odoc-generator-for-tags match))
           (source (helm-odoc-make-source
                    (format "Document for: %s" match) generator)))
      (helm-odoc-run source "*helm-odoc-tag*"))))

;;;###autoload
(defun helm-c-odoc-for-property (property value)
  "Preconfigured `helm' for Org document files.

Display only candidates where its associated headline has
PROPERTY equal to VALUE.

PROPERTY and VALUE is a key-value pair associated with an Org
entry. Please refer to 'Properties and columns' section in Org
document for full description."
  (interactive "sProperty: \nsValue: ")
  (if (zerop (length property))
      (helm-c-odoc-hln)
    (let* ((generator (helm-odoc-generator-for-property property value))
           (source (helm-odoc-make-source
                    (format "Document for: %s = %s" property value)
                    generator)))
      (helm-odoc-run source "*helm-odoc-property*"))))



;;; Minor mode definition

(define-key helm-odoc-sub-keymap "dh" 'helm-c-odoc-hln)
(define-key helm-odoc-sub-keymap "dt" 'helm-c-odoc-for-tags)
(define-key helm-odoc-sub-keymap "dp" 'helm-c-odoc-for-property)
(define-key helm-odoc-mode-map helm-odoc-prefix-key helm-odoc-sub-keymap)

(defvar helm-odoc-mode-menu nil
  "Menu for helm-odoc commands.")

;;;###autoload
(define-minor-mode helm-odoc-mode
  "Toggle helm-odoc mode.

With a prefix argument ARG, enable helm-odoc mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Key bindings:
\\{helm-odoc-mode-map}

The commands are also available under \"Doc\" menu on the menu
bar."
  :lighter nil
  :keymap helm-odoc-mode-map
  (if helm-odoc-mode
      (progn
        (easy-menu-add helm-odoc-mode-menu))
    (easy-menu-remove helm-odoc-mode-menu)))

(easy-menu-define helm-odoc-mode-menu
  helm-odoc-mode-map
  "helm-odoc menu"
  `("Doc"
    ["Search headlines" helm-c-odoc-hln
     :help "Find document matching headline"]
    ["Search for tags ..." helm-c-odoc-for-tags
     :help "Find document matching tags"]
    ["Search for property ..." helm-c-odoc-for-property
     :help "Find document matching property"]))

(defvar helm-odoc-dont-activate nil
  "Non-nil means don't enable helm-odoc mode for this buffer.

If value of the variable is a function, then it is called without
argument and the return value decides whether the mode should be
activated or not.")

(make-variable-buffer-local 'helm-odoc-dont-activate)

(defun helm-odoc-mode-on ()
  "Turn on helm-odoc mode.

Do this unless `helm-odoc-dont-activate' is non-nil."
  (interactive)
  (unless (if (functionp helm-odoc-dont-activate)
              (funcall helm-odoc-dont-activate)
            helm-odoc-dont-activate)
    (helm-odoc-mode 1)))

(define-globalized-minor-mode helm-odoc-global-mode helm-odoc-mode helm-odoc-mode-on)



(provide 'helm-odoc)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-odoc.el ends here
