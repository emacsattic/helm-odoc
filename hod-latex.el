;;; hod-latex.el --- Finding LaTeX help document.

;; Copyright (C) 2012-2013 Triet H. Lai
;; Author: Triet H. Lai <th_lai@yahoo.com.au>
;; Maintainer: Triet H. Lai <th_lai@yahoo.com.au>
;; Keywords: convenience, docs, help, outlines, tex
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
;; LaTeX help documents, which are included in this package are
;; written in Org format.  This extension assists users to find
;; relevant information from a collection of those documents.
;;
;; - `helm-c-latex-doc':       finding headlines, tags in the documents.
;;
;; - `helm-c-latex-doc-hln':   finding headlines in the help documents.
;;
;; - `helm-c-latex-doc-cmd':   searching help sections for LaTeX commands.
;;
;; - `helm-c-latex-doc-env':   searching help sections for LaTeX environments.
;;
;; - `helm-c-latex-doc-pkg':   searching help sections for LaTeX packages.
;;
;; - `helm-c-latex-doc-param': searching help sections for LaTeX parameters.
;;
;; To filter out irrelevant sections in the help documents, the
;; commands: `helm-c-latex-doc-cmd', `helm-c-latex-doc-env',
;; `helm-c-latex-doc-pkg', and `helm-c-latex-doc-param' use:
;;
;; - PROPERTY="LatexCategory", with one of following
;;
;; - VALUE={command | environment | package | parameter } respectively.
;;
;; When using `helm-c-latex-doc' command, you can use Org TAGS such as
;; 'math', 'font', 'counter', etc.  marked in the documents to narrow
;; down candidates.
;;
;; Adding your own LaTeX help documents is simple:
;;
;; - Put your Org files, etc.  under some directory, say
;;   "~/emacs.d/helm-odoc/my-latex-doc"
;;
;; - Customise variable `hod-latex-directories', e.g.:
;;   (add-to-list 'hod-latex-directories
;;                "~/emacs.d/helm-odoc/my-latex-doc")
;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'easymenu)
(require 'helm)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-plugin)
(require 'org)
(require 'helm-odoc)



(defvar hod-latex--doc-map (make-sparse-keymap)
  "Sub-keymap contains key bindings for hod-latex commands.")

(defgroup hod-latex nil
  "Finding LaTeX help document in Org format."
  :prefix "hod-latex-" :group 'helm-odoc)

(defcustom hod-latex-directories "latex-doc"
  "Directory or directories that contain(s) LaTeX reference documents.

The top-level directory where LaTeX reference documents written
in Org format are stored. This variable can also be a list of
top-level directories. In that case, please ensure these
directories are not overlapped, otherwise there will be some
duplication in the document content. The directories can be
specified relative to the path where this file resides.

To skip certain directory and its content, please create a hidden
file '.helm-odoc-skip' in that directory."
  :type '(choice (string :tag "Single directory (string)")
                 (repeat :args (string) :tag "List of directories (strings)"))
  :group 'hod-latex)

(defcustom hod-latex-completion-tags
  '("color" "font" "layout" "graphics" "box" "rule" "space"
    "math" "symbol" "greek" "operator" "align"
    "counter" "length" "rubber_length"
    "bibliography" "table" "reference" "list")
  "List of tags might be used in the Org files.

`helm-c-latex-doc' command uses this list for completion."
  :type '(choice (repeat :args (string) :tag "List of tags (strings)"))
  :group 'hod-latex)



(defvar hod-latex--buffer-name "*hod LaTeX*"
  "Name of buffer that contains LaTeX reference documents.")

(defconst hod-latex--property-key "LatexCategory")
(defconst hod-latex--cmd-property "command")
(defconst hod-latex--env-property "environment")
(defconst hod-latex--pkg-property "package")
(defconst hod-latex--param-property "parameter")

;;; Helm completion for headlines of reference document about LaTeX

(defvar helm-c-source-hod-latex-headline
  (let ((generator (helm-odoc-generator
                    hod-latex--buffer-name
                    hod-latex-directories)))
    (helm-odoc-make-source "LaTeX Document" generator))
  "helm source for LaTeX reference document.")

;;;###autoload
(defun helm-c-latex-doc-hln ()
  "Preconfigured `helm' for LaTeX help documents.

Initially display all headlines found in the LaTeX reference
documents."
  (interactive)
  (helm-odoc-run 'helm-c-source-hod-latex-headline
                  "*hod-latex-headline*"
                  hod-latex--buffer-name
                  hod-latex-directories))



;;; Helm completion for reference document about LaTeX Commands

(defvar helm-c-source-hod-latex-cmd
  (let ((generator (helm-odoc-generator-for-property
                    hod-latex--property-key
                    hod-latex--cmd-property
                    hod-latex--buffer-name
                    hod-latex-directories)))
    (helm-odoc-make-source "LaTeX Commands" generator))
  "helm source for help document on LaTeX commands.")

;;;###autoload
(defun helm-c-latex-doc-cmd ()
  "Preconfigured `helm' for help documents on LaTeX commands.

Candidates are Org headlines that describe LaTeX commands."
  (interactive)
  (helm-odoc-run 'helm-c-source-hod-latex-cmd
                  "*hod-latex-cmd*"
                  hod-latex--buffer-name
                  hod-latex-directories))



;;; Helm completion for reference document about LaTeX Environments

(defvar helm-c-source-hod-latex-env
  (let ((generator (helm-odoc-generator-for-property
                    hod-latex--property-key
                    hod-latex--env-property
                    hod-latex--buffer-name
                    hod-latex-directories)))
    (helm-odoc-make-source "LaTeX Environments" generator))
  "helm source for help document on LaTeX environments.")

;;;###autoload
(defun helm-c-latex-doc-env ()
  "Preconfigured `helm' for help documents on LaTeX environments.
  
Candidates are Org headlines that describe LaTeX environments."
  (interactive)
  (helm-odoc-run 'helm-c-source-hod-latex-env
                  "*hod-latex-env*"
                  hod-latex--buffer-name
                  hod-latex-directories))



;;; Helm completion for reference document about LaTeX Packages

(defvar helm-c-source-hod-latex-pkg
  (let ((generator (helm-odoc-generator-for-property
                    hod-latex--property-key
                    hod-latex--pkg-property
                    hod-latex--buffer-name
                    hod-latex-directories)))
    (helm-odoc-make-source "LaTeX Packages" generator))
  "helm source for help document on LaTeX packages.")

;;;###autoload
(defun helm-c-latex-doc-pkg ()
  "Preconfigured `helm' for help documents on LaTeX environments.
  
Candidates are Org headlines that describe LaTeX packages."
  (interactive)
  (helm-odoc-run 'helm-c-source-hod-latex-pkg
                  "*hod-latex-pkg*"
                  hod-latex--buffer-name
                  hod-latex-directories))



;;; Helm completion for reference document about LaTeX Parameters

(defvar helm-c-source-hod-latex-param
  (let ((generator (helm-odoc-generator-for-property
                    hod-latex--property-key
                    hod-latex--param-property
                    hod-latex--buffer-name
                    hod-latex-directories)))
    (helm-odoc-make-source "LaTeX Parameters" generator))
  "helm source for help document on LaTeX parameters.")

;;;###autoload
(defun helm-c-latex-doc-param ()
  "Preconfigured `helm' for help documents on LaTeX parameters.
  
Candidates are Org headlines that describe LaTeX parameters."
  (interactive)
  (helm-odoc-run 'helm-c-source-hod-latex-param
                  "*hod-latex-param*"
                  hod-latex--buffer-name
                  hod-latex-directories))



;;; Helm completion for reference document entries that match given tags

;;;###autoload
(defun helm-c-latex-doc (&optional match)
  "Preconfigured `helm' for LaTeX help entries matching MATCH.

Display only candidates that have tags in headline matching MATCH
as described in 'Match syntax' section of Org mode
documentation."
  (interactive)
  (when (called-interactively-p 'interactive)
    (let* ((completion-tags (helm-odoc-get-buffer-tags
                             hod-latex--buffer-name
                             hod-latex-completion-tags))
           (answer (completing-read
                    (if match
                        (format "Tags: (default \"%s\"): " match)
                      "Tags: ")
                    completion-tags nil nil nil nil match)))
      (unless (string= answer match)
        (setq match answer))))
  (if (or (zerop (length match))
          (helm-odoc-match-all-tag-p match))
      (helm-c-latex-doc-hln)
    (let* ((generator (helm-odoc-generator-for-tags
                       match
                       hod-latex--buffer-name
                       hod-latex-directories))
           (source (helm-odoc-make-source
                    (format "LaTeX document for: %s" match) generator)))
      (helm-odoc-run source "*hod-latex-tag*"
                     hod-latex--buffer-name
                     hod-latex-directories))))



;;; Key bindings and menu

(define-key hod-latex--doc-map "d" 'helm-c-latex-doc)
(define-key hod-latex--doc-map "h" 'helm-c-latex-doc-hln)
(define-key hod-latex--doc-map "c" 'helm-c-latex-doc-cmd)
(define-key hod-latex--doc-map "e" 'helm-c-latex-doc-env)
(define-key hod-latex--doc-map "p" 'helm-c-latex-doc-pkg)
(define-key hod-latex--doc-map "m" 'helm-c-latex-doc-param)

(define-key helm-odoc-sub-keymap "l" hod-latex--doc-map)

;; Sub menu: Doc -> LaTeX
(easy-menu-add-item helm-odoc-mode-menu nil
  `("LaTeX"
    ["LaTeX commands" helm-c-latex-doc-cmd
     :help "Find document for LaTeX commands"]
    ["LaTeX environments" helm-c-latex-doc-env
     :help "Find document for LaTeX environments"]
    ["LaTeX packages" helm-c-latex-doc-pkg
     :help "Find document for LaTeX packages"]
    ["LaTeX parameters" helm-c-latex-doc-param
     :help "Find document for LaTeX parameters"]
    ["Search for tags" helm-c-latex-doc
     :help "Find LaTeX document matching tags"]
    ["Search for headline" helm-c-latex-doc-hln
     :help "Find LaTeX document matching headline"]))



(provide 'hod-latex)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; hod-latex.el ends here
