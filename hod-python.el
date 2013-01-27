;;; hod-python.el --- Finding Python help document.

;; Copyright (C) 2013 Triet H. Lai
;; Author: Triet H. Lai <th_lai@yahoo.com.au>
;; Maintainer: Triet H. Lai <th_lai@yahoo.com.au>
;; Keywords: convenience, docs, help, outlines
;; Created: 2013

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

;;; Code:
(eval-when-compile (require 'cl))
(require 'easymenu)
(require 'helm)
(require 'org)
(require 'helm-odoc)



(defvar hod-python--doc-map (make-sparse-keymap)
  "Sub-keymap contains key bindings for hod-python commands.")

(defgroup hod-python nil
  "Finding Python help document in Org format."
  :prefix "hod-python-" :group 'helm-odoc)



(defvar hod-python--numpy-directory
  (concat (file-name-as-directory "python-doc") "numpy")
  "Directory that contains help document for Numpy module.")

(defvar helm-c-source-hod-python-numpy
  (let ((generator (helm-odoc-generator
                    "*org-py-numpy*"
                    hod-python--numpy-directory)))
    (helm-odoc-make-source "Python Numpy Document" generator))
  "helm source for Numpy document.")

;;;###autoload
(defun helm-c-python-doc-numpy ()
  "Preconfigured `helm' for Numpy help documents."
  (interactive)
  (helm-odoc-run 'helm-c-source-hod-python-numpy
                  "*hod-py-numpy*"
                  "*org-py-numpy*"
                  hod-python--numpy-directory))



;;; Key bindings and menu

(define-key hod-python--doc-map "n" 'helm-c-python-doc-numpy)

(define-key helm-odoc-sub-keymap "p" hod-python--doc-map)

;; Sub menu: Doc -> Python
(easy-menu-add-item helm-odoc-mode-menu nil
  `("Python"
    ["Numpy" helm-c-python-doc-numpy
     :help "Find document for Numpy"]
     ))

(provide 'hod-python)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; hod-python.el ends here
