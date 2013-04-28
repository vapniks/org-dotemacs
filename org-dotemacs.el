;;; org-dotemacs.el --- Store your emacs config as an org file.

;; Filename: org-dotemacs.el
;; Description: Store your emacs config as an org file, and load code snippets based on tag matches.
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-04-27 20:19:18
;; Version: 0.1
;; Last-Updated: 2013-04-27 20:19:18
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/org-dotemacs.el
;; Keywords: local
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((org "7.9.3"))
;;
;; Features that might be required by this library:
;;
;; org
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; Keeping your emacs config in an org file makes it easier for you to keep your .emacs under control,
;; and avoid dotemacs bankruptcy (http://www.emacswiki.org/emacs/DotEmacsBankruptcy).
;; With your config code stored in an org file you can easily edit the structure and keep notes.
;; This library allows you to load elisp code from an org file on emacs startup.
;; You can also limit the code that is loaded to certain tagged headers using an org tag match
;; (see "Matching tags and properties" in the org manual).
;;

;;; Installation:
;;
;; First you need to create an org file ~/.dotemacs.org and add your config code to emacs-lisp code blocks in the file,
;; e.g. like this:
;;
;; * Display settings code
;; #+NAME: display_settings
;; #+BEGIN_SRC emacs-lisp
;; (setq line-number-mode t)
;; (setq column-number-mode t)
;; (setq frame-title-format "%b")
;; (set-background-color "Black")
;; (set-foreground-color "White")
;; (set-cursor-color "White")
;; #+END_SRC
;; * Scrolling settings code
;; #+NAME: scroll_settings
;; #+BEGIN_SRC emacs-lisp
;; (mouse-wheel-mode t)
;; (setq scroll-step 1)
;; (setq scroll-conservatively 5)
;; #+END_SRC

;; Put org-dotemacs.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Then add the following lines to the end of your .emacs file
;; (load-file "~/.emacs.d/org-dotemacs.el")
;; (org-dotemacs-load-file)

;; If you stored your org file somewhere else you can specify the location as the first argument to
;; the `org-dotemacs-load-file' function, e.g:
;;
;; (org-dotemacs-load-file "~/.emacs.d/my_emacs_config.org")
;;
;; You can also specify a tag match in the second argument to limit which code blocks are loaded, e.g:
;;
;; (org-dotemacs-load-file "~/.emacs.d/my_emacs_config.org" "linux|basic-windows")
;;
;; See the org manual "Matching tags and properties" section for more details on tag matches.
;;

;;; Customize:
;;
;; To automatically insert descriptions of customizable variables defined in this buffer
;; place point at the beginning of the next line and do: M-x insert-customizable-variable-descriptions

;;
;; All of the above can customized by:
;;      M-x customize-group RET org-dotemacs RET
;;

;;; Change log:
;;	
;; 2013/04/27
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; Error checking on file name: make sure we don't overwrite ~/.emacs ~/.emacs.el or ~/.emacs.d/init.el
;; Documentation.
;; Upload to elpa/melpa/marmalade
;;
;; Would be great to evaluate the code blocks directly instead of saving to a file first.
;; The blocks could be wrapped in condition-case statements so that blocks with errors are skipped over.
;; Could also introduce dependency properties and load blocks in order according to these dependencies.

;;; Require
(eval-when-compile (require 'cl))
(require 'org)

;;; Code:

;;;###autoload
(defun org-dotemacs-extract-subtrees (match)
  "Extract subtrees in current org-mode buffer that match tag MATCH.
MATCH should be a tag match as detailed in the org manual.
The copied subtrees will be placed in a new buffer which is returned by this function.
If called interactively MATCH is prompted from the user, and the new buffer containing
the copied subtrees will be visited."
  (interactive '(nil))
  (let ((buf (generate-new-buffer (buffer-name)))
        todo-only copied-areas)
    (org-scan-tags (lambda nil
                     (unless (loop for pair in copied-areas
                                   if (and (>= (point) (car pair))
                                           (< (point) (cdr pair)))
                                   return t)
                       (let ((start (point)) end)
                         (org-copy-subtree)
                         (setq end (+ start (length (car kill-ring))))
                         (push (cons start end) copied-areas))
                       (with-current-buffer buf 
                         (goto-char (point-max))
                         (yank))))
                   (cdr (org-make-tags-matcher match)) todo-only)
    (with-current-buffer buf (org-mode))
    (if (called-interactively-p 'any)
        (switch-to-buffer buf)
      buf)))

;;;###autoload
(defun org-dotemacs-extract-code (match target-file)
  "Extract code in subtrees in current org-mode buffer that match tag MATCH, and save them to TARGET-FILE.
This function should be called within an org-mode buffer.
MATCH should be a tag match as detailed in the org manual."
  (interactive (list nil (read-file-name "File to save code to: " user-emacs-directory)))
  (let ((buf (org-dotemacs-extract-subtrees match)))
    (with-current-buffer buf
      (let ((tempfile (concat temporary-file-directory (buffer-name))))
        ;; for some reason org requires the buffer to be saved first
        (write-file tempfile)
        (org-babel-tangle nil target-file 'emacs-lisp)
        (delete-file tempfile))
      (kill-buffer buf))))

;;;###autoload
(defun* org-dotemacs-load-file (&optional (file "~/.dotemacs.org") (match "") savefile)
  "Load the elisp code from code blocks in org FILE under headers matching tag MATCH.
The elisp code will be saved to a file with the same name as FILE but with a \".el\" extension,
unless SAVEFILE is supplied in which case it will be saved there instead."
  (interactive (list (read-file-name "File to load: " user-emacs-directory) nil))
  (require 'ob-core)
  (let* ((age (lambda (file)
		(float-time
		 (time-subtract (current-time)
				(nth 5 (or (file-attributes (file-truename file))
					   (file-attributes file)))))))
	 (base-name (file-name-sans-extension (or savefile file)))
	 (target-file (concat base-name ".el")))
    (unless (and (file-exists-p target-file)
		 (> (funcall age file) (funcall age target-file)))
      (let ((visited-p (get-file-buffer (expand-file-name file)))
            to-be-removed)
        (save-window-excursion
          (find-file file)
          (setq to-be-removed (current-buffer))
          (org-dotemacs-extract-code match target-file))
        (unless visited-p
          (kill-buffer to-be-removed))))
    (load-file target-file)))


(provide 'org-dotemacs)

;;; org-dotemacs.el ends here

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "org-dotemacs.el" (buffer-name) (buffer-string) "update")
