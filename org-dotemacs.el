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
;; Upload to elpa/melpa/marmalade and emacswiki
;;
;; Would be great to evaluate the code blocks directly instead of saving to a file first.
;; The blocks could be wrapped in condition-case statements so that blocks with errors are skipped over,
;; or loaded at a later time with an error report at the end.
;; Could also introduce dependency properties and load blocks in order according to these dependencies.
;; 

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
(defun* org-dotemacs-load-file (&optional (file "~/.dotemacs.org") (match "") target-file)
  "Load the elisp code from code blocks in org FILE under headers matching tag MATCH.
If TARGET-FILE is supplied it should be a filename to save the elisp code to, but it should
not be any of the default config files .emacs, .emacs.el, .emacs.elc or init.el
 (the function will halt with an error in those cases)."
  (interactive (list (read-file-name "File to load: " user-emacs-directory nil t nil
                                     (lambda (file)
                                       (string-match "\\.org$" file)))
                     nil
                     (read-file-name "Save to file: " user-emacs-directory)))
  (if (string-match "\\(?:\\.emacs\\(?:\\.elc?\\)?\\|init\\.elc?\\)$" target-file)
      (error "Refuse to overwrite %s" target-file))
  (require 'ob-core)
  (flet ((age (file) (float-time
                      (time-subtract (current-time)
                                     (nth 5 (or (file-attributes (file-truename file))
                                                (file-attributes file)))))))
    (if (and target-file
             (file-exists-p target-file)
             (> (age file) (age target-file)))
        (load-file target-file)
      (let ((visited-p (get-file-buffer (expand-file-name file)))
            matchbuf to-be-removed)
        (save-window-excursion
          (find-file file)
          (setq to-be-removed (current-buffer))
          (setq matchbuf (org-dotemacs-extract-subtrees match))
          (with-current-buffer matchbuf
            ;; Write the buffer out first to prevent org-babel-pre-tangle-hook
            ;; prompting for a filename to save it in.
            (write-file (concat temporary-file-directory (buffer-name)))
            (org-dotemacs-load-blocks target-file))
          (kill-buffer matchbuf))
        (unless visited-p
          (kill-buffer to-be-removed))))))

;; Based on `org-babel-tangle'
;;;###autoload
(defun org-dotemacs-load-blocks (&optional target-file) ;
  "Load the emacs-lisp code blocks in the current org-mode file.
Save the blocks to TARGET-FILE if it is non-nil."
  (run-hooks 'org-babel-pre-tangle-hook)
  (save-restriction
    (save-excursion
      (let* ((block-counter 1)
             (org-babel-default-header-args
              (org-babel-merge-params org-babel-default-header-args
                                      (list (cons :tangle (or target-file "yes")))))
             (specs (cdar (org-babel-tangle-collect-blocks 'emacs-lisp)))
             evaluated-blocks unevaluated-blocks fail)
        ;; delete any old versions of file
        (if (and target-file (file-exists-p target-file))
            (delete-file target-file))
        (flet ((get-spec (name) (cdr (assoc name (nth 4 spec)))))
          (mapc
           (lambda (spec)
             (let ((blockname (or (get-spec :name)
                                  (concat "block_" (number-to-string block-counter))))
                   (blockdependencies (get-spec :depends)))
               (with-temp-buffer
                 (ignore-errors (emacs-lisp-mode))
                 (org-babel-spec-to-string spec)
                 ;; evaluate the code
                 (message "Evaluating %s code block" blockname)
                 (setq fail nil)
                 (condition-case err
                     (eval-buffer)
                   (error
                    (setq fail t)
                    (message "Error in %s code block: %s"
                             blockname (error-message-string err))
                    (setq unevaluated-blocks (append unevaluated-blocks (list blockname)))))
                 (unless fail
                   (setq evaluated-blocks (append evaluated-blocks (list blockname)))
                   ;; We avoid append-to-file as it does not work with tramp.
                   (when target-file
                     ;; save source-block to file
                     (let ((content (buffer-string)))
                       (with-temp-buffer
                         (if (file-exists-p target-file)
                             (insert-file-contents target-file))
                         (goto-char (point-max))
                         (insert content)
                         (write-region nil nil target-file)))))
                 ;; update counter
                 (setq block-counter (+ 1 block-counter)))))
           specs))
        (message "Tangled %d code block%s from %s" (- block-counter 1)
                 (if (<= block-counter 2) "" "s")
                 (file-name-nondirectory
                  (buffer-file-name (or (buffer-base-buffer) (current-buffer)))))
        (if (not unevaluated-blocks)
            (message "\nAll blocks evaluated successfully!")
          (message "\nSuccessfully evaluated the following %d code blocks: %s"
                   (length evaluated-blocks)
                   (mapconcat 'identity evaluated-blocks " "))
          (message "\nThe following %d code block%s had errors: %s\n"
                   (length unevaluated-blocks)
                   (if (= 1 (length unevaluated-blocks)) "" "s")
                   (mapconcat 'identity unevaluated-blocks " "))))
      ;; run `org-babel-post-tangle-hook' in tangled file
      (when (and org-babel-post-tangle-hook
                 target-file
                 (file-exists-p target-file))
        (org-babel-with-temp-filebuffer target-file
          (run-hooks 'org-babel-post-tangle-hook))))))

(provide 'org-dotemacs)

;;; org-dotemacs.el ends here

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "org-dotemacs.el" (buffer-name) (buffer-string) "update")
