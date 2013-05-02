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
;; (see "Matching tags and properties" in the org manual), and get emacs to try reloading blocks.
;;

;;; Installation:
;;
;; First you need to create an org file ~/.dotemacs.org and add your config code to emacs-lisp code blocks in the file,
;; e.g. like this:
;;
;; * Display settings code
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
;; Upload to elpa/melpa/marmalade and emacswiki
;;
;; 

;;; Require
(eval-when-compile (require 'cl))
(require 'org)

;;; Code:

(defvar org-dotemacs-error-handling 'skip
  "Indicates how errors should be handled by `org-dotemacs-load-blocks'.
If eq to 'skip then errors will be skipped over (default).
If eq to 'retry then `org-dotemacs-load-blocks' will attempt to reload any blocks containing errors,
after each successfully loaded block.
In all other cases errors will cause evaluation to halt as normal.
In all cases errors will be reported in the *Messages* buffer as normal.

This variable can be set from the command line using the dotemacs-error-handling argument.")

(defvar org-dotemacs-tag-match nil
  "An org tag match string indicating which code blocks to load with `org-dotemacs-load-file'.
If non-nil the value of this variable will override the match argument to `org-dotemacs-load-file'.")

(let* ((errpos (or (position-if (lambda (x) (equal x "-error-handling")) command-line-args)
		   (position-if (lambda (x) (equal x "--error-handling")) command-line-args)))
       (errval (if errpos (nth (+ 1 errpos) command-line-args)))
       (tagpos (or (position-if (lambda (x) (equal x "-tag-match")) command-line-args)
		   (position-if (lambda (x) (equal x "--tag-match")) command-line-args)))
       (tagval (if tagpos (nth (+ 1 tagpos) command-line-args))))
  (if errval 
      (setq org-dotemacs-error-handling (intern errval)))
  (if tagval (setq org-dotemacs-tag-match tagval)))

(message "org-dotemacs: error-handling = %s" (concat "'" (symbol-name org-dotemacs-error-handling)))
(message "org-dotemacs: tag-match = %s" org-dotemacs-tag-match)

;; This function was obtained from string-fns.el by Noah Friedman <friedman@splode.com>
;;;###autoload
(defun string-split (string &optional separator limit)
  "Split STRING at occurences of SEPARATOR.  Return a list of substrings.
Optional argument SEPARATOR can be any regexp, but anything matching the
 separator will never appear in any of the returned substrings.
 If not specified, SEPARATOR defaults to \"[ \\f\\t\\n\\r\\v]+\".
If optional arg LIMIT is specified, split into no more than that many
 fields \(though it may split into fewer\)."
  (or separator (setq separator "[ \f\t\n\r\v]+"))
  (let ((string-list nil)
        (len (length string))
        (pos 0)
        (splits 0)
        str)
    (save-match-data
      (while (<= pos len)
        (setq splits (1+ splits))
        (cond ((and limit
                    (>= splits limit))
               (setq str (substring string pos))
               (setq pos (1+ len)))
              ((string-match separator string pos)
               (setq str (substring string pos (match-beginning 0)))
               (setq pos (match-end 0)))
              (t
               (setq str (substring string pos))
               (setq pos (1+ len))))
        (setq string-list (cons str string-list))))
    (nreverse string-list)))

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
  (if (and target-file (string-match "\\(?:\\.emacs\\(?:\\.elc?\\)?\\|init\\.elc?\\)$" target-file))
      (error "Refuse to overwrite %s" target-file))
  (require 'ob-core)
  (cl-flet ((age (file) (float-time
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
          (setq matchbuf (org-dotemacs-extract-subtrees (or org-dotemacs-tag-match match)))
          (with-current-buffer matchbuf
            ;; Write the buffer out first to prevent org-babel-pre-tangle-hook
            ;; prompting for a filename to save it in.
            (write-file (concat temporary-file-directory (buffer-name)))
            (org-dotemacs-load-blocks target-file))
          (kill-buffer matchbuf))
        (unless visited-p
          (kill-buffer to-be-removed))))))

;;;###autoload
(defun* org-dotemacs-load-blocks (&optional target-file (errorhandling org-dotemacs-error-handling))
  "Load the emacs-lisp code blocks in the current org-mode file.
Save the blocks to TARGET-FILE if it is non-nil.
See the definition of `org-dotemacs-error-handling' for an explanation of the ERRORHANDLING
argument which uses `org-dotemacs-error-handling' for its default value."
  (run-hooks 'org-babel-pre-tangle-hook)
  (save-restriction
    (save-excursion
      (let* ((block-counter 1)
             (org-babel-default-header-args
              (org-babel-merge-params org-babel-default-header-args
                                      (list (cons :tangle (or target-file "yes")))))
             (specs (cdar (org-babel-tangle-collect-blocks 'emacs-lisp)))
             (get-spec (lambda (spec name) (cdr (assoc name (nth 4 spec)))))
             (try-eval (lambda (spec blockname)
                         (let ((dependencies (funcall get-spec spec :depends))
                               fail)
                           (if (cl-subsetp (and dependencies (string-split dependencies))
                                           evaluated-blocks)
                               (with-temp-buffer
                                 (ignore-errors (emacs-lisp-mode))
                                 (org-babel-spec-to-string spec)
                                 ;; evaluate the code
                                 (message "Evaluating %s code block" blockname)
                                 (setq fail nil)
                                 (if (member errorhandling '(skip retry))
                                     (condition-case err
                                         (eval-buffer)
                                       (error
                                        (setq fail 'error)
                                        (message "Error in %s code block: %s"
                                                 blockname (error-message-string err))))
                                   (eval-buffer))
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
                                 fail) 'unmet))))
             evaluated-blocks unevaluated-blocks unmet-dependencies)
        ;; delete any old versions of file
        (if (and target-file (file-exists-p target-file))
            (delete-file target-file))
        (mapc
         (lambda (spec)
           (let ((blockname (or (funcall get-spec spec :name)
                                (concat "block_" (number-to-string block-counter)))))
             (let ((fail (funcall try-eval spec blockname)))
               (case fail
                 (error
                  (setq unevaluated-blocks (append unevaluated-blocks (list (cons blockname spec)))))
                 (unmet
                  (setq unmet-dependencies (append unmet-dependencies (list (cons blockname spec)))))
                 (nil (if (eq errorhandling 'retry)
                          (while (not fail)
                            (loop for blk in (append unevaluated-blocks unmet-dependencies)
                                  do (setq fail (funcall try-eval (car blk) (cdr blk)))
                                  unless fail do (setq unevaluated-blocks (remove blk unevaluated-blocks))
                                  (setq unmet-dependencies (remove blk unmet-dependencies))
                                  and return t))))))
             (setq block-counter (+ 1 block-counter))))
         specs)
        (if (not unevaluated-blocks)
            (message "\norg-dotemacs: All blocks evaluated successfully!")
          (message "\norg-dotemacs: Successfully evaluated the following %d code blocks: %s"
                   (length evaluated-blocks)
                   (mapconcat 'identity evaluated-blocks " "))
          (message "\norg-dotemacs: The following %d code block%s errors: %s\n"
                   (length unevaluated-blocks)
                   (if (= 1 (length unevaluated-blocks)) " has" "s have")
                   (mapconcat 'car unevaluated-blocks " "))
          (message "\norg-dotemacs: The following %d code block%s unmet dependencies: %s\n"
                   (length unmet-dependencies)
                   (if (= 1 (length unmet-dependencies)) " has" "s have")
                   (mapconcat 'car unmet-dependencies " "))))
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
