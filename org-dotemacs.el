;; org-dotemacs.el --- Store your emacs config as an org file, and choose which bits to load.

;; Filename: org-dotemacs.el
;; Description: Store your emacs config as an org file, and load code snippets based on tag matches.
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-04-27 20:19:18
;; Version: 0.3
;; Last-Updated: 2013-09-09 20:19:18
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/org-dotemacs
;; Keywords: local
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((org "7.9.3") (cl-lib "1.0"))
;;
;; Features that might be required by this library:
;;
;; org cl
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
;; Bitcoin donations gratefully accepted: 1Ph9srQBspJCDS9CnGUyWJPTrU4ydX9Aa3
;;
;; Keeping your emacs config in an org file makes it easier for you to keep your .emacs under control,
;; and avoid DotEmacsBankruptcy.
;; With your config code stored in an org file you can easily edit the structure and keep notes.
;; Note: it can also be used for organizing other elisp config files such as .gnus.el and .ercrc.el.
;; 
;; This library allows you to load elisp code from an org file on emacs startup.
;; You can also limit the code that is loaded to certain tagged headers using an org tag match,
;; and specify dependencies between code blocks.
;; Using tag matches you can also reuse the same org file for different emacs setups by specifying different
;; tag matches for each setup, or load parts of the file on demand.
;; 
;;; Commands/Usage 
;; 
;; The main command is `org-dotemacs-load-default' which loads your default org-dotemacs file (~/.dotemacs.org)
;; and prompts for a tag match to specify which code blocks to load. 
;; In this way you can load bits of config code when you need them.
;; 
;; You can also put this command in your InitFile (see Installation below) to load the code on startup.
;; To change the default org file use the `org-dotemacs-default-file' option.
;; If you want to load a different org file from your default one, use `org-dotemacs-load-file'.
;; 
;; For faster loading you may prefer to keep your config code in a separate elisp file, and just update this file now and again
;; by exporting the code from the org file.
;; Use the `org-dotemacs-load-file' command for this and specify a target elisp file when prompted.
;; 
;;; Structure of the org file 
;; 
;; The elisp code should be contained in emacs-lisp code blocks, e.g:
;; 
;; #+BEGIN_SRC emacs-lisp
;; (setq line-number-mode t)
;; (setq column-number-mode t)
;; (setq frame-title-format "%b")
;; (set-background-color "Black")
;; (set-foreground-color "White")
;; (set-cursor-color "White")
;; #+END_SRC
;; 
;; Ideally you should have each code block under a separate org subtree, then you can use properties to
;; name the blocks and define dependencies, and tags and todo states to specify which blocks
;; should be loaded (see below). You can specify that certain blocks are loaded only when certain conditions hold
;; by customizing `org-dotemacs-conditional-tags'. By default operating system tags (linux, windows, mac, hurd, freebsd,
;; unix) are set to only load when the corresponding operating system is being used (as reported by the `system-type' variable).
;; So for example, any blocks tagged with "linux" will only be loaded if `system-type' is eq to 'gnu/linux.
;; These conditional tags are overridden by any tag-match supplied to the command line.
;; 
;; I prefer to keep all my code block subtrees under a single header, and use other headers for keeping notes,
;; defining buffer-wide properties, etc. This way I can get a nice column view of the code blocks
;; (see the columns view section below).
;; 
;;;  Block dependencies 
;; 
;; You can enforce dependencies between code blocks by defining NAME & DEPENDS properties for the subtrees containing the
;; blocks (preferred). The NAME property should contain the name of the block, and the DEPENDS property should contain a space
;; separated list of block names that this block depends on.
;; These properties will be applied to all code blocks in the subtree (see "Properties and Columns" in the org manual for
;; more details).
;; 
;; The NAME property can be overridden on a per block basis by adding a :name header arg to a block, and dependencies can be
;; augmented by adding a :depends header arg (see "Header arguments" in the org manual).
;; However it is recommended to keep a separate subtree for each code block and use properties for defining headers and names
;; since then you can get a column view of the dependencies (see below).
;; 
;; A block will not be loaded until all of its dependencies have been loaded.
;; 
;;;  Tags and TODO states 
;; 
;; You can tag your subtrees and use tag matches to specify which blocks to evaluate in calls to `org-dotemacs-load-file'
;; and `org-dotemacs-load-default'. See "Matching tags and properties" in the org manual for more information on tag matches.
;; 
;; Also, by default any blocks in a subtree marked with a todo state of BROKEN will not be evaluated.
;; You can specify which TODO states to include/exclude for evaluation by customizing the `org-dotemacs-include-todo' and
;; `org-dotemacs-exclude-todo' options.
;; 
;; To add the BROKEN state to the list of todo states for the file you need to add buffer-wide todo states by adding a line
;; like this somewhere in your org file (see "Per file keywords" in the org manual).
;; 
;; #+TODO: BROKEN CHECK TODO
;; 
;;;  Columns View 
;; 
;; If you use properties for defining names and dependencies then you can get a nice column view of your code subtrees
;; with the following columns view specification:
;; 
;; #+COLUMNS: %35ITEM %15NAME %35DEPENDS %15TAGS %TODO
;; 
;; This can be placed anywhere in your dotemacs org file.
;; Then if you press C-c C-x C-c on the toplevel header for your code blocks you'll get a column view that allows you
;; to easily change the names, dependencies, tags and todo states.
;; 
;;;  Error handling 
;; 
;; Error handling can be controlled by customizing `org-dotemacs-error-handling' or by setting the error-handling
;; command line option when starting emacs.
;; By default code blocks with unmet dependencies or errors are skipped over as soon as an error is encountered,
;; but you can also specify that org-dotemacs should halt or try to reload the blocks.
;; In the latter case each time a new block is successfully loaded, any unsuccessful blocks will be retried.
;; 
;;;  Command line options 
;; 
;; org-dotemacs.el will look for two command line options when loaded: error-handling (for setting the value of
;; `org-dotemacs-error-handling') and tag-match (for specifying which headers to load).
;; For example if you enter the following at the command line:
;; 
;;        emacs --error-handling retry --tag-match "settings-mouse"
;; 
;; Then only code blocks tagged "settings" but not "mouse" will be loaded, and org-dotemacs will try to reload any
;; blocks that have errors. If no tag-match is specified on the command line then `org-dotemacs-conditional-tags'
;; will be used to determine which blocks can be loaded by default.
;; 
;;;  Installation 
;; 
;; Put org-dotemacs.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;; 
;; Then make sure you have an ~/.dotemacs.org file and add the following lines to
;; the end of your .emacs file:
;; 
;; (load-file "~/.emacs.d/org-dotemacs.el")
;; (org-dotemacs-load-default)
;; 
;; or if you want to just load code blocks matching a tag match:
;; 
;; (load-file "~/.emacs.d/org-dotemacs.el")
;; (org-dotemacs-load-default "<TAG-MATCH>")
;; 
;; To load a different org file either customize `org-dotemacs-default-file' or use the
;; `org-dotemacs-load-file' function, e.g:
;; 
;; (load-file "~/.emacs.d/org-dotemacs.el")
;; (org-dotemacs-load-file "<TAG-MATCH>" "~/.emacs.d/my_emacs_config.org")
;; 


;;; Customize:
;;
;; `org-dotemacs-conditional-tags' : A list of tags/regexps and corresponding conditions for loading blocks.
;; `org-dotemacs-default-file' : The default org file containing the code blocks to load when `org-dotemacs-load-file' is called.
;; `org-dotemacs-error-handling' : Indicates how errors should be handled by `org-dotemacs-load-blocks'.
;; `org-dotemacs-include-todo' : A regular expression matching TODO states to be included.
;; `org-dotemacs-exclude-todo' : A regular expression matching TODO states to be excluded.
;;
;; All of the above can customized by:
;;      M-x customize-group RET org-dotemacs RET
;;

;;; Change log:
;; 4-May-2013      
;;    Last-Updated: 2013-04-27 20:19:18 (Joe Bloggs)
;;    ;;    
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
;; Option to show prominent warning message if some blocks didn't load (e.g. in large font in dedicated buffer after startup)
;; 

;;; Require
(eval-when-compile (require 'cl))
(require 'org)


(defmacro chong-debug (&rest args)
  ""
  (let ((header (car args)))
    (if header
      `(progn ,@args))
    ))

(defalias 'cl-flet 'flet)

(defun cl-position-if (cl-pred cl-list &rest cl-keys)
  (apply 'position nil cl-list :if cl-pred cl-keys))

(defun cl-position-if-not (cl-pred cl-list &rest cl-keys)
  (apply 'position nil cl-list :if-not cl-pred cl-keys))


(defun cl-member (cl-item cl-list &rest cl-keys)
  (declare (compiler-macro cl--compiler-macro-member))
  (if cl-keys
      (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-list (not (cl-check-test cl-item (car cl-list))))
	  (setq cl-list (cdr cl-list)))
	cl-list)
    (if (and (numberp cl-item) (not (integerp cl-item)))
	(member cl-item cl-list)
      (memq cl-item cl-list))))

(defun cl-subsetp (cl-list1 cl-list2 &rest cl-keys)
      (cond ((null cl-list1) t) ((null cl-list2) nil)
	    ((equal cl-list1 cl-list2) t)
	    (t (cl-parsing-keywords
		(:key)
		(:test :test-not)
		(while (and cl-list1
			    (apply 'cl-member (cl-check-key (car cl-list1))
				   cl-list2 cl-keys))
		  (pop cl-list1))
		(null cl-list1)))))

;;; Code:

;; M-x customize-group RET org-dotemacs RET  原来是这么使用 group 的
(defgroup org-dotemacs nil
  "Store your emacs config as an org file, and choose which bits to load."
  :group 'org)

(defcustom org-dotemacs-default-file "~/.dotemacs.org"
  "The default org file containing the code blocks to load when `org-dotemacs-load-file' is called."
  :group 'org-dotemacs
  :type '(file :must-match t))

(defcustom org-dotemacs-error-handling 'skip
  "Indicates how errors should be handled by `org-dotemacs-load-blocks'.
If eq to 'skip then errors will be skipped over (default).
If eq to 'retry then `org-dotemacs-load-blocks' will attempt to reload any blocks containing errors,
after each successfully loaded block.
In all other cases errors will cause evaluation to halt as normal.
In all cases errors will be reported in the *Messages* buffer as normal.

This variable can be set from the command line using the dotemacs-error-handling argument."
  :group 'org-dotemacs
  :type 'symbol)

(defcustom org-dotemacs-include-todo nil
  "A regular expression matching TODO states to be included.
If non-nil then only headers with TODO states matching this regexp will be checked for code blocks.
See also `org-dotemacs-exclude-todo'."
  :group 'org-dotemacs
  :type 'regexp)

(defcustom org-dotemacs-exclude-todo "BROKEN"
  "A regular expression matching TODO states to be excluded.
If non-nil then headers with TODO states matching this regexp will not be checked for code blocks.
See also `org-dotemacs-include-todo'."
  :group 'org-dotemacs
  :type 'regexp)

(defcustom org-dotemacs-conditional-tags '(("linux" . (eq system-type 'gnu/linux))
                                           ("windows" . (member system-type '(ms-dos windows-nt cygwin)))
                                           ("mac" . (eq system-type 'darwin))
                                           ("hurd" . (eq system-type 'gnu))
                                           ("freebsd" . (eq system-type 'gnu/kfreebsd))
                                           ("unix" . (member system-type '(aix berkely-unix hpux irix usg-unix-v))))
  "A list of tags/regexps and corresponding conditions for loading blocks.
If a block has a tag matching a regexp in this list then it will only be loaded if the corresponding
condition evaluates to non-nil. All other blocks are loaded as normal.
This behaviour is overridden if a tag-match is supplied on the command line."
  :group 'org-dotemacs
  :type '(repeat (cons (string :tag "Tag/regexp:")
                       (sexp :tag "Predicate expression:"))))

;;;###autoload
;; simple-call-tree-info: DONE
(defun org-dotemacs-default-match nil
  "Returns the default tag match string based on items in `org-dotemacs-conditional-tags' (which see)."
  (let ((str (loop for (regex . condition) in org-dotemacs-conditional-tags
                   if (eval condition) concat (concat regex "\\|"))))
    (if (not (equal str ""))
        (concat "-{" (substring str 0 -2) "}"))))

(defvar org-dotemacs-tag-match nil
  "An org tag match string indicating which code blocks to load with `org-dotemacs-load-file'.
This overrides the match argument to `org-dotemacs-load-file' and is set by the emacs command line
argument '--tag-match'.")

(defvar org-dotemacs-evaluated-blocks nil
  "A list of names of blocks that have already been evaluated")

;;;; main interactive

;;;###autoload
;; simple-call-tree-info: CHANGE  
(defun* org-dotemacs-load-file (&optional match
                                          (file org-dotemacs-default-file)
                                          target-file
                                          (error-handling org-dotemacs-error-handling))
  "Load the elisp code from code blocks in org FILE under headers matching tag MATCH.
Tag matches supplied at the command line get priority over those supplied by the MATCH argument,
and if both of these are nil then `org-dotemacs-default-match' will be used to create a tag match.
If you need to override the command line tag-match set `org-dotemacs-tag-match' to nil.
If TARGET-FILE is supplied it should be a filename to save the elisp code to, but it should
not be any of the default config files .emacs, .emacs.el, .emacs.elc or init.el
 (the function will halt with an error in those cases). If TARGET-FILE is newer than FILE then
TARGET-FILE will be loaded and FILE will not be processed. Otherwise TARGET-FILE will be overwritten
by the code blocks in FILE.
The optional argument ERROR-HANDLING determines how errors are handled and takes default value
`org-dotemacs-error-handling' (which see)."
  (interactive (list nil
                     (read-file-name (format "File to load (default %s): " org-dotemacs-default-file)
                                     (file-name-directory org-dotemacs-default-file)
                                     org-dotemacs-default-file
                                     t nil
                                     (lambda (file)
                                       (string-match "\\.org$" file)))
                     (if (y-or-n-p "Save elisp code to separate file?")
                         (read-file-name "Save to file: " user-emacs-directory))))
  (if (and target-file (string-match "\\(?:\\.emacs\\(?:\\.elc?\\)?\\|init\\.elc?\\)$" target-file))
      (error "org-dotemacs: Refuse to overwrite %s" target-file))
  ;;(require 'ob-core)
  (require 'ob)
  (cl-flet ((age (file) (float-time
                         (time-subtract (current-time)
                                        (nth 5 (or (file-attributes (file-truename file))
                                                   (file-attributes file)))))))
    (if (and target-file
             (file-exists-p target-file)
             (> (age file) (age target-file)))
        (load-file target-file)
      (org-dotemacs-load-blocks-1 file  match target-file)
      )))

(defvar org-dot-emacs-new-name 0  "")

(defun org-dot-emacs-new-name ()
  ""
  (let ((temp "*org-dot-emacs*-"))
    (setq org-dot-emacs-new-name (1+ org-dot-emacs-new-name))
    (concat temp org-dot-emacs-new-name)
    ))
;;(org-dotemacs-load-blocks-1 "dotemacs-test.org" "tag1")
(defun* org-dotemacs-load-blocks-1 (file match &optional target-file
                                       (error-handling org-dotemacs-error-handling))
 (let* ((todo-only nil)
        (matcher (cdr (org-make-tags-matcher match))) ; 判定
        (blocks nil)
        unevaluated-blocks unmet-dependencies)
   (org-update-init)
   (org-babel-map-src-blocks file
     (let* ((tags-list (org-get-tags-at))	      ; tag有什么用吗？
            (name (org-entry-get (point) "NAME" nil)) ;subsetp 的意思是 是否是子集
	    (deps (org-entry-get (point) "DEPENDS" org-use-property-inheritance)) ; 是不是所有的依赖都满足了
	    (dep-blocks nil))		; 用他赖存储所有被依赖的其他block 如果不符合也没有关系
       (when deps			; 在他有依赖的时候就得到依赖
	 (setq deps (split-string deps "[ \s\t]+" t)))
       (unless name (setq name (org-dot-emacs-new-name)))
       (unless body (setq body ""))
       (cond ((and (equal lang "emacs-lisp")
                (eval matcher))		; 直接依赖
	      (org-update-graphy name deps body)
	      (add-to-list 'org-dot-emacs-main-graph name))
	     (t (org-update-graphy name deps body))	; 当他被别人间接依赖的时候
	    )
       ))
   (chong-debug  ;; main 首先是符合条件的名字集合 然后是所有依赖的集合（仅仅是名字）
    (message "org-dot-emacs-main-graph is")(print org-dot-emacs-main-graph))
   (org-dot-emacs-main-graph ) ;; update the main-graphy
   (chong-debug
    (message "org-dot-emacs-main-graph updated is :")(print org-dot-emacs-main-graph))
   (dolist (name org-dot-emacs-main-graph) ; subset 是图（根据main中所有的名字而得到的）
     (add-to-list 'org-dot-emacs-graph-subset (assoc name org-dot-emacs-graph)))
   (setq org-dot-emacs-main-graph (topological-sort org-dot-emacs-graph-subset :test 'equal))
   (org-dot-emacs-load org-dot-emacs-main-graph)
   (if target-file
       (org-dot-emacs-write-file target-file)
     (message "org-dot-emacs has loaded %s" file)
     )
   ))

(defun org-dot-emacs-tag-file (file-name tags)
  (let ()
    (concat file-name))
  )

(defun org-dot-emacs-write-file (file-name)
  "write to the file of file-name"
  (let (block-body)
    (with-temp-buffer
      (mapc
       (lambda (block-name)
	 (setq block-body (gethash block-name org-dot-emacs-blocks))
	 (insert "\n" body "\n")
	 )
       (reverse evaluated-blocks))
      (write-file filename))))

;;;; graph relatived

;; The following function was swiped from el-get-dependencies.el : https://github.com/dimitri/el-get/
;; simple-call-tree-info: DONE
;;;###autoload
(unless (functionp 'topological-sort)
  (defun* topological-sort (graph &key (test 'eql))
    "Returns a list of packages to install in order.
  Graph is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
Test is used to compare elements, and should be a suitable test for
hash-tables.  Topological-sort returns two values.  The first is a
list of objects sorted toplogically.  The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
    (let* ((entries (make-hash-table :test test))
           ;; avoid obsolete `flet' & backward-incompatible `cl-flet'
           (entry (lambda (v)
                    "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
                    (or (gethash v entries)
                        (puthash v (cons 0 '()) entries)))))
      ;; populate entries initially
      (dolist (gvertex graph)
        (destructuring-bind (vertex &rest dependencies) gvertex
          (let ((ventry (funcall entry vertex)))
            (dolist (dependency dependencies)
              (let ((dentry (funcall entry dependency)))
                (unless (funcall test dependency vertex)
                  (incf (car ventry))
                  (push vertex (cdr dentry))))))))
      (chong-debug nil
       (message "the new graphy generated by topological-sort :")
       (print entries)
       )
      ;; L is the list of sorted elements, and S the set of vertices
      ;; with no outstanding dependencies.
      (let ((L '())
            (S (loop for entry being each hash-value of entries
                     using (hash-key vertex)
                     when (zerop (car entry)) collect vertex)))
        ;; Until there are no vertices with no outstanding dependencies,
        ;; process vertices from S, adding them to L.
        (do* () ((endp S))
          (let* ((v (pop S)) (ventry (funcall entry v)))
            (remhash v entries)
            (dolist (dependant (cdr ventry) (push v L))
              (when (zerop (decf (car (funcall entry dependant))))
                (push dependant S)))))
	(chong-debug nil
	 (message "new graphy after deleted zero dependecy :")
	 (print entries))
        ;; return (1) the list of sorted items, (2) whether all items
        ;; were sorted, and (3) if there were unsorted vertices, the
        ;; hash table mapping these vertices to their dependants
        (let ((all-sorted-p (zerop (hash-table-count entries))))
          (values (nreverse L)   all-sorted-p
                  (unless all-sorted-p (dependecy-view entries)))))))
)

(defun dependecy-view (hash-table-graph)
  "用一个用户友好的方式显示元素之间的依赖关系
用不用首先排序一下呢？ 先不想那么复杂了吧"
  (let ((finish nil) root dep-list  cur cur-be-deps ret)
    (let ((max 0)  be-deps)
      (maphash
       (lambda (k v)
	 (setq be-deps (cdr v))
	 (setcar v 1)
	 (when (> (length be-deps) max)
	   (setq cur k
		 max (length be-deps))))
       hash-table-graph)
      (chong-debug nil
       (message "the `cur' is: %s  and items be depent are" cur )
       (print (gethash cur hash-table-graph))))
    (setq root cur)  (push cur dep-list)
    (setq cur-be-deps (gethash cur hash-table-graph))
    (setq cur (nth (car cur-be-deps) cur-be-deps))
    (when (equal (length (cdr cur-be-deps)) 0)
      (error "the graphy no dependecy")) 
    (setcar cur-be-deps (1+ (car cur-be-deps)))
    (setq finish nil)
    (while (not finish) 
      (when (null cur) (error "pushed nil")) ;; 当有错误的时候 就停止打印
      (push cur dep-list)  ;; 可以省略他的  在以后的时候才push
      (chong-debug nil (message "pushed %s" cur) (print dep-list))
      (setq cur-be-deps (gethash cur hash-table-graph))
      (if (equal 0 (length (cdr cur-be-deps))) ; 说明他已经遇到了空尾 应该回溯了
	(progn
	  (pop dep-list) ; 遇到了空尾的时候就弹出自己 并将先前的也弹出 那cur是谁呢？
	  (setq cur (pop dep-list)))
	(setq cur (nth (car cur-be-deps) cur-be-deps)) ;正常的时候是得到被依赖 并继续
	(if (not (null cur))			       ;如果能够继续的找到下一个节点 就改变指针
	    (setcar cur-be-deps (1+ (car cur-be-deps)))
	  (if (equal root (car dep-list)) ;; 当root得到的也是nil的时候
	      (setq finish t)
	    (pop dep-list) (setcar cur-be-deps 1)	; 然后将我的指针变成 0
	    (setq cur (pop dep-list))		; 将我的前任也弹出  会继续处理我的前任的
	    )))
      ;;(chong-debug (message "befor judge ： cur %s  dep-list:" cur) (print dep-list))
      (when (member cur dep-list)	;当出现重复的时候 就应该
	(push  (cons cur dep-list) ret)
	(setq cur (nth (car cur-be-deps) cur-be-deps))
	(setcar cur-be-deps (1+ (car cur-be-deps))) ;设置下一个节点
	(while (and cur (member cur dep-list))
	    (push (cons cur dep-list) ret))
	(when (null cur) (setq cur (pop dep-list))))
      ) ret))


(defun mystr (&rest args)
  "like `make-string' with more args "
  (with-temp-buffer
    (let ((i nil) )
	(mapc
	 (lambda (it)
	   (cond
	    ((numberp it)(setq i it))
	    ((and (stringp it) (numberp i))
	     (insert (make-string i (string-to-char it)))
	     (setq i nil))
	    ((stringp it ) (insert it))))
	 args))
    (buffer-string)))

(defvar  cycle-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m"
      (lambda ()
	(interactive)
	(let* ((file (get-text-property (point) 'file))
	       (name (thing-at-point 'symbol))
	       (buf (find-buffer-visiting file)))
	  (if buf
	      (switch-to-buffer buf)
	    (find-file file)
	    )
	  (goto-char 1)
	  ;;find the named block
	  (progn
	    (message "finding the position of named block: %s" name))
	  )
	))
    map)
  "you can use [return] jump to the block define position")

(defun show-cycle (a-list &optional direct)
  ""
  (let* ((file "~/.test.org")
	 (tail (cdr a-list))
	(last-name (if (stringp (car (last tail)))
		       (car (last tail))
		     (symbol-name (car (last tail)))))
	(first-name (if (stringp (car tail))
		       (car tail)
		     (symbol-name (cadr a-list))))	
	(pre (/ (length first-name) 2))
	(suf (/ (length last-name) 2)))
    (with-temp-buffer
      (let (list-str)
	(setq list-str
	      (mapconcat
	       (lambda (item)
		 (let ((name (if (stringp item)
				 item
			       (symbol-name item))))
		   (propertize name 'file file 'local-map cycle-map)
		   ))
	       tail (if direct " --> " " <-- ")))
	(print list-str)
	(setq mid-len (- (length list-str) pre suf))
	(insert (mystr  pre " " mid-len "-" "\n")
		(mystr  pre " " "|" (- mid-len 2) " " "|" "\n")
		(mystr  pre " " (if direct "V" "|") (- mid-len 2) " "
			(if direct "|" "V") "\n"))
	(insert list-str "\n")
	(buffer-string)))))
  
(defvar org-dot-emacs-graph nil
  "an assocate list ,it's item is (name . (dependants..))
will be filte to get `org-dot-emacs-graph-subset'")

(defvar org-dot-emacs-graph-subset nil
  "an assocate list ,as the argument of `topological-sort'")

(defvar org-dot-emacs-main-graph nil
  "alist ,all matched block will be there")

(defvar org-dot-emacs-blocks (make-hash-table :test 'equal)
  "a hashtable ,the key is the name of block,
and the value is body as in `org-babel-map-src-blocks'")

;;
(defun org-update-init ()
  "clear global variable"
  (setq org-dot-emacs-graph nil)
  (setq org-dot-emacs-graph-subset nil)
  (setq org-dot-emacs-main-graph nil)
  (setq org-dot-emacs-blocks (make-hash-table :test 'equal)))

(defun org-update-graphy (name deps body)
  "update global variable"
  (let ((old-body (gethash name org-dot-emacs-blocks))
	)
    (if old-body
	(error "The name %s has been defined above" name)
      (setq org-dot-emacs-graph (cons (cons name  deps) org-dot-emacs-graph))
      (puthash name body org-dot-emacs-blocks))
    ))

(defun org-dot-emacs-main-graph (&optional number)
  "get all dependeced block-names"
  (let ((tail (if (and number (numberp number))
		  (nthcdr number org-dot-emacs-main-graph)
		org-dot-emacs-main-graph))
	ret deps
	(count 0))
    (chong-debug (message "in func main the tail is:")(print tail))
    (dolist (it tail)
      (setq deps (cdr (assoc it org-dot-emacs-graph)))
      (chong-debug
       (when (null deps)
	 (message "the deps : %s --> nil" it)))
      (dolist (dep deps)
	(chong-debug
	 (message "the deps : %s  --> %s" it dep))
	(if (member dep org-dot-emacs-main-graph)
	    nil
	  (chong-debug (message "new item :%s" dep))
	  (add-to-list 'org-dot-emacs-main-graph dep t) ;append to main-graphy
	  (setq count (1+ count)))))
    (when (> count 0)
	(org-dot-emacs-main-graph
	 (- (length org-dot-emacs-main-graph) count))
	)))

;;;;;     load blocks

(defun org-dot-emacs-load (main-graph)
  "load th blocks in `org-dot-emacs-main-graph',return by `topological-sort'"
  (let ((undeps (car main-graph))
	(deps (nth 2 main-graph))
	undep-error fail new-deps error-item dep
	(count 0))
    (setq undep-error (org-dot-emacs-load-undeps undeps))
    (while (and (< count 5) deps)
      (while  deps  ;; dep 本身就是一个list
	(setq dep (pop deps))
	(setq error-item nil)
	(dolist (dep-item dep)
	  (setq fail (try-eval-1 dep-item))
	  (when fail
	    (push  dep-item error-item))
	  )
	(when error-item
	  (push error-item new-deps)))
      (setq deps new-deps)(setq new-deps nil)
      (setq count (1+ count))
      )
    (chong-debug nil (message "undep errors")(print undep-error))
    (org-dot-emacs-load-undeps undep-error 'report) ;装载两次
    (dolist (dep deps)
      (org-dot-emacs-load-undeps dep 'report) ;这个时候 只能用出错的方式做提醒了
      )))


(defun org-dot-emacs-load-undeps (undeps-list &optional report-error)
  ""
  (let (error-blocks fail)
    (dolist (block-name undeps-list)
      (setq fail (try-eval-1 block-name))
      (when fail
	;; errored will be the errored blocks
	(push block-name  error-blocks)
	(if report-error
	    (message "block %s load error with : %s" block-name fail))
	))
    error-blocks))

(defun try-eval-1 (blockname)
  "load the code named blockname.
no dependant should be handle,only the block itself"
  (let* (fail
	 (code-str (gethash blockname org-dot-emacs-blocks))
	 )
    (if (member blockname org-dotemacs-evaluated-blocks)
	(progn
	  (message "the block named %s has be loaded." blockname)
	  nil
	  )
	(with-temp-buffer
	  (ignore-errors (emacs-lisp-mode))
	  (insert code-str)
	  (message "org-dotemacs: Evaluating %s code-block" blockname)
	  (setq fail nil)
	  (condition-case err
	      (eval-buffer)
	    (error
	     (setq fail (error-message-string err))
	     (message "org-dotemacs: Error in %s code block: %s"
		      blockname fail))))
	  (unless fail
	    (setq org-dotemacs-evaluated-blocks (cons blockname org-dotemacs-evaluated-blocks )))
	  fail)))


;; (org-dotemacs-load-file "tag1" "dotemacs-test.org")

;;;###autoload
;; simple-call-tree-info: CHANGE  
(defun* org-dotemacs-load-default (&optional match)
  "Load code from `org-dotemacs-default-file' matching tag MATCH.
Unlike `org-dotemacs-load-file' the user is not prompted for the location of any files,
and no code is saved."
  (interactive (list nil))
  (org-dotemacs-load-file match org-dotemacs-default-file nil))

;; Code to handle command line arguments
(let* ((errpos (or (cl-position-if (lambda (x) (equal x "-error-handling")) command-line-args)
		   (cl-position-if (lambda (x) (equal x "--error-handling")) command-line-args)))
       (errval (if errpos (nth (+ 1 errpos) command-line-args)))
       (tagpos (or (cl-position-if (lambda (x) (equal x "-tag-match")) command-line-args)
		   (cl-position-if (lambda (x) (equal x "--tag-match")) command-line-args)))
       (tagval (if tagpos (nth (+ 1 tagpos) command-line-args))))
  (if errval 
      (setq org-dotemacs-error-handling (intern errval)))
  (if tagval (setq org-dotemacs-tag-match tagval)))

(message "org-dotemacs: error-handling = %s" (concat "'" (symbol-name org-dotemacs-error-handling)))
(message "org-dotemacs: tag-match = %s" org-dotemacs-tag-match)

(provide 'org-dotemacs)			;(buffer-size)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "org-dotemacs.el" (buffer-name) (buffer-string) "update")

;;; org-dotemacs.el ends here
