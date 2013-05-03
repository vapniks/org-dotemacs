# org-dotemacs.el --- Store your emacs config as an org file, and choose which bits to load.

* Filename: org-dotemacs.el
* Description: Store your emacs config as an org file, and load code snippets based on tag matches.
* Author: Joe Bloggs <vapniks@yahoo.com>
* Maintainer: Joe Bloggs <vapniks@yahoo.com>
Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
* Created: 2013-04-27 20:19:18
* Version: 0.1
* Last-Updated: 2013-04-27 20:19:18
          By: Joe Bloggs
* URL: http://www.emacswiki.org/emacs/download/org-dotemacs.el
* Keywords: local
* Compatibility: GNU Emacs 24.3.1
* Package-Requires: ((org "7.9.3") (cl-lib "1.0"))
;; Features that might be required by this library:
;; org cl

# This file is NOT part of GNU Emacs

# License
Licensed under the [GPL version 3](http://www.gnu.org/licenses/) or later.

# Commentary: 
;; Bitcoin donations gratefully accepted: 1Ph9srQBspJCDS9CnGUyWJPTrU4ydX9Aa3
;; Keeping your emacs config in an org file makes it easier for you to keep your .emacs under control,
and avoid dotemacs bankruptcy (http://www.emacswiki.org/emacs/DotEmacsBankruptcy).
With your config code stored in an org file you can easily edit the structure and keep notes.
This library allows you to load elisp code from an org file on emacs startup.
You can also limit the code that is loaded to certain tagged headers using an org tag match,
and specify dependencies between code blocks.

First you need to create an org file ~/.dotemacs.org and add your config code to emacs-lisp code blocks
in this file. You can use another file location if you wish, see `org-dotemacs-default-file' below.
;; To aid debugging you can name the code blocks by adding NAME properties to the corresponding org headers
(see the "Properties and columns" in the org manual). You can also introduce dependencies between the
blocks by creating DEPENDS properties containing space separated lists of block names. org-dotemacs will
only load a block if all its dependencies have already been successfully loaded.
You may also decide to tag the headers so that you can filter out which code blocks to load with a tag match
(see "Matching tags and properties" in the org manual). 
After doing that your .dotemacs.org file might look something like this:

* Display settings code                :settings:
  :PROPERTIES:
  :DEPENDS: 
  :NAME:     display_settings
  :END:
#+BEGIN_SRC emacs-lisp
(setq line-number-mode t)
(setq column-number-mode t)
(setq frame-title-format "%b")
(set-background-color "Black")
(set-foreground-color "White")
(set-cursor-color "White")
#+END_SRC
* Scrolling settings code              :settings:mouse:
  :PROPERTIES:
  :DEPENDS:  display_settings other_settings
  :NAME:     scrolling_settings
  :END:
#+BEGIN_SRC emacs-lisp
(mouse-wheel-mode t)
(setq scroll-step 1)
(setq scroll-conservatively 5)
#+END_SRC

To load code blocks on startup you need to ensure that this file is loaded before calling
`org-dotemacs-load-default', see installation below.
If you do M-x org-dotemacs-load-default, you will be prompted for a tag match and the corresponding
code blocks will be loaded (enter an empty tag match string to load all blocks). In this way you can load
just the parts of your config file that you need, when you need them.
If you have another org dotemacs file you can use `org-dotemacs-load-file' to load the code blocks from
this file and optionally save them to an elisp file. This can be loaded from your .emacs faster than
the org file.
;; Error handling can be controlled by customizing `org-dotemacs-error-handling' or by setting the error-handling
command line option when starting emacs.
By default code blocks with unmet dependencies or errors are skipped over as soon as an error is encountered,
but you can also specify that org-dotemacs should halt or try to reload the blocks.
In the latter case each time a new block is successfully loaded, any unsuccessful blocks will be retried.
;; Command line options:
;; org-dotemacs.el will look for two command line options when loaded: error-handling (for setting the value of
`org-dotemacs-error-handling') and tag-match (for specifying which headers to load).
For example if you enter the following at the command line: emacs --error-handling retry --tag-match "settings-mouse"
Then only code blocks tagged "settings" but not "mouse" will be loaded, and org-dotemacs will try to reload any
blocks that have errors.


# Installation
;; Put org-dotemacs.el in a directory in your load-path, e.g. ~/.emacs.d/
You can add a directory to your load-path with the following line in ~/.emacs
(add-to-list 'load-path (expand-file-name "~/elisp"))
where ~/elisp is the directory you want to add 
(you don't need to do this for ~/.emacs.d - it's added by default).
;; Then make sure you have an ~/.dotemacs.org file and add the following lines to
the end of your .emacs file:
;; (load-file "~/.emacs.d/org-dotemacs.el")
(org-dotemacs-load-default)
;; or if you want to just load code blocks matching a tag match:
;; (load-file "~/.emacs.d/org-dotemacs.el")
(org-dotemacs-load-default "<TAG-MATCH>")
;; See the org manual "Matching tags and properties" section for more details on tag matches.

To load a different org file either customize `org-dotemacs-default-file' or use the
`org-dotemacs-load-file' function, e.g:
;; (load-file "~/.emacs.d/org-dotemacs.el")
(org-dotemacs-load-file "~/.emacs.d/my_emacs_config.org" "<TAG-MATCH>")

# Customize
;; `org-dotemacs-default-file' : The default org file containing the code blocks to load when `org-dotemacs-load-file'
                              is called.
`org-dotemacs-error-handling' : Specify how errors are handled.
;; All of the above can customized by:
     M-x customize-group RET org-dotemacs RET

# Change log

2013/04/27
     * First released.


# Acknowledgements
;; 

# TODO
;; Upload to elpa/melpa/marmalade and emacswiki
;; 

# Require
