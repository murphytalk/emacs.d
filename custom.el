;;===========================================================================
(server-start)


;;===========================================================================

;;-----------------------------------------
;;Title format : buffer name @ hostname 
;;------------------------------------------
(setq frame-title-format (concat "%b@emacs." system-name))

;;turn off toolbar
(tool-bar-mode 0)
;;turn off menubar
(menu-bar-mode 0)
;;use visual bell
(setq visible-bell 1)

;;-------------------------------------------
;; save emacs custom craps in another file
;;-------------------------------------------
(setq custom-file "~/.emacs.d/.my-emacs-custom.el")
;(load custom-file)

;;-------------------------------------------
;; color theme
;;-------------------------------------------
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-robin-hood)
;(color-theme-ld-dark)
;(abyss-theme)

;;-------------------------------------------
;; GUI font
(when (display-graphic-p)
  (if *win64*
    (setq my-font "Consolas-10")
    (setq my-font "Inconsolata-11")
  )
  (set-default-font my-font)
  (set-face-attribute 'default t :font my-font)
  (set-fontset-font "fontset-default"
                    'gb18030 '("Microsoft YaHei" . "unicode-bmp"))
)
;;-------------------------------------------

;;---------------------------------------------------
;; highlight the matching parens
;;---------------------------------------------------
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;--------------------------------------------------------------------------------------
;; Go to the matching paren if on a paren; otherwise insert %.
;;--------------------------------------------------------------------------------------
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t (self-insert-command (or arg 1)))))

;;and bind it to Ctrl-% key
;(global-set-key "%" 'match-paren)
(global-set-key [(control ?\% )] 'match-paren)


;;==========================================================================
;; MoinMoin mode
;;==========================================================================
(load-file "~/.emacs.d/lisp/screen-lines.el")
(load-file "~/.emacs.d/lisp/moinmoin-mode.el")


;;==========================================================================
;; org publication
;;==========================================================================
(setq org-idx (getenv "ORGIDX"))
(when (not (equal nil org-idx))
  (require 'ox-publish)
  ;; Setup
  (setq org-base-dir (file-name-directory org-idx))
  (setq org-pub-dir (concat org-base-dir "html"))
  (setq org-publish-project-alist
        `(("org-files"
           :base-directory  ,org-base-dir
           :base-extension "org"
           :recursive t
           :publishing-directory ,org-pub-dir
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :auto-preamble t
           :html-head-extra "<link rel=\"stylesheet\" href=\"https://jgkamat.github.io/src/jgkamat.css\">"
           )
          ("static-files"
           :base-directory ,org-base-dir
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory ,org-pub-dir
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("org" :components ("org-files" "static-files"))))
  )

;;==========================================================================
;; Code parser
;;==========================================================================
;; for cscope
(setq *has-cscope* (not (equal nil (executable-find "cscope"))))
(when *has-cscope*
  (load-file "~/.emacs.d/lisp/xcscope.el")
  (require 'xcscope)
  (cscope-setup)
  (add-hook 'python-mode-hook (function cscope-minor-mode))
  (setq cscope-option-do-not-update-database 't))

;; for cquery
(setq *has-cquery* (not (equal nil (executable-find "cquery"))))
(when *has-cquery*
  (require 'cquery))

;;==========================================================================
;; Emacs version compatability
;;==========================================================================
(when (or *emacs25* (and *emacs24* (not *emacs24old)))
  (superword-mode t))

;;==========================================================================
;;Override upstream configuration
;;==========================================================================
;; w3m search
;; C-u S g RET <search term> RET in w3m
(setq w3m-search-engine-alist
      '(("g" "http://www.google.co.jp/search?q=%s" utf-8)
        ;; stackoverflow search
        ("q" "http://www.google.co.jp/search?q=%s+site:stackoverflow.com" utf-8)
        ;; elisp code search
        ("s" "http://www.google.co.jp/search?q=%s+filetype:el"  utf-8)
        ;; wikipedia
        ("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
        ;; online dictionary
        ("d" "http://dictionary.reference.com/search?q=%s" utf-8)
        ;; java google search
        ("java" "https://www.google.co.jp/search?q=java+%s" utf-8)
        ;; financial dictionary
        ("f" "http://financial-dictionary.thefreedictionary.com/%s" utf-8)
        ;; javascript search on mozilla.orgb
        ("j" "http://www.google.co.jp/search?q=%s+site:developer.mozilla.org" utf-8)))



;;==========================================================================
;;File/buffer name convention: this-is-a-post.rst
;;==========================================================================
(defun my-blog-rst ()
  (interactive)
  (setq title (replace-regexp-in-string ".rst" "" (replace-regexp-in-string "-" " " (buffer-name))))
  (setq bar (make-string (length title) ?=))
  (insert bar)
  (newline)
  (insert title)
  (newline)
  (insert bar)
  (newline)
  (insert ":category: ")
  (newline)
  (insert ":tags: ")
  (newline)
  (insert ":date: ")
  (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
  (newline)
  (newline)
  )

;;===========================================================================
;; Keys mapping
;;===========================================================================
(global-set-key [(control -)] 'set-mark-command)
(when (not (equal nil org-idx))
  (global-set-key [f2] '(lambda()
            (interactive)
            (find-file org-idx))))
(global-set-key [f4] 'ibuffer)
(global-set-key [(meta g)] 'goto-line)

;;https://github.com/redguardtoo/elpa-mirror
;;run M-x elpamr-create-mirror-for-installed
;(add-to-list 'load-path "~/.emacs.d/site-lisp/elpa-mirror")
;(require 'elpa-mirror)
