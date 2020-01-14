;;===========================================================================
(server-start)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
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
;; GUI font
;(load-theme 'gruber-darker 't)
(load-theme 'molokai 't)
;(load-theme 'darkburn 't)
;(load-theme 'cyberpunk 't)

(setq neo-window-fixed-size nil)
(when (display-graphic-p)
  ;;run M-x all-the-icons-install-fonts to use icons theme
  (setq neo-theme 'icons)
  (if *win64*
      (setq my-font "Consolas-10")
    (if *is-a-mac*
        (setq my-font "SF Mono-12")
      (setq my-font "Fira Code Retina-10")))
  (set-default-font my-font)
  (set-face-attribute 'default t
                      :font my-font)
  (set-fontset-font "fontset-default" 'gb18030 '("Microsoft YaHei" . "unicode-bmp")))
;;--------------------------------------------------

;;---------------------------------------------------
;; highlight the matching parens
;;---------------------------------------------------
(show-paren-mode t)
(setq show-paren-style 'parentheses)


;;==========================================================================
;; mess around with elisp
;; all those stolen or my own little functions
;;
(require 'elisp-format)


;;--------------------------------------------------------------------------
;; Go to the matching paren if on a paren; otherwise insert %.
;;--------------------------------------------------------------------------
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(")
         (forward-list 1)
         (backward-char 1))
        ((looking-at "\\s\)")
         (forward-char 1)
         (backward-list 1))
        (t (self-insert-command (or arg
                                    1)))))
;;and bind it to Ctrl-% key
;;(global-set-key "%" 'match-paren)
(global-set-key [(control ?\% )] 'match-paren)


;;--------------------------------------------------------------------------
;; https://www.emacswiki.org/emacs/CopyWithoutSelection
;;--------------------------------------------------------------------------
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))
(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion (let ((beg (get-point begin-of-thing 1))
                        (end (get-point end-of-thing arg)))
                    (copy-region-as-kill beg end))))
(defun paste-to-mark
    (&optional
     arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe (lambda()
                   (if (string= "shell-mode" major-mode)
                       (progn (comint-next-prompt 25535)
                              (yank))
                     (progn (goto-char (mark))
                            (yank) )))))
    (if arg (if (= arg 1) nil (funcall pasteMe))
      (funcall pasteMe))))

(defun copy-word
    (&optional
     arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

;;==========================================================================
;; MoinMoin mode
;;==========================================================================
(load-file "~/.emacs.d/lisp/screen-lines.el")
(load-file "~/.emacs.d/lisp/moinmoin-mode.el")

;;==========================================================================
;; Python related stuff
;;==========================================================================
(add-to-list 'auto-mode-alist '("\\SConscript$" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConstruct$" . python-mode))
(setq python3 (executable-find "python3"))
(when (not (equal nil python3))
  (setq elpy-rpc-python-command python3)
  (setq elpy-interactive-python-command python3)
  )
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; machine specific extra config
;; org-idx could be set here
(setq host-custom-init (concat "~/" system-name ".el"))
(if (file-exists-p host-custom-init)
    (load-file host-custom-init))

;;==========================================================================
;; org publication
;;==========================================================================
(when (not (boundp 'org-idx))
  (setq org-idx (getenv "ORGIDX")))

(when (not (equal nil org-idx))
  (require 'ox-publish)
  ;; Setup
  (setq org-base-dir (file-name-directory org-idx))
  (setq org-pub-dir (concat org-base-dir "html"))
  (setq org-publish-project-alist `(("org-files" :base-directory  ,org-base-dir
                                     :base-extension "org"
                                     :recursive t
                                     :publishing-directory ,org-pub-dir
                                     :publishing-function org-html-publish-to-html
                                     :headline-levels 4
                                     :auto-preamble t
                                     :html-head-extra
                                     "<link rel=\"stylesheet\" href=\"https://jgkamat.github.io/src/jgkamat.css\">")
                                    ("static-files" :base-directory ,org-base-dir
                                     :base-extension
                                     "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                                     :publishing-directory ,org-pub-dir
                                     :recursive t
                                     :publishing-function org-publish-attachment)
                                    ("org" :components ("org-files" "static-files")))))

;;==========================================================================
;; Code parser
;;==========================================================================
;; for cscope
(when *has-cscope* (load-file "~/.emacs.d/lisp/xcscope.el")
      (require 'xcscope)
      (cscope-setup)
      (add-hook 'python-mode-hook (function cscope-minor-mode))
      (setq cscope-option-do-not-update-database 't))

;; for cquery
(when *has-cquery* (require 'cquery)
      (when *use-lsp-ui* (require 'lsp-ui)
            (add-hook 'lsp-mode-hook 'lsp-ui-mode)
            (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
            (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
            )
      )

;;==========================================================================
;; Emacs version compatability
;;==========================================================================
(when (or *emacs25*
          (and *emacs24*
               (not *emacs24old)))
  (superword-mode t))

;;===========================================================================
;; host specific config
;;===========================================================================
(setq host-custom-init (concat "~/" system-name ".el"))
(if (file-exists-p host-custom-init)
    (load-file host-custom-init))

;;===========================================================================
;; Keys mapping
;;===========================================================================

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; https://github.com/emacs-helm/helm-ls-git
(global-set-key (kbd "M-p") 'helm-ls-git-ls)

(global-set-key [(control -)] 'set-mark-command)
(global-set-key [f2] 'deft)
(global-set-key [f4] 'ibuffer)
(global-set-key [f5] 'neotree-toggle)
(global-set-key [(meta g)] 'goto-line)


(when *is-a-mac*
  (global-unset-key [home])
  (global-set-key [home] 'move-beginning-of-line)
  (global-unset-key [end])
  (global-set-key [end] 'move-end-of-line)
  )

;;===========================================================================
;;export installed packages
;;
;;https://github.com/redguardtoo/elpa-mirror
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/elpa-mirror")
;;(require 'elpa-mirror)
;;(elpamr-create-mirror-for-installed)
