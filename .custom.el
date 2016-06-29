;;===========================================================================
;;{{{ Font/UI/layout

;;===========================================================================
(set-fontset-font "fontset-default"
'gb18030 '("Microsoft YaHei" . "unicode-bmp"))

;;===========================================================================
;;-----------------------------------------
;;change application title format
;;------------------------------------------
(setq frame-title-format "%b@emacs")

;;turn off toolbar
(tool-bar-mode 0)
;;turn off menubar
(menu-bar-mode 0)

;;-------------------------------------------
;; save emacs custom craps in another file
;;-------------------------------------------
(setq custom-file "~/.emacs.d/.my-emacs-custom.el")
;(load custom-file)

;;-------------------------------------------
;; color theme
;;-------------------------------------------
;(require 'color-theme-ld-dark)
;(color-theme-ld-dark)
(load "~/my-color-theme")
(my-color-theme)

;;-------------------------------------------
;; font
(set-default-font "Consolas-10")
(set-face-attribute 'default t :font "Consolas-10")
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


;;===========================================================================
;;{{{ Keys mapping
;;===========================================================================
(global-set-key [(control -)] 'set-mark-command)
(global-set-key [f2] '(lambda()
			(interactive)
			(find-file (getenv "ORGIDX"))))
(global-set-key [f4] 'ibuffer)

 (if (file-exists-p "~/local.el") (load-file "~/local.el"))
