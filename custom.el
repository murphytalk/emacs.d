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
(require 'color-theme)
(color-theme-initialize)
;(color-theme-robin-hood)
(color-theme-ld-dark)
;(abyss-theme)

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



;;==========================================================================
;;Do Re Mi
;;https://www.emacswiki.org/emacs/DoReMi
;;==========================================================================
(load-file "~/.emacs.d/doremi.el")
(load-file "~/.emacs.d/doremi-cmd.el")


;;==========================================================================
;;File/buffer name convention: this-is-a-post.rst
;;==========================================================================
(defun murphy-blog-rst ()
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
;;{{{ Keys mapping
;;===========================================================================
(global-set-key [(control -)] 'set-mark-command)
(global-set-key [f2] '(lambda()
			(interactive)
			(find-file (getenv "ORGIDX"))))
(global-set-key [f4] 'ibuffer)
(global-set-key [(meta g)] 'goto-line)
