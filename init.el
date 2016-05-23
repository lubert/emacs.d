;;; init.el --- emacs configuration settings

;;; Commentary:
;; Sane defaults and uses repositories as much as possible

;;; Code:

;; ---------------------
;; -- Global Settings --
;; ---------------------
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "defuns-config.el")

;; --------------
;; -- Packages --
;; --------------

; Add repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; List packages to install
(ensure-package-installed
 'auto-complete
 'ac-js2
 'cider
 'find-file-in-repository
 'flycheck
 'flymake-go
 'go-mode
 'haskell-mode
 'highlight-symbol
 'js2-mode
 'json-mode
 'less-css-mode
 'linum-off
 'magit
 'markdown-mode
 'nav
 'scss-mode
 'visible-mark
 'web-mode
 'ws-butler
 'zenburn-theme
)


;; -------------------
;; -- Configuration --
;; -------------------

;; autocompile
(add-hook 'after-save-hook (lambda ()
			    (autocompile "~/.emacs.d/init.el")))

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; guess-style
(add-to-list 'load-path "~/.emacs.d/lisp/guess-style/")
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)
(autoload 'guess-style-guess-tab-width "guess-style" nil t)

;; etags
(require 'etags)

;; ido
(require 'ido)
(ido-mode t)

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)
(setq js2-highlight-level 3)

;; json-mode
(add-hook 'json-mode-hook (lambda ()
                            (setq js-indent-level 2)))

;; linum
(require 'linum)
(global-linum-mode 1)
(add-hook 'term-mode-hook (lambda ()
			    (linum-mode -1)))
(add-hook 'grep-mode-hook (lambda ()
			    (linum-mode -1)))
(add-hook 'nav-mode-hook (lambda ()
			   (linum-mode -1)))
(add-hook 'eww-mode-hook (lambda ()
			   (linum-mode -1)))

(setq linum-format 'linum-highlight-current-line)

;; nav
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-nav/")
(require 'nav)

;; python-mode
(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook (lambda ()
			      (when indent-tabs-mode
				(guess-style-guess-tab-width))))

;; recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; visible-mark
(require 'visible-mark)
(global-visible-mark-mode 1)

;; web-mode
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook 'my-web-mode-hook)


;; ---------------
;; -- Variables --
;; ---------------
(menu-bar-mode 0) ; Disable menu bar
(normal-erase-is-backspace-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq css-indent-offset 2)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)
(setq require-final-newline t)
(setq show-trailing-whitespace t)
(ws-butler-global-mode 1)
(global-auto-revert-mode 1)
(global-subword-mode 1)
(add-hook 'after-init-hook #'global-flycheck-mode)
(electric-pair-mode 1)


;; ------------
;; -- Macros --
;; ------------
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-=" 'align-equals)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key "\C-c\ \C-c" 'comment-or-uncomment-region)
(global-set-key "\C-ct" '(lambda ()(interactive)(ansi-term "/bin/bash")))
(global-set-key "\M-n" 'highlight-symbol-next)
(global-set-key "\M-p" 'highlight-symbol-prev)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-i" 'back-window)
(global-set-key "\C-z" 'zap-to-char)
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\M-d" 'subword-kill)
(global-set-key "\M-h" 'subword-backward-kill)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key (kbd "<f1>") 'nav-toggle)
(global-set-key (kbd "<f2>") 'highlight-symbol)
(global-set-key (kbd "<f3>") 'highlight-symbol-query-replace)
(global-set-key (kbd "<f4>") 'linum-mode)
(global-set-key (kbd "C-x f") 'find-file-in-repository)
(global-set-key (kbd "C-x s") 'vc-git-grep2)
(global-set-key ">" 'my-indent-region)
(global-set-key "<" 'my-unindent-region)
(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-.") 'ido-find-file-in-tag-files)


;; -----------
;; -- Theme --
;; -----------
(load-theme 'zenburn t)
(set-face-attribute 'linum nil :background "#303030")


(provide 'init)

;;; init.el ends here
