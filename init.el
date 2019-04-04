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
 'fill-column-indicator
 'find-file-in-repository
 'flycheck
 'go-mode
 'highlight-symbol
 'js2-mode
 'rjsx-mode
 'json-mode
 'less-css-mode
 'linum-off
 'magit
 'multiple-cursors
 'nav
 'perspective
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

;; ac-js2
(require 'ac-js2)
(add-hook 'js2-mode-hook 'ac-js2-mode)

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

;; fill-column-indicator
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-character-color "darkred")
(define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (eq major-mode 'dired-mode)))
          (fci-mode 1))))
  (global-fci-mode 1)
(defvar sanityinc/fci-mode-suppressed nil)
(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
  (when fci-mode
    (turn-off-fci-mode)))
(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;; flycheck
(require 'flycheck)
(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; ido
(require 'ido)
(ido-mode t)

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
(setq js2-basic-offset 2)
(setq js2-highlight-level 3)

;; rjsx-mode
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode))

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

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)

;; nav
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-nav/")
(require 'nav)

;; perspective
(require 'perspective)
(persp-mode 1)

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
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  )
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
(setq tab-width 2)
(setq js-indent-level 2)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq sort-fold-case t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)
(setq require-final-newline t)
(setq show-trailing-whitespace t)
(ws-butler-global-mode 1)
(global-auto-revert-mode 1)
(global-subword-mode 1)
(add-hook 'after-init-hook #'global-flycheck-mode)
(electric-pair-mode 1)
(setq backup-directory-alist `(("." . "~/.saves")))


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
(global-set-key (kbd "C-x t") 'nav-toggle)
(global-set-key (kbd "C-x f") 'find-file-in-repository)
(global-set-key (kbd "C-x s") 'vc-git-grep2)
(global-set-key ">" 'my-indent-region)
(global-set-key "<" 'my-unindent-region)
(global-set-key [remap find-tag] 'ido-find-tag)
(global-set-key (kbd "C-x /") 'mc/edit-lines)
(global-set-key (kbd "C-x .") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x ,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x >") 'mc/mark-all-like-this)


;; -------------------
;; -- Customization --
;; -------------------

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; Alphabetize comma-separated lists
(defun sort-list ()
  (interactive)
  (sort-regexp-fields nil "[a-z]+" "\\&" (region-beginning) (region-end)))

;; -----------
;; -- Theme --
;; -----------
(load-theme 'zenburn t)
(set-face-attribute 'linum nil :background "#303030")


(provide 'init)

;;; init.el ends here
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
