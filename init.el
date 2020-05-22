;; init.el --- emacs configuration settings

;;; Commentary:
;; Sane defaults and uses repositories as much as possible

;;; Code:

;; -------------------
;; -- Configuration --
;; -------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; ---------------
;; -- Variables --
;; ---------------
(menu-bar-mode 0) ; Disable menu bar
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq sort-fold-case t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)
(setq require-final-newline t)

(global-auto-revert-mode 1)
(global-subword-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 6    ; how many of the newest versions to keep
      kept-old-versions 2    ; and how many of the old
      )

;; ------------
;; -- Macros --
;; ------------
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-d" 'subword-kill)
(global-set-key "\M-h" 'subword-backward-kill)
(global-set-key "\M-z" 'zap-up-to-char)
(global-set-key "\C-xs" 'vc-git-grep)
(autoload 'zap-up-to-char "misc" 'interactive)

;; --------------
;; -- Packages --
;; --------------

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :ensure)

(use-package company-irony
  :after (company irony)
  :init (add-to-list 'company-backends 'company-irony)
  :ensure)

(use-package company-irony-c-headers
  :after (company irony company-irony)
  :init (add-to-list 'company-backends 'company-irony-c-headers)
  :ensure)

(use-package company-prescient
  :after (company prescient)
  :config (company-prescient-mode 1)
  :ensure)

(use-package counsel
  :after (ivy)
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-mode 1)
  :ensure)

(use-package display-line-numbers
  :config
  (defun display-line-numbers--turn-on ()
    (if (and
         (not (member major-mode '(ansi-term-mode
                                   eshell-mode
                                   eww-mode
                                   grep-mode
                                   nav-mode
                                   shell-mode
                                   term-mode
                                   vterm-mode)))
         (not (minibufferp)))
        (display-line-numbers-mode)))
  (global-display-line-numbers-mode))

(use-package docker-compose-mode
  :ensure)

(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy)
  :ensure)

(use-package elpy
  :init (with-eval-after-load 'python (elpy-enable))
  :commands elpy-enable
  :ensure)

(use-package flycheck
  :config (global-flycheck-mode)
  :ensure)

(use-package flycheck-irony
  :after (flycheck)
  :init (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  :ensure)

(use-package highlight-symbol
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :ensure)

(use-package hydra
  :after (dumb-jump)
  :demand t
  :bind (("M-g" . dumb-jump-hydra/body))
  :init
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  :ensure)

(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :ensure)

(use-package irony-eldoc
  :after (irony)
  :init (add-hook 'irony-mode-hook #'irony-eldoc)
  :ensure)

(use-package ivy
  :init (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config (ivy-mode 1)
  :ensure)

(use-package ivy-prescient
  :after (counsel prescient)
  :init (ivy-prescient-mode 1)
  :ensure)

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure)

(use-package mood-line
  :config (mood-line-mode)
  :ensure)

(use-package nav
  :bind ("C-x t" . nav-toggle)
  :ensure)

(use-package perspective
  :config (persp-mode)
  :ensure)

(use-package prescient
  :ensure)

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind ("C-x f" . projectile-find-file)
  :ensure)

(use-package recentf
  :config
  (setq recentf-max-menu-items 25)
  (recentf-mode 1)
  :bind ("C-x C-r" . recentf-open-files))

(use-package ruby-mode
  :config (setq ruby-insert-encoding-magic-comment nil)
  :ensure)

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :ensure)

(use-package swiper
  :after (ivy counsel)
  :bind ("C-s" . counsel-grep-or-swiper)
  :ensure)

(use-package uniquify
  :config
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/"))

(use-package visible-mark
  :config (global-visible-mark-mode 1)
  :ensure)

(use-package ws-butler
  :config (ws-butler-global-mode 1)
  :ensure)

(use-package zenburn-theme
  :config (load-theme 'zenburn t)
  :ensure)

;; -------------------
;; -- Customization --
;; -------------------

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

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

(provide 'init)
;;; init.el ends here
