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
;; -- Functions --
;; ---------------

(defvar linum-current-line 1 "Current line number.")
(defvar linum-border-width 1 "Border width for linum.")
(defface linum-current-line `((t :inherit linum
                                 :foreground "goldenrod"
                                 :weight bold))
  "Face for displaying the current line number."
  :group 'linum)

(defadvice linum-update (before advice-linum-update activate)
  "Set the current line."
  (setq linum-current-line (line-number-at-pos)
        ;; It's the same algorithm that linum dynamic. I only had added one
        ;; space in front of the first digit.
        linum-border-width (number-to-string
                            (+ 1 (length
                                  (number-to-string
                                   (count-lines (point-min) (point-max))))))))

(defun linum-highlight-current-line (line-number)
  "Highlight the current LINE-NUMBER using `linum-current-line' face."
  (let ((face (if (= line-number linum-current-line)
                  'linum-current-line
                'linum)))
    (propertize (format (concat "%" linum-border-width "d ") line-number)
                'face face)))

;; ------------
;; -- Macros --
;; ------------

(global-set-key "\C-ct" '(lambda ()(interactive)(ansi-term "/bin/bash")))
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-d" 'subword-kill)
(global-set-key "\M-h" 'subword-backward-kill)
(autoload 'zap-up-to-char "misc" 'interactive)
(global-set-key "\M-z" 'zap-up-to-char)

;; --------------
;; -- Packages --
;; --------------

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :after (company irony)
  :init
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :ensure t
  :after (company irony company-irony)
  :init
  (add-to-list 'company-backends 'company-irony-c-headers))

(use-package counsel
  :init
  (counsel-mode 1)
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package dumb-jump
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flycheck-irony
  :ensure t
  :after (flycheck)
  :init
  (eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package highlight-symbol
  :ensure t
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev)))

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
  :ensure t)

(use-package irony-eldoc
  :ensure t
  :after (irony)
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package linum
  :init
  (add-hook 'term-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'grep-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'nav-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'eww-mode-hook (lambda () (linum-mode -1)))
  :config
  (global-linum-mode 1)
  (set-face-attribute 'linum nil :background "#303030")
  (setq linum-format 'linum-highlight-current-line))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package nav
  :ensure t
  :bind ("C-x t" . nav-toggle))

(use-package perspective
  :init
  (persp-mode)
  :ensure t)

(use-package projectile
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  :bind (("C-x f" . projectile-find-file) )
  :ensure t)

(use-package recentf
  :init
  (setq recentf-max-menu-items 25)
  :config
  (recentf-mode 1)
  :bind ("C-x C-r" . recentf-open-files))

(use-package smex
  :init
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :ensure t)

(use-package uniquify
  :init
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/"))

(use-package visible-mark
  :ensure t
  :config
  (global-visible-mark-mode 1))

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode 1))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package zoom
  :config
  (custom-set-variables
   '(zoom-ignored-major-modes '(nav-mode)))
  :ensure t)

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
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; -------------------
;; -- Customization --
;; -------------------

;; ;; Makes *scratch* empty.
(setq initial-scratch-message "")

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

(provide 'init)
;;; init.el ends here
