;; init.el --- emacs configuration settings

;;; Commentary:
;; Sane defaults and uses repositories as much as possible

;;; Code:

;; ----------
;; -- Init --
;; ----------
; Temporarily suppress gc
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; -----------
;; -- Modes --
;; -----------

(column-number-mode 1)
(electric-indent-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(global-subword-mode 1)
(menu-bar-mode 0)

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; -------------------
;; -- Customization --
;; -------------------

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defvar read-process-output-max (* 1024 1024))
(defvar sort-fold-case t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq require-final-newline t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq tab-width 2)
(setq vc-follow-symlinks t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 6    ; how many of the newest versions to keep
      kept-old-versions 2    ; and how many of the old
      )
(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; ------------
;; -- Macros --
;; ------------
(defun vc-git-grep2 (regexp dir)
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep" nil nil 'grep-history)
    nil))
      (t (let* ((regexp (grep-read-regexp))
    (dir (read-directory-name "In directory: " nil default-directory t)))
     (list regexp dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (> 4 5)
    (if (string= command "git grep")
     (setq command nil))
  (setq dir (file-name-as-directory (expand-file-name dir)))
  (setq command
     (grep-expand-template "git grep -n -i -e <R>" regexp))
  (when command
    (if (equal current-prefix-arg '(4))
     (setq command
     (read-from-minibuffer "Confirm: " command nil nil 'grep-history))
   (add-to-history 'grep-history command))))
      (when command
  (let ((default-directory dir)
     (compilation-environment '("PAGER=")))
    ;; Setting process-setup-function makes exit-message-function work
    ;; even when async processes aren't supported.
    (compilation-start command 'grep-mode))
  (if (eq next-error-last-buffer (current-buffer))
   (setq default-directory dir))))))

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.
If a mode is quoted, it is left as is.  If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-enlist (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-d" 'subword-kill)
(global-set-key "\M-h" 'subword-backward-kill)
(global-set-key "\M-z" 'zap-up-to-char)
(global-set-key "\C-xs" 'vc-git-grep2)
(autoload 'zap-up-to-char "misc" 'interactive)

;; --------------
;; -- Packages --
;; --------------

(use-package company
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations 't)
  :hook (after-init . global-company-mode)
  :ensure)

(use-package company-irony
  :after (company irony)
  :config (add-to-list 'company-backends 'company-irony)
  :ensure)

(use-package company-irony-c-headers
  :after (company irony company-irony)
  :config (add-to-list 'company-backends 'company-irony-c-headers)
  :ensure)

(use-package company-prescient
  :after (company prescient)
  :config (company-prescient-mode 1)
  :ensure)

(use-package counsel
  :after (ivy)
  :custom
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :config (counsel-mode 1)
  :ensure)

(use-package counsel-projectile
  :after (counsel)
  :commands
  counsel-projectile-switch-to-buffer
  counsel-projectile-find-dir
  counsel-projectile-find-file
  counsel-projectile-switch-project
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
  :custom (dumb-jump-selector 'ivy)
  :hook (prog-mode . dumb-jump-mode)
  :ensure)

(use-package eglot
  :after (company yasnippet)
  :hook
  (python-mode . eglot-ensure)
  (ruby-mode . eglot-ensure)
  :ensure)

(use-package elpy
  :defer
  :init (advice-add 'python-mode :before 'elpy-enable)
  :ensure)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :ensure)

(use-package flycheck-irony
  :after (flycheck irony)
  :hook (flycheck-mode . flycheck-irony-setup)
  :ensure)

(use-package highlight-symbol
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :ensure)

(use-package highlight-parentheses
  :config (global-highlight-parentheses-mode 1)
  :ensure)

(use-package gcmh
  :ensure)

(use-package hydra
  :bind (("C-c j" . dumb-jump-hydra/body)
         ("C-c m" . magit-hydra/body)
         ("C-c p" . projectile-hydra/body))
  :commands
  hydra-default-pre
  hydra-keyboard-quit
  hydra--call-interactively-remap-maybe
  hydra-show-hint
  hydra-set-transient-map
  :config
  (defhydra dumb-jump-hydra (:color blue :columns 3)
     "Dumb Jump"
     ("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
     ("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))
   (defhydra magit-hydra (:color blue :columns 3)
     "Magit"
     ("b" magit-blame-addition "blame")
     ("c" magit-clone "clone")
     ("i" magit-init "init")
     ("l" magit-log-buffer-file "commit log (current file)")
     ("L" magit-log-current "commit log (project)")
     ("s" magit-status "status"))
   (defhydra projectile-hydra (:color blue :columns 3)
     "Projectile"
     ("b" counsel-projectile-switch-to-buffer "list")
     ("k" projectile-kill-buffers "kill all")
     ("S" projectile-save-project-buffers "save all")
     ("d" counsel-projectile-find-dir "directory")
     ("D" projectile-dired "root")
     ("f" counsel-projectile-find-file "file")
     ("p" counsel-projectile-switch-project "project")
     ("i" projectile-invalidate-cache "reset cache")
     ("r" projectile-replace "replace")
     ("R" projectile-replace-regexp "regexp replace")
     ("s" counsel-rg "search"))
  :ensure)

(use-package irony
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :ensure)

(use-package irony-eldoc
  :after (irony)
  :hook (irony-mode . irony-eldoc)
  :ensure)

(use-package ivy
  :custom (ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config (ivy-mode 1)
  :ensure)

(use-package ivy-prescient
  :after (counsel prescient)
  :config (ivy-prescient-mode 1)
  :ensure)

(use-package js2-mode
  :mode "\\.js\\'"
  :ensure)

(use-package magit
  :defer
  :ensure)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown")
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
  :custom
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  :config (projectile-mode +1)
  :bind ("C-x f" . projectile-find-file)
  :ensure)

(use-package rjsx-mode
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("components\\/.*\\.js\\'" . rjsx-mode))
  :ensure)

(use-package robe
  :after (company ruby-mode)
  :hook (ruby-mode . robe-mode)
  :config (push 'company-robe company-backends)
  :ensure)

(use-package ruby-mode
  :custom (ruby-insert-encoding-magic-comment nil))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :ensure)

(use-package swiper
  :after (ivy counsel)
  :bind ("C-s" . counsel-grep-or-swiper)
  :ensure)

(use-package terraform-mode
  :mode "\\.tf\\'"
  :ensure)

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config (flycheck-add-mode 'typescript-tslint 'web-mode)
  :ensure)

(use-package uniquify
  :custom
  (uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "/"))

(use-package visible-mark
  :config (global-visible-mark-mode 1)
  :ensure)

(use-package web-mode
  :hook (web-mode . (lambda ()
                      (when (string-equal "tsx" (file-name-extension buffer-file-name))
                        (setup-tide-mode))))
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :ensure)

(use-package ws-butler
  :config (ws-butler-global-mode 1)
  :ensure)

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize)
  :ensure)

(use-package yasnippet
  :commands yas--get-snippet-tables
  :config (yas-global-mode)
  :hook (yas-minor-mode . my/disable-yas-if-no-snippets)
  :preface
  (defun my/disable-yas-if-no-snippets ()
    (when (and yas-minor-mode (null (yas--get-snippet-tables)))
      (yas-minor-mode -1)))
  :ensure)

(use-package zenburn-theme
  :config (load-theme 'zenburn t)
  :ensure)

;; ------------------
;; -- Optimization --
;; ------------------

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(defvar ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a notable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Delete files to trash on macOS, as an extra layer of precaution against
;; accidentally deleting wanted files.
(setq delete-by-moving-to-trash IS-MAC)

;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold 16777216  ; 16mb
      gc-cons-percentage 0.6)
(with-eval-after-load 'gcmh
  ;; But restore this later, otherwise we risk freezing and stuttering!
  (setq gc-cons-percentage 0.1))

(provide 'init)
;;; init.el ends here
