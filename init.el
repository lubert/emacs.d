;; init.el --- emacs configuration settings

;;; Commentary:
;; Sane defaults and uses repositories as much as possible

;;; Code:

;; -------------------
;; -- Configuration --
;; -------------------
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
(setq read-process-output-max (* 1024 1024))

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
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))

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
If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-enlist (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).
\(fn HOOKS [:append :local] FUNCTIONS)"

  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'doom-unquote rest)
                     (doom-enlist (doom-unquote (car rest))))))

            ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,append-p ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

;; --------------
;; -- Packages --
;; --------------

(use-package company
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

(use-package company-lsp
  :after (company lsp-mode)
  :config (add-to-list 'company-backends 'company-lsp)
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
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
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

(use-package gcmh
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
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-mode 1)
  :ensure)

(use-package ivy-prescient
  :after (counsel prescient)
  :config (ivy-prescient-mode 1)
  :ensure)

(use-package js2-mode
  :mode "\\.js\\'"
  :ensure)

(use-package lsp-mode
  :hook (js2-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :ensure)

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown")
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

(use-package rjsx-mode
  :mode (("\\.jsx\\'" . rjsx-mode)
         ("components\\/.*\\.js\\'" . rjsx-mode))
  :ensure)

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

(use-package terraform-mode
  :mode "\\.tf\\'"
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

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :ensure)

(use-package ws-butler
  :config (ws-butler-global-mode 1)
  :ensure)

(use-package yasnippet
  :config
  (yas-reload-all)
  :hook (prog-mode . yas-minor-mode)
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
(setq ffap-machine-p-known 'reject)

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
