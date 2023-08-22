(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq package-enable-at-startup nil)

(add-to-list 'image-types 'svg)

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
	(append
	 (split-string-and-unquote path ":")
	 exec-path)))

(defun single-font-size ()
  "Reset all faces to the height of the default face."
  (dolist (f (face-list))
    (when (not (equal 'default f))
      (set-face-attribute f nil :height 1.0))))

(load-theme 'tango-dark t)
(add-hook 'after-init-hook
          'single-font-size)

(when (window-system)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(when (not (window-system))
  (menu-bar-mode -1))

;; Y/N
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package clipetty)
(global-clipetty-mode)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-banner-logo-title "Personal Development Environment")
(setq dashboard-startup-banner "~/.emacs.d/fuyuko.png" )
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
			(agenda . 5)
			(registers . 5)))

(use-package all-the-icons
  :ensure t)

(setq display-battery-mode 1)
(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		mode-line-mule-info
		mode-line-client
		mode-line-modified
		mode-line-remote
		mode-line-frame-identification
		mode-line-buffer-identification
		" "
		mode-line-position
		(:eval
		 (if vc-mode
		     (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
			    (face (cond ((string-match "^ -" noback) 'mode-line-vc)
					((string-match "^ [:@]" noback) 'mode-line-vc-edit)
					((string-match "^ [!\\?]" noback) 'mode-line-vc-modified))))
		       (format " %s" (substring noback 2)))))
		"  "
		mode-line-misc-info
		mode-line-end-spaces
		))

(use-package neotree :ensure t)
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq x-underline-at-descent-line t)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
(use-package evil-escape
  :init
  (evil-escape-mode)
  :config
  (setq-default evil-escape-key-sequence "jk")
  )

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :ensure t)
  :config
  (general-create-definer neko/leader-keys
    :keymaps '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "SPC")
(general-auto-unbind-keys t)
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

(use-package hydra
  :ensure t)

(use-package which-key
  :diminish (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

(use-package elcord :ensure t)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (XXX-mode . lsp)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(setq lsp-ui-doc-position 'at-point)
(setq lsp-completion-provider :none)
(setq lsp-ui-doc-show-with-cursor t)

(neko/leader-keys
  "l"  '(:ignore t :which-key "LSP")
  "lg" '(lsp-goto-type-definition :which-key "Go to definition")
  "li" '(lsp-goto-implementation :which-key "Go to implementation")
  "lc" '(helm-lsp-code-actions :which-key "Code action")
  "ll" '(lsp-avy-lens :which-key "Code lens")
  "lr" '(lsp-rename :which-key "Code lens")
  "lf" '(lsp-format-buffer :which-key "Format buffer")
  "ld" '(lsp-ui-peek-find-definitions :which-key "Goto declaration")
  "le" '(helm-lsp-diagnostics :which-key "Error diagnostics")
  "la" '(lsp-ui-peek-find-implementation :which-key "Code implement"))

(use-package dap-mode
	 :ensure t
	 ;; Uncomment the config below if you want all UI panes to be hidden by default!
	 ;; :custom
	 ;; (lsp-enable-dap-auto-configure nil)
	 ;; :config
	 ;; (dap-ui-mode 1)
	 :commands dap-debug
	 :config
	 (dap-tooltip-mode 1)
	 ;; use tooltips for mouse hover
	 ;; if it is not enabled `dap-mode' will use the minibuffer.
	 (tooltip-mode 1)
	 ;; displays floating panel with debug buttons
	 ;; requies emacs 26+
	 (dap-ui-controls-mode 1)
	 ;; Set up Node debugging
	 (require 'dap-hydra)
	 (general-define-key
	  :keymaps 'lsp-mode-map
	  :prefix lsp-keymap-prefix
	  "d" '(dap-hydra t :wk "debugger"))
	 )

(neko/leader-keys
  "d"  '(:ignore t :which-key "Debugging")
  "ds" '(dap-debug t :wk "Start debug")
  "db" '(dap-breakpoint-toggle t :wk "Toggle breakpoint")
  "dd" '(dap-hydra t :wk "Debugger"))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (
	 :map company-active-map
	 ("<tab>" . company-complete-common-or-cycle)
	 ("C-j" . company-select-next-or-abort)
	 ("C-k" . company-select-previous-or-abort)
	 ("C-l" . company-other-backend)
	 ("C-h" . nil)
	 )
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(setq company-backends '((company-capf company-yasnippet)))

(use-package flycheck :ensure t)

(use-package yasnippet :ensure t
  :config
  ;; (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-golang/")
  (yas-global-mode 1)
  )

(use-package go-mode
  :ensure t
  :hook
  (
   (go-mode . lsp-deferred)
   (go-mode . company-mode))
  :config
  (setq gofmt-command "gofmt")
  (require 'lsp-go)
  (setq lsp-go-analyses
	'((fieldalignment . t)
	  (nilness . t)
	  (httpresponse . t)
	  (unusedwrite . t)
	  (unusedparams . t)
	  )))

(provide 'gopls-config)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

(add-hook 'go-mode-hook (lambda()
			  (flycheck-golangci-lint-setup)
			  (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

(defun my/get-config-path (config-file-name)
  "get the path to config-file-name in the current project as a string, when in `go-mode`."
  (when (eq major-mode 'go-mode)
    (let* ((project-root (projectile-project-root))
	   (config-path (concat project-root config-file-name)))
      (if (file-exists-p config-path)
	  config-path
	(error "configuration file '%s' not found in project root '%s'" config-file-name project-root)))))

(setq my/config-path (my/get-config-path "src/config.json"))
(setq flycheck-golangci-lint-config my/config-path)

(use-package rjsx-mode)
(add-hook 'js-mode-hook #'lsp)

(use-package react-snippets)

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(add-hook 'typescript-mode-hook #'lsp)

(use-package org
  :config
  (setq org-ellipsis " ...")
  (setq org-agenda-files
	'("~/Orgs/")))

(defun nolinum ()
  (global-display-line-numbers-mode 0))

(add-hook 'org-mode-hook 'nolinum)

(global-set-key (kbd "C-c c") #'org-capture)


(setq org-capture-templates '(("t" "Todo [inbox]" entry
			       (file+headline "~/Orgs/inbox.org" "Tasks")
			       "* TODO %i%?")
			      ("T" "Tickler" entry
			       (file+headline "~/Orgs/tickler.org" "Tickler")
			       "* %i%? \n %U")
			      ("b" "Braindump" entry
			       (file+headline "~/Orgs/braindump.org" "Braindump")
			       "* %? \n")))

(setq org-refile-targets '(("~/Orgs/agenda.org" :maxlevel . 3)
			   ("~/Orgs/someday.org" :level . 1)
			   ("~/Orgs/tickler.org" :maxlevel . 2)))

(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (js . t)
   (latex . t)
   ))
