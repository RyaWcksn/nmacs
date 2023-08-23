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

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
	(append
	 (split-string-and-unquote path ":")
	 exec-path)))

(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

(use-package dracula-theme)

(defun single-font-size ()
  "Reset all faces to the height of the default face."
  (dolist (f (face-list))
    (when (not (equal 'default f))
      (set-face-attribute f nil :height 1.0))))

(load-theme 'dracula t)
(add-hook 'after-init-hook
          'single-font-size)

(when (window-system)
  (add-to-list 'image-types 'svg)
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
(setq dashboard-icon-type 'all-the-icons) ;; use `all-the-icons' package
(setq dashboard-show-shortcuts nil) 
(setq dashboard-image-banner-max-height 200)
(setq dashboard-items '((recents  . 3)
			(bookmarks . 3)
			(projects . 3)
			(agenda . 3)
			(registers . 3)))

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

(tab-bar-mode 1)                           ;; enable tab bar
(setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
(setq tab-bar-close-button-show nil)       ;; hide tab close / X button
(setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
(setq tab-bar-tab-hints t)                 ;; show tab numbers
(defun neko/current-tab-name ()
  (alist-get 'name (tab-bar--current-tab)))

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

(use-package helm
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  )

(neko/leader-keys
  "h"  '(helm-command-prefix :which-key "Helm"))

(use-package elcord :ensure t)

(use-package mpdel)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-inlay-hints-enable t
	lsp-inlay-hints-prefix " » "
	lsp-inlay-hints-highlight   "#6B8E23")
  (setq lsp-lens-enable t)
  (setq lsp-ui-sideline-enable t)


  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 ;;(XXX-mode . lsp)
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

(use-package lsp-java
  :if (executable-find "mvn")
  :init
  :config (add-hook 'java-mode-hook 'lsp)
  (use-package request :defer t)
  :custom
  (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/")))
(require 'lsp-java-boot)
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
(setq lsp-java-java-path
      "/usr/lib/jvm/java-20-openjdk-amd64/bin/java")

(use-package hydra)
(use-package smerge-mode
  :config
  (defhydra hydra-smerge (:color red :hint nil)
    "
Navigate       Keep               other
----------------------------------------
_j_: previous  _RET_: current       _e_: ediff
_k_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: combine       _q_: quit
               _b_: base
"
    ("k" smerge-next)
    ("j" smerge-prev)
    ("RET" smerge-keep-current)
    ("m" smerge-keep-mine)
    ("o" smerge-keep-other)
    ("b" smerge-keep-base)
    ("a" smerge-keep-all)
    ("e" smerge-ediff)
    ("J" previous-line)
    ("K" forward-line)
    ("r" smerge-refine)
    ("u" undo)
    ("q" nil :exit t))

  (defun enable-smerge-maybe ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode +1)
          (hydra-smerge/body))))))


(neko/leader-keys
     "gm"  '(scimax-smerge/body :which-key "Toggle smerge")
     )

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)))
  ;; NOounsTE: Set this to the folder where you keep your Git repos!

  (neko/leader-keys
     "p"  '(:ignore t :which-key "Projectile")
     "pp" '(projectile-command-map :which-key "Command map")
     "pf" '(projectile-find-file :which-key "Find File"))

(use-package vterm
    :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-displey-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun open-magit-in-vertical-split ()
  (interactive)
  (magit-status))

(neko/leader-keys
    "g" '(:ignore t :which-key "Git")
    "gs" '(open-magit-in-vertical-split :which-key "Magit"))

(use-package jest)

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

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(n)" "|" "DONE(d)" "CANCEL(c)")))

(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (js . t)
   (latex . t)
   ))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun neko/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/README.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'neko/org-babel-tangle-config)))

(use-package calfw :ensure t)
(use-package calfw-org :ensure t)
(require 'calfw-org)

(use-package perspective
  :bind
  ("C-x k" . persp-kill-buffer*)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))


(neko/leader-keys
  "p"  '(:ignore t :which-key "Perspective")
  "ps" '(persp-switch :which-key "Switch perspective")
  "pm" '(persp-merge :which-key "Merge perspective")
  "pb" '(persp-list-buffers :which-key "Buffers perspective")
  )

(neko/leader-keys
  "]" '(persp-next :which-key "Next perspective")
  "[" '(persp-prev :which-key "Prev perspective")
  )

(neko/leader-keys
  "<left>" '(tab-bar-switch-to-prev-tab :which-key "Prev Tab")
  "<right>" '(tab-bar-switch-to-next-tab :which-key "Next Tab")
  "n" '(tab-bar-new-tab :which-key "New Tab")
  "w"  '(:ignore t :which-key "Window")
  "ws" '(evil-window-split :which-key "Split")
  "wj" '(evil-window-down :which-key "Go Bottom")
  "wk" '(evil-window-up :which-key "Go Top")
  "wh" '(evil-window-left :which-key "Go Left")
  "wl" '(evil-window-right :which-key "Go Right")
  "wv" '(evil-window-vsplit :which-key "Vsplit")
  "wq" '(delete-window :which-key "Quit")
  "wb" '(switch-to-buffer :which-key "Switch Buffer"))

(neko/leader-keys
  ";" '(helm-M-x :which-key "Meta")
  "/" '(comment-region :which-key "Comment region")
  "s" '(evil-save :which-key "Save")
  "b" '(:ignore t :which-key "Buffer")
  "bb" '(counsel-switch-buffer :which-key "Switch buffer")
  "bk" '(kill-buffer :which-key "Kill buffer")
  "qq" '(kill-buffer-and-window :which-key "Kill buffer")
  "ba" '(kill-other-buffers :which-key "Kill other buffer except this"))

(neko/leader-keys
  "o" '(:ignore t :which-key "Open")
  "oa" '(org-agenda :which-key "Org Agenda")
  "oc" '(cfw:open-org-calendar :which-key "Calendar")
  "oe" '(neotree :which-key "Neotree")
  "od" '(dired :which-key "Dired"))