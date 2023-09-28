(setq byte-compile-warnings '(cl-functions))
(global-auto-revert-mode t)

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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(use-package dracula-theme)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t))

(use-package atom-one-dark-theme)

(set-frame-font "JetBrains Mono 10" nil t)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono 10"))

;;

(dolist (char/ligature-re
	 `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
	   (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
	   (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
	   (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
			       "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>"
			       "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+"
			       "</" "<*")
			   (+ "<"))))
	   (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
	   (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
	   (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
	   (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
	   (?&  . ,(rx (+ "&")))
	   (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>"
			       "|]" "|}" "|=")
			   (+ "|"))))
	   (?.  . ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
	   (?+  . ,(rx (or "+>" (+ "+"))))
	   (?\[ . ,(rx (or "[<" "[|")))
	   (?\{ . ,(rx "{|"))
	   (?\? . ,(rx (or (or "?." "?=" "?:") (+ "?"))))
	   (?#  . ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(")
			   (+ "#"))))
	   (?\; . ,(rx (+ ";")))
	   (?_  . ,(rx (or "_|_" "__")))
	   (?~  . ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
	   (?$  . ,(rx "$>"))
	   (?^  . ,(rx "^="))
	   (?\] . ,(rx "]#"))))
  (let ((char (car char/ligature-re))
	(ligature-re (cdr char/ligature-re)))
    (set-char-table-range composition-function-table char
			  `([,ligature-re 0 font-shape-gstring]))))

(when (window-system)
  (add-to-list 'image-types 'svg)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (load-theme 'atom-one-dark t)
  (scroll-bar-mode 1))

(when (not (window-system))
  (tool-bar-mode -1)
  (menu-bar-mode -1))

(if (daemonp)
  (load-theme 'atom-one-dark t))

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
(setq dashboard-agenda-time-string-format "%Y-%m-%d %H:%M")
(setq dashboard-items '((projects . 7)
			(bookmarks . 3)
			(agenda . 2)))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package all-the-icons
  :ensure t)

(setq display-battery-mode t)
(setq display-time-mode t)
(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
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
(setq mode-line-position (list "(%l,%c)"))

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

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
  '(("." . "~/.saves/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(use-package smartparens
  :config
  (smartparens-global-mode 1))

(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(set-face-attribute 'show-paren-match nil :background nil :underline t)
(setq show-paren-style 'parenthesis) ; can be 'expression' 'parenthesis' and 'mixed'

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-Y-yank-to-eol 1)
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

(evil-set-initial-state 'pdf-view-mode 'normal)

(evil-define-key 'normal 'lsp-ui-doc-mode
  [?K] #'lsp-ui-doc-focus-frame)
(evil-define-key 'normal 'lsp-ui-doc-frame-mode
  [?q] #'lsp-ui-doc-unfocus-frame)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

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

(use-package elcord :ensure t
  :config
  (setq elcord-display-line-numbers nil)
  (setq elcord-editor-icon "Emacs (Material)")
  )

(use-package emms
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-player-list '(emms-player-vlc)
	emms-info-functions '(emms-info-native))
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/Music/"))

(use-package company-tabnine)
(setq company-backends '(company-tabnine))

(use-package tree-sitter)
(use-package tree-sitter-langs)
(global-tree-sitter-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-inlay-hints-enable t
	lsp-inlay-hints-prefix " » "
	lsp-inlay-hints-highlight   "#6B8E23")
  (setq lsp-lens-enable t)
  (setq lsp-ui-sideline-enable t)

  :custom
  (lsp-completion-provider :none) 
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-enable-file-watchers t)
  (lsp-enable-imenu t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-xref t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-idle-delay 0.4)
  (lsp-inlay-hint-enable t)
  (lsp-lens-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-semantic-tokens-apply-modifiers t)
  (lsp-semantic-tokens-enable t)
  (lsp-semantic-tokens-warn-on-missing-face nil)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation t)



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

  :custom
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-height 45)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)

  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(defun my/lsp-ui-doc-scroll-up ()
  "Scroll up inside LSP UI documentation frame."
  (interactive)
  (scroll-up-command))

(defun my/lsp-ui-doc-scroll-down ()
  "Scroll down inside LSP UI documentation frame."
  (interactive)
  (scroll-down-command))

(defun my/setup-lsp-ui-doc-scroll-keys ()
  "Set up custom scroll keys for LSP UI documentation frame."
  (local-set-key (kbd "j") 'my/lsp-ui-doc-scroll-down)
  (local-set-key (kbd "k") 'my/lsp-ui-doc-scroll-up))

(add-hook 'lsp-ui-doc-frame-mode-hook 'my/setup-lsp-ui-doc-scroll-keys)

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
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
	(yas-global-mode 1)
	)

(use-package go-mode
  :ensure t
  :init
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "go.mod")
    (add-to-list 'projectile-project-root-files-bottom-up "go.sum"))
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

(use-package go-add-tags :ensure t)
(use-package go-gen-test :ensure t)
(use-package godoctor)

(use-package go-snippets)

(add-to-list 'tree-sitter-major-mode-language-alist
	     '(go-mode . go))

;; Hook Tree-sitter mode into your major modes
(add-hook 'go-mode-hook #'tree-sitter-hl-mode)

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

(use-package yaml-mode
  :mode "\\.ya?ml\\'")
(add-hook 'yaml-mode-hook
	  (lambda ()
	    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

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
     "f"  '(:ignore t :which-key "Projectile Find")
     "fs" '(projectile-grep :which-key "Find String")
     "fr" '(projectile-replace :which-key "Find Replace")
     "fa" '(projectile-replace-regexp :which-key "Find Replace Project")
     "ft" '(projectile-find-test-file :which-key "Find Tests")
     "ff" '(projectile-find-file :which-key "Find File"))

(use-package vterm
    :ensure t)

(defun vterm-split-window-below ()
  (interactive)
  (vterm)
  (split-window-below -12)
  (previous-buffer)
  (other-window 1))

(defun vterm-toggle ()
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (delete-window)
    (vterm-split-window-below)))

(neko/leader-keys
  "ot" '(vterm-toggle :which-key "Open Vterm"))

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

(use-package docker
  :ensure t)

(neko/leader-keys
  "c"  '(:ignore t :which-key "Container")
  "cd" '(docker-compose :which-key "Docker Compose")
  "ci" '(docker-images :which-key "Docker Images")
  )

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
	kubernetes-redraw-frequency 3600))

(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info)
	 ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
		   :background nil
		   :height 100
		   :italic t)))
  :config
  (global-blamer-mode 1))

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

(setq org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
      org-agenda-time-grid '((weekly today require-timed)
			     (800 1000 1200 1400 1600 1800 2000)
			     "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
      org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
				 (todo . " %i %-12:c")
				 (tags . " %i %-12:c")
				 (search . " %i %-12:c")))

(setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
						    "\n"
						    (org-agenda-format-date-aligned date))))
(setq org-cycle-separator-lines 2)
(setq org-agenda-category-icon-alist
      `(("Work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
	("Personal" ,(list (all-the-icons-material "person")) nil nil :ascent center)
	("Calendar" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)
	("Reading" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)))

(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (js . t)
   (latex . t)
   (calc . t)
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

(use-package calfw :ensure t
  :config
  (setq cfw:face-item-separator-color nil
	cfw:render-line-breaker 'cfw:render-line-breaker-none
	cfw:fchar-junction ?╋
	cfw:fchar-vertical-line ?┃
	cfw:fchar-horizontal-line ?━
	cfw:fchar-left-junction ?┣
	cfw:fchar-right-junction ?┫
	cfw:fchar-top-junction ?┯
	cfw:fchar-top-left-corner ?┏
	cfw:fchar-top-right-corner ?┓)
  )
(use-package calfw-org :ensure t
  :commands (cfw:open-org-calendar
	     cfw:org-create-source
	     cfw:org-create-file-source
	     cfw:open-org-calendar-withkevin)
  )
(require 'calfw-org)

(use-package org-fragtog)

(add-hook 'org-mode-hook
	   'org-fragtog-mode)

(setq org-startup-with-latex-preview t
      org-format-latex-options (plist-put
				org-format-latex-options :scale 3.0))

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
  "pk" '(persp-kill :which-key "Kill perspective")
  )

(neko/leader-keys
  "]" '(persp-next :which-key "Next perspective")
  "[" '(persp-prev :which-key "Prev perspective")
  )

(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))
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
  "wr" '(reload-dotemacs :which-key "Reload")
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
