;; https://tarao.hatenablog.com/entry/20150221/1424518030#tips-isolated-setup
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(require 'package)
(setopt package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'use-package)
(straight-use-package 'use-package)

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package comp
  :config
  (setopt native-comp-async-jobs-number 8
          native-comp-speed 1
          native-comp-always-compile t))

(setq create-lockfile nil)

(setopt auto-save-timeout 30
        auto-save-interval 300
        auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "backup/") t))
        backup-directory-alist `((".*" . ,(locate-user-emacs-file "backup")))
        delete-old-versions t
        version-control t
        column-number-mode t
        enable-recursive-minibuffers t
        frame-resize-pixelwise t
        frame-inhibit-implied-resize t
        history-length 1000
        history-delete-duplicates t
        mouse-wheel-scroll-amount '(1 ((control) . 5))
        ring-bell-function 'ignore
        scroll-preserve-screen-position t
        scroll-conservatively 100
        size-indication-mode t
        use-short-answers t)

(with-eval-after-load 'simple
  (setopt idle-update-delay 1.0))

(with-eval-after-load 'frame
  (blink-cursor-mode -1))

(global-display-line-numbers-mode)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package visual-regexp
  :ensure t
  :bind
  (("M-%" . 'vr/replace)
   ("C-M-%" . 'vr/query-replace)
   ("C-c m" . 'vr/mc-mark)))

(use-package autorevert
  :custom
  (auto-revert-interval 1)
  :config
  (global-auto-revert-mode))

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(use-package ddskk
  :ensure t
  :custom
  ((skk-server-host "localhost")
   (skk-server-portnum 1178)
   (skk-japanese-message-and-error t)
   (skk-dcomp-activate t)
   (skk-comp-prefix t)
   (skk-share-private-jisyo t))
  :init
  (defvar dired-bind-jump nil) ;; dired-x C-x C-j が奪われないように
  :bind
  (("C-j" . skk-mode)
   ("C-x C-j" . skk-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  ;; https://murase-syuka.hatenablog.com/entry/20140815/1408061850
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30)))
  :hook prog-mode)

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

(setopt modus-themes-bold-constructs t
	      modus-themes-italic-constructs t)
(load-theme 'modus-vivendi-deuteranopia t)

(setq-default indent-tabs-mode nil
              tab-width 2
              show-trailing-whitespace t)

(use-package delsel
  :config
  (delete-selection-mode))

(use-package paren
  :init
  (show-paren-mode t))

(use-package elec-pair
  :init
  (electric-pair-mode t))

(use-package puni
  :ensure t
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))

(use-package which-key
  :config
  (which-key-mode))

(use-package whitespace
  :custom
  (whitespace-style '(face trailing tabs empty sparce-mark tab-mark))
  :config
  (setq whitespace-space-regexp "\\(\u3000+\\)"
        whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1])
                                      (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  :init
  (global-whitespace-mode t))

(use-package hl-line
  :init
  (global-hl-line-mode t))

(use-package recentf
  :init
  (recentf-mode t))

(use-package helpful
  :ensure t
  :bind
  (("C-c C-d" . #'helpful-at-point)
   ("C-h v" . #'helpful-variable)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 20)
  (vertico-resize t))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-c h" . consult-history)
         ("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("M-g g" . consult-goto-line)
         ("M-s f" . consult-find)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package affe
  :ensure t
  :init
  (setq affe-highlight-function 'orderless-highlight-matches
        affe-regexp-function 'orderless-pattern-compiler
        affe-find-command "fd --color=never --full-path")
  :bind
  (("M-s g" . affe-grep)
   ("M-s f" . affe-find)))

(use-package consult-ghq
  :ensure t
  :init
  (global-set-key (kbd "C-c g") 'consult-ghq-find))

(use-package editorconfig
  :ensure t
  :straight '(editorconfig
              :type git
              :host github
              :repo "editorconfig/editorconfig-emacs")
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet))

(use-package company
  :ensure t
  :after company-statistics
  :bind (("M-<tab>" . company-complete)
         :map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :init
  (global-company-mode)
  :config
  (define-key emacs-lisp-mode-map (kbd "C-M-i") nil)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (setq completion-ignore-case t
        company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance)))

(use-package company-statistics
  :ensure t
  :init
  (company-statistics-mode))

(use-package company-dwim
  :straight '(company-dwim
              :type git
              :host github
              :repo "zk-phi/company-dwim")
  :ensure t
  :init
  (define-key company-active-map (kbd "TAB") 'company-dwim)
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-dwim-frontend
          company-echo-metadata-frontend)))

(use-package company-anywhere
  :straight '(company-anywhere
              :type git
              :host github
              :repo "zk-phi/company-anywhere")
  :ensure t)

(use-package company-same-mode-buffers
  :straight '(company-same-mode-buffers
              :type git
              :host github
              :repo "zk-phi/company-same-mode-buffers")
  :after company
  :ensure t
  :init
  (require 'company-same-mode-buffers)
  (company-same-mode-buffers-initialize)
  :config
  (setq company-backends
        '((company-capf :with company-same-mode-buffers)
          (company-dabbrev-code :with company-same-mode-buffers)
          company-keywords
          company-files
          company-dabbrev)))

(use-package mistty
  :ensure t
  :bind (("C-c s" . mistty)
         :map mistty-prompt-map
         ("M-<up>" . mistty-send-key)
         ("M-<down>" . mistty-send-key)
         ("M-<left>" . mistty-send-key)
         ("M-<right>" . mistty-send-key)))

(use-package tramp
  :custom
  (tramp-default-method "sshx")
  :init
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
    ;; Go の devcontainer 向け定設
    (add-to-list 'tramp-remote-path "/go/bin")))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-modeline-diagnostics-enable t
        lsp-use-plists t
        lsp-log-io nil)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\tmp\\'")

  (defun lsp-save-hooks ()
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'lsp-mode-hook #'lsp-save-hooks)

  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (go-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-side t
        lsp-idle-delay 0.500)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :commands lsp-ui-mode)

(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode)
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols)))

;; emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package treesit-auto
  :ensure t
  :config
  ;; Go は go-mode を使いたいので、明示的に抜く
  ;; see: https://github.com/renzmann/treesit-auto?tab=readme-ov-file#choose-which-languages-treesit-auto-should-consider
  (delete 'go treesit-auto-langs)
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(setq treesit-extra-load-path `(locate-user-emacs-file "tree-sitter"))
(use-package treesit
  :config
  (setq treesit-font-lock-level 4))

(use-package tree-sitter
  :ensure t
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package flymake
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package eldoc
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x C-b" . magit-blame))

(use-package git-gutter-fringe
  :ensure t
  :requires (git-gutter fringe-helper)
  :init
  (global-git-gutter-mode))

(use-package git-link
  :ensure t
  :custom
  ((git-link-open-in-browser t)
   (git-link-use-commit t)
   (git-link-use-single-line-number t)))

(use-package js2-mode
  :ensure t
  :custom
  (js-indent-level 2)
  :hook
  (js-mode-hook . js2-minor-mode))

(use-package typescript-mode
  :ensure t
  :mode
  (("\\.ts\\'" . typescript-ts-mode)
   ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  ((typescript-indent-level 2)
   (typescript-ts-mode-indent-offset 2))
  :config
  (defun disable-lsp-format-buffer ()
    (setq-local lsp-format-buffer nil))
  (add-hook 'typescript-ts-mode-hook #'disable-lsp-format-buffer)
  (add-hook 'tsx-ts-mode-hook #'disable-lsp-format-buffer)
  :hook
  ((typescript-ts-mode . lsp)
   (tsx-ts-mode . lsp)))

(use-package add-node-modules-path
  :ensure t
  :config
  (setq add-node-modules-path-command '("echo \"$(npm root)/.bin\""))
  (add-hook 'typescript-ts-mode-hook #'add-node-modules-path)
  (add-hook 'tsx-ts-mode-hook #'add-node-modules-path))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2))

(use-package prettier
  :ensure t
  :hook
  ((typescript-ts-mode . prettier-mode)
   (tsx-ts-mode . prettier-mode)))

;;; Golang
(use-package go-mode
  :ensure t
  :config
  (defun go-mode-whitespace-style ()
    "golang ではハードタブを可視化しない"
    (setq whitespace-style
          '(face
            trailing
            spaces
            space-mark)))
  (add-hook 'go-mode-hook #'go-mode-whitespace-style)
  :custom
  ((gofmt-command "goimports")
   (lsp-register-custom-settings
    '(("gopls.completeUnimported" t t)
      ("gopls.staticcheck" t t))))
  :hook
  ((go-mode . eldoc-mode)
   (before-save-hook . gofmt-before-save)))

;; Note: .dir-locals.el で flycheck-golangci-lint-config の設定を書くこと
(use-package flycheck-golangci-lint
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

(use-package go-gen-test
  :ensure t)

(use-package go-fill-struct
  :ensure t)

(use-package go-errcheck
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")

(use-package docker-compose-mode
  :ensure t)

(use-package graphql-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setopt markdown-fontify-code-blocks-natively t)
  (setopt markdown-indent-on-enter 'indent-and-new-item))

(use-package fish-mode
  :ensure t)

(use-package copilot
  :after (editorconfig f)
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (setq copilot-indent-offset-warning-disable t)
  (define-key copilot-completion-map (kbd "C-t") 'copilot-accept-completion)
  :hook (prog-mode . copilot-mode))














;; for macOS
(when (eq system-type 'darwin)

  ;; Terraform
  (use-package terraform-mode
    :ensure t
    :config
    (defun my-terraform-mode-init ()
      (outline-minor-mode 1)
      )

    (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

  ;; Jsonnet
  (use-package jsonnet-mode
    :ensure t)
  )

;;; Private PC Settings
;; 私用だと WSL なので、この判定で十分
(when (and (eq system-type 'gnu/linux)
           (getenv "WSL_DISTRO_NAME"))
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "clip.exe"))

  (defun wsl-paste ()
    (interactive)
    (insert
     ;;; trim new line
     (replace-regexp-in-string
      "\^M"
      ""
      (shell-command-to-string "powershell.exe -command 'Get-Clipboard'"))))

  (setq visible-bell t)
  )

