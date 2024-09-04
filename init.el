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

(setq package-enable-at-startup nil)

(require 'use-package)
(straight-use-package 'use-package)

(require 'server)
(unless (server-running-p)
  (server-start))

(setq warning-minimum-log-level :error)

(use-package comp
  :custom
  (native-comp-async-report-warnings-errors nil))

(setq auto-save-timeout 15
      auto-save-interval 60
      backup-directory-alist `((".*" . ,(locate-user-emacs-file "backup")))
      auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "backup/") t))
      version-control t
      delete-old-versions t)

(setq create-lockfile nil
      debug-on-error t
      frame-resize-pixelwise t
      enable-recursive-minibuffers t
      history-delete-duplicates t
      scroll-preserver-screen-position t
      scroll-conservatively 100
      mouse-wheel-scroll-amout '(1 ((control) . 5))
      column-number-mode t
      size-indication-mode t
      hisotry-length 1000)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package visual-regexp
  :ensure t
  :config
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace))

(use-package autorevert
  :custom
  (auto-revert-interval 1)
  :config
  (global-auto-revert-mode))

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(use-package ddskk
  :ensure t
  :init
  (setq skk-server-host "localhost"
        skk-server-portnum 1178
        skk-japanese-message-and-error t
        skk-dcomp-activate t
        skk-comp-prefix t
        skk-share-private-jisyo t)
  :bind ("C-j" . skk-mode))

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

(set-face-attribute 'default nil :family "UDEV Gothic NF" :height 150)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  :config
  (load-theme 'modus-vivendi-deuteranopia t))

(set-frame-parameter nil 'alpha 90)

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

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))

(use-package which-key
  :ensure t
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

(use-package eglot
  :ensure t
  :config
  (setq eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:inlayHintProvider)
        eglot-confirm-server-initiated-edits nil))

(use-package eglot-booster
  :ensure t
  :straight '(eglot-booster
              :type git
              :host github
              :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode))

(use-package consult-eglot
  :ensure t)

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
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

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
  :hook
  ((typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)))

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

  ;; gopls settings
  ;; see: https://github.com/golang/tools/blob/master/gopls/doc/settings.md
  (setq-default eglot-workspace-configuration
                '((:gopls .
                          ((staticcheck . t)
                           (matcher . "CaseSensitive")))))

  ;; Organizing imports
  (add-hook 'before-save-hook
            (lambda ()
              (call-interactively 'eglot-code-action-organize-imports))
            nil t)

  :custom
  (gofmt-command "goimports")
  :hook
  (go-mode . eglot-ensure))

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

  (use-package auth-source-1password
    :straight '(auth-source-1password
                :type git
                :host github
                :repo "dlobraico/auth-source-1password")
    :ensure t
    :custom
    (auth-source-1password-executable "op.exe") ;; for WSL
    :init
    (auth-source-1password-enable))

  (use-package llm
    :ensure t
    :straight '(llm
                :type git
                :host github
                :repo "ahyatt/llm"))

  (use-package ellama
    :ensure t
    :requires (llm)
    :init
    (require 'llm-gemini)
    ;; ellama-translateで翻訳する言語
    (setopt ellama-language "Japanese")

    ;; ellama-ask-selection などで生成されるファイルのネーミングルール
    (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)

    ;; デフォルトのプロバイダー
    (setopt ellama-provider (make-llm-gemini
                             :key (auth-source-pick-first-password :host "GoogleGeminiAPIKey" :user "key")
                             :chat-model "gemini-1.5-flash"))

    ;; プロンプトの上書き
    ;; see: https://qiita.com/keita44_f4/items/2386e1623b3e3199efc0
    (setopt ellama-summarize-prompt-template "Text:\n%s\n要約して")
    (setopt ellama-generate-commit-message-template "あなたは熟練のプログラマーです。後の変更点をもとに完結なコミットメッセージを書いてください。コミットメッセージの形式は、1行目は変更点の要約、2行目は空行、それ以降の行は変更全体の詳細な説明です、です。出力はプロンプト無しで最終的なコミットメッセージだけにしてください。\n\n変更点:\n%s\n")
    ))
