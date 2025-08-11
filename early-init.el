(setopt debug-on-error t)
(setopt warning-suppress-type '((comp)))
(setopt package-enable-at-startup nil)

;; UI
;; see: https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/dcebc13578d42055f8a4#early-init.el
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(scroll-bar-mode -1)
(tab-bar-mode t)

;; GC
;; see: https://zenn.dev/takeokunn/articles/56010618502ccc#gc%E3%81%AE%E8%A8%AD%E5%AE%9A
;; see: https://apribase.net/2024/07/23/emacs-optimize/
(setq gc-cons-threshold (* 128 1024 1024)
      garbage-collection-messages nil)

;; font
;; see: https://apribase.net/2024/07/06/emacs-default-frame-alist/
(add-to-list 'default-frame-alist '(font . "UDEV Gothic NF-16"))

;; lsp-mode
;; see: https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")
