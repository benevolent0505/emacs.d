;; UI
;; see: https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/dcebc13578d42055f8a4#early-init.el
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(toggle-scroll-bar 0)
(scroll-bar-mode -1)

;; GC
;; see: https://zenn.dev/takeokunn/articles/56010618502ccc#gc%E3%81%AE%E8%A8%AD%E5%AE%9A
(setq gc-cons-threshold (* 128 1024 1024))
