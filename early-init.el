;; UI
;; see: https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/dcebc13578d42055f8a4#early-init.el
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(scroll-bar-mode -1)
(tab-bar-mode t)

(setq default-frame-alist
      (append '((width . 188)
                (height . 56))
              default-frame-alist))

;; GC
;; see: https://zenn.dev/takeokunn/articles/56010618502ccc#gc%E3%81%AE%E8%A8%AD%E5%AE%9A
;; see: https://apribase.net/2024/07/23/emacs-optimize/
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      garbage-collection-messages nil)
(setq read-process-output-max (* 3 1024 1024)
      process-adaptive-read-buffering nil)

;; font
;; see: https://apribase.net/2024/07/06/emacs-default-frame-alist/
(add-to-list 'default-frame-alist '(font . "UDEV Gothic NF-16"))
