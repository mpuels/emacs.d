(require-package 'flymd)

;; Open preview in Firefox, as it won't work in Chrome.
;; More info: https://github.com/mola-T/flymd/blob/master/browser.md#chrome-windows-or-uix
(defun my-flymd-browser-function (url)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)

(provide 'init-flymd)
