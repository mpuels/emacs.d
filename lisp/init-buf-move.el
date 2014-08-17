(require-package 'buffer-move)

(global-set-key (kbd "C-M-ö") 'buf-move-left)
(global-set-key (kbd "C-M-#") 'buf-move-right)
(global-set-key (kbd "C-M-ü") 'buf-move-up)
(global-set-key (kbd "C-M-ä") 'buf-move-down)

(provide 'init-buf-move)
