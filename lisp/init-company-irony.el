(require-package 'company-irony)

(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(setq company-clang-executable "/usr/bin/clang-3.6")

(provide 'init-company-irony)
