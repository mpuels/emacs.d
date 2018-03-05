;; The following 'load'ed packages are no longer available on melpa.org.
(load "~/.emacs.d/site-lisp/dired+")
(load "~/.emacs.d/site-lisp/dired-sort.el")
(load "~/.emacs.d/site-lisp/dired-details.el")
(load "~/.emacs.d/site-lisp/dired-details+.el")

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
(setq dired-listing-switches "-al --group-directories-first")

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%"))))

(when (maybe-require-package 'diff-hl)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(add-hook 'dired-load-hook
          (lambda ()
                      (load "dired-x")
                      ;; Set global variables here.  For example:
                      ;; (setq dired-guess-shell-gnutar "gtar")
                      ))

(require 'dired-details+)
(setq dired-details-hidden-string "")
(setq dired-dwim-target t)
(setq dired-load-hook (quote ((lambda nil (load "dired-x")))))
;(setq dired-mode-hook (quote (cscope:hook dired-extra-startup dired-omit-mode)))

(put 'dired-find-alternate-file 'disabled nil)

(defun dired-mode-keys ()
  "Modify keymaps used by `dired-mode'."
  (local-set-key (kbd "<tab>") 'dired-hide-subdir)
  (local-set-key (kbd "<backtab>") 'dired-hide-all))

(add-hook 'dired-mode-hook 'dired-mode-keys)

(provide 'init-dired)
