(require-package 'openwith)

(setq openwith-associations
      (quote (("\\.eps\\'" "okular" (file))
              ("\\.pdf\\'" "okular" (file))
              ("\\.mp3\\'" "xmms" (file))
              ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
              ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file))
              ("\\.djvu\\'" "djview" (file))
              ("\\.odt\\'" "libreoffice" (file))
              ("\\.wav\\'" "play" (file)))))

(provide 'init-openwith)
