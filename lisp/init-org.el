(when (< emacs-major-version 24)
  (require-package 'org))
(require-package 'org-fstree)
(when *is-a-mac*
  (require-package 'org-mac-link)
  (autoload 'org-mac-grab-link "org-mac-link" nil t)
  (require-package 'org-mac-iCal))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; reftex: Add hot key to add citations in various formats.
;; Reftex looks for a line \bibliography{ref.bib} in the org file to infere the
;; bib db.
;; Taken from
;;   http://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
;; and
;;   http://www-public.it-sudparis.eu/~berger_o/weblog/2012/03/23/how-to-manage-and-export-bibliographic-notesrefs-in-org-mode/
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t)
         (reftex-parse-all)
         ;add a custom reftex cite format to insert links
         (reftex-set-cite-format
          '((?b . "[[bib:%l]]")
            (?n . "[[note:%l]]")
            (?p . "[[paper:%l]]")
            (?B . "[[bib:%l][%l-bib]]")
            (?N . "[[note:%l][%l-note]]")
            (?P . "[[paper:%l][%l-paper]]")
            (?t . "%t")
            (?h . "** %t\n   :PROPERTIES:\n   :CUSTOM_ID: %l\n   :END:\n   [[paper:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(defun my-rtcite-export-handler (path desc format)
  (message "my-rtcite-export-handler is called : path = %s, desc = %s, format = %s" path desc format)
  (let* ((search (when (string-match "::#?\\(.+\\)\\'" path)
                   (match-string 1 path)))
         (path (substring path 0 (match-beginning 0))))
    (cond ((eq format 'latex)
           (if (or (not desc)
                   (equal 0 (search "rtcite:" desc)))
               (format "\\cite{%s}" search)
             (format "\\cite[%s]{%s}" desc search))))))

(require 'org)

(org-add-link-type "rtcite"
                   'org-bibtex-open
                   'my-rtcite-export-handler)



;; Replace links to org entries by chapter numbers in latex export.
(setq org-export-latex-hyperref-format "\\ref{%s}")

(setq org-agenda-custom-commands
      (quote
       (("n" "NEXTs"
         ((todo "NEXT" nil))
         ((ps-landscape-mode t) (ps-font-size 14) (ps-number-of-columns 1))
         ("~/org/org2hpda/org-agenda-nexts.ps"))
        ("p" "Show agenda (4 weeks) for PDA" agenda ""
         ((org-agenda-span (quote year))
          (org-agenda-tag-filter-preset (quote ("-tickle"))))
         ("~/org/org2hpda/org-agenda.ics"))
        ("h" "NEXT @heim" tags "@heim+TODO=\"NEXT\"" nil)
        ("b" "NEXT besorgung" tags "besorgung+TODO=\"NEXT\"" nil)
        ("w" "WAITING" tags "TODO=\"WAITING\"" nil)
        ("g" "Texts about postgresql" tags "read+pgsql+TODO=\"NEXT\"" nil)
        ("r" "Reading / self studying projects" tags "read+TODO=\"NEXT\"" nil))))

(setq org-agenda-files
      (quote ("~/org")))

(setq org-capture-templates
      (quote (("i" "Internetkauf" entry
               (file+headline "~/org/org.org" "Projekte")
               (file "~/org/capture-templates/template_internetkauf.org"))
              ("n" "Notiz" entry (file "~/org/inbox.org")
               (file "~/org/capture-templates/template_task.org"))
              ("b" "Buchkauf" entry
               (file+headline "~/org/org.org" "Projekte")
               (file "~/org/capture-templates/template_buchkauf.org")))))

(setq org-confirm-babel-evaluate nil)

(setq org-default-notes-file "~/org/inbox.org")

(setq org-export-allow-BIND t)

(setq org-export-allow-bind-keywords t)

(setq org-export-backends (quote (ascii html icalendar latex odt)))

(setq org-export-latex-classes
      (quote (("article"
               "\\documentclass[11pt]{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
               )
              ("report"
               "\\documentclass[11pt]{report}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
              ("book"
               "\\documentclass[11pt]{book}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
              ("beamer" "\\documentclass{beamer}" org-beamer-sectioning))))

(setq org-latex-classes
      (quote
       (("article"
         "\\documentclass[11pt]{article}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ("report"
         "\\documentclass[11pt]{report}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("book"
         "\\documentclass[11pt]{book}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("scrartcl"
         "\\documentclass{scrartcl}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ("scrbook"
         "\\documentclass{scrbook}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("scrreprt"
         "\\documentclass{scrreprt}"
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

(setq org-latex-default-packages-alist
      (quote
       (("AUTO" "inputenc" t)
        ("T1" "fontenc" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("" "soul" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "latexsym" t)
        ("" "amssymb" t)
        ("" "amstext" nil)
        ("" "hyperref" nil)
        "\\tolerance=1000"
        ("" "amsmath" nil)
        ("" "blkarray" nil)
        ("" "gauss" nil) ("" "color" nil))))

(setq org-latex-pdf-process (quote ("latexmk -pdf %b")))

(setq org-latex-to-pdf-process (quote ("latexmk -pdf %b")))

(setq org-taskjuggler-valid-resource-attributes
      (quote (limits
              vacation
              shift
              booking
              efficiency
              journalentry
              rate
              workinghours
              flags
              leaves)))

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday 1
      org-agenda-span 7
      org-agenda-include-diary nil
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)


; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


(require-package 'org-pomodoro)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil))
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (sh . t)
     (sql . nil)
     (sqlite . t))))


(provide 'init-org)
