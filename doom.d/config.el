;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Specify font here
(setq doom-font (font-spec :family "Tamzen" :size 12.75)
      doom-variable-pitch-font (font-spec :family "SFNS Display" :size 13.5 :weight 'Regular))
;; Emoji and nerdfont stuff
(set-fontset-font t 'symbol "Apple Color Emoji" nil)
(set-fontset-font t 'symbol "GohuFont Nerd Font" nil 'append)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Fix native comp
(setq comp-async-env-modifier-form "")

;; Basic settings
(setq-default
 delete-by-moving-to-trash t                           ; Delete files to trash
 indent-tabs-mode nil                                  ; Disable tab and use space
 tab-width 4                                           ; Use 4 space tab
 evil-shift-width 4
 standard-indent 4
 line-spacing 4
 x-stretch-cursor t                                    ; Draw cursor as wide as the character under the cursor
 history-length 1000
 prescient-history-length 1000
 )


(setq
 doom-theme 'doom-palenight                            ; Set theme https://github.com/hlissner/emacs-doom-themes
 doom-themes-enable-bold t
 doom-themes-enable-italic t
 fancy-splash-image "~/Pictures/doomdashboardlogo.png" ; Set dashboard image
 display-line-numbers-type 'relative                   ; Set vim-like relative number
 doom-modeline-height 40                               ; Set doom modeline(bottombar) height
 org-directory "~/org/"                                ; Set org mode files directory
 projectile-project-search-path '("~/code")            ; Set directory to search projects
 auto-save-default t                                   ; Enable autosaving
 doom-modeline-enable-word-count nil
 )


;; Set tabs to space
(setq-hook! 'nix-mode-hook
  tab-width 4
  evil-shift-width 4
  standard-indent 4
  indent-tabs-mode nil
  )
(setq-hook! 'haskell-mode-hook
  tab-width 4
  evil-shift-width 4
  standard-indent 4
  indent-tabs-mode nil
  )

; Centaur tabline
(use-package! centaur-tabs
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (setq
   centaur-tabs-height 30
   centaur-tabs-set-icons t
   centaur-tabs-set-modified-marker t
   centaur-tabs-set-bar 'left))

; Disable current line highlight
(remove-hook! (prog-mode text-mode conf-mode special-mode) 'hl-line-mode)

; Add projects to projectile
(projectile-add-known-project "~/.nixconfig")

;; Set dired default to ranger mode
(use-package! ranger
    :config (setq ranger-override-dired 'ranger))

;; org mode
(setq org-startup-with-inline-images t)

;; Setup emms
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/Music/")

;; elcord(discord rpc)
;; (setq
;;  elcord-use-major-mode-as-main-icon t                  ; Use filetype icon as main icon
;;  )
;; (elcord-mode)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;opacity adjustment
;; (set-frame-parameter (selected-frame)'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist'(alpha . (90 . 90)))

;; Load tabnine
;; (use-package! company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)
;; Load company emoji
;; (use-package! company-emoji)
;; (add-to-list 'company-backends #'company-emoji)

;; Autocompletion(company)
(setq company-idle-delay 0
      company-minimum-prefix-length 2
      company-show-numbers t
      )

;; Use vim-like mark folding
;; (use-package! origami
;;   :config
;;   (global-origami-mode 1)

;;   (defun nin-origami-toggle-node ()
;;     (interactive)
;;     (if (equal major-mode 'org-mode)
;; 	(org-cycle)
;;       (save-excursion ;; leave point where it is
;; 	(goto-char (point-at-eol))             ;; then go to the end of line
;; 	(origami-toggle-node (current-buffer) (point)))))                 ;; and try to fold

;;   (add-hook 'prog-mode-hook
;; 	    (lambda ()
;; 	      ;; parsers see in variable origami-parser-alist
;; 	      (setq-local origami-fold-style 'triple-braces)
;; 	      (origami-mode)
;; 	      (origami-close-all-nodes (current-buffer))
;; 	      ))
;;   (evil-define-key 'normal prog-mode-map (kbd "<tab>") 'nin-origami-toggle-node)

;;   (define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
;;   (define-key evil-normal-state-map "zR" 'origami-close-all-nodes)
;;   (define-key evil-normal-state-map "zM" 'origami-open-all-nodes)
;;   (define-key evil-normal-state-map "zr" 'origami-close-node-recursively)
;;   (define-key evil-normal-state-map "zm" 'origami-open-node-recursively)
;;   (define-key evil-normal-state-map "zo" 'origami-show-node)
;;   (define-key evil-normal-state-map "zc" 'origami-close-node)
;;   (define-key evil-normal-state-map "zj" 'origami-forward-fold)
;;   (define-key evil-normal-state-map "zk" 'origami-previous-fold)
;;   (define-key evil-visual-state-map "zf"
;;     '(lambda ()
;;        "create fold and add comment to it"
;;        (interactive)
;;        (setq start (region-beginning))
;;        (setq end (region-end))
;;        (deactivate-mark)
;;        (and (< end start)
;; 	    (setq start (prog1 end (setq end start))))
;;        (goto-char start)
;;        (beginning-of-line)
;;        (indent-according-to-mode)
;;        (if (equal major-mode 'emacs-lisp-mode)
;; 	   (insert ";; ")
;; 	 (insert comment-start " "))

;;        (setq start (point))
;;        (insert " {{{")
;;        (newline-and-indent)
;;        (goto-char end)
;;        (end-of-line)
;;        (and (not (bolp))
;; 	    (eq 0 (forward-line))
;; 	    (eobp)
;; 	    (insert ?\n))
;;        (indent-according-to-mode)
;;        (if (equal major-mode 'emacs-lisp-mode)
;; 	   (insert ";; }}}")

;; 	 (if (equal comment-end "")
;; 	     (insert comment-start " }}}")
;; 	   (insert comment-end "}}}")))
;;        (newline-and-indent)
;;        (goto-char start)
;;        ))
;;   )

(use-package lsp-mode)

;; Haskell stuff
(use-package! haskell-mode
  :config
  (setq haskell-mode-stylish-haskell-path "brittany")
)
(use-package! lsp-haskell
 :config
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;; (setq lsp-log-io t)
)

(require 'fast-scroll)
(add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
(add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
(add-hook 'fast-scroll-start-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'fast-scroll-end-hook (lambda () (display-line-numbers-mode 1)))
(fast-scroll-config)
(fast-scroll-mode 1)
(setq fast-scroll-throttle 0.9)

(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
