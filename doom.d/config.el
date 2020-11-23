;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Specify font here
(setq doom-font (font-spec :family "Spleen" :size 16)
      doom-variable-pitch-font (font-spec :family "SFNS Display" :size 16 :weight 'Regular))

;; Config emoji and nerdfont
(add-hook! 'doom-load-theme-hook
           :append
           (defun my/init-extra-fonts-h(&optional frame)
             (with-selected-frame (or frame (selected-frame))
               (set-fontset-font t 'symbol "Spleen" nil)
               (set-fontset-font t 'symbol "Apple Color Emoji" nil 'append)
               (set-fontset-font t 'symbol "GohuFont Nerd Font" nil 'append))))

(setq emojify-display-style 'unicode)                  ; Use unicode emoji

;; custom font set faces
(custom-set-faces!
  '(mode-line :family "SFNS Display" :height 120)
  '(mode-line-inactive :family "SFNS Display" :height 120)
  '(variable-pitch :family "SFNS Display" :height 140))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Fix native comp
(setq comp-async-env-modifier-form "")

;; faster GC
(use-package! gcmh
  :init
  (gcmh-mode 1))

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
 doom-themes-treemacs-theme "doom-colors"
 display-line-numbers-type 'relative                   ; Set vim-like relative number
 org-directory "~/org/"                                ; Set org mode files directory
 projectile-project-search-path '("~/code")            ; Set directory to search projects
 auto-save-default t                                   ; Enable autosaving
 doom-modeline-enable-word-count nil
 )

;; Set tabs to space
(setq-hook! 'haskell-mode-hook
  tab-width 4
  evil-shift-width 4
  standard-indent 4
  indent-tabs-mode nil
  )

; Centaur tabline
(use-package! centaur-tabs
  :hook (doom-first-file . centaur-tabs-mode)
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-style "chamfer"
        centaur-tabs-set-bar 'under
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "•"
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)
  :config
  (add-hook '+doom-dashboard-mode-hook #'centaur-tabs-local-mode)
  (add-hook '+popup-buffer-mode-hook #'centaur-tabs-local-mode)
  (centaur-tabs-mode t)
  (centaur-tabs-change-fonts "SFNS Display" 140)
  (centaur-tabs-group-by-projectile-project)
  (setq
   centaur-tabs-height 32
   centaur-tabs-set-icons t
   centaur-tabs-set-modified-marker t
   centaur-tabs-set-bar 'under))

;; Company completion
(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  )

(set-company-backend! '(c-mode
                        c++-mode
                        haskell-mode
                        lisp-mode
                        sh-mode
                        css-mode
                        web-mode
                        js-mode
                        python-mode
                        rust-mode
                        org-mode
                        nix-mode)
  '(:separate company-files))

;; LSP language server
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  (setq lsp-ui-sideline-enable nil
        lsp-modeline-diagnostics-enable nil))

; Add projects to projectile
(projectile-add-known-project "~/.nixconfig")

;; Set dired default to ranger mode
(use-package! ranger
    :config (setq ranger-override-dired 'ranger))

;; org mode
(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Evil mode
;; disable evil-escape (jk -> escape)
(after! evil-escape (evil-escape-mode -1))
(after! evil (setq evil-ex-substitute-global t))

;; Magit syntax highlight
(after! magit
  (magit-delta-mode +1))

;; indent guide character
(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive t
        highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))


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

;; opacity
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 60))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 60)))
(set-frame-parameter nil 'alpha nil) ;; let picom manage it
