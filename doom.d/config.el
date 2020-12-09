;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;This file is generated from "README.org"
(setq comp-async-env-modifier-form "")
(setq doom-font "CozetteVector-10.5"
      doom-variable-pitch-font "ETBembo-11")
(add-hook! 'doom-load-theme-hook
           :append
           (defun my/init-extra-fonts-h(&optional frame)
             (with-selected-frame (or frame (selected-frame))
               (set-fontset-font t 'symbol "CozetteVector" nil)
               (set-fontset-font t 'symbol "Apple Color Emoji" nil 'append)
               (set-fontset-font t 'symbol "GohuFont Nerd Font" nil 'append))))
(custom-set-faces!
  '(mode-line :family "Liberation Sans" :height 105)
  '(mode-line-inactive :family "Liberation Sans" :height 105)
  '(doom-dashboard-menu-title :foreground "#c792ea")
  '(org-document-title :height 225)
  )
(setq emojify-display-style 'unicode)
(setq doom-theme 'doom-palenight)
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)
(setq +doom-dashboard-banner-file "emacs-logo.png"
      +doom-dashboard-banner-dir "/etc")
(setq doom-themes-treemacs-theme "doom-colors")
(setq doom-themes-treemacs-enable-variable-pitch nil)
(custom-set-faces!
  '(treemacs-root-face :family "Liberation Sans" :height 105)
  '(treemacs-git-unmodified-face :family "Liberation Sans" :height 105)
  '(treemacs-git-modified-face :family "Liberation Sans" :height 105)
  '(treemacs-git-renamed-face :family "Liberation Sans" :height 105)
  '(treemacs-git-ignored-face :family "Liberation Sans" :height 105)
  '(treemacs-git-untracked-face :family "Liberation Sans" :height 105)
  '(treemacs-git-added-face :family "Liberation Sans" :height 105)
  '(treemacs-git-conflict-face :family "Liberation Sans" :height 105)
  '(treemacs-directory-face :family "Liberation Sans" :height 105)
  '(treemacs-directory-collapsed-face :family "Liberation Sans" :height 105)
  '(treemacs-file-face :family "Liberation Sans" :height 105)
  '(treemacs-tags-face :family "Liberation Sans" :height 105)
  )

(defun my/treemacs-setup-title ()
  (let ((bg (face-attribute 'default :background))
        (fg (face-attribute 'default :foreground)))
    (face-remap-add-relative 'header-line
                             :family "Liberation Sans"
                             :height 100
                             :background bg :foreground fg
                             :box `(:line-width ,(/ (line-pixel-height) 2) :color ,bg)))
  (setq header-line-format
        '((:eval
           (let* ((text "Explorer")
                  (extra-align (+ (/ (length text) 2) 1))
                  (width (- (/ (window-width) 2) extra-align)))
             (concat (make-string width ?\s) text))))))

(add-hook 'treemacs-mode-hook #'my/treemacs-setup-title)
(setq display-line-numbers-type nil)
(setq-default
 indent-tabs-mode nil
 tab-width 4
 evil-shift-width 4
 standard-indent 4
 line-spacing 4
 )
(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-responsive t))

(projectile-add-known-project "~/.nixconfig")
(setq doom-modeline-enable-word-count nil)
(setq doom-modeline-height 20)
(setq posframe-gtk-resize-child-frames 'resize-mode)
(setq fast-but-imprecise-scrolling t)
(add-hook 'prog-mode-hook 'pixel-scroll-mode)
(add-hook 'text-mode-hook 'pixel-scroll-mode)
(setq pixel-dead-time 0)
(setq pixel-resolution-fine-flag t)
(remove-hook! doom-first-buffer #'global-hl-line-mode)
(use-package! centaur-tabs
  :hook (doom-first-file . centaur-tabs-mode)
  :init
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-style "chamfer")
  (setq centaur-tabs-set-bar 'under)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "•")
  (setq centaur-tabs-close-button "")
  (setq centaur-tabs-cycle-scope 'tabs)
  :config
  (add-hook '+doom-dashboard-mode-hook #'centaur-tabs-local-mode)
  (add-hook '+popup-buffer-mode-hook #'centaur-tabs-local-mode)
  (centaur-tabs-change-fonts "Liberation Sans" 110)
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-set-bar 'under)
  (setq centaur-tabs-height 30)
  (setq centaur-tabs-left-edge-margin "         ")
  (setq centaur-tabs-right-edge-margin "         ")
  (centaur-tabs-mode t)
  :bind
  (:map evil-normal-state-map
   ("g t" . centaur-tabs-forward)
   ("g T" . centaur-tabs-backward))
  )
;; Company completion
(after! company
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers nil)
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
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-ui-sideline-enable nil)
  )
(setq-hook! 'haskell-mode-hook
  tab-width 4
  evil-shift-width 4
  standard-indent 4
  indent-tabs-mode nil
  )
(setq-default
 history-length 1000
 prescient-history-length 1000
 )
(add-hook 'text-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(setq org-directory "~/org/")
(setq org-startup-folded t)
(setq org-startup-with-inline-images t)
(use-package! mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))
(setq mixed-pitch-set-height t)
(setq org-superstar-headline-bullets-list '("🌑" "💜" "👛" "❄" "🌷"))
(setq org-hide-emphasis-markers t)
(with-eval-after-load 'org
  (set-face-attribute 'org-level-1 nil :height 1.5)
  (set-face-attribute 'org-level-2 nil :height 1.25)
  (set-face-attribute 'org-level-3 nil :height 1.0)
  (set-face-attribute 'org-level-4 nil :height 1.0)
  (set-face-attribute 'org-level-5 nil :height 1.0)
  )
(setq org-ellipsis "")
(defun my-set-margins ()
  (setq left-margin-width 2)
  (setq right-margin-width 2))
(add-hook 'org-mode-hook 'my-set-margins)
(after! evil-escape (evil-escape-mode -1))
(after! evil (setq evil-ex-substitute-global t))
(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
(setq auto-save-default t)
(setq-default delete-by-moving-to-trash t)
(setq-default x-stretch-cursor t)
(use-package! ranger
    :config (setq ranger-override-dired 'ranger))
(use-package! haskell-mode
  :config
  (setq haskell-mode-stylish-haskell-path "brittany")
)
(use-package! lsp-haskell
 :config
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
)
(set-frame-parameter nil 'alpha nil) ;; let picom manage it
(after! twittering-mode
  (setq twittering-icon-mode t)
  )
