;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Disable packages

;; Basic stuffs
(package! centaur-tabs
  :recipe(:host github :repo "MoritzMaxeiner/centaur-tabs" :branch "daemon"))
(package! magit-delta)

;; org-mode
(package! mixed-pitch)

;; Filetypes
(package! vimrc-mode)
(package! fish-mode)

;; company(autocompletion) packages
;; (package! company-tabnine)
;; (package! company-emoji)
