;;; init.el --- jjba Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024 Josep Bigorra

;; Version: 0.1.0
;; Author: Josep Bigorra <jjbigorra@gmail.com>
;; Maintainer: Josep Bigorra <jjbigorra@gmail.com>
;; Keywords: emacs, configuration, dotfiles
;; Package: emacs

;; init.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; init.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil 
                              :depth 1 
                              :files (:defaults "elpaca-test.el" (:exclude "extensions")) 
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory)) 
       (build (expand-file-name "elpaca/" elpaca-builds-directory)) 
       (order (cdr elpaca-order)) 
       (default-directory repo)) 
  (add-to-list 'load-path (if (file-exists-p build) build repo)) 
  (unless (file-exists-p repo) 
    (make-directory repo t) 
    (when (< emacs-major-version 28) 
      (require 'subr-x)) 
    (condition-case-unless-debug err (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*")) 
																							((zerop (apply #'call-process `("git" nil ,buffer t "clone" ,@(when-let ((depth (plist-get order :depth))) 
																																																							(list (format "--depth=%d" depth) "--no-single-branch")) 
																																							,(plist-get order 
																																													:repo) ,repo)))) 
                                              ((zerop (call-process "git" nil buffer t "checkout" (or (plist-get order 
                                                                                                                 :ref) "--")))) 
                                              (emacs (concat invocation-directory invocation-name)) 
                                              ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch" "--eval" "(byte-recompile-directory \".\" 0 'force)"))) 
                                              ((require 'elpaca)) 
                                              ((elpaca-generate-autoloads "elpaca" repo))) 
                                         (progn (message "%s" (buffer-string)) 
                                                (kill-buffer buffer)) 
                                       (error "%s" (with-current-buffer buffer (buffer-string)))) 
      ((error) 
       (warn "%s" err) 
       (delete-directory repo 'recursive)))) 
  (unless (require 'elpaca-autoloads nil t) 
    (require 'elpaca) 
    (elpaca-generate-autoloads "elpaca" repo) 
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


;; Declare jjba customizations
(defgroup jjba ()
  "JJBA customization group."
  :group 'tools
  )
(defcustom jjba-font-mono "Iosevka Comfy Wide" "My personal choice for monospaced font family." 
  :type 'string)

(defun jjba-set-base-faces () "Adjust the base Emacs faces to my preferences.
According to size, color and font family"
			 (set-face-attribute 'default nil 
													 :height (round (tkngt 114)) 
                           :font jjba-font-mono) 
			 (set-face-attribute 'mode-line nil 
													 :height (tkngt 0.7) 
													 :font jjba-font-mono) 
			 (set-face-attribute 'mode-line-active nil 
													 :height (tkngt 0.7) 
													 :font jjba-font-mono) 
			 (set-face-attribute 'mode-line-inactive nil 
													 :height (tkngt 0.7) 
													 :font jjba-font-mono))

;; Declare jjba packages

(use-package git-riddance 
  :ensure (:host github 
                 :repo "jjba23/git-riddance.el" 
                 :branch "trunk"))

(use-package tekengrootte 
  :ensure (:host github 
                 :repo "jjba23/tekengrootte.el" 
                 :branch "trunk") 
  :after (auto-dark) 
  :bind (("C-c f c" . tekengrootte-set-scale-colossal) 
         ("C-c f j" . tekengrootte-set-scale-jumbo) 
         ("C-c f x" . tekengrootte-set-scale-larger) 
         ("C-c f l" . tekengrootte-set-scale-large) 
         ("C-c f r" . tekengrootte-set-scale-regular) 
         ("C-c f s" . tekengrootte-set-scale-small) 
         ("C-c f t" . tekengrootte-set-scale-tiny)) 
  :hook ((tekengrootte-set-scale . (lambda () 
                                     (jjba-set-base-faces)))) 
  :config (jjba-set-base-faces))

(use-package ef-themes 
  :ensure t 
  :demand t)

(use-package auto-dark 
  :ensure t 
  :demand t 
  :init (setq auto-dark-polling-interval-seconds 4 auto-dark-allow-osascript nil auto-dark-allow-powershell nil) 
  :config (add-hook 'auto-dark-dark-mode-hook (lambda () 
                                                (load-theme 'ef-dream t))) 
  (add-hook 'auto-dark-light-mode-hook (lambda () 
                                         (load-theme 'ef-day t))) 
  (auto-dark-mode t))

(use-package vertico 
  :ensure t 
  :demand t 
  :init (setq vertico-cycle t vertico-resize t) 
  :config (vertico-mode))

(use-package marginalia 
  :ensure t 
  :demand t 
  :after (vertico) 
  :config (marginalia-mode))

(use-package nerd-icons 
  :ensure t 
  :demand t)

(use-package nerd-icons-completion 
  :ensure t 
  :demand t 
  :after (nerd-icons marginalia) 
  :hook ((marginalia-mode . nerd-icons-completion-marginalia-setup)) 
  :config (nerd-icons-completion-mode))


(use-package flymake-collection 
  :ensure t 
  :demand t 
  :hook ((after-init . flymake-collection-hook-setup) 
         (emacs-lisp-mode . flymake-mode)))

(use-package transient 
  :ensure t 
  :demand t)

(use-package magit 
  :ensure t 
  :demand t 
  :after (transient))

;; (use-package java-ts-mode
;;   :ensure t
;;   :demand t
;;   :config
;;   (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
;;   )

;; (use-package json-ts-mode
;;   :ensure t
;;   :demand t
;;   :config
;;   (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
;;   )

(use-package nix-ts-mode 
  :ensure t 
  :demand t)

(use-package markdown-mode 
  :ensure t 
  :demand t)

(use-package ripgrep 
  :ensure t 
  :demand t)

(use-package super-save 
  :ensure t 
  :demand t 
  :init (setq super-save-auto-save-when-idle t auto-save-default nil make-backup-files nil) 
  :config (super-save-mode +1) 
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-package spacious-padding 
  :ensure t 
  :demand t 
  :init (setq spacious-padding-widths '( :internal-border-width 18 
                                         :header-line-width 2 
                                         :mode-line-width 2 
                                         :tab-width 4 
                                         :right-divider-width 2 
                                         :scroll-bar-width 8 
                                         :left-fringe-width 16 
                                         :right-fringe-width 16)) 
  (setq spacious-padding-subtle-mode-line nil) 
  :config (spacious-padding-mode))

(use-package which-key 
  :ensure t 
  :demand t)

(use-package rainbow-mode 
  :ensure t 
  :demand t)

(use-package ob-nix 
  :ensure t 
  :demand t)

(use-package ob-http 
  :ensure t 
  :demand t)

(use-package ob-mermaid 
  :ensure t 
  :demand t)

(use-package orderless
  :ensure t
	:demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package bug-hunter 
  :ensure t 
  :demand t)

(use-package sly 
  :ensure t 
  :demand t 
  :config (setq inferior-lisp-program "sbcl"))

(use-package f 
  :ensure t 
  :demand t)

(use-package speed-type 
  :ensure t 
  :demand t)

(use-package dape 
  :ensure t 
  :demand t 
  :init (setq dape-buffer-window-arrangement 'gud))

(use-package dired-hacks-utils 
  :ensure t 
  :demand t)

(use-package dired-subtree 
  :ensure t 
  :demand t 
  :bind ( :map dired-mode-map (("<TAB>" . dired-subtree-toggle) 
                               ("C-<tab>" . dired-subtree-toggle) 
                               ("C-<TAB>" . dired-subtree-toggle))))

(use-package dired-open-with 
  :ensure t 
  :demand t)

(use-package move-text 
  :ensure t 
  :demand t 
  :config (move-text-default-bindings))

(use-package corfu
  :ensure t 
  :demand t
	:init
	(setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.2
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-echo-documentation 0.25
        corfu-preview-current 'insert
        corfu-preselect-first nil
        corfu-popupinfo-delay '(0.8 . 0.8))
  :config
	(global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode 1)
	)

(use-package nerd-icons-corfu
	:ensure t
	:demand t
	:after (corfu nerd-icons)
	:config
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
	)

(use-package haskell-mode
	:ensure t
	:demand t)

(use-package scala-mode
	:ensure t
	:demand t)

(use-package typescript-mode
	:ensure t
	:demand t)

(use-package smartparens
	:ensure t
	:demand t
	:hook ((prog-mode . smartparens-mode))
	:config
	(require 'smartparens-config)	
	)

(use-package consult 
  :ensure t 
  :demand t 
  :bind (("C-c h" . consult-history) 
         ("C-c m" . consult-mode-command) 
         ("C-c k" . consult-kmacro) 
         ("C-x M-:" . consult-complex-command) 
         ("C-x b" . consult-buffer) 
         ("C-x 4 b" . consult-buffer-other-window) 
         ("C-x 5 b" . consult-buffer-other-frame) 
         ("C-x r b" . consult-bookmark) 
         ("C-x p b" . consult-project-buffer) 
         ("M-#" . consult-register-load) 
         ("M-'" . consult-register-store) 
         ("C-M-#" . consult-register) 
         ("M-y" . consult-yank-pop) 
         ("M-g e" . consult-compile-error) 
         ("M-g f" . consult-flymake) 
         ("M-g g" . consult-goto-line) 
         ("M-g M-g" . consult-goto-line) 
         ("M-g o" . consult-outline) 
         ("M-g m" . consult-mark) 
         ("M-g k" . consult-global-mark) 
         ("M-g i" . consult-imenu) 
         ("M-g I" . consult-imenu-multi) 
         ("M-s d" . consult-find) 
         ("M-s D" . consult-locate) 
         ("M-s g" . consult-grep) 
         ("M-s G" . consult-git-grep) 
         ("M-s r" . consult-ripgrep) 
         ("M-s l" . consult-line) 
         ("M-s L" . consult-line-multi) 
         ("M-s k" . consult-keep-lines) 
         ("M-s u" . consult-focus-lines) 
         ("M-s e" . consult-isearch-history)) 
  :hook ((completion-list-mode . consult-preview-at-point-mode)) 
  :init (setq register-preview-delay 0.5 register-preview-function #'consult-register-format xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview 
              :override #'consult-register-window) 
  :config (consult-customize consult-theme 
                             :preview-key '(:debounce 0.2 any) 
                             consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file consult-xref consult--source-bookmark consult--source-file-register consult--source-recent-file consult--source-project-recent-file 
                             :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") 
  (global-set-key [f6] 'consult-recent-file)
  )

(use-package elisp-format 
  :ensure t 
  :demand t)

(use-package nerd-icons-dired 
  :ensure t 
  :demand t 
  :hook ((dired-mode . nerd-icons-dired-mode)))



(defun jjba-bookmark-emacs-config ()
  "Visit jjba bookmark: Emacs main init.el config file."
  (interactive) 
	(find-file "/home/joe/Ontwikkeling/Persoonlijk/dotfiles/users/joe/emacs/init.el"))

(defun new-frame-setup (frame)
  (if (display-graphic-p frame)
      (progn
				(message "window system")
				(tekengrootte-set-scale-regular)
				)
    (message "not a window system")))

;; Configure Emacs native features

(use-package emacs 
  :ensure nil 
  :bind (("C-x C-b" . ibuffer) 
         ("C-c a h" . highlight-compare-buffers) 
         ("C-c b e" . jjba-bookmark-emacs-config)) 
  :hook ((text-mode . visual-line-mode) 
         (dired-mode . (lambda () (dired-hide-details-mode 1))))
  (after-make-frame-functions . new-frame-setup)
  :config (setq-default user-personal-name "Joe"
												user-personal-full-name "Josep Jesus Bigorra Algaba"
												user-personal-email "jjbigorra@gmail.com"
												user-personal-initials "JJBA")
  
  (setq org-todo-keywords '((sequence "TODO" "WIP" "REVIEWING" "|" "DONE")))
  (setq-default line-spacing 2 pgtk-wait-for-event-timeout 0 electric-indent-inhibit t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq backward-delete-char-untabify-method 'hungry)
  
  (setq treesit-font-lock-level 4
				ring-bell-function #'ignore
				frame-resize-pixelwise t
				inhibit-startup-message t
        completion-cycle-threshold 3
				tab-always-indent 'complete
				text-mode-ispell-word-completion nil
        vc-follow-symlinks t
				delete-by-moving-to-trash t
				tab-width 2)
  
  (savehist-mode 1) 
  (tool-bar-mode -1) 
  (scroll-bar-mode -1) 
  (menu-bar-mode -1) 
  (delete-selection-mode +1)
  (set-frame-parameter nil 'alpha-background 90) 
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  (when (fboundp 'windmove-default-keybindings) 
    (windmove-default-keybindings))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq dired-listing-switches "-lAh --group-directories-first" dired-kill-when-opening-new-dired-buffer t)

  
  )



;;; init.el ends here

