(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


;; Declare Joe's customizations

(defcustom joe-font-mono "Iosevka Comfy Wide"
  ""
  :type 'string)

;; Declare Joe's packages



(use-package git-riddance
  :ensure (:host github :repo "jjba23/git-riddance.el" :branch "trunk"))

(use-package tekengrootte
  :ensure (:host github :repo "jjba23/tekengrootte.el" :branch "trunk")
  :config
  (defun gezichten-apply ()
    ;; (set-face-attribute 'default
    ;; 			:height (round (tkngt 114))
    ;; 			:font joe-font-mono
    ;; 			)
    )
  (gezichten-apply)
  )  

(use-package ef-themes
  :ensure t
  :demand t)

(use-package auto-dark
  :ensure t
  :demand t
  :init
  (setq auto-dark-polling-interval-seconds 4)
  (setq auto-dark-allow-osascript nil)
  (setq auto-dark-allow-powershell nil)
  :config
  (add-hook 'auto-dark-dark-mode-hook
            (lambda ()
              (load-theme 'ef-autumn t)
              ))
  (add-hook 'auto-dark-light-mode-hook
            (lambda ()
	      (load-theme 'ef-day t)
              ))
  (auto-dark-mode t)
  )

(use-package vertico
  :ensure t
  :demand t
  :init
  (setq vertico-cycle t
	vertico-resize t)
  :config
  (vertico-mode)
  )

(use-package marginalia
  :ensure t
  :demand t
  :after (vertico)
  :config
  (marginalia-mode)
  )

(use-package nerd-icons
  :ensure t
  :demand t)

(use-package magit
  :ensure t
  :demand t)

(use-package which-key
  :ensure t
  :demand t)

(use-package rainbow-mode
  :ensure t
  :demand t)

(use-package org-tempo
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

(use-package bug-hunter
  :ensure t
  :demand t)

(use-package sly
  :ensure t
  :demand t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package eshell-prompt-extras
  :ensure t
  :demand t
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (set-face-attribute 'epe-pipeline-delimiter-face nil :foreground "#7f849c")
  (set-face-attribute 'epe-pipeline-user-face nil :foreground "#cba6f7")
  (set-face-attribute 'epe-pipeline-host-face nil :foreground "#cba6f7")
  (set-face-attribute 'epe-pipeline-time-face nil :foreground "#7f849c")    	
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-pipeline)
  )

(use-package f :ensure t :demand t)

(use-package speed-type :ensure t :demand t)

(use-package dape
  :ensure t
  :demand t
  :init
  (setq dape-buffer-window-arrangement 'gud)
  )

(use-package dired-hacks-utils
  :ensure t
  :demand t)

(use-package dired-subtree
  :ensure t
  :demand t
  :bind ( :map dired-mode-map (
			       ("<TAB>" . dired-subtree-toggle)
			       ("C-<tab>" . dired-subtree-toggle)
			       ("C-<TAB>" . dired-subtree-toggle)
			       ) ) 
  )

(use-package dired-open-with :ensure t :demand t)

(use-package nerd-icons-dired
  :ensure t
  :demand t
  :hook ((dired-mode . nerd-icons-dired-mode))
  )

;; Configure Emacs native features

(use-package emacs
  :ensure nil
  :config
  (setq ring-bell-function #'ignore)
  )
