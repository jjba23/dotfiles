;; Joe's dotfiles
;; Copyright (C) 2023  Josep Jesus Bigorra Algaba (jjbigorra@gmail.com)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(setq-default user-personal-name "Joe")
(setq-default user-personal-full-name "Josep Jesus Bigorra Algaba")
(setq-default user-personal-email "jjbigorra@gmail.com")
(setq-default user-personal-initials "JJBA")

(setq org-todo-keywords
      '((sequence "TODO" "WIP" "REVIEWING" "|" "DONE")))

(defun dired-joe-show-hidden (value)
  (interactive "p")
  (setq dired-listing-switches "-lAh --group-directories-first")
  )
(defun dired-joe-hide-hidden (value)
  (interactive "p")
  (setq dired-listing-switches "-lh --group-directories-first")
  )

(savehist-mode 1)
(set-frame-parameter nil 'alpha-background 92)
(add-to-list 'default-frame-alist '(alpha-background . 92))
(setq frame-resize-pixelwise t)
(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))
(setq backward-delete-char-untabify-method 'hungry)
(setq-default electric-indent-inhibit t)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks t)
(delete-selection-mode +1)
(setq completion-cycle-threshold 3)

(setq tab-always-indent 'complete)
(setq text-mode-ispell-word-completion nil)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(add-hook 'text-mode-hook 'visual-line-mode)
(setq dired-listing-switches "-lAh --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)
(setq delete-by-moving-to-trash t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c a h") 'highlight-compare-buffers)
(setq reb-re-syntax 'string)


(defun me-md-mode ()
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  )
(add-hook 'markdown-mode-hook 'me-md-mode)


(setq tab-width 4)

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(setq font-scale 1.1)
(defun mk-font-size (x)
  "Adapt a font size to a scaled Emacs compatible one"
  (interactive)
  (* font-scale x)
  )

(defun set-font-scale-jumbo ()
  "Set font size to jumbo globally"
  (interactive)
  (setq font-scale 1.3)
  (joe/set-faces)
  )

(defun set-font-scale-larger ()
  "Set font size to larger globally"
  (interactive)
  (setq font-scale 1.2)
  (joe/set-faces)
  )

(defun set-font-scale-large ()
  "Set font size to large globally"
  (interactive)
  (setq font-scale 1.1)
  (joe/set-faces)
  )

(defun set-font-scale-regular ()
  "Set font size to regular globally"
  (interactive)
  (setq font-scale 0.9)
  (joe/set-faces)
  )

(defun set-font-scale-small ()
  "Set font size to small globally"
  (interactive)
  (setq font-scale 0.8)
  (joe/set-faces)
  )

(defun set-font-scale-tiny ()
  "Set font size to tiny globally"
  (interactive)
  (setq font-scale 0.7)
  (joe/set-faces)
  )


;; Set font to regular at startup
(set-font-scale-small)

(setq-default line-spacing 2)

(setq-default pgtk-wait-for-event-timeout 0)

;; (setq treesit-font-lock-level 3)
(setq treesit-font-lock-level 4)

(defun org-format-outline-path (path &optional width prefix separator)
  "Format the outline path PATH for display.
WIDTH is the maximum number of characters that is available.
PREFIX is a prefix to be included in the returned string,
such as the file name.
SEPARATOR is inserted between the different parts of the path,
the default is \"/\"."
  (setq width (or width 79))
  (setq path (delq nil path))
  (unless (> width 0)
    (user-error "Argument `width' must be positive"))
  (setq separator (or separator "/"))
  (let* ((org-odd-levels-only nil)
	 (fpath (concat
		 prefix (and prefix path separator)
		 (mapconcat
		  (lambda (s) (replace-regexp-in-string "[ \t]+\\'" "" s))
		  (cl-loop for head in path
			   for n from 0
			   for face = (nth (% n org-n-level-faces) org-level-faces)
			   collect (org-add-props
				       head nil 'face
				       `(:foreground ,(face-foreground face nil t) :weight bold)))
		  separator))))
    (when (> (length fpath) width)
      (if (< width 7)
	  ;; It's unlikely that `width' will be this small, but don't
	  ;; waste characters by adding ".." if it is.
	  (setq fpath (substring fpath 0 width))
	(setf (substring fpath (- width 2)) "..")))
    fpath))


(setq-default indent-tabs-mode nil)

(global-prettify-symbols-mode +1)

(defun slurp-file (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun add-browser-bookmark ()
  "Add a bookmark to my Nyxt browser bookmarks in Lisp"
  (interactive)
  (let* (
         (bookmark-file-path "~/Ontwikkeling/Persoonlijk/dotfiles/users/joe/nyxt/lisp/bookmarks.lisp")
         (bookmark-content (read (slurp-file bookmark-file-path)))
         (url (read-string "Enter a URL you wish to bookmark:"))
         (title (read-string "Enter a title for the bookmark:"))
         (tags (read-string "Enter some tags for the bookmark ( delimited by , ):"))
         (new-bookmark (list
                         :url url
                         :title title
                         :tags (split-string tags ",")))
         )
    
    (push new-bookmark bookmark-content)
    (setq-local new-file-contents (pp-to-string bookmark-content))
    (f-write-text new-file-contents 'utf-8 bookmark-file-path)
    ))

(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(setq dired-guess-shell-alist-user '(("\\.pdf\\'" "evince")
                                   ("\\.doc\\'" "libreoffice")
                                   ("\\.docx\\'" "libreoffice")
                                   ("\\.ppt\\'" "libreoffice")
                                   ("\\.pptx\\'" "libreoffice")
                                   ("\\.xls\\'" "libreoffice")
                                   ("\\.xlsx\\'" "libreoffice")
                                   ("\\.webp\\'" "ristretto")
                                   ("\\.jpg\\'" "ristretto")
                                   ("\\.png\\'" "ristretto")
                                   ("\\.java\\'" "idea")))




(defun git-riddance ()
  "Completely rewrite Git history of the specified directory with a new commit and branch"
  (interactive)
  (let* (
         (dir (read-directory-name "[git-riddance] Directory of target repo:" "." ))
         (default-directory dir)
         (remote (read-string "[git-riddance] Origin to force push to:"))
         (remote-name (read-string "[git-riddance] Name of origin to force push to:" "github"))
         (branch (read-string "[git-riddance] Name of the new branch:"  "trunk"))
         (commit-message (read-string "[git-riddance] New commit message:"  "Proprietary software is an injustice!"))
         )
        ;;Checkout/create orphan branch (this branch won't show in git branch command):        
        (shell-command "git checkout --orphan latest_branch")
        ;;Add all the files to the newly created branch:
        (shell-command "git add -A")
        ;;Commit the changes:
        (shell-command (format "git commit -am \"%s\"" commit-message))
        ;;Delete main (default) branch (this step is permanent):
        (shell-command (format "git branch -D %s || true" branch))
        ;;Rename the current branch to main:
        (shell-command (format "git branch -m %s" branch))
        ;;Finally, all changes are completed on your local repository,
        ;; and force update your remote repository:
        (shell-command (format "git remote add %s %s" remote-name remote))
        (shell-command (format "git push -uf %s %s" remote-name branch))
    )
  )

(defun nixos-rebuild ()
  "Rebuild NixOS Joe's dotfiles"
  (interactive)
  (let ((default-directory "~/Ontwikkeling/Persoonlijk/dotfiles"))
    (async-shell-command "nix develop -c cabal run dotfiles -- rebuild-system")
    ))


(defun nixos-restart-emacs ()
  "Restart the Emacs session and server."
  (interactive)
  (async-shell-command "systemctl --user restart emacs")
  )

(setq pop-up-windows nil)

(defun nixos-emacs-config-file-joe ()
  "Visit the Emacs `main` configuration file in Emacs, where package configurations reside"
  (interactive)
  (find-file "~/Ontwikkeling/Persoonlijk/dotfiles/users/joe/emacs/packages.nix")
  )

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
