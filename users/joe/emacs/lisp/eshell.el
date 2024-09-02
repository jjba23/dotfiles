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

(require 'eshell)

(setq my-visual-commands '("ssh" "tail" "top" "btop" "htop" "neofetch" "gfetch" "onefetch"))

(let ((count 1))
  (defun make-eshell-next-number ()
    (interactive)
    (eshell)
    (rename-buffer (concat "*eshell" (number-to-string count) "*"))
    (setq count (1+ count))))

(defun eshell/web (&rest args)
  ""
  (insert "GDK_BACKEND=x11 nyxt")
  (eshell-send-input)
  )
(defun eshell/nyxt (&rest args)
  ""
  (insert "GDK_BACKEND=x11 nyxt")
  (eshell-send-input)
  )

(defun eshell/aboutnix (&rest args)
  ""
  (insert "nix-info -m")
  (eshell-send-input)
  )

(defun eshell/fetch (&rest args)
  ""
  (insert "fastfetch")
  (eshell-send-input)
  )

(defun eshell/neofetch (&rest args)
  ""
  (insert "fastfetch")
  (eshell-send-input)
  )

(defun eshell/gfetch (&rest args)
  ""
  (insert "onefetch")
  (eshell-send-input)
  )

(defun eshell/scala-validate (&rest args)
  ""
  (insert "sbt \"scalafixAll; scalafmt; test;\"")
  (eshell-send-input)
  )

(defun eshell/scala-fmt (&rest args)
  ""
  (insert "sbt \"scalafixAll; scalafmt;\"")
  (eshell-send-input)
  )

(defun eshell/color-picker (&rest args)
  ""
  (insert "hyprpicker -a -n -f hex")
  (eshell-send-input)
  )


(eval-after-load "eshell"
  '(progn
     
     (setenv "PAGER" "cat")

     (setq
      eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions t)

     (add-hook 'eshell-mode-hook
               (lambda ()
		 (dolist (x my-visual-commands)
                   (add-to-list 'eshell-visual-commands x))
		 ))



     (setq my/eshell-aliases
	   '((g  . magit)
             (gco  . magit-clone)
	     (gl . magit-log)
	     (d  . dired)
             (e  . find-file)
             (vi  . find-file)
	     (o  . find-file)	
	     (oo . find-file-other-window)
	     (l  . find-file)
             (ll  . find-file)
	     (eshell/clear . eshell/clear-scrollback)))

     (mapc (lambda (alias)
	     (defalias (car alias) (cdr alias)))
	   my/eshell-aliases)
     )
  )


