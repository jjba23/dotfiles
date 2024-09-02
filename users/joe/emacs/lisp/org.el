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

;; (setq org-directory "~/Ontwikkeling/Persoonlijk/private-notes")
;; (setq org-agenda-files '("Tasks.org" "Birthdays.org" "Habits.org"))
;; (setq org-contacts-files '("Contacts.org"))

(require 'org)
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-adapt-indentation t)
(setq org-indent-indentation-per-level 2)
(setq org-log-done 'time)
(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)
(setq org-html-head-include-default-style nil)
(setq org-return-follows-link  t)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (awk . t)
   (calc .t)
   (C . t)
   (css . t)
   (emacs-lisp . t)
   (dot . t)
   (lisp . t)
   (java . t)
   (js . t)
   (haskell . t)
   (mermaid . t)
   (restclient . t)
   (perl . t)
   (python . t)
   (sass . t)
   (shell . t)
   (sql . t)
   (R . t)
   (nix . t)
   (groovy . t)
   (org . t)
   (sqlite . t)
   (makefile .t )
   )
 )

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Ontwikkeling/Persoonlijk/private-notes/Tasks.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Ontwikkeling/Persoonlijk/private-notes/Journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("c" "Contacts" entry (file "~/Ontwikkeling/Persoonlijk/private-notes/Contacts.org")
         "* %(org-contacts-template-name)
                :PROPERTIES:
                :EMAIL: %(org-contacts-template-email)
                :PHONE:
                :ALIAS:
                :NICKNAME:
                :IGNORE:
                :ICON:
                :NOTE:
                :ADDRESS:
                :BIRTHDAY:
                :END:")
        )
      )

(add-to-list
 'org-capture-templates
 `("wN" "Web link" entry (file+headline ,(car org-agenda-files) "Links to read later")
   "* TODO %?%a :readings: \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Fri\"))\n"
   :immediate-finish t :empty-lines 2))


(defun me-org-mode ()
  (variable-pitch-mode 1)
  (org-modern-mode)
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (org-toggle-inline-images)
  )
(add-hook 'org-mode-hook 'me-org-mode)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
(add-to-list 'org-structure-template-alist '("nix" . "src nix"))
