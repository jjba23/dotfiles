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

(require 'mu4e)
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-get-mail-command "true")
(setq mu4e-update-interval nil)
(setq mu4e-maildir "~/Mail/jjbigorra@gmail.com/")
(setq mu4e-headers-result-limit 1000)    

(setq mu4e-drafts-folder "/Concepten")
(setq mu4e-refile-folder "/Archive")
(setq mu4e-sent-folder   "/Archive")
(setq mu4e-trash-folder  "/Prullenbak")

(setq mu4e-maildir-shortcuts
      '(
	(:maildir "/Inbox"    :key ?i)
        (:maildir "/Archive" :key ?s)
        (:maildir "/Prullenbak"     :key ?t)
        (:maildir "/Concepten"    :key ?d)
        (:maildir "/Archive"  :key ?a)
	)
      )
