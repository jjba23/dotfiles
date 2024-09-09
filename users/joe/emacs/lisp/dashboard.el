(require 'dashboard)
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)
(setq dashboard-items '(
			(projects  . 8)
                        (bookmarks . 8)                        
			(recents   . 10)
                        ;;(agenda    . 5)
                        ;;(registers . 5)
			))
(setq dashboard-navigation-cycle t)
;; (setq dashboard-startup-banner 'logo)
(setq dashboard-startup-banner '("/home/joe/Ontwikkeling/Persoonlijk/dotfiles/resources/images/emacs-dashboard-small.png" . "/home/joe/Ontwikkeling/Persoonlijk/dotfiles/resources/images/emacs-dashboard-text.txt"))
(setq dashboard-startupify-list '(
                                  (dashboard-append "λ Welcome to the Church of Emacs λ")
                                  dashboard-insert-newline
				  dashboard-insert-footer
				  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  ))

(dashboard-setup-startup-hook)

