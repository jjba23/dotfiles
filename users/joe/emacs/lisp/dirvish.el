(require 'dirvish)
;; (setq dirvish-attributes
;;     '(vc-state subtree-state collapse git-msg file-time file-size))
;; (setq dirvish-attributes
;;       '(subtree-state collapse file-time file-size))
;; (setq dirvish-attributes
;;       '(nerd-icons collapse subtree-state file-time file-size))
(setq dirvish-attributes
      '(nerd-icons collapse subtree-state))
(setq dired-mouse-drag-files t)                   
(setq mouse-drag-and-drop-region-cross-program t) 

(define-key dirvish-mode-map (kbd "TAB") 'dirvish-toggle-subtree)
(define-key dirvish-mode-map (kbd "C-TAB") 'dirvish-toggle-subtree)
(define-key dirvish-mode-map (kbd "C-<tab>") 'dirvish-toggle-subtree)
(dirvish-override-dired-mode)
