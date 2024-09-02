;; (catppuccin-reload)
;; (load-theme 'catppuccin :no-confirm)    
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)                
          	(with-selected-frame frame
          	  (joe/set-faces)
          	  )))

  (joe/set-faces)
  )
