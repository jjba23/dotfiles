(add-hook 'server-after-make-frame-hook (lambda () (joe/set-faces))
          
          
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)                
;;           	(with-selected-frame frame
;;           	  (joe/set-faces)
;;           	  )))

;;   (joe/set-faces)
;;   )
