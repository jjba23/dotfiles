;; adding some tools to path is easy with Nix
(setq auth-sources '("~/.authinfo" "~/.netrc"))

;; (:eval (list (nyan-create)))


(defun joe/load-flavor (flavor)
  (interactive)
  (if (equal 'dark flavor)
    (message "loading dark palette")
    (setq joe/palette dark-palette)
    )
  (if (equal 'light flavor)
    (message "loading light palette")
    (setq joe/palette light-palette)
    )
  )

(defun joe/get-color (color)
  (alist-get color joe/palette "#FFFFFF")
)
