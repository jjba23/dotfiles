;; adding some tools to path is easy with Nix
(setq auth-sources '("~/.authinfo" "~/.netrc"))

;; (:eval (list (nyan-create)))


(defun joe/load-flavor (flavor)
  (interactive)
  (message (format "loading %s Emacs flavor" flavor))
  (if (equal 'dark flavor)
    (setq joe/palette dark-palette)
    )
  (if (equal 'light flavor)
    (setq joe/palette light-palette)
    )
  )

(defun joe/get-color (color)
  (alist-get color joe/palette "#FFFFFF")
)
