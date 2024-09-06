(in-package #:nyxt-user)


(define-panel-command hsplit-internal (&key (url (quri:render-uri (url (current-buffer)))))
  (panel "*Duplicate panel*" :right)
  "Duplicate the current buffer URL in the panel buffer on the right. A poor man's hsplit :)"
  (setf (ffi-width panel) 650)
  (run-thread "URL loader"
              (sleep 0.3)
              (buffer-load (quri:uri url) :buffer panel))
  "")

(define-command-global close-all-panels ()
  "Close all the panel buffers there are."
  (alexandria:when-let ((panels (nyxt/renderer/gtk::panel-buffers-right (current-window))))
    (delete-panel-buffer :window (current-window) :panels panels))
  (alexandria:when-let ((panels (nyxt/renderer/gtk::panel-buffers-left (current-window))))
    (delete-panel-buffer :window (current-window) :panels panels)))

(define-command-global hsplit ()
  "Based on `hsplit-internal' above."
  (if (nyxt/renderer/gtk::panel-buffers-right (current-window))
      (delete-all-panel-buffers (current-window))
      (hsplit-internal)))

;; set cookies to accept all
(defmethod customize-instance ((browser browser) &key)
  (setf (slot-value browser 'default-cookie-policy) :accept))

(defvar *my-search-engines*
  (list
   '("w" "https://en.wikipedia.org/w/index.php?search=~a" "https://en.wikipedia.org/")
   '("h" "https://hoogle.haskell.org/?hoogle=~a" "https://hoogle.haskell.org/")
   '("g" "https://google.com/search?q=~a" "https://google.com")
   )
  "List of search engines.")

;; set my preferred search engines
(define-configuration buffer
  "Go through the search engines above and make-search-engine out of them."
  ((search-engines
    (append
     (mapcar (lambda (engine) (apply 'make-search-engine engine))
             *my-search-engines*)
     ))))

;; ad blocking
(define-configuration web-buffer
    ((default-modes
      (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%)
      )))

;; Emacs keybindings
(define-configuration buffer
    ((default-modes
      (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

;; Nyxt browser with Catppuccin Mocha theme
(defvar *my-dark-theme*
  (make-instance 'theme:theme
                           :font-family "Inter"
                           :monospace-font-family "Mononoki"
                           :dark-p t
                           :background-color- "rgb(49, 50, 68)"
                           :background-color "rgb(36, 39, 58)"
                           :background-color+ "rgb(17, 17, 27)"
                           :on-background-color "rgb(205, 214, 244)"
                           :primary-color- "rgb(198, 161, 242)"
                           :primary-color "rgb(203, 166, 247)"
                           :primary-color+ "rgb(208, 171, 252)"
                           :on-primarycolor "rgb(36, 39, 58)"
                           :secondary-color- "rgb(203, 165, 251)"
                           :secondary-color "rgb(198, 160, 246)"
                           :secondary-color+ "rgb(193, 155, 241)"
                           :on-secondary-color "rgb(36, 39, 58)"
                           :action-color- "#763df2"
                           :action-color "#571fd2"
                           :action-color+ "#481fa2"
                           :highlight-color- "rgb(198, 161, 242)"
                           :highlight-color "rgb(203, 166, 247)"
                           :highlight-color+ "rgb(208, 171, 252)"
                           :success-color- "#05f4cd"
                           :success-color "#4cfbcf"
                           :success-color+ "#87fcdf"
                           :warning-color- "#fca904"
                           :warning-color "#fcba04"
                           :warning-color+ "#ffd152"
                           :codeblock-color- "#44355a"
                           :codeblock-color "#2e243d"
                           :codeblock-color+ "#221a2d"
                           :text-color- "rgb(186, 194, 222)"
                           :text-color "rgb(205, 214, 244)"
                           :text-color+ "rgb(255, 255, 255)"
                           :contrast-text-color "#0c0c0d"))

(define-configuration browser
    ((theme *my-dark-theme*))
  )


;; override and define some custom keybindings, also for Emacs mode
(define-configuration :document-mode
  "Add basic keybindings."
  ((keyscheme-map
    (keymaps:define-keyscheme-map
        "custom" (list :import %slot-value%)
      nyxt/keyscheme:emacs
      (list                       
       "M-s l" :search-buffer
       "C-x 3" 'hsplit
       "C-x 0" 'close-all-panels                      
       )))))

(define-configuration :status-buffer
  "Display modes as short glyphs."
  ((glyph-mode-presentation-p t)))

(define-configuration :force-https-mode ((glyph "ϕ")))
(define-configuration :user-script-mode ((glyph "u")))
(define-configuration :blocker-mode ((glyph "β")))
(define-configuration :proxy-mode ((glyph "π")))
(define-configuration :reduce-tracking-mode ((glyph "τ")))
(define-configuration :certificate-exception-mode ((glyph "χ")))
(define-configuration :style-mode ((glyph "ϕ")))
(define-configuration :cruise-control-mode ((glyph "σ")))

;; This automatically darkens WebKit-native interfaces and sends the
;; "prefers-color-scheme: dark" to all the supporting websites.
(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")


