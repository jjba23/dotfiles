# Joe's dotfiles
# Copyright (C) 2023  Josep Jesus Bigorra Algaba (jjbigorra@gmail.com)

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

{ lib, pkgs }:
let
  bindings = import ./bindings.nix;
  mkHook = { mode, lambda }: "(${mode} . ${lambda})";
in {
  nerd-icons = { };
  nerd-icons-completion = {
    after = [ "nerd-icons" "marginalia" ];
    hook = [
      (mkHook {
        mode = "marginalia-mode";
        lambda = "nerd-icons-completion-marginalia-setup";
      })
    ];
    config = ''
      (nerd-icons-completion-mode)
    '';
  };

  dockerfile-ts-mode = { mode = [ ''"Dockerfile\\'"'' ]; };

  emacsql-sqlite3 = {
    defer = lib.mkDefault true;
    config = ''
      (setq emacsql-sqlite3-executable "${pkgs.sqlite}/bin/sqlite3")
    '';
  };

  flymake-collection = {
    hook = [
      (mkHook {
        mode = "after-init";
        lambda = "flymake-collection-hook-setup";
      })
      (mkHook {
        mode = "scss-mode";
        lambda = "flymake-stylelint-enable";
      })
      (mkHook {
        mode = "css-mode";
        lambda = "flymake-stylelint-enable";
      })
    ];
    init = ''
      ${builtins.readFile ./lisp/flymake-stylelint.el}
    '';
  };

  java-ts-mode = {
    mode = [ ''"\\.java\\'"'' ];
    init = ''
      (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
    '';
  };

  json-ts-mode = {
    mode = [ ''"\\.json\\'"'' ];
    config = ''
      (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
    '';
  };

  markdown-mode = {
    mode = [ ''"\\.mdwn\\'"'' ''"\\.markdown\\'"'' ''"\\.md\\'"'' ];
  };

  nix-ts-mode = { mode = [ ''"\\.nix\\'"'' ]; };

  magit = { };

  forge = {
    after = [ "magit" ];
    config = ''
      (add-to-list 'forge-alist '("git.internal.zorgdomein.nl" "git.internal.zorgdomein.nl/api/v4" "git.internal.zorgdomein.nl" forge-gitlab-repository))
    '';
  };

  org-roam = {
    defines = [ "org-roam-graph-executable" ];
    init = ''
      (setq org-roam-directory (file-truename "~/Ontwikkeling/Persoonlijk/private-notes/Roam")
            org-roam-v2-ack t
            org-roam-graph-executable "${pkgs.graphviz}/bin/dot"
            org-roam-node-display-template (concat "$\{title:*} " (propertize "$\{tags:10}" 'face 'org-tag)))          
    '';
    config = ''
      (org-roam-db-autosync-mode)
      (org-roam-setup)        
    '';
    bind = bindings.org-roam;
  };

  pandoc-mode = {
    config = ''
      (setq pandoc-binary "${pkgs.pandoc}/bin/pandoc")
    '';
  };

  protobuf-mode = { mode = [ ''"\\.proto\\'"'' ]; };

  ripgrep = {
    init = ''
      (setq ripgrep-executable "${pkgs.ripgrep}/bin/rg")
    '';
  };

  terraform-mode = { mode = [ ''"\\.tf\\(vars\\)?\\'"'' ]; };

  toml-ts-mode = {
    mode = [ ''"\\.toml\\'"'' ];
    config = ''
      (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))
    '';
  };

  yaml-ts-mode = {
    mode = [ ''"\\.\\(e?ya?\\|ra\\)ml\\'"'' ];
    config = ''
      (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
    '';
  };

  which-key = { config = builtins.readFile ./lisp/which-key.el; };

  crux = {
    config = ''
      (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
      (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
      (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
    '';
  };

  easy-kill = {
    config = ''
      (global-set-key [remap kill-ring-save] 'easy-kill)
    '';
  };

  guru-mode = {
    config = ''
      (setq guru-warn-only t)
      (guru-global-mode +1)
    '';
  };

  super-save = {
    init = ''
      (setq super-save-auto-save-when-idle t
            auto-save-default nil
            make-backup-files nil)
    '';
    config = ''
      (super-save-mode +1)
      (add-to-list 'super-save-hook-triggers 'find-file-hook)
    '';
  };

  undo-tree = { };

  expand-region = { };

  move-text = {
    config = ''
      (move-text-default-bindings)
    '';
  };

  vertico = {
    init = ''
      (setq vertico-cycle t
            vertico-resize t)          
    '';
    config = ''
      (vertico-mode)
    '';
  };

  nerd-icons-ibuffer = {
    hook = [ "(ibuffer-mode . nerd-icons-ibuffer-mode)" ];
    init = ''
      (setq nerd-icons-ibuffer-icon t
            nerd-icons-ibuffer-color-icon t
            nerd-icons-ibuffer-icon-size 1.0
            nerd-icons-ibuffer-human-readable-size t
            inhibit-compacting-font-caches t)
    '';
  };

  marginalia = {
    after = [ "vertico" ];
    config = ''
      (marginalia-mode)
    '';
    bind = { "M-A" = "marginalia-cycle"; };
  };

  string-inflection = { };

  s = { };

  dash = { };

  origami = { bind = bindings.origami; };

  rg = {
    config = ''
      (rg-enable-menu)
    '';
  };

  emojify = {
    init = ''
      (setq emojify-display-style 'unicode
            emojify-emoji-styles '(unicode))
    '';
    hook = [
      (mkHook {
        mode = "after-init";
        lambda = "global-emojify-mode";
      })
    ];
    bind = { "C-c w" = "emojify-insert-emoji"; };
  };

  emojify-logos = { };

  treesit-auto = {
    config = ''
      (global-treesit-auto-mode)
    '';
  };

  helpful = { bind = bindings.helpful; };

  orderless = {
    init = ''
      (setq completion-styles '(orderless basic)
            completion-category-defaults nil
            completion-category-overrides '((file (styles partial-completion))))
    '';
  };

  consult = {
    hook = [
      (mkHook {
        mode = "completion-list-mode";
        lambda = "consult-preview-at-point-mode";
      })
    ];
    bind = bindings.consult;
    init = ''
      (setq register-preview-delay 0.5
            register-preview-function #'consult-register-format
            xref-show-xrefs-function #'consult-xref
            xref-show-definitions-function #'consult-xref)

      (advice-add #'register-preview :override #'consult-register-window)
    '';
    config = ''
      (consult-customize
        consult-theme :preview-key '(:debounce 0.2 any)
        consult-ripgrep consult-git-grep consult-grep
        consult-bookmark consult-recent-file consult-xref
        consult--source-bookmark consult--source-file-register
        consult--source-recent-file consult--source-project-recent-file
        :preview-key '(:debounce 0.4 any))

      (setq consult-narrow-key "<")
      (global-set-key [f6] 'consult-recent-file)
    '';
  };
  nerd-icons-corfu = {
    after = [ "corfu" "nerd-icons" ];
    config = ''
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
    '';
  };
  corfu = {
    init = ''
      (setq corfu-cycle t
            corfu-auto t
            corfu-auto-prefix 2
            corfu-auto-delay 0.0
            corfu-quit-at-boundary 'separator
            corfu-quit-no-match 'separator
            corfu-echo-documentation 0.25
            corfu-preview-current 'insert
            corfu-preselect-first nil
            corfu-popupinfo-delay '(0.5 . 0.5))
    '';
    config = ''
      (global-corfu-mode)
      (corfu-history-mode)
      (corfu-popupinfo-mode 1)
    '';
  };

  eglot = {
    hook = [
      (mkHook {
        mode = "scala-ts-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "sh-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "python-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "java-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "yaml-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "haskell-mode";
        lambda = "eglot-ensure";
      })
      # (mkHook {
      #   mode = "go-mode";
      #   lambda = "eglot-ensure";
      # })
      (mkHook {
        mode = "dockerfile-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "nix-ts-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "typescript-ts-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "markdown-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "scss-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "css-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "css-ts-mode";
        lambda = "eglot-ensure";
      })
      (mkHook {
        mode = "python-ts-mode";
        lambda = "eglot-ensure";
      })
      # (mkHook {
      #   mode = "dart-mode";
      #   lambda = "eglot-ensure";
      # })
    ];
    config = builtins.readFile ./lisp/eglot.el;
    bind = bindings.eglot;
  };

  haskell-mode = { };

  scala-ts-mode = { };

  groovy-mode = { };

  page-break-lines = {
    config = ''
      (global-page-break-lines-mode)
    '';
  };

  typescript-mode = { after = [ "tree-siter" ]; };

  smartparens = {
    config = ''
      (require 'smartparens-config)
      (add-hook 'prog-mode-hook #'smartparens-mode)        
    '';
  };

  dabbrev = {
    config = ''
      (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
      (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
      (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
    '';
    bind = bindings.dabbrev;
  };

  pdf-tools = {
    defer = true;
    config = ''
      (pdf-tools-install)
      (setq-default pdf-view-display-size 'fit-page)
    '';
  };

  cape = {
    init = ''
      (add-to-list 'completion-at-point-functions #'cape-dabbrev)
      (add-to-list 'completion-at-point-functions #'cape-file)
    '';
    bind = bindings.cape;
  };

  org = {
    config = builtins.readFile ./lisp/org.el;
    bind = bindings.org;
  };

  org-modern = {
    hook = [ "(org-agenda-finalize . org-modern-agenda)" ];
    config = ''
      (setq org-modern-star 'replace)
    '';
  };

  org-contrib = { };

  org-present = { };

  org-auto-tangle = {
    hook = [
      (mkHook {
        mode = "org-mode";
        lambda = "org-auto-tangle-mode";
      })
    ];
    after = [ "org" "org-contrib" ];
  };

  org-roam-ui = {
    after = [ "org-roam" ];
    config = ''
      (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
    '';
  };

  direnv = { bind = bindings.direnv; };
  dirvish = { config = builtins.readFile ./lisp/dirvish.el; };
  goto-line-preview = {
    config = ''
      (global-set-key [remap goto-line] 'goto-line-preview)
    '';
  };

  org-contacts = { };
  simple-modeline = {
    hook = [
      (mkHook {
        mode = "after-init";
        lambda = "simple-modeline-mode";
      })
    ];
  };

  emacs = {
    after = [ "markdown-mode" "org" ];
    config = ''
      ${builtins.readFile ./lisp/emacs.el}
    '';
    bind = lib.mkMerge [ bindings.joe bindings.flymake ];
  };

  fancy-compilation = {
    init = ''
      (setq fancy-compilation-override-colors nil)
    '';
    config = ''
      (with-eval-after-load 'compile
        (fancy-compilation-mode))
    '';
  };

  pomodoro = {
    config = ''
      (pomodoro-add-to-mode-line)
    '';
    bind = bindings.pomodoro;
  };
  dashboard = {
    init = ''
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
      (setq dashboard-startup-banner '(
          "/home/joe/Ontwikkeling/Persoonlijk/dotfiles/resources/images/emacs-dashboard-small.png" . "/home/joe/Ontwikkeling/Persoonlijk/dotfiles/resources/texts/emacs-dashboard.txt")
          )
      (setq dashboard-startupify-list '(
                                        dashboard-insert-newline
                                        dashboard-insert-banner
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



    '';
    config = ''
      (dashboard-setup-startup-hook)
    '';
  };
  mu4e = {
    defer = 2;
    bind = bindings.mu4e;
    config = builtins.readFile ./lisp/mu4e.el;
  };
  rainbow-mode = { };
  org-tempo = { };
  ob-mermaid = { };
  mermaid-mode = {
    config = ''
      (setq mermaid-mode-map
        (let ((map mermaid-mode-map))
          (define-key map (kbd "C-c C-c") nil)
          (define-key map (kbd "C-c C-f") nil)
          (define-key map (kbd "C-c C-b") nil)
          (define-key map (kbd "C-c C-r") nil)
          (define-key map (kbd "C-c C-o") nil)
          (define-key map (kbd "C-c C-d") nil)
          (define-key map (kbd "C-c C-d c") 'mermaid-compile)
          (define-key map (kbd "C-c C-d c") 'mermaid-compile)
          (define-key map (kbd "C-c C-d f") 'mermaid-compile-file)
          (define-key map (kbd "C-c C-d b") 'mermaid-compile-buffer)
          (define-key map (kbd "C-c C-d r") 'mermaid-compile-region)
          (define-key map (kbd "C-c C-d o") 'mermaid-open-browser)
          (define-key map (kbd "C-c C-d d") 'mermaid-open-doc)
          map))
    '';
  };
  vterm = { config = builtins.readFile ./lisp/vterm.el; };
  ob-nix = { };
  ob-http = { };
  spacious-padding = {
    hook = [
      (mkHook {
        mode = "after-init";
        lambda = "spacious-padding-mode";
      })
    ];
    bind = bindings.spacious-padding;
    config = builtins.readFile ./lisp/spacious-padding.el;
  };
  dart-mode = {
    after = [ "eglot" ];
    config = ''
      (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
    '';
  };
  flutter = { after = [ "eglot" "dart-mode" ]; };
  eshell = { config = builtins.readFile ./lisp/eshell.el; };
  transmission = { };
  bug-hunter = { };
  ox-ioslide = { };
  sly = {
    config = ''
      (setq inferior-lisp-program "sbcl")
    '';
  };
  f = { };
  eshell-prompt-extras = {
    config = ''
      (autoload 'epe-theme-lambda "eshell-prompt-extras")
      (set-face-attribute 'epe-pipeline-delimiter-face nil :foreground "#7f849c")
      (set-face-attribute 'epe-pipeline-user-face nil :foreground "#cba6f7")
      (set-face-attribute 'epe-pipeline-host-face nil :foreground "#cba6f7")
      (set-face-attribute 'epe-pipeline-time-face nil :foreground "#7f849c")    	
      (setq eshell-highlight-prompt nil
           eshell-prompt-function 'epe-theme-pipeline)
    '';
  };
  speed-type = { };
  auto-dark = {
    after = [ "org" "fancy-compilation" "dashboard" ];
    init = ''
      (setq auto-dark-dark-theme 'modus-vivendi)
      (setq auto-dark-light-theme 'modus-operandi)
      (setq auto-dark-polling-interval-seconds 4)
      (setq auto-dark-allow-osascript nil)
      (setq auto-dark-allow-powershell nil)

    '';
    config = ''
      (add-hook 'auto-dark-dark-mode-hook
          (lambda ()
            (joe/load-flavor 'dark)
            (joe/set-faces)
            ))

      (add-hook 'auto-dark-light-mode-hook
          (lambda ()
            (joe/load-flavor 'light)
            (joe/set-faces)
            ))

      (auto-dark-mode t)
    '';
  };
  dape = {
    init = ''
      (setq dape-buffer-window-arrangement 'gud)
    '';
  };

}
