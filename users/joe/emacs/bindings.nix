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

{
  origami = {
    "C-c o o" = "origami-mode";
    "C-c o x" = "origami-open-node";
    "C-c o c" = "origami-close-node";
    "C-c o a" = "origami-toggle-all-nodes";
  };
  org-roam = {
    "C-c n l" = "org-roam-buffer-toggle";
    "C-c n f" = "org-roam-node-find";
    "C-c n g" = "org-roam-graph";
    "C-c n i" = "org-roam-node-insert";
    "C-c n c" = "org-roam-capture";
    "C-c n j" = "org-roam-dailies-capture-today";
  };
  consult = {
    "C-c h" = "consult-history";
    "C-c m" = "consult-mode-command";
    "C-c k" = "consult-kmacro";
    "C-x M-:" = "consult-complex-command";
    "C-x b" = "consult-buffer";
    "C-x 4 b" = "consult-buffer-other-window";
    "C-x 5 b" = "consult-buffer-other-frame";
    "C-x r b" = "consult-bookmark";
    "C-x p b" = "consult-project-buffer";
    "M-#" = "consult-register-load";
    "M-'" = "consult-register-store";
    "C-M-#" = "consult-register";
    "M-y" = "consult-yank-pop";
    "M-g e" = "consult-compile-error";
    "M-g f" = "consult-flymake";
    "M-g g" = "consult-goto-line";
    "M-g M-g" = "consult-goto-line";
    "M-g o" = "consult-outline";
    "M-g m" = "consult-mark";
    "M-g k" = "consult-global-mark";
    "M-g i" = "consult-imenu";
    "M-g I" = "consult-imenu-multi";
    "M-s d" = "consult-find";
    "M-s D" = "consult-locate";
    "M-s g" = "consult-grep";
    "M-s G" = "consult-git-grep";
    "M-s r" = "consult-ripgrep";
    "M-s l" = "consult-line";
    "M-s L" = "consult-line-multi";
    "M-s k" = "consult-keep-lines";
    "M-s u" = "consult-focus-lines";
    "M-s e" = "consult-isearch-history";
  };
  mu4e = {
    "C-c C-m m" = "mu4e";
    "C-c C-m s" = "mu4e-search";
    "C-c C-m h" = "mu4e-display-manual";
    "C-c C-m f" = "mu4e-headers-toggle-full-search";
  };
  pomodoro = {
    "C-c C-t s" = "pomodoro-start";
    "C-c C-t g" = "pomodoro-stop";
    "C-c C-t p" = "pomodoro-pause";
    "C-c C-t r" = "pomodoro-resume";
  };
  org = {
    "C-c a a" = "org-agenda";
    "C-C c t" = "org-contacts";
    "C-c c c" = "org-capture";
  };
  cape = {
    "M-p p p" = "completion-at-point";
    "M-p p t" = "complete-tag";
    "M-p p d" = "cape-dabbrev";
    "M-p p h" = "cape-history";
    "M-p p f" = "cape-file";
    "M-p p k" = "cape-keyword";
    "M-p p a" = "cape-abbrev";
    "M-p p i" = "cape-ispell";
    "M-p p l" = "cape-line";
    "M-p p w" = "cape-dict";
  };
  dabbrev = {
    "M-/" = "dabbrev-completion";
    "C-M-/" = "dabbrev-expand";
  };
  eglot = {
    "C-c i i" = "eglot-find-implementation";
    "C-c i e" = "eglot";
    "C-c i k" = "eglot-shutdown-all";
    "C-c i r" = "eglot-rename";
    "C-c i x" = "eglot-reconnect";
    "C-c i a" = "eglot-code-actions";
    "C-c i m" = "eglot-menu";
    "C-c i f" = "eglot-format-buffer";
    "C-c i d" = "flutter-run-or-hot-reload";
  };
  helpful = {
    "C-h f" = "helpful-callable";
    "C-h v" = "helpful-variable";
    "C-h k" = "helpful-key";
    "C-c C-d" = "helpful-at-point";
    "C-h F" = "helpful-function";
    "C-h C" = "helpful-command";
  };
  joe = {
    "C-c f j" = "set-font-scale-jumbo";
    "C-c f x" = "set-font-scale-larger";
    "C-c f l" = "set-font-scale-large";
    "C-c f r" = "set-font-scale-regular";
    "C-c f s" = "set-font-scale-small";
    "C-c f t" = "set-font-scale-tiny";
    "C-c v l" = "global-display-line-numbers-mode";
    "C-c t t" = "make-eshell-next-number";
    "C-c t l" = "eshell/clear";
    "C-c t k" = "eshell-kill-process";
    "C-c b b" = "add-browser-bookmark";
    "C-c # b" = "nixos-rebuild";
    "C-c # r" = "nixos-restart-emacs";
    "C-c # c" = "nixos-emacs-config-file-joe";
  };
  direnv = {
    "C-c d d" = "direnv-mode";
    "C-c d a" = "direnv-allow";
  };
  spacious-padding = { "C-c C-p p" = "spacious-padding-mode"; };
  flymake = {
    "C-c ! d" = "flymake-show-buffer-diagnostics";
    "C-c ! n" = "flymake-goto-next-error";
    "C-c ! p" = "flymake-goto-prev-error";
    "C-c ! f" = "flymake-mode";
  };
}
