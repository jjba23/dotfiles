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

#
#
# Joe's Emacs vanilla setup from scratch, with Nix and Emacs Lisp
#
#

{ pkgs, ... }:

{
  services.emacs.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30;
    # when in wayland, this can be nice:  package = pkgs.emacs30-pgtk;
    extraPackages = epkgs: [
      epkgs.treesit-grammars.with-all-grammars
      epkgs.vterm
      epkgs.mu4e
    ];
  };

  home.file = {
    ".emacs.d/init.el".source = ./emacs/init.el;
    ".emacs.d/early-init.el".source = ./emacs/early-init.el;
    ".emacs.d/nix-bridge.el".text = ''
      (setq emacsql-sqlite3-executable "${pkgs.sqlite}/bin/sqlite3"
            org-roam-graph-executable "${pkgs.graphviz}/bin/dot"
            pandoc-binary "${pkgs.pandoc}/bin/pandoc"
            ripgrep-executable "${pkgs.ripgrep}/bin/rg"
      )
    '';
  };

}
