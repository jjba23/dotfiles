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

{ pkgs, lib, osConfig, ... }:

let
  # 'latte is light, 'frappe, 'macchiato, or 'mocha are dark
  catppuccinFlavor =
    if osConfig.masterOptions.joe.lightMode.enable then "latte" else "mocha";
  joe-faces = import ./joe-faces.nix { inherit osConfig lib; };
  prelude = builtins.readFile ./lisp/prelude.el;
  usePackage = import ./packages.nix { inherit lib pkgs; };
in {
  imports = [ ./init-maker-emacs ];

  services.emacs.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: [
      epkgs.vterm
      epkgs.treesit-grammars.with-all-grammars
    ];
  };

  programs.emacs.init = {
    enable = true;
    recommendedGcSettings = true;
    usePackageVerbose = true;
    inherit usePackage;

    prelude = ''
      ${prelude}
      (setq catppuccin-flavor '${catppuccinFlavor})
      (setq ob-mermaid-cli-path "${pkgs.mermaid-cli}/bin/mmdc")      
      ${joe-faces}      
    '';
  };
}

