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

{ osConfig, lib, ... }:
let
  darkPalette = import ./palettes/dark.nix;
  lightPalette = import ./palettes/light.nix;
  mkPalette = import ./mk-palette.nix;

  joeFaces = lib.lists.flatten [
    generalFaces
    markdownFaces
    orgFaces
    dashboardFaces
    modelineFaces
    devFaces
    gitFaces
    diredFaces
  ];

  myFaces = builtins.concatStringsSep " " (map mkSetFaceAttribute joeFaces);
  serif = osConfig.masterOptions.joe.serifFontFamily;
  sans = osConfig.masterOptions.joe.sansFontFamily;
  mono = osConfig.masterOptions.joe.monospacedFontFamily;
  bold = "bold";
  unspecified = "'unspecified";
  mkSetFaceAttribute = import ./mk-set-face-attribute.nix { inherit osConfig; };
  themeColor = import ./theme-color.nix;
  devFaces = import ./dev-faces.nix { inherit osConfig; };
  diredFaces = [{
    face = "dired-directory";
    height = "1.0";
    font = mono;
    foregroundSexp = themeColor "mauve";
  }];
  gitFaces = [
    {
      face = "git-commit-summary";
      height = "1.0";
      font = mono;
      foregroundSexp = themeColor "sky";
    }
    {
      face = "magit-branch-remote";
      height = "1.0";
      font = mono;
      foregroundSexp = themeColor "pink";
    }
  ];
  generalFaces = [
    {
      face = "line-number";
      height = "0.6";
      font = mono;
      backgroundSexp = "'unspecified";
      foregroundSexp = themeColor "surface2";
    }
    {
      face = "line-number-current-line";
      height = "0.6";
      font = mono;
      foregroundSexp = themeColor "text";
    }
    {
      face = "default";
      # the default face must be an integer value (imagine *100)
      # so we scale it just the same as with "altitude" sizing for Emacs and round
      heightSexp = "(round (tkngt 114))";
      font = mono;
      foregroundSexp = themeColor "text";
      backgroundSexp = themeColor "mantle";
    }
    {
      face = "button";
      font = sans;
      height = "1.0";
      foregroundSexp = themeColor "mauve";
    }
    {
      face = "widget-button";
      font = sans;
      height = "1.0";
      foregroundSexp = themeColor "lavender";
    }
    {
      face = "fixed-pitch";
      height = "1.1";
      font = mono;
      foregroundSexp = themeColor "text";
    }
    {
      face = "variable-pitch";
      height = "1.3";
      foregroundSexp = themeColor "text";
    }
    {
      face = "variable-pitch-text";
      height = "1.3";
      foregroundSexp = themeColor "text";
    }

    {
      face = "header-line";
      font = sans;
      height = "0.6";
      backgroundSexp = themeColor "surface1";
      foregroundSexp = themeColor "text";
    }
    {
      face = "flymake-errline";
      underlineSexp = themeColor "red";
      font = mono;
    }
    {
      face = "flymake-warnline";
      underlineSexp = themeColor "yellow";
      font = mono;
    }
    {
      face = "flymake-error";
      underlineSexp = themeColor "red";
      font = mono;
      heightSexp = "(round (tkngt 114))";
    }
    {
      face = "flymake-warning";
      underlineSexp = themeColor "yellow";
      font = mono;
      heightSexp = "(round (tkngt 114))";
    }
    {
      face = "window-divider";
      font = null;
      height = null;
      background = themeColor "mauve";
      foreground = themeColor "mauve";
    }
  ];
  markdownFaces = [
    {
      face = "markdown-header-face-1";
      height = "1.6";
      font = serif;
      weight = bold;
    }
    {
      face = "markdown-header-face-2";
      height = "1.6";
      font = serif;
      weight = bold;
    }
    {
      face = "markdown-header-face-3";
      height = "1.5";
      font = serif;
      weight = bold;
    }
    {
      face = "markdown-header-face-4";
      height = "1.4";
      font = serif;
      weight = bold;
    }
    {
      face = "markdown-header-face-5";
      height = "1.3";
      font = serif;
      weight = bold;
    }
    {
      face = "markdown-pre-face";
      height = "1.0";
      font = mono;
    }
    {
      face = "markdown-code-face";
      height = "1.0";
      font = mono;
    }
    {
      face = "markdown-inline-code-face";
      height = "1.0";
      font = mono;
    }
  ];
  orgFaces = [
    {
      face = "org-code";
      font = mono;
      inherits = [ "shadow" "fixed-pitch" ];
      height = "1.0";
    }
    {
      face = "org-verbatim";
      inherits = [ "shadow" "fixed-pitch" ];
      height = "1.0";
    }
    {
      face = "org-special-keyword";
      inherits = [ "font-lock-comment-face" "fixed-pitch" ];
      height = "1.0";
    }
    {
      face = "org-special-keyword";
      inherits = [ "font-lock-comment-face" "fixed-pitch" ];
      height = "1.0";
    }
    {
      face = "org-meta-line";
      font = mono;
      inherits = [ "font-lock-comment-face" ];
      height = "1.0";
    }
    {
      face = "org-document-info-keyword";
      font = mono;
      inherits = [ "font-lock-comment-face" ];
      height = "1.0";
    }
    {
      face = "org-checkbox";
      inherits = [ "fixed-pitch" ];
      height = "1.0";
    }
    {
      face = "org-table";
      inherits = [ "fixed-pitch" ];
      height = "1.0";
    }
    {
      face = "org-block";
      font = mono;
      height = "1.0";
      inherits = [ "fixed-pitch" ];
      #foreground = "unspecified";
      #backgroundSexp = "(ef-themes-get-color-value 'bg-alt :overrides)";
    }
    {
      face = "org-level-1";
      height = "1.6";
      font = serif;
      weight = bold;
    }
    {
      face = "org-level-2";
      height = "1.6";
      font = serif;
      weight = bold;
    }
    {
      face = "org-level-3";
      height = "1.5";
      font = serif;
      weight = bold;
    }
    {
      face = "org-level-4";
      height = "1.4";
      font = serif;
      weight = bold;
    }
    {
      face = "org-level-5";
      height = "1.3";
      font = serif;
      weight = bold;
    }
    {
      face = "org-level-6";
      height = "1.3";
      font = serif;
      weight = bold;
    }
    {
      face = "org-level-7";
      height = "1.2";
      font = serif;
      weight = bold;
    }
    {
      face = "org-level-8";
      height = "1.2";
      font = serif;
      weight = bold;
    }
  ];
  dashboardFaces = [
    {
      face = "dashboard-banner-logo-title";
      font = serif;
      height = "1.5";
      weight = bold;
    }
    {
      face = "dashboard-footer-face";
      font = serif;
      height = "1.4";
      weight = bold;
    }
  ];
  modelineFaces = [
    {
      face = "mode-line";
      font = sans;
      height = "0.7";
      backgroundSexp = themeColor "surface0";
      foregroundSexp = themeColor "text";
    }
    {
      face = "mode-line-active";
      font = sans;
      height = "0.7";
      backgroundSexp = themeColor "surface0";
      foregroundSexp = themeColor "text";
    }
    {
      face = "mode-line-inactive";
      font = sans;
      height = "0.7";
      backgroundSexp = themeColor "mantle";
      foregroundSexp = themeColor "overlay1";
    }
    {
      face = "fringe";
      backgroundSexp = unspecified;
    }
  ];

in ''
  (defun joe/set-faces ()
    (interactive)
    ${myFaces}
  )

  (setq light-palette ${mkPalette lightPalette})
  (setq dark-palette ${mkPalette darkPalette})
  (setq joe/palette light-palette)

''

