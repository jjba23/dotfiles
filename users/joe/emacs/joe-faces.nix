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
  darkPalette = {
    rosewater = "#f5e0dc";
    flamingo = "#f2cdcd";
    pink = "#f5c2e7";
    mauve = "#cba67f";
    red = "#f38ba9";
    maroon = "#eba0ac";
    peach = "#fab387";
    yellow = "#f9e2af";
    green = "#a6e3a1";
    teal = "#94e2d5";
    sky = "#89dceb";
    sapphire = "#74c7ec";
    blue = "#89b4fa";
    lavender = "#b4befe";
    text = "#cdd6f4";
    subtext1 = "#bac2de";
    subtext0 = "#a6adc8";
    overlay2 = "#9399b2";
    overlay1 = "#7f849c";
    overlay0 = "#6c7086";
    surface2 = "#585b70";
    surface1 = "#45475a";
    surface0 = "#313244";
    base = "#1e1e2e";
    mantle = "#181825";
    crust = "#11111b";
  };
  lightPalette = {
    rosewater = "#dc8a78";
    flamingo = "#dd7878";
    pink = "#ea76cb";
    mauve = "#8839ef";
    red = "#d20f39";
    maroon = "#e64553";
    peach = "#fe640b";
    yellow = "#df8e1d";
    green = "#40a02b";
    teal = "#179299";
    sky = "#04a5e5";
    sapphire = "#209fb5";
    blue = "#1e66f5";
    lavender = "#7287fd";
    text = "#4c4f69";
    subtext1 = "#5c5f77";
    subtext0 = "#6c6f85";
    overlay2 = "#7c7f93";
    overlay1 = "#8c8fa1";
    overlay0 = "#9ca0b0";
    surface2 = "#acb0be";
    surface1 = "#bcc0cc";
    surface0 = "#ccd0da";
    base = "#eff1f5";
    mantle = "#e6e9ef";
    crust = "#dce0e8";
  };
  mkPalette = p: ''
    '(      
      (rosewater . "${p.rosewater}")
      (flamingo . "${p.flamingo}")
      (pink . "${p.pink}")
      (mauve . "${p.mauve}")
      (red . "${p.red}")
      (maroon . "${p.maroon}")
      (peach . "${p.peach}")
      (yellow . "${p.yellow}")
      (green . "${p.green}")
      (teal . "${p.teal}")
      (sky . "${p.sky}")
      (sapphire . "${p.sapphire}")
      (blue . "${p.blue}")
      (lavender . "${p.lavender}")
      (text . "${p.text}")
      (subtext1 . "${p.subtext1}")
      (subtext0 . "${p.subtext0}")
      (overlay2 . "${p.overlay2}")
      (overlay1 . "${p.overlay1}")
      (overlay0 . "${p.overlay0}")
      (surface2 . "${p.surface2}")
      (surface1 . "${p.surface1}")
      (surface0 . "${p.surface0}")
      (base . "${p.base}")
      (mantle . "${p.mantle}")
      (crust . "${p.crust}")
    )
  '';

  joeFaces = lib.lists.flatten [
    generalFaces
    markdownFaces
    orgFaces
    dashboardFaces
    modelineFaces
  ];

  myFaces = builtins.concatStringsSep " " (map mkSetFaceAttribute joeFaces);
  serif = osConfig.masterOptions.joe.serifFontFamily;
  sans = osConfig.masterOptions.joe.sansFontFamily;
  mono = osConfig.masterOptions.joe.monospacedFontFamily;
  bold = "bold";
  unspecified = "'unspecified";
  mkSetFaceAttribute = import ./mk-set-face-attribute.nix { inherit osConfig; };
  themeColor = x: "(joe/get-color '${x})";
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
      heightSexp = "(round (mk-font-size 114))";
      font = mono;
      foregroundSexp = themeColor "text";
      backgroundSexp = themeColor "mantle";
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
      face = "font-lock-comment-face";
      font = mono;
      italic = true;
      height = "1.0";
      foregroundSexp = themeColor "surface1";
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
      heightSexp = "(round (mk-font-size 114))";
    }
    {
      face = "flymake-warning";
      underlineSexp = themeColor "yellow";
      font = mono;
      heightSexp = "(round (mk-font-size 114))";
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
      height = "1.3";
      weight = bold;
    }
    {
      face = "dashboard-footer-face";
      font = serif;
      height = "1.2";
      weight = bold;
    }
  ];
  modelineFaces = [
    {
      face = "mode-line";
      font = sans;
      height = "0.6";
      backgroundSexp = themeColor "surface1";
      foregroundSexp = themeColor "text";
    }
    {
      face = "mode-line-active";
      font = sans;
      height = "0.6";
      backgroundSexp = themeColor "surface1";
      foregroundSexp = themeColor "text";
    }
    {
      face = "mode-line-inactive";
      font = sans;
      height = "0.6";
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
