{ lib, ... }:

with lib; {
  lightMode = { enable = mkEnableOption "light UI mode"; };
  monospacedFontFamily = mkOption {
    description = ''
      Monospaced font for many applications and UI.
    '';
    default = "Mononoki";
  };
  sansFontFamily = mkOption {
    description = ''
      Sans Serif font for many applications and UI.
    '';
    default = "Inter";
  };
  serifFontFamily = mkOption {
    description = ''
      Serif font for many applications and UI.
    '';
    default = "Inter";
  };
}
