{ lib, ... }:

with lib; {
  lightMode = { enable = mkEnableOption "light UI mode"; };
  monospacedFontFamily = mkOption {
    description = ''
      Monospaced font for many applications and UI.
    '';
    default = "JetBrains Mono";
  };
  sansFontFamily = mkOption {
    description = ''
      Sans Serif font for many applications and UI.
    '';
    default = "Aileron";
  };
  serifFontFamily = mkOption {
    description = ''
      Serif font for many applications and UI.
    '';
    default = "Aileron";
  };
}
