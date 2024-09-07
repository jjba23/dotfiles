{ osConfig, ... }:
let
  themeColor = import ./theme-color.nix;
  mono = osConfig.masterOptions.joe.monospacedFontFamily;
in [
  {
    face = "font-lock-comment-face";
    font = mono;
    italic = true;
    height = "1.0";
    foregroundSexp = themeColor "overlay2";
  }
  {
    face = "haskell-pragma-face";
    font = mono;
    height = "1.0";
    foregroundSexp = themeColor "subtext0";
  }
  {
    face = "haskell-type-face";
    font = mono;
    height = "1.0";
    foregroundSexp = themeColor "peach";
  }
  {
    face = "font-lock-keyword-face";
    font = mono;
    height = "1.0";
    foregroundSexp = themeColor "mauve";
  }
  {
    face = "font-lock-variable-name-face";
    font = mono;
    height = "1.0";
    foregroundSexp = themeColor "text";
  }
  {
    face = "font-lock-constant-face";
    font = mono;
    height = "1.0";
    foregroundSexp = themeColor "lavender";
  }
  {
    face = "font-lock-function-call-face";
    font = mono;
    height = "1.0";
    foregroundSexp = themeColor "lavender";
  }
  {
    face = "font-lock-string-face";
    font = mono;
    height = "1.0";
    foregroundSexp = themeColor "green";
  }
]
