{ osConfig, ... }:
let
  themeColor = import ./theme-color.nix;
  mono = osConfig.masterOptions.joe.monospacedFontFamily;
  simpleMonoFace = { face, color, italic ? false }: {
    inherit italic face;
    font = mono;
    height = "1.0";
    foregroundSexp = themeColor color;
  };
in [
  (simpleMonoFace {
    face = "font-lock-comment-face";
    italic = true;
    color = "overlay2";
  })
  (simpleMonoFace {
    face = "font-lock-doc-face";
    italic = true;
    color = "overlay2";
  })
  (simpleMonoFace {
    face = "haskell-pragma-face";
    color = "subtext0";
  })
  (simpleMonoFace {
    face = "haskell-type-face";
    color = "peach";
  })
  (simpleMonoFace {
    face = "font-lock-keyword-face";
    color = "mauve";
  })
  (simpleMonoFace {
    face = "font-lock-variable-name-face";
    color = "text";
  })
  (simpleMonoFace {
    face = "font-lock-function-name-face";
    color = "teal";
  })
  (simpleMonoFace {
    face = "font-lock-constant-face";
    color = "lavender";
  })
  (simpleMonoFace {
    face = "font-lock-function-call-face";
    color = "lavender";
  })
  (simpleMonoFace {
    face = "font-lock-string-face";
    color = "green";
  })
  (simpleMonoFace {
    face = "font-lock-type-face";
    color = "rosewater";
  })
  {
    face = "eglot-inlay-hint-face";
    font = mono;
    height = "0.8";
    foregroundSexp = themeColor "surface2";
  }
]
