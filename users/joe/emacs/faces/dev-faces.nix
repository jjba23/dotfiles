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
