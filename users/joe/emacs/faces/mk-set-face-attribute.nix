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
({ face, height ? null, heightSexp ? null
  , font ? osConfig.masterOptions.joe.sansFontFamily, inherits ? null
  , background ? null, backgroundSexp ? null, foreground ? null
  , foregroundSexp ? null, weight ? null, italic ? false, box ? null
  , underline ? null, underlineSexp ? null }:

  let
    ifNullEmptyElse = x: tpl: if x == null then "" else tpl;
    height_ = if heightSexp == null then
      (ifNullEmptyElse height ":height (tkngt ${height})")
    else
      ":height ${heightSexp}";
    inherits_ = ifNullEmptyElse inherits
      ":inherit '(${builtins.concatStringsSep " " inherits})";
    foreground_ = if foregroundSexp == null then
      (ifNullEmptyElse foreground '':foreground "${foreground}"'')
    else
      ":foreground ${foregroundSexp}";
    #
    #
    underline_ = if underlineSexp == null then
      (ifNullEmptyElse underline '':underline "${underline}"'')
    else
      ":underline ${underlineSexp}";
    #
    #
    background_ = if backgroundSexp == null then
      (ifNullEmptyElse background '':background "${background}"'')
    else
      ":background ${backgroundSexp}";
    #
    #
    weight_ = ifNullEmptyElse weight ":weight '${weight}";
    #
    italic_ = if italic then ":italic t" else ":italic nil";
    #
    box_ = ifNullEmptyElse box ":box ${box}";
    #
    font_ = ifNullEmptyElse font '':font "${font}"'';
  in ''
    (set-face-attribute '${face} nil
        ${height_} ${weight_}
        ${font_} ${foreground_} ${background_} ${italic_} 
        ${box_} ${underline_} ${inherits_}
    )''

)
