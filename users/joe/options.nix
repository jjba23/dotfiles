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

{ lib, ... }:

with lib; {
  lightMode = { enable = mkEnableOption "light UI mode"; };
  monospacedFontFamily = mkOption {
    description = ''
      Monospaced font for many applications and UI.
    '';
    default = "Iosevka Comfy Wide";
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
