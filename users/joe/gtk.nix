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
  gtkExtraConfig = {
    gtk-toolbar-style = "GTK_TOOLBAR_ICONS";
    gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";
    gtk-button-images = 1;
    gtk-menu-images = 1;
    gtk-enable-event-sounds = 1;
    gtk-enable-input-feedback-sounds = 0;
    gtk-xft-antialias = 1;
    gtk-xft-hinting = 1;
  };
in {
  gtk = {
    enable = true;
    font = {
      name = osConfig.masterOptions.joe.sansFontFamily;
      size = 11;
    };
    gtk3.extraConfig = gtkExtraConfig;
    gtk4.extraConfig = gtkExtraConfig;
    theme.name = if osConfig.masterOptions.joe.lightMode.enable then
      "adw-gtk3"
    else
      "adw-gtk3-dark";
  };
}

