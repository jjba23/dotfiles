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
with lib.hm.gvariant; {
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
    "org/gnome/shell/weather" = { automatic-location = true; };
    "org/gnome/shell/extensions/vitals" = {
      hot-sensors =
        [ "_memory_usage_" "_processor_usage_" "_temperature_processor_0_" ];
    };

    "com/raggesilver/BlackBox" = {
      show-headerbar = true;
      font = "Intel One Mono 12";
      terminal-bell = false;
      opacity = 100;
      theme-dark = "Dracula";
      scrollback-mode = 0;
      scrollback-lines = 1000000;
      terminal-padding =
        mkTuple [ (mkUint32 22) (mkUint32 22) (mkUint32 22) (mkUint32 22) ];
      pretty = false;
    };
  };
}

