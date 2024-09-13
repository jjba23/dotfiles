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

{
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
      font = "Iosevka Comfy Wide Expanded 11";
      terminal-bell = false;
      opacity = 97;
      theme-dark = "Dracula";
    };
  };

}

