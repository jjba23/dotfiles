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

#
#
# Manon's personal user account configuration in Nix Home Manager
#
#

{
  imports = [ ./packages.nix ];

  home = {
    stateVersion = "24.05";
    username = "manon";
    homeDirectory = "/home/manon";
    sessionVariables = { MOZ_ENABLE_WAYLAND = 1; };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  programs = {
    home-manager.enable = true;
    git = {
      enable = true;
      userName = "Manon van den Bout";
      userEmail = "manonvdbout@gmail.com";
    };

    nix-index.enableFishIntegration = true;

    fish = {
      enable = true;
      shellAliases = {
        ll = "eza -lAh --group-directories-first";
        l = "eza -lAh --group-directories-first";
        ls = "eza";
      };
    };
  };
  dconf.settings = {
    # ...
    "org/gnome/shell" = {
      disable-user-extensions = false;
      # `gnome-extensions list` for a list
      enabled-extensions = [
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "trayIconsReloaded@selfmade.pl"
        "Vitals@CoreCoding.com"
        "dash-to-panel@jderose9.github.com"
        "sound-output-device-chooser@kgshank.net"
        "space-bar@luchrioh"
        "blur-my-shell@aunetx"
      ];
    };
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      enable-hot-corners = false;
    };
    "org/gnome/desktop/wm/preferences" = {
      button-layout = "appmenu:minimize,maximize,close";
    };
    "org/gnome/desktop/peripherals/touchpad" = {
      two-finger-scrolling-enabled = true;
      tap-to-click = true;
      natural-scroll = false;
    };
    "org/gnome/desktop/sound" = { allow-volume-above-100-percent = true; };
    "org/gnome/shell/extensions/dash-to-panel" = {
      trans-use-custom-opacity = true;
      trans-panel-opacity = 0.4;
    };
  };
}

