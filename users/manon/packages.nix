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

{ pkgs, ... }:

{
  home.packages = with pkgs; [

    # Development
    ripgrep
    jq
    yq

    vscode
    jetbrains.pycharm-community
    gedit

    # Nerd
    neofetch
    btop
    eza

    # fish shell
    fishPlugins.z
    fishPlugins.done
    fishPlugins.forgit
    fishPlugins.hydro
    fishPlugins.sponge
    fishPlugins.colored-man-pages

    # CLI
    tree

    # web
    brave

    # Office
    libreoffice

    # Multimedia
    obs-studio
    vlc
    audacity

    gimp
    inkscape
    qbittorrent

    #
    _1password-gui

    dconf

    # Fonts
    fira
    fira-mono
    fira-code
    noto-fonts
    noto-fonts-emoji
    libre-baskerville

    gnomeExtensions.user-themes
    gnomeExtensions.tray-icons-reloaded
    gnomeExtensions.vitals
    gnomeExtensions.dash-to-panel
    gnomeExtensions.sound-output-device-chooser
    gnomeExtensions.space-bar
    gnomeExtensions.blur-my-shell

  ];
}
