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

{ pkgs, lib, config, ... }:

let
  # Apple hardware-specific packages 
  selectedAppleSpecific = lib.lists.flatten [
    (if config.masterOptions.isApple.enable then [
      pkgs.facetimehd-firmware
      pkgs.facetimehd-calibration
    ] else
      [ ])
  ];
  devPackages = with pkgs; [
    caddy
    pfetch-rs
    guile
    guile-hall
    stack
    guix
    watchexec

  ];
  # Fonts
  fontPackages = with pkgs; [
    noto-fonts
    intel-one-mono
    liberation_ttf
    crimson
    crimson-pro
    noto-fonts-emoji
    libre-baskerville
    roboto
    roboto-slab
    roboto-mono
    roboto-serif
    google-fonts
    inter
    inconsolata
    iosevka
  ];
  # More software, uncategorized
  morePackages = with pkgs; [
    python3
    jdk21
    coreutils-full
    dconf
    kdePackages.kdenlive
    eyedropper
    fragments
    firefox
    git
    gnumake
    autoconf
    vim
    neovim
    zip
    unzip
    xz
    home-manager
    htop
    zsh
    lm_sensors
    tlp
    acpi
    grub2_efi
    openvpn3
    screen
    virt-manager
    vlc
    audacity
    gimp
    curl
    wget
    libpqxx
    postgresql
    brightnessctl
    playerctl
    pamixer
    xdg-utils
    bluez
    gsettings-desktop-schemas
    gparted
    statix
    manix
    deadnix
    dejavu_fonts
    vulnix
    fzf
    sysstat
    libwebp
    imagemagick
    gimp
    system-config-printer
    hplipWithPlugin
    hplip
    libreoffice
    obs-studio
    transmission_4
    brave

    cinnamon.mint-y-icons
    cinnamon.mint-x-icons
    cinnamon.mint-l-icons
    cinnamon.mint-themes
    cinnamon.mint-artwork
    cinnamon.mint-cursor-themes

    dockbarx
    xfce.xfce4-dockbarx-plugin

    xfce.thunar-volman
    xarchiver
    thunderbird
    xfce.xfce4-weather-plugin
    xfce.xfce4-systemload-plugin
    xfce.thunar-archive-plugin
    xfce.xfce4-timer-plugin
    xfce.xfce4-whiskermenu-plugin
    xfce.xfce4-sensors-plugin

    xfce.xfwm4-themes
    xfce.xfdashboard
    xfce.xfce4-appfinder
    xfce.xfce4-pulseaudio-plugin
    xfce.xfce4-clipman-plugin

    marwaita-icons
    proselint
  ];
in {
  environment.systemPackages =
    lib.mkMerge [ morePackages selectedAppleSpecific devPackages ];
  fonts.packages = fontPackages;
}
