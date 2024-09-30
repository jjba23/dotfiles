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
    noto-fonts-emoji
    libre-baskerville
    roboto
    roboto-slab
    roboto-mono
    roboto-serif
    google-fonts
    inter
    inconsolata
    iosevka-comfy.comfy-wide
    iosevka-comfy.comfy-wide-fixed
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
    canta-theme
    cinnamon.mint-y-icons
    cinnamon.mint-x-icons
    cinnamon.mint-l-icons
    cinnamon.mint-themes
    cinnamon.mint-artwork
    cinnamon.mint-cursor-themes
    numix-gtk-theme
    numix-icon-theme
    numix-cursor-theme
    dockbarx
    xfce.xfce4-dockbarx-plugin
    xfce.thunar-archive-plugin
    xfce.thunar-volman
    xarchiver
  ];
in {
  environment.systemPackages =
    lib.mkMerge [ morePackages selectedAppleSpecific devPackages ];
  fonts.packages = fontPackages;
}
