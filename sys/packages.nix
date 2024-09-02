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
  # Fonts
  fontPackages = with pkgs; [
    ibm-plex
    fira
    fira-mono
    fira-code
    noto-fonts
    noto-fonts-emoji
    libre-baskerville
    roboto
    roboto-slab
    roboto-mono
    roboto-serif
    inter
    aileron
    nerdfonts
    google-fonts
    jetbrains-mono
  ];
  gnomeExtensionPackages = with pkgs; [
    gnomeExtensions.blur-my-shell
    gnomeExtensions.gsconnect
    gnomeExtensions.places-status-indicator
    gnomeExtensions.tiling-shell
    gnomeExtensions.removable-drive-menu
    gnomeExtensions.vitals
    gnomeExtensions.applications-menu
    gnomeExtensions.move-clock
    gnomeExtensions.caffeine
  ];
  # More software, uncategorized
  morePackages = with pkgs; [
    python3
    jdk21
    coreutils-full
    dconf
    gedit
    gnome-terminal
    pitivi
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
    fish
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
    xdg-desktop-portal
    bluez
    gsettings-desktop-schemas
    partition-manager
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
    gnome.gnome-disk-utility
    gnome.cheese
    gnome.gnome-tweaks
    libreoffice
    gnome.gnome-calculator
    gnome.gnome-calendar
    gnupg
    pinentry-all
    poetry
  ];
in {
  environment.systemPackages =
    lib.mkMerge [ morePackages gnomeExtensionPackages selectedAppleSpecific ];
  fonts.packages = fontPackages;
}
