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

{ lib, config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ./sys ./options.nix ./per-host.nix ];

  system.stateVersion = "24.05";
  system.autoUpgrade = {
    enable = true;
    dates = "daily";
    channel = "https://nixos.org/channels/nixos-unstable";
  };

  time.timeZone = config.masterOptions.systemTimezone;

  i18n.defaultLocale = config.masterOptions.systemLocale;
  i18n.extraLocaleSettings = {
    LC_ADDRESS = config.masterOptions.systemLocale;
    LC_IDENTIFICATION = config.masterOptions.systemLocale;
    LC_MEASUREMENT = config.masterOptions.systemLocale;
    LC_MONETARY = config.masterOptions.systemLocale;
    LC_NAME = config.masterOptions.systemLocale;
    LC_NUMERIC = config.masterOptions.systemLocale;
    LC_PAPER = config.masterOptions.systemLocale;
    LC_TELEPHONE = config.masterOptions.systemLocale;
    LC_TIME = config.masterOptions.systemLocale;
  };

  services = {
    xserver = {
      enable = true;
      displayManager = {
        gdm = {
          enable = true;
          wayland = true;
          banner = "Powered by Free Software. Free as in Freedom!";
        };
      };
      xkb = {
        layout = config.masterOptions.keyLayout;
        variant = "";
      };
      desktopManager = { gnome = { enable = true; }; };
      videoDrivers =
        lib.mkIf config.masterOptions.proprietaryNvidia.enable [ "nvidia" ];
    };
  };

  # Optional, hint electron apps to use wayland:
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  programs = { direnv.enable = true; };

  #
  # Users
  #

  users.users = {
    # Don't forget to set a password with ‘passwd’.
    joe = {
      isNormalUser = true;
      description = "Joe";
      extraGroups = [
        "networkmanager"
        "network"
        "audio"
        "wheel"
        "docker"
        "libvirtd"
        "lpadmin"
        "lp"
      ];
      shell = pkgs.zsh;
    };

    manon = {
      isNormalUser = true;
      description = "Manon";
      extraGroups =
        [ "networkmanager" "network" "audio" "wheel" "docker" "libvirtd" ];
      shell = pkgs.zsh;
    };

    claudio = {
      isNormalUser = true;
      description = "Claudio";
      extraGroups =
        [ "networkmanager" "network" "audio" "wheel" "docker" "libvirtd" ];
      shell = pkgs.zsh;
    };
  };

  # BTRFS compression
  fileSystems = { "/".options = [ "compress=zstd" ]; };

  virtualisation = {
    docker = {
      enable = true;
      storageDriver = "btrfs";
    };

    libvirtd.enable = true;
  };

  xdg.portal = { enable = true; };

  services.power-profiles-daemon.enable = true;

  # NVIDIA
  # Enable OpenGL
  hardware.opengl = lib.mkIf config.masterOptions.proprietaryNvidia.enable {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  hardware.nvidia = lib.mkIf config.masterOptions.proprietaryNvidia.enable {

    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    powerManagement.enable = false;
    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of 
    # supported GPUs is at: 
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus 
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
    open = false;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 20d";
  };

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
  services.guix.enable = true;
}
