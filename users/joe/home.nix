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
# Joe's personal user account configuration in Nix Home Manager
#
#
{ lib, ... }:
let
  mkGitRepoUpdater = { dir, name }: {
    "git-repo-updater-${name}" = {
      Unit = { Description = "Git Repo ${name} Updater"; };
      Service = {
        WorkingDirectory = dir;
        ExecStart =
          ''bash -c "ls | xargs -P10 -I{} git -C {} fetch --all --tags" '';
        RemainAfterExit = "no";
      };
    };
  };
  mkGitRepoUpdaterTimer = { name }: {
    "git-repo-updater-${name}-timer" = {
      Unit = { Description = "Git Repo Updater ${name} Timer"; };
      Timer = {
        OnCalendar = "*-*-* *:*/30:00";
        Unit = "git-repo-updater-${name}.service";
      };
    };
  };
in {
  imports = [ ./default.nix ];

  home = {
    username = "joe";
    homeDirectory = "/home/joe";
    stateVersion = "24.05";
    sessionVariables = {
      TERM = "xterm-256color";
      fish_greeting = "";
      VISUAL = "emacsclient -c";
      EDITOR = "emacsclient -t";
      GDK_BACKEND = "wayland";
      SDL_VIDEODRIVER = "wayland";
      XDG_SESSION_TYPE = "wayland";
      GTK_USE_PORTAL = "0";
      AWS_PROFILE = "read_only_user_ecr";

    };
  };

  programs.home-manager.enable = true;
  services.home-manager.autoUpgrade = {
    enable = true;
    frequency = "weekly";
  };

  programs.nix-index = {
    enable = true;
    enableFishIntegration = true;
  };

  home.file.".config/xsettingsd/xsettingsd.conf".text = ''
    Net/EnableEventSounds 1
    EnableInputFeedbackSounds 0
    Xft/Antialias 1
    Xft/Hinting 1
  '';

  systemd.user.services = lib.mkMerge [
    (mkGitRepoUpdater {
      dir = "/home/joe/Ontwikkeling/Persoonlijk";
      name = "persoonlijk";
    })
    (mkGitRepoUpdater {
      dir = "/home/joe/Ontwikkeling/Werk/backend/component";
      name = "werk-backend-component";
    })
  ];
  systemd.user.timers = lib.mkMerge [
    (mkGitRepoUpdaterTimer { name = "persoonlijk"; })
    (mkGitRepoUpdaterTimer { name = "werk-backend-component"; })
  ];
  home.file.".config/user-dirs.dirs".text = ''
    XDG_DESKTOP_DIR="$HOME/Bureaublad"
    XDG_DOWNLOAD_DIR="$HOME/Downloads"
    XDG_TEMPLATES_DIR="$HOME/Sjablonen"
    XDG_PUBLICSHARE_DIR="$HOME/Openbaar"
    XDG_DOCUMENTS_DIR="$HOME/Documenten"
    XDG_MUSIC_DIR="$HOME/Muziek"
    XDG_PICTURES_DIR="$HOME/Afbeeldingen"
    XDG_VIDEOS_DIR="$HOME/Video's"
  '';
}

