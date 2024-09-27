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
# ZSH shell customizations and settings 
#
{ config, pkgs, ... }: {
  programs.nix-index.enableZshIntegration = true;
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    plugins = [
      {
        name = "zsh-autosuggestions";
        src = pkgs.zsh-autosuggestions;
        file = "share/zsh-autosuggestions/zsh-autosuggestions";
      }
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
    ];

    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    history = {
      size = 1000000;
      path = "${config.xdg.dataHome}/zsh/history";
    };

    initExtra = "source ~/.p10k.zsh";

    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
        "1password"
        "cabal"
        "colored-man-pages"
        "command-not-found"
        "direnv"
        "docker-compose"
        "httpie"
        "scala"
        "sbt"
        "systemd"
        "systemadmin"
        "z"
      ];
    };

    shellAliases = {
      gco = "git checkout";
      npu = "nix-prefetch-url";
      ll = "ls -lAh --group-directories-first";
      l = "ls -lAh --group-directories-first";
      ls = "ls";
      gcl = "git clone";
      gpl = "git pull";
      gcm = "git commit";
      et = "emacsclient -t";
      e = "emacsclient -t";
      stress-cpu = "openssl speed -multi $(grep -ci processor /proc/cpuinfo)";
      opensearch-forward =
        "kubectl -n local port-forward opensearch-cluster-master-0 9200:9200";
      kafka-forward = "kubectl -n local port-forward kafka-0 9092:9092";
      postgresql-forward =
        "kubectl -n local port-forward postgresql-0 5432:5432";
      vpn =
        "openvpn3 session-start --config ~/Documenten/work-vpn.ovpn && openvpn3 session-auth";
      nr =
        "cd ~/Ontwikkeling/Persoonlijk/dotfiles && nix develop -c cabal run dotfiles -- rebuild-system";
      nrr =
        "cd ~/Ontwikkeling/Persoonlijk/dotfiles && nix develop -c cabal run dotfiles -- rebuild-system && systemctl --user --no-pager restart emacs";
      rr = "systemctl --user --no-pager restart emacs";
      aboutnix = "nix-info -m";
      fetch = "pfetch";
      neofetch = "pfetch";
      gfetch = "onefetch";
      scala-validate = "sbt scalafixAll && sbt scalafmt && sbt test";
      scala-fmt = "sbt scalafixAll && sbt scalafmt";
      gpg-new = "gpg --expert --pinentry-mode=loopback --full-gen-key";
      gpg-list = "gpg --list-secret-keys --keyid-format=long";
    };
  };

  home.file.".p10k.zsh".source = ./.p10k.zsh;
}
