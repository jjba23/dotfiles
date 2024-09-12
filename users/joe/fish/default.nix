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
# Fish shell customizations and settings 
#
#

{
  programs.nix-index.enableFishIntegration = true;
  programs.fish = {
    enable = true;
    shellAliases = {
      gco = "git checkout";
      npu = "nix-prefetch-url";
      ll = "eza -lAh --group-directories-first";
      l = "eza -lAh --group-directories-first";
      ls = "eza";
      gcl = "git clone";
      gpl = "git pull";
      gcm = "git commit";
      et = "emacsclient -t";
      e = "emacsclient -t";
      wikimusic =
        "/home/joe/Ontwikkeling/Persoonlijk/wikimusic-ui/build/linux/x64/release/bundle/wikimusic_ui";
      stress-cpu = "openssl speed -multi $(grep -ci processor /proc/cpuinfo)";
      zd-opensearch-forward =
        "kubectl -n local port-forward opensearch-cluster-master-0 9200:9200";
      zd-kafka-forward = "kubectl -n local port-forward kafka-0 9092:9092";
      zd-postgresql-forward =
        "kubectl -n local port-forward postgresql-0 5432:5432";
      zd-vpn =
        "openvpn3 session-start --config ~/Documenten/bigorra-zorgdomein-vpn.ovpn && openvpn3 session-auth";
      nr =
        "cd ~/Ontwikkeling/Persoonlijk/dotfiles && nix develop -c cabal run dotfiles -- rebuild-system";
      rr = "systemctl --user --no-pager restart emacs";
      lock = "hyprlock";
      aboutnix = "nix-info -m";
      fetch = "fastfetch";
      neofetch = "fastfetch";
      gfetch = "onefetch";
      scala-validate = "sbt scalafixAll && sbt scalafmt && sbt test";
      scala-fmt = "sbt scalafixAll && sbt scalafmt";
      gpg-new = "gpg --expert --pinentry-mode=loopback --full-gen-key";
      gpg-list = "gpg --list-secret-keys --keyid-format=long";
    };
  };

}
