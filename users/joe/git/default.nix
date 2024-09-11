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
let
  gitIdentity = pkgs.writeShellScriptBin "git-identity"
    (builtins.readFile ./git-identity.bash);
in {
  home.packages = with pkgs; [ gitIdentity fzf ];

  programs.git = {
    enable = true;

    aliases = {
      identity = "! git-identity";
      id = "! git-identity";
    };

    extraConfig = {
      pull = { rebase = false; };
      user = {
        useConfigOnly = true;
        work = {
          name = "Josep Bigorra";
          email = "josepbigorraalgaba@vandebron.nl";
          signingkey = "3B6D20502E380697!";
        };
        personal = {
          name = "Josep Bigorra";
          email = "jjbigorra@gmail.com";
          signingkey = "24F46738CE114AF6!";
        };
      };
    };

    ignores = [ "bloop/" ".metals" ".stack-work" "target" "metals.sbt" ];
  };
}

