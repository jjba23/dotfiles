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
# Joe's SSH customizations and settings
#
#

{
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "gitlab.com/" = { identityFile = "/home/joe/.ssh/gitlab_prive"; };
      "github.com/jjba23" = { identityFile = "/home/joe/.ssh/gitlab_prive"; };
      "github.com/Vandebron" = { identityFile = "/home/joe/.ssh/vandebron"; };
      "aws-nixos" = {
        hostname = "ec2-35-180-109-81.eu-west-3.compute.amazonaws.com";
        identityFile = "/home/joe/.ssh/gitlab_prive";
      };
    };
  };
}
