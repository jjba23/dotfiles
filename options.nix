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

{ lib, ... }:

let joeOptions = import ./users/joe/options.nix { inherit lib; };
in with lib; {
  options.masterOptions = {
    proprietaryNvidia = {
      enable = mkEnableOption "proprietary NVIDIA drivers";
    };
    systemLocale = mkOption {
      description = ''
        Locale to use for the system, e.g. "nl_NL.UTF-8"
      '';
      default = "nl_NL.UTF-8";
    };
    systemTimezone = mkOption {
      description = ''
        Timezone to use for the system, e.g. "Europe/Amsterdam"
      '';
      default = "Europe/Amsterdam";
    };
    keyLayout = mkOption {
      description = ''
        Keyboard layout to use
      '';
      default = "us";
    };
    defaultSession = mkOption {
      description = ''
        The default session to use when logging in this machine,
        unless you manually select another one in GDM.
      '';
      default = "gnome";
    };
    isApple = { enable = mkEnableOption "Apple hardware"; };
    joe = joeOptions;
    # accounts = {
    # TODO optional user account creation
    # joe = mkEnableOption "Joe's user account"
    # manon = mkEnableOption "Manon's user account";
    # claudio = mkEnableOption "Claudio's user account";
    # };
  };
}

