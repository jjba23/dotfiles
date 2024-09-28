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

{ lib, pkgs, ... }:

let
  # Language servers, programming helpers, code assist, etc.
  languageServerPackages = with pkgs; [
    yaml-language-server
    jdt-language-server
    nil
    nodePackages.bash-language-server
    helm-ls
    nodePackages_latest.typescript-language-server
    postgres-lsp
    terraform-ls
    metals
    vscode-langservers-extracted
    stylelint
    marksman
    android-file-transfer
    pyright
    black
  ];
  # Programming languages, compilers, etc.
  programmingLanguagePackages = with pkgs; [
    nixfmt
    nodejs_20
    sbcl
    terraform
    go
    typescript
    dart-sass
    protobuf
    cachix
    scala_2_13
  ];
  # General development packages
  devPackages = with pkgs; [
    sqlitebrowser
    postgresql
    textpieces
    postman
    dbgate
    ripgrep
    jq
    yq
    kubectl
    kubernetes-helm
    graphviz
    dart-sass
    gawk
    awscli2
    docker-compose
    amazon-ecr-credential-helper
    libgcc
    gcc
    gnuplot
    mermaid-cli
    mermaid-filter
  ];
  # Libraries
  libs = with pkgs; [
    webkitgtk
    gtksourceview5
    gobject-introspection
    zlib
    gmp
    ncurses
  ];
  # Command Line Interface packages, utilities, etc.
  cliPackages = with pkgs; [
    fzf
    fd
    feh
    nix-info
    onefetch
    tree
    pandoc
    neofetch
    fastfetch
    neo-cowsay
    clolcat
    fortune
    gomatrix
    nyancat
  ];
  # Scala development related packages
  scalaPackages = with pkgs; [ sbt scalafmt scalafix scala-cli coursier ];
  # Multimedia, video, photo software, etc.
  multimediaPackages = with pkgs; [
    obs-studio
    vlc
    audacity
    inkscape
    digikam
    cava
    ardour
    pavucontrol
    qpwgraph
  ];

  # Games
  gamePackages = with pkgs; [ wesnoth prismlauncher ];
  # Office and productivity packages
  officePackages = with pkgs; [ _1password-gui ];
  # Themes
  themePackages = [ ];
  # Mail related software
  emailPackages = with pkgs; [ isync mu mailutils ];
  # Network utilities
  networkPackages = with pkgs; [ dig wireshark burpsuite httpie openssl ];
  # Music software
  musicPackages = with pkgs; [ audio-sharing ];
  # More software, uncategorized
  morePackages = with pkgs; [ sops ispell libnotify ];
in {
  home.packages = lib.mkMerge [
    morePackages
    themePackages
    officePackages
    emailPackages
    gamePackages
    multimediaPackages
    networkPackages
    languageServerPackages
    programmingLanguagePackages
    scalaPackages
    cliPackages
    musicPackages
    devPackages
    libs
  ];
}

