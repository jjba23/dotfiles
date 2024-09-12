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
    postgresql_15
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
    freshfetch
    tree
    pandoc
    neofetch
    fastfetch
    btop
    eza
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
    krita
    cava
    ardour
    amberol
    pavucontrol
    qpwgraph
  ];

  # Games
  gamePackages = with pkgs; [ wesnoth prismlauncher cartridges ];
  # Office and productivity packages
  officePackages = with pkgs; [ evince _1password-gui ];
  # Themes
  themePackages = with pkgs; [ adw-gtk3 ];
  # Mail related software
  emailPackages = with pkgs; [ isync mu mailutils ];
  # Network utilities
  networkPackages = with pkgs; [ dig wireshark burpsuite httpie openssl ];
  # Music software
  musicPackages = with pkgs; [ spotify-player fretboard audio-sharing ];
  # More software, uncategorized
  morePackages = with pkgs; [
    sops
    ispell
    libnotify
    apostrophe
    blanket
    collision
    curtail
    #decibels
    gnome-decoder
    dialect
    deja-dup
    eartag
    elastic
    emblem
    #gaphor
    gnome-graphs
    health
    impression
    komikku
    #letterpress
    lorem
    mousai
    newsflash
    gnome-obfuscate
    #polari
    shortwave
    switcheroo
    valuta
    warp
    wike
    sqlitebrowser

    zsh-powerlevel10k
    zsh-autosuggestions
    thefuck
    zsh-syntax-highlighting

  ];
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

