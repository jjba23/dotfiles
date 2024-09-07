{ lib, ... }:
let
  mkAutoStart = { binaryPath, exec, name, fullName ? name, description ? ""
    , inTerminal ? false, icon ? "org.gnome.Console", type ? "Application"
    , categories ? [ ], version ? "1.0" }: {
      home.file.".config/autostart/${name}.desktop".text = ''
        [Desktop Entry]
        Type=${type}
        Version=${version}
        Name=${fullName}
        Comment=${description}
        Path=${binaryPath}
        Exec=${exec}
        Icon=${icon}
        Terminal=${inTerminal}
        Categories=${lib.strings.intersperse ";" categories}
      '';
    };
  myAutoStarts = [{
    binaryPath = "/etc/profiles/per-user/joe/bin/systemctl";
    exec = "systemctl --user start emacs";
    name = "emacs-daemon";
    fullName = "Emacs Daemon";
  }];
  autoStarts = map mkAutoStart myAutoStarts;
in autoStarts
