{ lib, ... }:
let
  mkAutoStart = { binaryPath, exec, name, fullName ? name, description ? ""
    , inTerminal ? false, icon ? "org.gnome.Console", type ? "Application"
    , categories ? [ ], version ? "1.0" }: ''
      [Desktop Entry]
      Type=${type}
      Version=${version}
      Name=${fullName}
      Comment=${description}
      Path=${binaryPath}
      Exec=${exec}
      Icon=${icon}
      Terminal=${if inTerminal then "true" else "false"}
      Categories=${
        lib.strings.concatStrings (lib.strings.intersperse ";" categories)
      }
    '';
in {
  home.file.".config/autostart/emacs-daemon.desktop".text = mkAutoStart {
    name = "emacs-daemon";
    fullName = "Emacs Daemon";
    binaryPath = "/etc/profiles/per-user/joe/bin/systemctl";
    exec = "systemctl --user --no-pager start emacs";
  };

}
