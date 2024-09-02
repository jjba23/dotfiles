{ osConfig, ... }:

let
  gtkExtraConfig = {
    gtk-toolbar-style = "GTK_TOOLBAR_ICONS";
    gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";
    gtk-button-images = 1;
    gtk-menu-images = 1;
    gtk-enable-event-sounds = 1;
    gtk-enable-input-feedback-sounds = 0;
    gtk-xft-antialias = 1;
    gtk-xft-hinting = 1;
  };
in {
  gtk = {
    enable = true;
    font = {
      name = osConfig.masterOptions.joe.sansFontFamily;
      size = 11;
    };
    gtk3.extraConfig = gtkExtraConfig;
    gtk4.extraConfig = gtkExtraConfig;
    theme = {
      name = if osConfig.masterOptions.joe.lightMode.enable then
        "Adw-gtk3"
      else
        "Adw-gtk3-dark";
    };
  };
  home.file.".config/gtk-3.0/gtk.css".text = ''
    VteTerminal,
    TerminalScreen,
    vte-terminal {
        padding: 12px 12px 12px 12px;
        -VteTerminal-inner-border: 12px 12px 12px 12px;
    }
  '';
}

