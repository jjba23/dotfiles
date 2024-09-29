{
  xfconf.settings = {
    xfce4-session = { "startup/ssh-agent/enabled" = true; };
    xsettings = {
      "Gtk/MonospaceFontName" = "Iosevka Comfy Wide Medium Expanded 12";
      "Gtk/FontName" = "Inter 12";
      "Gtk/IconThemeName" = "Mint X";
      "Gtk/ThemeName" = "Mint-Y-Dark";
    };
    xpanel = {
      "panels/panel2/autohide-behavior" = 1;
      "panels/panel2/background-style" = 0;
      "panels/panel2/length" = 1.0;
      "panels/panel2/plugin-ids" = [ 7 ];
      "panels/panel2/position" = "p=10;x=830;y=1025";
      "panels/panel2/position-locked" = true;
      "panels/panel2/size" = 62;
    };
    xfwm4 = {
      "general/theme" = "RedmondXP";
      "general/title_font" = "Inter Medium 11";
    };
    xfce4-terminal = {
      "color-background" = "#1a731a731a73";
      "color-foreground" = "#f6f6f5f5f4f4";
      "font-name" = "Iosevka Comfy Wide Medium Expanded 12";
    };
    xfce4-keyboard-shortcuts = {
      # commands
      "commands/custom/<Super>c" = "exo-open --launch TerminalEmulator";
      "commands/custom/<Super>e" = "emacsclient -c";
      "commands/custom/<Super>t" = "mousepad -c";
      "commands/custom/<Super>f" = "thunar";

      # window manager
      "xfwm4/custom/<Super>Up" = "maximize_window_key";
      "xfwm4/custom/<Super>Left" = "tile_left_key";
      "xfwm4/custom/<Super>Right" = "tile_right_key";
      "xfwm4/custom/<Super>Down" = "minimize_window_key";
      "xfwm4/custom/<Super>p" = "right_workspace_key";
      "xfwm4/custom/<Super>n" = "left_workspace_key";
      "xfwm4/custom/<Super>1" = "workspace_1_key";
      "xfwm4/custom/<Super>2" = "workspace_2_key";
      "xfwm4/custom/<Super>3" = "workspace_3_key";
      "xfwm4/custom/<Super>4" = "workspace_4_key";
      "xfwm4/custom/<Super>k" = "close_window_key";
    };
  };
}
