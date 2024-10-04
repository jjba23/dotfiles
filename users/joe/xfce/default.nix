{
  home.file.".config/gtk-3.0/gtk.css".text = ''
    .xfce4-panel {
        border-bottom-left-radius: 13px;
        border-bottom-right-radius: 13px;
        border-top-left-radius: 13px;
        border-top-right-radius: 13px;
     }
      
     .xfce4-panel .tasklist .toggle :checked {
       border-radius: 10px;
     }
      
     .tasklist button:checked {
       border-bottom: 3px solid white;
     }
      
     .flat,
     .toggle {
       font-family: "Iosevka Comfy Wide Medium Expanded";
       font-size: 16px;
       padding: 4px;
     }
     .flat:hover,
     .toggle:hover {
       font-family: "Iosevka Comfy Wide Medium Expanded";
       font-size: 16px;
       padding: 4px;
     }
     .flat:checked,
     .toggle:checked {
       font-family: "Iosevka Comfy Wide Medium Expanded";
       font-size: 16px;
       border-bottom: 3px solid white;
       padding: 4px;
     }
  '';

  xfconf.settings = {
    xfce4-session = { "startup/ssh-agent/enabled" = true; };
    xsettings = {
      "Gtk/MonospaceFontName" = "Iosevka Comfy Wide Medium Expanded 12";
      "Gtk/FontName" = "Inter 12";
      "Net/IconThemeName" = "Mint-Y";
      "Net/ThemeName" = "Mint-Y-Dark";
    };
    xfce4-panel = {
      # panel 1
      "panels/panel1/icon-size" = 24;
      "panels/panel1/length" = 100;
      "panels/panel1/plugin-ids" = [ 1 2 3 4 5 6 8 9 10 11 12 13 14 ];
      "panels/panel1/position" = "p=6;x=0;y=0";
      "panels/panel1/position-locked" = true;
      "panels/panel1/size" = 32;

      # panel 2
      "panels/panel2/autohide-behavior" = 1;
      "panels/panel2/background-style" = 0;
      "panels/panel2/length" = 1;
      "panels/panel2/plugin-ids" = [ 7 ];
      "panels/panel2/position" = "p=10;x=830;y=1025";
      "panels/panel2/position-locked" = true;
      "panels/panel2/size" = 62;
    };
    xfwm4 = {
      "general/theme" = "Mint-Y-Dark";
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
      "commands/custom/<Super>b" = "firefox";
      "commands/custom/<Super>;" = "1password";
      "commands/custom/<Super>p" = "";
      "commands/custom/<Super>n" = "";

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
