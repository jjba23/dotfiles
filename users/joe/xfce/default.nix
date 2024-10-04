{

  gtk = {
    enable = true;
    font = {
      name = "Roboto Condensed";
      size = 12;
    };
    theme.name = "Mint-Y-Dark";
  };

  home.file.".config/gtk-3.0/gtk.css".text = ''
    .xfce4-panel {
        font-family: "Roboto Condensed";
        background-color: rgba(22,22,22,0.8);        
     }
      
     .xfce4-panel .tasklist .toggle :checked {
       /*border-radius: 10px;*/
     }
      
     .tasklist button:checked {
       border-bottom: 3px solid white;
     }
      
     .flat,
     .toggle {
       font-family: "Roboto Condensed";
       font-size: 16px;
       padding: 4px;
     }
     .flat:hover,
     .toggle:hover {
       font-family: "Roboto Condensed";
       font-size: 16px;
       padding: 4px;
     }
     .flat:checked,
     .toggle:checked {
       font-family: "Roboto Condensed";
       font-size: 16px;
       border-bottom: 3px solid white;
       padding: 4px;
     }
      VteTerminal, vte-terminal {
        padding: 14px;
      } 
  '';

  xfconf.settings = {
    xfce4-session = { "startup/ssh-agent/enabled" = true; };
    xsettings = {
      "Gtk/MonospaceFontName" = "Roboto Mono 12";
      "Gtk/FontName" = "Roboto Condensed 13";
      "Net/IconThemeName" = "Mint-Y";
      "Net/ThemeName" = "Mint-Y-Dark";
    };
    xfce4-panel = {
      # panel 1
      "panels/panel-1/icon-size" = 24;
      "panels/panel-1/length" = 100;
      "panels/panel-1/plugin-ids" = [ 1 16 5 4 3 6 8 9 10 11 12 13 14 ];
      "panels/panel-1/position" = "p=6;x=0;y=8";
      "panels/panel-1/position-locked" = true;
      "panels/panel-1/size" = 38;

      # panel 2
      "panels/panel-2/autohide-behavior" = 1;
      "panels/panel-2/length" = 100;
      "panels/panel-2/plugin-ids" = [ 7 ];
      "panels/panel-2/position" = "p=10;x=0;y=1025";
      "panels/panel-2/position-locked" = true;
      "panels/panel-2/size" = 68;

      # plugins
      "plugins/plugin-14" = "actions";
      "plugins/plugin-16" = "directorymenu";
      "plugins/plugin-3/expand" = true;
      "plugins/plugin-3/style" = 0;
      "plugins/plugin-4" = "pager";
      "plugins/plugin-4/miniature-view" = false;
      "plugins/plugin-4/rows" = 1;
      "plugins/plugin-5/style" = 0;
      "plugins/plugin-12" = "clock";
      "plugins/plugin-12/digital-date-font" = "Roboto Condensed 13";
      "plugins/plugin-12/digital-date-format" = "%A %e %b, %H:%M";
      "plugins/plugin-12/digital-layout" = 2;
      "plugins/plugin-12/digital-time-font" = "Roboto Condensed 13";
      "plugins/plugin-12/tooltip-format" = "%A %d %B %Y";
      "plugins/plugin-1" = "whiskermenu";
      "plugins/plugin-1/button-icon" = "format-justify-center";
      "plugins/plugin-1/button-title" = "  menu";
      "plugins/plugin-1/default-category" = 0;
      "plugins/plugin-1/launcher-show-name" = false;
      "plugins/plugin-1/position-categories-alternate" = false;
      "plugins/plugin-1/position-categories-horizontal" = false;
      "plugins/plugin-1/position-profile-alternate" = true;
      "plugins/plugin-1/position-search-alternate" = false;
      "plugins/plugin-1/recent-items-max" = 15;
      "plugins/plugin-1/show-button-title" = true;
      "plugins/plugin-1/view-mode" = 1;
      "plugins/plugin-7" = "dockbarx";
      "plugins/plugin-7/block-autohide" = false;
    };
    xfwm4 = {
      "general/theme" = "Mint-Y-Dark";
      "general/title_font" = "Roboto Condensed 13";
    };
    xfce4-terminal = {
      "color-background" = "#1a731a731a73";
      "color-foreground" = "#f6f6f5f5f4f4";
      "font-name" = "Roboto Mono 12";
    };
    xfce4-keyboard-shortcuts = {
      # commands
      "commands/custom/<Super>c" = "exo-open --launch TerminalEmulator";
      "commands/custom/<Super>e" = "emacsclient -c";
      "commands/custom/<Super>t" = "mousepad -c";
      "commands/custom/<Super>f" = "thunar";
      "commands/custom/<Super>b" = "firefox";
      "commands/custom/<Super>;" = "1password";
      "commands/custom/<Super>r" = "xfce4-appfinder";
      "commands/custom/<Super>p" = null;
      "commands/custom/<Super>n" = null;

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
