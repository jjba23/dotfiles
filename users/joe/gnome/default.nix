{ pkgs, osConfig, ... }:
let

  favoriteApps = map favoriteApp [
    "emacs"
    "org.gnome.Terminal"
    "org.gnome.Nautilus"
    "org.gnome.Geary"
    "firefox"
    "org.gnome.Settings"
    "org.gnome.SystemMonitor"
    "org.pitivi.Pitivi"
    "1password"
    "org.gnome.Calendar"
    "org.gnome.Calculator"
    "org.gnome.Characters"
    "org.gnome.Weather"
  ];
  myCustomKeybindingNames =
    map mkBindingStr [ "term" "emacs" "gnome-characters" "eyedropper" ];

  mkBindingStr = x:
    "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/${x}/";

  mkBinding =
    { withSuper ? false, withShift ? false, withControl ? false, key }:
    let
      superString = if withSuper then "<Super>" else "";
      controlString = if withControl then "<Control>" else "";
      shiftString = if withShift then "<Shift>" else "";
    in "${controlString}${superString}${shiftString}${key}";
  terminalProfile = {
    visible-name = "joe";
    use-system-font = false;
    font = "Mononoki 11";
    audible-bell = false;
    scrollback-lines = 1000000;
    use-theme-colors = true;
  };

  favoriteApp = x: "${x}.desktop";
  shellKeybindings = {
    show-screenshot-ui = [
      (mkBinding {
        withSuper = true;
        key = "period";
      })
    ];
    screenshot = [
      (mkBinding {
        withSuper = true;
        withShift = true;
        key = "period";
      })
    ];
    show-screen-recording-ui = [ ];
    screenshot-window = [ ];

    focus-active-notification = [ ];
    toggle-quick-settings = [
      (mkBinding {
        withSuper = true;
        key = "Delete";
      })
    ];
    switch-to-application-1 = [ ];
    switch-to-application-2 = [ ];
    switch-to-application-3 = [ ];
    switch-to-application-4 = [ ];
    switch-to-application-5 = [ ];
    switch-to-application-6 = [ ];
    switch-to-application-7 = [ ];
    switch-to-application-8 = [ ];
    switch-to-application-9 = [ ];
    toggle-message-tray = [
      (mkBinding {
        withSuper = true;
        key = "[";
      })
    ];
  };
  mediaKeys = {
    home = [
      (mkBinding {
        withSuper = true;
        key = "f";
      })
    ];
    calculator = [
      (mkBinding {
        withSuper = true;
        key = "c";
      })
    ];
    www = [
      (mkBinding {
        withSuper = true;
        key = "b";
      })
    ];
    control-center = [
      (mkBinding {
        withSuper = true;
        key = "s";
      })
    ];
    search = [
      (mkBinding {
        withSuper = true;
        key = "space";
      })
    ];
    custom-keybindings = myCustomKeybindingNames;
  };
  wmKeybindings = {
    switch-input-source = [ ];
    switch-input-source-backward = [ ];
    close = [
      (mkBinding {
        withSuper = true;
        key = "k";
      })
    ];
    switch-to-workspace-left = [
      (mkBinding {
        withSuper = true;
        key = "p";
      })
    ];
    switch-to-workspace-right = [
      (mkBinding {
        withSuper = true;
        key = "n";
      })
    ];
    focus-active-notification = [ ];
    move-to-workspace-1 = [
      (mkBinding {
        withSuper = true;
        withShift = true;
        key = "1";
      })
    ];
    switch-to-workspace-1 = [
      (mkBinding {
        withSuper = true;
        key = "1";
      })
    ];
    move-to-workspace-2 = [
      (mkBinding {
        withSuper = true;
        withShift = true;
        key = "2";
      })
    ];
    switch-to-workspace-2 = [
      (mkBinding {
        withSuper = true;
        key = "2";
      })
    ];
    move-to-workspace-3 = [
      (mkBinding {
        withSuper = true;
        withShift = true;
        key = "3";
      })
    ];
    switch-to-workspace-3 = [
      (mkBinding {
        withSuper = true;
        key = "3";
      })
    ];
    move-to-workspace-4 = [
      (mkBinding {
        withSuper = true;
        withShift = true;
        key = "4";
      })
    ];
    switch-to-workspace-4 = [
      (mkBinding {
        withSuper = true;
        key = "4";
      })
    ];
    move-to-workspace-right = [
      (mkBinding {
        withSuper = true;
        withShift = true;
        key = "n";
      })
    ];
    move-to-workspace-left = [
      (mkBinding {
        withSuper = true;
        withShift = true;
        key = "p";
      })
    ];
    move-to-workspace-first = [
      (mkBinding {
        withSuper = true;
        withShift = true;
        key = "a";
      })
    ];
    switch-to-workspace-first = [
      (mkBinding {
        withSuper = true;
        key = "a";
      })
    ];
    move-to-workspace-last = [ ];
    switch-to-workspace-last = [ ];
  };
  terminalKeybindings = {
    copy = "<Alt>w";
    paste = "<Primary>y";
    find = "<Primary>s";
  };
in {
  dconf.settings = {
    "org/gnome/settings-daemon/plugins/power" = {
      power-button-action = "nothing";
    };
    "org/gnome/settings-daemon/plugins/media-keys" = mediaKeys;

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/term" = {
      binding = mkBinding {
        withSuper = true;
        key = "t";
      };
      command = "gnome-terminal";
      name = "Open a terminal window";
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/emacs" = {
      binding = mkBinding {
        withSuper = true;
        key = "e";
      };
      command = "emacs";
      name = "Open a new Emacs session";
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/gnome-characters" =
      {
        binding = mkBinding {
          withSuper = true;
          key = "j";
        };
        command = "gnome-characters";
        name = "Open a emoji & char picker";
      };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/eyedropper" =
      {
        binding = mkBinding {
          withSuper = true;
          key = "y";
        };
        command = "eyedropper";
        name = "Color picker";
      };

    "org/gnome/shell/keybindings" = shellKeybindings;
    "org/gnome/desktop/interface" = {
      show-battery-percentage = true;
      gtk-key-theme = "Emacs";
      color-scheme = if osConfig.masterOptions.joe.lightMode.enable then
        "default"
      else
        "prefer-dark";
      clock-show-weekday = true;
    };
    "org/gnome/mutter" = {
      dynamic-workspaces = true;
      center-new-windows = true;
    };
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = with pkgs.gnomeExtensions; [
        blur-my-shell.extensionUuid
        gsconnect.extensionUuid
        places-status-indicator.extensionUuid
        tiling-shell.extensionUuid
        removable-drive-menu.extensionUuid
        vitals.extensionUuid
        move-clock.extensionUuid
        caffeine.extensionUuid
        dash-to-dock.extensionUuid
      ];
      favorite-apps = favoriteApps;
    };
    "org/gnome/desktop/wm/preferences" = {
      focus-mode = "sloppy";
      auto-raise = true;
      button-layout = "appmenu:minimize,maximize,close";
    };
    "org/gnome/desktop/input-sources" = {
      xkb-options = [ "terminate:ctrl_alt_bksp" "caps:ctrl_modifier" ];
    };
    "org/gnome/desktop/peripherals/touchpad" = {
      click-method = "fingers";
      natural-scroll = false;
      two-finger-scrolling-enabled = true;
    };
    "org/gnome/desktop/wm/keybindings" = wmKeybindings;
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = true;
      night-light-temperature = 3100;
    };
    "org/gnome/terminal/legacy/profiles:/:8dcb80c6-c836-4a6c-aeaf-087be449f5c2" =
      terminalProfile;
    "org/gnome/terminal/legacy/profiles:" = {
      list = [ "8dcb80c6-c836-4a6c-aeaf-087be449f5c2" ];
    };
    "org/gnome/terminal/legacy/keybindings" = terminalKeybindings;
    "org/gnome/desktop/sound" = { allow-volume-above-100-percent = true; };
    "org/gnome/shell/extensions/dash-to-dock" = { hot-keys = false; };
  };
}

