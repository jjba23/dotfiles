{
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
    "org/gnome/shell/weather" = { automatic-location = true; };
    "org/gnome/shell/extensions/vitals" = {
      hot-sensors =
        [ "_memory_usage_" "_processor_usage_" "_temperature_processor_0_" ];
    };
  };

}

