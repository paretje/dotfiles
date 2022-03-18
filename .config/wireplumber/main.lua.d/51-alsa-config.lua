rule = {
  matches = {
    {
      { "device.name", "matches", "alsa_card.*" },
    },
  },
  apply_properties = {
    -- TODO: get home dir
    -- TODO: best location?
    ["device.profile-set"] = "/home/kevin/.config/pipewire/alsa-card-profile/laptop.conf",
  },
}

table.insert(alsa_monitor.rules, rule)
