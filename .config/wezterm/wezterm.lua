local wezterm = require 'wezterm';

return {
  --default_prog = {"bash", "-l"},
  --default_prog = {wezterm.home_dir .. "/AppData/Local/Programs/Git/bin/bash", "-l"},
  default_prog = {wezterm.home_dir .. "/scoop/apps/git/current/bin/bash", "-l"},
  launch_menu = {
    {
      args = {"top"},
    },
    {
      -- Optional label to show in the launcher. If omitted, a label
      -- is derived from the `args`
      label = "Bash",
      -- The argument array to spawn.  If omitted the default program
      -- will be used as described in the documentation above
      args = {"bash", "-l"},

      -- You can specify an alternative current working directory;
      -- if you don't specify one then a default based on the OSC 7
      -- escape sequence will be used (see the Shell Integration
      -- docs), falling back to the home directory.
      -- cwd = "/some/path"

      -- You can override environment variables just for this command
      -- by setting this here.  It has the same semantics as the main
      -- set_environment_variables configuration option described above
      -- set_environment_variables = { FOO = "bar" },
    },
  },
  --only with this light theme I can see command line in nvim
  color_scheme = "Tango Adapted",
  --dpi = 96.0,
  --font = wezterm.font("Campbell"),
  --font = wezterm.font("Consolas"),
  --only this font has acceptable smoothing
  font = wezterm.font("Ubuntu Mono"),
  font_size = 18.0,
  --font_antialias = "None",
  --font_antialias = "Greyscale",
  --font_antialias = "Subpixel",
  --font_hinting = "None",
  --font_hinting = "Vertical",
  --font_hinting = "VerticalSubpixel",
  --font_hinting = "Full",
  font_rules= {
    {
      italic = false,
      bold = false,
      font = wezterm.font("Consolas"),
    },
    {
      italic = false,
      bold = false,
      font = wezterm.font("Ubuntu Mono"),
    },
  },
  colors = {
    tab_bar = {

      -- The color of the strip that goes along the top of the window
      background = "#ffffff",

      -- The active tab is the one that has focus in the window
      active_tab = {
        -- The color of the background area for the tab
        bg_color = "#eeeeee",
        -- The color of the text for the tab
        fg_color = "#000000",

        -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
        -- label shown for this tab.
        -- The default is "Normal"
        intensity = "Normal",

        -- Specify whether you want "None", "Single" or "Double" underline for
        -- label shown for this tab.
        -- The default is "None"
        underline = "None",

        -- Specify whether you want the text to be italic (true) or not (false)
        -- for this tab.  The default is false.
        italic = false,

        -- Specify whether you want the text to be rendered with strikethrough (true)
        -- or not for this tab.  The default is false.
        strikethrough = false,
      },

      -- Inactive tabs are the tabs that do not have focus
      inactive_tab = {
        bg_color = "#aaaaaa",
        fg_color = "#444444",

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `inactive_tab`.
      },

      -- You can configure some alternate styling when the mouse pointer
      -- moves over inactive tabs
      inactive_tab_hover = {
        bg_color = "#ffffff",
        fg_color = "#000000",

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `inactive_tab_hover`.
      }
    }
  }
}
