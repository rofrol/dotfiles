local wezterm = require 'wezterm';

return {
  default_prog = {"bash", "-l"},
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
  }
}
