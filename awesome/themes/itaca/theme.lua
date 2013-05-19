-- itaca, awesome3 theme, by joedicastro

--{{{ Main
local awful = require("awful")
awful.util = require("awful.util")

theme = {}

themedir        = awful.util.getdir("config") .. "/themes/itaca"

--}}}

-- {{{ Background.
theme.wallpaper = themedir .. "/kiss.jpg"
--}}}

-- {{{ Miscellaneous
theme.font          = "Ubuntu Bold 9"
theme.taglist_font  = "Ubuntu Bold 9"
theme.tasklist_font = "Ubuntu Bold 8"

theme.bg_normal     = "#000000"
theme.bg_focus      = "#000000"
theme.bg_urgent     = "#00ff00"
theme.bg_minimize   = "#000000"

theme.fg_normal     = "#999999"
theme.fg_focus      = "#87FFFF"
theme.fg_urgent     = "#111111"
theme.fg_minimize   = "#000000"

theme.border_width  = 0
theme.border_normal = "#333333"
theme.border_focus  = "#1279bf"
theme.border_marked = "#91231c"


theme.bg_widget        = "#333333"
theme.fg_widget        = "#908884"
theme.fg_center_widget = "#636363"
theme.fg_end_widget    = "#ffffff"
theme.fg_off_widget    = "#22211f"

theme.taglist_squares_sel         = themedir .. "/tasklist_f.png"
theme.taglist_squares_unsel       = themedir .. "/tasklist_u.png"
theme.tasklist_floating_icon      = themedir .. "/floating.png"

theme.menu_submenu_icon = themedir .. "/submenu.png"
theme.menu_height   = 16
theme.menu_width    = 180

theme.awesome_icon = themedir .. "/awesome16.png"
-- }}}

return theme

-- {{{ Folding for Vim
-- This fold the sections in vim, for a better handling
-- vim:foldmethod=marker
-- }}}
