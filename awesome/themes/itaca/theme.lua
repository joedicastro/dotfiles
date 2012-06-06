-- itaca, awesome3 theme, by joedicastro 

--{{{ Main
require("awful.util")

theme = {}

home          = os.getenv("HOME")
config        = awful.util.getdir("config")
shared        = "/usr/share/awesome"
if not awful.util.file_readable(shared .. "/icons/awesome16.png") then
    shared    = "/usr/share/local/awesome"
end
sharedicons   = shared .. "/icons"
sharedthemes  = shared .. "/themes"
themes        = config .. "/themes"
themename     = "/itaca"
if not awful.util.file_readable(themes .. themename .. "/theme.lua") then
       themes = sharedthemes
end
themedir      = themes .. themename

-- {{{ Background.
wallpaper1    = themedir .. "/background.jpg"
wallpaper2    = themedir .. "/background.png"
wallpaper3    = sharedthemes .. "/zenburn/zenburn-background.png"
wallpaper4    = sharedthemes .. "/default/background.png"
wpscript      = home .. "/.wallpaper"

if awful.util.file_readable(wallpaper1) then
	theme.wallpaper_cmd = { "awsetbg " .. wallpaper1 }
elseif awful.util.file_readable(wallpaper2) then
	theme.wallpaper_cmd = { "awsetbg " .. wallpaper2 }
elseif awful.util.file_readable(wpscript) then
	theme.wallpaper_cmd = { "sh " .. wpscript }
elseif awful.util.file_readable(wallpaper3) then
	theme.wallpaper_cmd = { "awsetbg " .. wallpaper3 }
else
	theme.wallpaper_cmd = { "awsetbg " .. wallpaper4 }
end

if awful.util.file_readable(config .. "/vain/init.lua") then
    theme.useless_gap_width  = "3"
end
--}}}
--}}}

-- {{{ Miscellaneous
theme.font          = "Ubuntu Bold 10"
theme.taglist_font  = "Ubuntu Bold 10"
theme.tasklist_font = "Ubuntu Bold 8"

theme.bg_normal     = "#000000"
theme.bg_focus      = "#000000"
theme.bg_urgent     = "#00ff00"
theme.bg_minimize   = "#000000"

theme.fg_normal     = "#999999"
theme.fg_focus      = "#87FFFF"
theme.fg_urgent     = "#111111"
theme.fg_minimize   = "#000000"

theme.border_width  = 2
theme.border_normal = "#333333"
theme.border_focus  = "#1279bf"
theme.border_marked = "#91231c"


theme.bg_widget        = "#333333"
theme.fg_widget        = "#908884"
theme.fg_center_widget = "#636363"
theme.fg_end_widget    = "#ffffff"
theme.fg_off_widget    = "#22211f"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

theme.taglist_squares_sel         = themedir .. "/tasklist_f.png"
theme.taglist_squares_unsel       = themedir .. "/tasklist_u.png"
theme.tasklist_floating_icon      = themedir .. "/floating.png"

theme.titlebar_close_button_normal = sharedthemes .. "/default/titlebar/close.png"
theme.titlebar_close_button_focus  = sharedthemes .. "/default/titlebar/closer.png"

theme.menu_submenu_icon = sharedthemes .. "/default/submenu.png"
theme.menu_height   = 16
theme.menu_width    = 180

theme.awesome_icon = themedir .. "/awesome16.png"
-- }}}

-- Titlebar icons {{{
theme.titlebar_close_button_normal = themedir .. "/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themedir .. "/titlebar/close_focus.png"

theme.titlebar_ontop_button_normal_inactive = themedir .. "/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = themedir .. "/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themedir .. "/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = themedir .. "/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themedir .. "/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = themedir .. "/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themedir .. "/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = themedir .. "/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themedir .. "/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = themedir .. "/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themedir .. "/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = themedir .. "/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themedir .. "/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themedir .. "/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themedir .. "/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themedir .. "/titlebar/maximized_focus_active.png"
-- }}}



-- {{{ Layout icons
theme.layout_tile       = themedir .. "/layouts/tile-red.png"
theme.layout_tileleft   = themedir .. "/layouts/tileleft-blue.png"
theme.layout_tilebottom = themedir .. "/layouts/tilebottom-red.png"
theme.layout_tiletop    = themedir .. "/layouts/tiletop-blue.png"
theme.layout_fairv      = themedir .. "/layouts/fairv-red.png"
theme.layout_fairh      = themedir .. "/layouts/fairh-blue.png"
theme.layout_spiral     = themedir .. "/layouts/spiral-red.png"
theme.layout_dwindle    = themedir .. "/layouts/dwindle-blue.png"
theme.layout_max        = themedir .. "/layouts/max-green.png"
theme.layout_fullscreen = themedir .. "/layouts/fullscreen-greyish.png"
theme.layout_magnifier  = themedir .. "/layouts/magnifier-green.png"
theme.layout_floating   = themedir .. "/layouts/floating-greyish.png"
-- }}}

return theme

-- {{{ Folding for Vim
-- This fold the sections in vim, for a better handling
-- vim:foldmethod=marker
-- }}}
