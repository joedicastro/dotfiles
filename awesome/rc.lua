--                                     _
--                          _ __ ___  | |_   _  __ _
--                         | '__/ __| | | | | |/ _` |
--                         | | | (__ _| | |_| | (_| |
--                         |_|  \___(_)_|\__,_|\__,_|
--
-- {{{ License
--
-- rc.lua, works with awesome 3.5.1x
-- author: joedicastro <joe [at] joedicastro.com>
-- based on multiple rc.lua files from different awesome users
--
-- This work is licensed under the Creative Commons Attribution Share
-- Alike License: http://creativecommons.org/licenses/by-sa/3.0/
--
-- }}}

-- {{{ Cheat Sheet
-- {{{ Key Bindings
-- ===============================================================  KEY BINDINGS

------------------------------------------------------------ Launch Applications
--
-- Win  +  Enter                    Terminal
-- Win  +  F1                       Start gif screencast recording LowRes
-- Win  +  F2                       Start gif screencast recording HighRes
-- Win  +  F3                       Start gif screencast recording FullRes
-- Win  +  F4                       Start gif screencast the left half of screen
-- Win  +  F5                       Start gif screencast the upper left quadrant
-- Win  +  F6                       Stop gif screencast recording
-- Win  +  F7                       Start mkv screencast recording
-- Win  +  F8                       Stop mkv screencast recording
--
------------------------------------------------------------------ Shell prompts
--
-- Win  +  r                        Launch a command line prompt into status bar
-- Win  +  /                        Launch dmenu
--
--------------------------------------------------------------------- Navigation
--
-- Win  +  j/k                      Focus on next/previous client
-- Alt  +  h                        Previous tag
-- Alt  +  l                        Next tag
-- Win  +  1-9                      Show tag 1-9
-- Win  +  Control  +  j/k          Focus next/previous Screen
--
----------------------------------------------------------------- Client Control
--
-- Win  +  m                        Maximize client
-- Win  +  n                        Minimize client
-- Win  +  Control  +  n            Restore (=unminimize) a random client
-- Win  +  f                        Set client fullscreen
-- Win  +  c                        Kill focused client
-- Win  +  t                        Toggle "always visible" (on top)
-- Win  +  Shift    +  r            Redraw focused client
--
------------------------------------------------------------------------ Layouts
--
-- Win  +  Shift    +  j/k          Swap clients
-- Win  +  o                        Move client to next screen
-- Win  +  l/h                      Change master width by 5%
-- Win  +  Shift    +  l/h          Number of windows for master area +1/-1
-- Win  +  Control  +  l/h          Number of columns for stack area +1/-1
-- Win  +  Space                    Next layout
-- Win  +  Control  +  Space        Toggle client floating state
-- Win  +  Control  +  1-9          Enable/Disable view of tag 1-9
-- Win  +  Shift    +  1-9          Tag current client with 1-9 tag
-- Win  +  Shift  +  Ctrl  +  1-9   Enable/Disable tag 1-9 for current client
--
------------------------------------------------------------------ Miscellaneous
--
-- Win  +  b                        Hide/Show status bar (Wibox)
-- Win  +  y                        Lock Screen
-- Print Screen                     Take a screenshot
-- Win  +  z/Z                      Screen zoom in/out
--
---------------------------------------------------------------- Awesome Control
--
-- Win  +  Ctrl     +  r            Restart Awesome
-- Win  +  Shift    +  q            Quit Awesome
-- Win  +  w                        Show Awesome menu
--
---------------------------------------------------------------- Multimedia keys
--
-- Play media key              Play mpd song in playlist
-- Stop media key              Stop mpd reproduction
-- Prev media key              Previous song in mpd playlist
-- Next media key              Next song in mpd playlist
-- Raise volume media key      Raise Volume 2dB
-- Lower volume media key      Lower Volume 2dB
-- Mute media key              Mute volume
--
-- }}}

-- {{{ Mouse Bindings
-- =============================================================  MOUSE BINDINGS

------------------------------------------------------------ Layout modification
--
-- Win + Button 1 on client         Move window
-- Win + Button 3 on client         Resize window
--
--
-- }}}
-- }}}

-- {{{ Load libraries
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Vicious library
vicious = require("vicious")
vicious.contrib = require("vicious.contrib")
-- Eminent library
require("eminent")
-- }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    awful.util.spawn_with_shell("notify-send -u 'critical' " ..
                                "'Oops, there were errors during startup!'" ..
                                awesome.startup_errors
                               )
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        awful.util.spawn_with_shell("notify-send -u 'critical' " ..
                                    "'Oops, an error happened!'" ..
                                    err
                                    )

        in_error = false
    end)
end
-- }}}

-- {{{ Run only one instance per program
function run_once(cmd)
  findme = cmd
  firstspace = cmd:find(" ")
  if firstspace then
    findme = cmd:sub(0, firstspace-1)
  end
  awful.util.spawn_with_shell("pgrep -u $USER -x " .. findme ..
                              " > /dev/null || (" .. cmd .. ")")
end
-- }}}

-- {{{ Some initializations
run_once('xcalib -c')
run_once('xcalib -co 92 -a')

-- set the local settings
os.setlocale('es_ES.UTF-8')
-- }}}

-- {{{ Variable definitions
-- Directories
home_dir = os.getenv("HOME")
cfg_dir = awful.util.getdir("config")
theme_dir = cfg_dir .. "/themes"

-- Themes define colours, icons, and wallpapers
beautiful.init(theme_dir .. "/itaca/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
editor = os.getenv("EDITOR") or "gvim"

-- Set the screen resolution for proper handling in various tasks.
scr_res = "1920x1200"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with
-- others.
modkey = "Mod4"
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Layouts
-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.max.fullscreen,
}
-- }}}

-- {{{ Tags
-- define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- numerals in Greek, cool!
    tags[s] = awful.tag({ "α", "β", "γ", "δ", "ε", "ς", "ζ", "η", "θ"}, s,
                        layouts[1])
end
-- }}}

-- {{{ Menu
-- create a launcher widget and a main menu
mydebugmenu = {
    { "crear rc_test.lua", cfg_dir .. "/awdt.py new" },
    { "editar rc_test.lua", editor .. " " .. cfg_dir .. "/rc_test.lua" },
    { "chequear rc_test.lua", cfg_dir .. "/awdt.py check" },
    { "iniciar test FullHD", cfg_dir .. "/awdt.py start -t -d 1"},
    { "iniciar test WXGA+", cfg_dir .. "/awdt.py start -t -s 1440x900 -d 2"},
    { "iniciar default WXGA+", cfg_dir .. "/awdt.py start -s 1440x900 -d 3"},
    { "reiniciar awesomes", cfg_dir .. "/awdt.py restart" },
    { "parar xephyr", cfg_dir .. "/awdt.py stop" },
    { "ver debug.log", editor .. " -R " .. cfg_dir .. "/awdt.log" },
}

myawesomemenu = {
    { "manual", terminal .. " -e man awesome" },
    { "editar configuración", editor .. " " .. cfg_dir .. "/rc.lua" },
    { "debug", mydebugmenu },
    { "reiniciar", awesome.restart },
    { "salir", awesome.quit }
}

mymainmenu = awful.menu({
    items = {
        { "awesome", myawesomemenu, beautiful.awesome_icon },
        { "abrir terminal", terminal }
    }
})
-- }}}

-- {{{ Wibox

-- {{{ Wibox Widgets
-- Widgets to show on Wibox

-- {{{ MPD widget
mpdwidget = wibox.widget.textbox()
vicious.register(mpdwidget, vicious.widgets.mpd,
    function (widget, args)
        if args["{state}"] == "Stop" then
            return ""
        elseif args["{state}"] == "Pause" then
            return '<span color="#404040">' .. args["{Artist}"]..' - '..
                    args["{Title}"] .. '</span>'
        else
            return args["{Artist}"]..' - '.. args["{Title}"]
        end
    end, 2)
-- }}}

-- {{{ Mem widget
memwidget = wibox.widget.textbox()
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem, "$1% $2MB", 10)
-- }}}

-- {{{ Cpu widget
cpuwidget = wibox.widget.textbox()
cpuwidget.width, cpuwidget.align = 150, "center"
vicious.cache(vicious.widgets.cpu)
vicious.register(cpuwidget, vicious.widgets.cpu, "$1% ", 3)
-- }}}

-- {{{ Filesystem widget
-- fswidget = wibox.widget.textbox()
-- vicious.cache(vicious.widgets.fs)
-- vicious.register(fswidget, vicious.widgets.fs,
--     "/ ${/ avail_p}% ~ ${/home avail_p}%", 61)
-- }}}

-- {{{ CPU Temperature & Fans velocity
cputemp = wibox.widget.textbox()
vicious.register(cputemp, vicious.contrib.sensors, " $1 ºC" , 3, "Physical id 0")
-- fan180mm = wibox.widget.textbox()
-- vicious.register(fan180mm, vicious.contrib.sensors, " $1 RPM" , 5, "fan4")
-- fan120mm = widget({ type = "textbox" })
-- vicious.register(fan120mm, vicious.contrib.sensors, " $1 RPM" , 5, "fan2")
-- }}}

-- {{{ Network usage widget
netwidget = wibox.widget.textbox()
vicious.cache(vicious.widgets.net)
vicious.register(netwidget, vicious.widgets.net,
                '<span color="#CC9393">${enp0s25 down_kb}</span>' ..
                ' <span color="#7F9F7F">${enp0s25 up_kb}</span>', 2)
-- }}}

-- {{{ Textclock widget
mytextclock = awful.widget.textclock(" %a %d %b %H:%M ", 15)
-- }}}

-- Sound Volume {{{
mute = wibox.widget.textbox()
vicious.register(mute, vicious.widgets.volume, "$2", 2, "Master")
soundvol = wibox.widget.textbox()
vicious.register(soundvol, vicious.widgets.volume, "$1%", 2, "PCM")
-- }}}

-- {{{ Space & Separator
space = wibox.widget.textbox()
space:set_text('     ')
-- }}}

-- }}}

-- {{{ Wibox itself
-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mytaglist = {}
mytasklist = {}

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()

    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.focused)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s, height = "17" })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mytaglist[s])
    left_layout:add(space)
    left_layout:add(mypromptbox[s])
    left_layout:add(space)

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(space)
    right_layout:add(mpdwidget)
    right_layout:add(space)
    right_layout:add(space)
    right_layout:add(cputemp)
    right_layout:add(space)
    -- right_layout:add(fan180mm)
    -- right_layout:add(space)
    right_layout:add(cpuwidget)
    right_layout:add(space)
    right_layout:add(memwidget)
    right_layout:add(space)
    right_layout:add(netwidget)
    right_layout:add(space)
    -- right_layout:add(fswidget)
    -- right_layout:add(space)
    right_layout:add(mute)
    right_layout:add(soundvol)
    right_layout:add(space)
    right_layout:add(mytextclock)
    if s == 1 then right_layout:add(wibox.widget.systray()) end

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)

end
-- }}}
-- }}}

-- {{{ Key bindings
-- {{{ Global Key bindings
globalkeys = awful.util.table.join(
    awful.key({ "Mod1",           }, "h",  awful.tag.viewprev       ),
    awful.key({ "Mod1",           }, "l",  awful.tag.viewnext       ),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w",
        function ()
            awful.menu.menu_keys.down = {"j"}
            awful.menu.menu_keys.up = {"k"}
            awful.menu.menu_keys.exec = {"l"}
            awful.menu.menu_keys.back = {"h"}
            mymainmenu:show({keygrabber=true})
        end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j",
        function ()
            awful.client.swap.byidx(  1)
        end),
    awful.key({ modkey, "Shift"   }, "k",
        function ()
            awful.client.swap.byidx( -1)
        end),
    awful.key({ modkey, "Control" }, "j",
        function ()
            awful.screen.focus_relative( 1)
        end),
    awful.key({ modkey, "Control" }, "k",
        function ()
            awful.screen.focus_relative(-1)
        end),

    -- hide / show Wibox
    awful.key({ modkey }, "b",
        function ()
            mywibox[mouse.screen].visible = not mywibox[mouse.screen].visible
        end),

    -- dmenu
    awful.key({ modkey }, "/",
        function()
            awful.util.spawn_with_shell(
                "export LANGUAGE=en_US.UTF8;export PATH=$PATH:~/.bin;" .. 
                "dmenu_run -b -i -fn " ..
                "'-*-dejavu sans mono-*-r-*-*-16-*-*-*-*-*-*-*' -p 'run:'")
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return",
        function ()
            awful.util.spawn(terminal)
        end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",
        function ()
            awful.tag.incmwfact( 0.05)
        end),
    awful.key({ modkey,           }, "h",
        function ()
            awful.tag.incmwfact(-0.05)
        end),
    awful.key({ modkey, "Shift"   }, "h",
        function ()
            awful.tag.incnmaster( 1)
        end),
    awful.key({ modkey, "Shift"   }, "l",
        function ()
            awful.tag.incnmaster(-1)
        end),
    awful.key({ modkey, "Control" }, "h",
        function ()
            awful.tag.incncol( 1)
        end),
    awful.key({ modkey, "Control" }, "l",
        function ()
            awful.tag.incncol(-1)
        end),
    awful.key({ modkey,           }, "space",
        function ()
            awful.layout.inc(layouts,  1)
        end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Capture screen
    awful.key({ }, "Print",
        function ()
            awful.util.spawn("scrot -q 100")
        end),

    -- Lock screen
    awful.key({ modkey }, "y",
        function ()
            awful.util.spawn("python2 " .. cfg_dir .."/slimlock.py")
        end),

   -- Multimedia keys
    awful.key({ }, "XF86AudioPlay",
        function ()
            awful.util.spawn("mpc toggle")
        end),
    awful.key({ }, "XF86AudioStop",
        function ()
            awful.util.spawn("mpc stop")
        end),
    awful.key({ }, "XF86AudioPrev",
        function ()
            awful.util.spawn("mpc prev")
        end),
    awful.key({ }, "XF86AudioNext",
        function ()
            awful.util.spawn("mpc next")
        end),

    awful.key({ }, "XF86AudioRaiseVolume",
        function ()
            awful.util.spawn("amixer sset PCM 2dB+")
        end),
    awful.key({ }, "XF86AudioLowerVolume",
        function ()
            awful.util.spawn("amixer sset PCM 2dB-")
        end),
    awful.key({ }, "XF86AudioMute",
        function ()
            awful.util.spawn("amixer sset Master toggle")
        end),

    -- Prompt
    awful.key({ modkey }, "r",
        function ()
            mypromptbox[mouse.screen]:run()
        end),

    -- Screen Zoom In/Out
    awful.key({ modkey }, "z",
        function ()
            awful.util.spawn("xrandr --output HDMI1 --mode 1024x768" ..
                            " --panning 1920x1200")
        end),

    awful.key({ modkey, "Shift" }, "z",
        function ()
            awful.util.spawn("xrandr --output HDMI1 --mode 1920x1200" ..
                            " --panning 1920x1200")
        end),

    -- Record screencasts
    awful.key({ modkey }, "F1",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.util.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                            " -r 2 -i :0.0 -b:v 500k -pix_fmt rgb24 -y" ..
                            " -loop 0 -s 640x400 " .. home_dir ..
                            "/animated.gif")
        end),

    awful.key({ modkey }, "F2",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.util.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                            " -r 2 -i :0.0 -b:v 500k -pix_fmt rgb24 -y" ..
                            " -loop 0 -s 1440x900 " .. home_dir ..
                            "/animated.gif")
        end),

    awful.key({ modkey }, "F3",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.util.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                            " -r 2 -i :0.0 -b:v 500k -pix_fmt rgb24 -y" ..
                            " -loop 0 -s " .. scr_res .. " " .. home_dir ..
                            "/animated.gif")
        end),

    awful.key({ modkey }, "F4",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.util.spawn("ffmpeg -f x11grab -s 957x1180" ..
                            " -r 2 -i :0.0+2,19 -b:v 500k -pix_fmt rgb24 -y" ..
                            " -loop 0 " .. home_dir ..  "/animated.gif")
        end),

    awful.key({ modkey }, "F5",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.util.spawn("ffmpeg -f x11grab -s 860x588" ..
                            " -r 2 -i :0.0+2,19 -b:v 500k -pix_fmt rgb24 -y" ..
                            " -loop 0 " .. home_dir ..  "/animated.gif")
        end),

    awful.key({ modkey }, "F6",
        function ()
            awful.util.spawn("killall ffmpeg")
            awful.util.spawn("convert ephemeral:" .. home_dir ..
                             "/animated.gif -fuzz 7% -layers Optimize " ..
                             home_dir .. "/screencast.gif")
        end),

    awful.key({ modkey }, "F7",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.mkv")
            awful.util.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                            " -r 25 -i :0.0 -sameq " .. home_dir ..
                            "/screencast.mkv")
            end),

    awful.key({ modkey }, "F8",
        function ()
            awful.util.spawn("killall ffmpeg")
        end)

)
-- }}}

-- {{{ Per client Key bindings
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
        end),
    awful.key({ modkey,           }, "c",
        function (c)
            c:kill()
        end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen),
    awful.key({ modkey, "Shift"   }, "r",
        function (c)
            c:redraw()
        end),
    awful.key({ modkey,           }, "t",
        function (c)
            c.ontop = not c.ontop
        end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)
-- }}}

-- {{{ Tags bindings
-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end
-- }}}

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "Firefox" },
      properties = { floating = false } },
    { rule = { class = "Thunderbird" },
      properties = { floating = false } },
    { rule = { class = "Chromium" },
      properties = { floating = false } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "Gvim" },
      properties = { size_hints_honor = false } },
    { rule = { class = "URxvt" },
      properties = { size_hints_honor = false } },
    { rule = { class = "Xephyr" },
      callback = awful.placement.centered }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    if not startup then
        -- Put windows in a smart way, only if they does not set an initial
        -- position.
        if not c.size_hints.user_position and
            not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)
-- }}}

-- {{{ Folding for Vim
-- This fold the sections in vim, for a better handling
-- vim:foldmethod=marker
-- }}}
