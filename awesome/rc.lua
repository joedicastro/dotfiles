-- {{{ License
-- rc.lua, works with awesome 3.4.1x
-- author: joedicastro <joe [at] joedicastro.com>
-- based on multiple rc.lua files from different awesome users
--
-- This work is licensed under the Creative Commons Attribution Share
-- Alike License: http://creativecommons.org/licenses/by-sa/3.0/
-- }}}

-- {{{ Cheat Sheet
-- {{{ Key Bindings
-- ===============================================================  KEY BINDINGS

------------------------------------------------------------ Launch Applications
--
-- Win  +  Enter                    Terminal
-- Win  +  F1                       Ranger (File manager)
-- Win  +  F2                       Firefox
-- Win  +  F3                       Thunderbird
-- Win  +  F4                       Gvim
-- Win  +  F5                       tmux
-- Win  +  F6                       ncmpcpp
-- Win  +  F7                       Start gif screencast recording LR + screenkey
-- Win  +  F8                       Start gif screencast recording LowRes
-- Win  +  F9                       Start gif screencast recording HighRes
-- Win  +  F10                      Stop gif screencast recording
-- Win  +  F11                      Start mkv screencast recording
-- Win  +  F12                      Stop mkv screencast recording
--
------------------------------------------------------------------ Shell prompts
--
-- Win  +  r                        Launch a command line prompt into status bar
-- Win  +  x                        Launch a Lua prompt into status bar
--
--------------------------------------------------------------------- Navigation
--
-- Win  +  j/k                      Focus on next/previous client
-- Win  +  u                        Focus first urgent client
-- Win  +  Left                     Previous tag
-- Win  +  Right                    Next tag
-- Win  +  1-9                      Show tag 1-9
-- Win  +  Control  +  j/k          Focus next/previous Screen
-- Win  +  Escape                   Revert tag history (previous selected tag)
-- Win  +  Tab                      Swap between clients
--
----------------------------------------------------------------- Client Control
--
-- Win  +  m                        Maximize client
-- Win  +  n                        Minimize client
-- Win  +  Control  +  n            Restore (=unminimize) a random client
-- Win  +  f                        Set client fullscreen
-- Win  +  c                        Kill focused client
-- Win  +  t                        Toggle "always visible" (on top)
-- Win  +  i                        Show/hide client border
-- Win  +  Shift    +  t            Show/hide client titlebar
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
-- Win  +  Shift    +  Space        Previous layout
-- Win  +  Control  +  Space        Toggle client floating state
-- Win  +  Control  +  Enter        Swap focused client with master
-- Win  +  Control  +  1-9          Enable/Disable view of tag 1-9
-- Win  +  Shift    +  1-9          Tag current client with 1-9 tag
-- Win  +  Shift  +  Ctrl  +  1-9   Enable/Disable tag 1-9 for current client
-- win  +  Shift    +  F1-9         Tag marked clients with 1-9 tag
-- Win  +  ,                        Move client to previous tag
-- Win  +  .                        Move client to next tag
--
------------------------------------------------------------------ Miscellaneous
--
-- Win  +  b                        Hide/Show status bar (Wibox)
-- Win  +  e                        Revelation
-- Win  +  p                        Menubar (app launcher)
-- Win  +  y                        Lock Screen
-- Print Screen                     Take a screenshot
--
---------------------------------------------------------------- Awesome Control
--
-- Win  +  Ctrl     +  r            Restart Awesome
-- Win  +  Shift    +  q            Quit Awesome
-- Win  +  w                        Show Awesome menu
--
--
----------------------------------------------------------------- System Control
--
-- Win  +  o                        Shutdown system
-- Win  +  s                        Suspend system
-- Win  +  z                        Reboot system
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
-- Special controls for an external keyboard with only volume control keys
-- Win  +  Raise volume        Next song in mpd playlist
-- Win  +  Lower volume        Previous song in mpd playlist
-- Win  +  Mute                Toggle mpd reproduction
--
-- }}}
-- {{{ Mouse Bindings
-- =============================================================  MOUSE BINDINGS

--------------------------------------------------------------------- Navigation
--
-- Button 1 on tag name             View tag
-- Button 4,5 on tag name           Switch to previous/next tag
-- Button 4,5 on root window        Switch to previous/next tag
-- Button 1,3,4,5 on layout symbol  Switch to previous/next layout
--
------------------------------------------------------------ Layout modification
--
-- Win + Button 1 on tag name       Tag current client with this tag only
-- Win + Button 3 on tag name       Toggle this tag for client
-- Button 3 on tag name             Add this tag to current view
-- Win + Button 1 on client         Move window
-- Win + Button 3 on client         Resize window
--
---------------------------------------------------------------- Awesome Control
--
-- Button 3                         Toggle awesome menu
--
------------------------------------------------------------------------ Widgets
--
-- Button 1 on mpd status            Play next song
-- Button 3 on mpd status            Play previous song
-- Button 1 on memory usage          Launch `memory_usage.py`
-- Button 1 on cpu usage             Launch htop
-- Button 3 on cpu usage             Launch nmon
-- Button 1 on filesystem usage      Launch ncdu
-- Button 1 on network usage         Launch wicd curses
-- Button 3 on network usage         Launch ethstatus
-- Button 1 on sound volume          Launch alsamixer
--
-- }}}
-- }}}

-- {{{ Load libraries
-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
-- Vicious library
vicious = require("vicious")
-- Eminent library
require("eminent")
-- Revelation
require("revelation")
-- Calendar
require("cal")
-- Menubar
require("menubar")
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
-- start the composite manager to provide transparency support
-- run_once("xcompmgr -I1 -O1 -Ff")
-- start the urxvt server
-- run_once("urxvtd -q -o -f")
-- Now these two programs are launched from .xinitrc
run_once('xcalib -c')
run_once('xcalib -co 96 -a')

-- set the local settings
os.setlocale('es_ES.UTF-8')
-- }}}

-- Naughty notifications config {{{

naughty.config.default_preset.timeout          = 15
-- naughty.config.default_preset.screen           = 1
-- naughty.config.default_preset.position         = "top_right"
naughty.config.default_preset.margin           = 20
-- naughty.config.default_preset.height           = 80
-- naughty.config.default_preset.width            = 400
naughty.config.default_preset.gap              = 20
-- naughty.config.default_preset.ontop            = true
-- naughty.config.default_preset.font             = beautiful.font or "Verdana 8"
-- naughty.config.default_preset.icon             = nil
naughty.config.default_preset.icon_size        = 28
naughty.config.default_preset.fg               = '#ffffff'
naughty.config.default_preset.bg               = '#112240' -- '#000000'
naughty.config.presets.normal.border_color     = '#000000' -- '#8AE234'
naughty.config.default_preset.border_width     = 0
-- naughty.config.default_preset.hover_timeout    = nil

naughty.config.presets.normal.opacity = 0.8
naughty.config.presets.low.opacity = 0.8
naughty.config.presets.critical.opacity = 0.8

-- }}}

-- {{{ Variable definitions
-- Directories
home_dir = os.getenv("HOME")
user = os.getenv("USER")
cfg_dir = awful.util.getdir("config")
theme_dir = cfg_dir .. "/themes"

-- Themes define colours, icons, and wallpapers
beautiful.init(theme_dir .. "/itaca/theme.lua")

-- Initialize pomodoro
local pomodoro = require("pomodoro")
pomodoro.init()

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

-- {{{ Layouts
-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.floating
}
-- }}}

-- {{{ Tags
-- define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- each screen has its own tag table.
    -- tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
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

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- Menubar {{{
menubar.cache_entries = true
menubar.app_folders = { "/usr/share/applications/",
                        "/usr/share/applications/kde4/"}

menubar.show_categories = false  -- Change to false if you want only programs to appear in the menu
-- menubar.set_icon_theme("thethememe name")

local menubar_change = {
     AudioVideo = "Multimedia",
     Development = "Desarrollo",
     Education = "Educacion",
     Game = "Juegos",
     Graphics = "Graficos",
     Office = "Oficina",
     Network = "Red",
     Settings = "Configuracion",
     System = "Sistema",
     Utility = "Accesorios",
}
for i,j in ipairs(menubar.menu_gen.all_categories) do
   menubar.menu_gen.all_categories[i].name =
       menubar_change[j.app_type] or j.name
end

menubar.g = {
   height = 40,
   y = 20
}
-- }}}

-- {{{ Wibox

-- {{{ Wibox Widgets
-- Widgets to show on Wibox

-- {{{ MPD widget
mpdwidget = widget({ type = 'textbox' })
vicious.register(mpdwidget, vicious.widgets.mpd,
    function (widget, args)
        if args["{state}"] == "Stop" then
            return ""
        elseif args["{state}"] == "Pause" then
            return '(' .. args["{Artist}"]..' - '.. args["{Title}"] .. ')'
        else
            return args["{Artist}"]..' - '.. args["{Title}"]
        end
    end, 1)
mpdwidget:buttons(
    awful.util.table.join(
        awful.button({}, 3,
            function ()
                awful.util.spawn("mpc prev", false)
            end),
        awful.button({}, 1,
            function ()
                awful.util.spawn("mpc next", false)
            end)
    )
)

-- }}}

-- {{{ Mem widget
memwidget = widget({ type = "textbox" })
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem, "$1% $2MB", 10)
memwidget:buttons(
    awful.util.table.join(
        awful.button({}, 1,
            function ()
                awful.util.spawn( terminal .. " -hold -e " .. home_dir ..
                "/.bin/memory_usage.py")
            end)
    )
)
-- }}}

-- {{{ Cpu widget
cpuwidget = widget({ type = "textbox" })
cpuwidget.width, cpuwidget.align = 80, "center"
vicious.cache(vicious.widgets.cpu)
vicious.register(cpuwidget, vicious.widgets.cpu, "$2% $3%", 3)
cpuwidget:buttons(
    awful.util.table.join(
        awful.button({}, 1,
            function ()
                awful.util.spawn( terminal .. " -e htop")
            end),
        awful.button({}, 3,
            function ()
                awful.util.spawn_with_shell("export NMON=cmLkndt4u;" .. terminal .. " -e nmon")
            end)
   )
)
-- }}}

-- {{{ Uptime & System Load widgets
uptimewidget = widget({ type = "textbox" })
-- loadwidget = widget({ type = "textbox" })
vicious.register(uptimewidget, vicious.widgets.uptime, "$2h $3m", 61)
-- vicious.register(loadwidget, vicious.widgets.uptime, "$4 $5 $6", 61)
-- }}}

-- {{{ Filesystem widget
fswidget = widget({ type = "textbox" })
vicious.register(fswidget, vicious.widgets.fs,
    "/ ${/ avail_p}% /home ${/home avail_p}%", 61)
fswidget:buttons(
    awful.util.table.join(
        awful.button({}, 1,
            function ()
                awful.util.spawn( terminal .. " -e ncdu -r")
            end)
    )
)
-- }}}

-- {{{ CPU & GPU Temperatures
cputemp = widget({ type = "textbox" })
vicious.register(cputemp, vicious.widgets.thermal, " $1ºC", 5, "thermal_zone0")

gputemp = widget({ type = "textbox" })
   function gpu_temp()
        local cmd = 'nvidia-smi -q -d TEMPERATURE | grep Gpu | cut -c35-36'
        local filedescriptor = io.popen(cmd)
        local value = filedescriptor:read()
        filedescriptor:close()
        return {value}
    end
vicious.register(gputemp, gpu_temp, "$1ºC", 5)
-- }}}

-- {{{ Battery widget
batwidget = widget({ type = "textbox" })
vicious.register(batwidget, vicious.widgets.bat,
    function(widget, args)
        if args[3] ~= 'N/A' then
            if args[2] == 100 then
                return ""
            else
                return args[1]..args[2].."% "..args[3]
            end
        end
    end, 30, "BAT0")
-- }}}

-- {{{ Fan velocity widget
fanspeed = widget({ type = "textbox" })
   function fan_speed()
        local filedescriptor = io.popen('cat /proc/i8k | cut -c20-22')
        local value = filedescriptor:read()
        filedescriptor:close()
        return {value}
    end
vicious.register(fanspeed, fan_speed, "$1", 1)
-- }}}

-- {{{ Kernel info
osinfo = widget({ type = "textbox"})
vicious.register(osinfo, vicious.widgets.os, "$2")
-- }}}

-- {{{ Network usage widget
netwidget = widget({ type = "textbox" })
vicious.cache(vicious.widgets.net)
vicious.register(netwidget, vicious.widgets.net,
                '<span color="#CC9393">${eth0 down_kb}</span>' ..
                ' <span color="#7F9F7F">${eth0 up_kb}</span>', 2)
netwidget:buttons(
    awful.util.table.join(
        awful.button({}, 1,
            function ()
                awful.util.spawn( terminal .. " -e wicd-curses")
            end),
        awful.button({}, 3,
            function ()
                awful.util.spawn( terminal .. " -e ethstatus -s 768")
            end)
    )
)
-- }}}

-- {{{ Textclock widget
mytextclock = awful.widget.textclock({ align = "right" }, " %a %d %b %H:%M ", 10)
cal.register(mytextclock, "<span color='green'><b>%s</b></span>")
-- }}}

-- Sound Volume {{{
soundvol = widget({ type = "textbox" })
vicious.register(soundvol, vicious.widgets.volume, "$2 $1%", 2, "PCM")
soundvol:buttons(
    awful.util.table.join(
        awful.button({}, 1,
            function ()
                awful.util.spawn( terminal .. " -e alsamixer")
            end)
        )
    )
-- }}}

-- {{{ Space & Separator
space = widget({ type = "textbox" })
space.width = 18

separator = widget({ type = "imagebox" })
separator.image = image(beautiful.widget_sep)

-- }}}

-- {{{ Create a systray
mysystray = widget({ type = "systray" })
-- }}}

-- }}}

-- {{{ Wibox itself
-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                         awful.button({ }, 1,
                            function (c)
                              if c == client.focus then
                                  c.minimized = true
                              else
                                  if not c:isvisible() then
                                      awful.tag.viewonly(c:tags()[1])
                                  end
                                  -- This will also un-minimize
                                  -- the client, if needed
                                  client.focus = c
                                  c:raise()
                              end
                            end),
                         awful.button({ }, 3,
                            function ()
                              if instance then
                                  instance:hide()
                                  instance = nil
                              else
                                  instance = awful.menu.clients({ width=250 })
                              end
                            end),
                         awful.button({ }, 4,
                            function ()
                              awful.client.focus.byidx(1)
                              if client.focus then client.focus:raise() end
                            end),
                         awful.button({ }, 5,
                            function ()
                              awful.client.focus.byidx(-1)
                              if client.focus then client.focus:raise() end
                            end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({
                        layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which
    -- layout we're using. We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1,
                                        function ()
                                           awful.layout.inc(layouts, 1)
                                        end),
                           awful.button({ }, 3,
                                        function ()
                                           awful.layout.inc(layouts, -1)
                                        end),
                           awful.button({ }, 4,
                                        function ()
                                           awful.layout.inc(layouts, 1)
                                        end),
                           awful.button({ }, 5,
                                        function ()
                                           awful.layout.inc(layouts, -1)
                                        end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all,
                                        mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(
                        function(c)
                          return awful.widget.tasklist.label.focused(c, s)
                          -- return awful.widget.tasklist.label.currenttags(c, s)
                        end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s, height = "20" })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mylayoutbox[s], space,
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        s == 1 and mysystray or nil, space,
        mytextclock, space,
        soundvol, space,
        osinfo, space,
        uptimewidget, space,
        -- loadwidget, space,
        batwidget, space,
        fswidget, space,
        netwidget, space,
        memwidget, space,
        cpuwidget, space,
        gputemp, space, cputemp, space,
        fanspeed, space,
        mpdwidget, space,
        pomodoro.widget, pomodoro.icon_widget, space,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

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
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- hide / show Wibox
    awful.key({ modkey }, "b",
        function ()
            mywibox[mouse.screen].visible = not mywibox[mouse.screen].visible
        end),

    -- Revelation
    awful.key({ modkey }, "e", revelation),

    -- Menubar
    awful.key({ modkey }, "p",
        function()
            menubar.show()
        end),

    -- dmenu
    awful.key({ modkey }, "-",
        function()
            awful.util.spawn_with_shell("dmenu_run -b -i -fn " ..
                     "'-*-dejavu sans mono-*-r-*-*-16-*-*-*-*-*-*-*' -p 'Run:'")
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
    awful.key({ modkey, "Shift"   }, "space",
        function ()
            awful.layout.inc(layouts, -1)
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
            awful.util.spawn("slimlock")
        end),

    -- Shutdown & Suspend & Reboot
    awful.key({ modkey }, "o",
        function ()
            awful.util.spawn("gksudo systemctl poweroff -m " ..
                             "'Se va a apagar el equipo, ¿estás seguro?'")
        end),
    awful.key({ modkey }, "s",
        function ()
            awful.util.spawn("gksudo \"sh -c 'systemctl suspend; sudo -u " .. 
                             user .. " slimlock'\" -m 'Se va a suspender el " .. 
                             "equipo, ¿estás seguro?'")
        end),

    awful.key({ modkey }, "z",
        function ()
            awful.util.spawn("gksudo systemctl reboot -m " ..
                             "'Se va a reiniciar el equipo, ¿estás seguro?'")
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
            awful.util.spawn("amixer sset PCM toggle")
        end),

    -- My external keyboard only have volume control keys
    awful.key({ modkey }, "XF86AudioRaiseVolume",
        function ()
            awful.util.spawn("mpc next")
        end),
    awful.key({ modkey }, "XF86AudioLowerVolume",
        function ()
            awful.util.spawn("mpc prev")
        end),
    awful.key({ modkey }, "XF86AudioMute",
        function ()
            awful.util.spawn("mpc toggle")
        end),

    -- Prompt
    awful.key({ modkey }, "r",
        function ()
            mypromptbox[mouse.screen]:run()
        end),

    awful.key({ modkey }, "x",
        function ()
          awful.prompt.run({ prompt = "Run Lua code: " },
          mypromptbox[mouse.screen].widget,
          awful.util.eval, nil,
          awful.util.getdir("cache") .. "/history_eval")
      end),

    -- Launch or raise apps
    awful.key({ modkey }, "F1",
        function ()
            awful.util.spawn_with_shell("export EDITOR=vim;" .. terminal ..
                                        " -e ranger")
        end),

    awful.key({ modkey }, "F2",
        function ()
            awful.util.spawn("firefox")
        end),

    awful.key({ modkey }, "F3",
        function ()
            awful.util.spawn("thunderbird")
        end),

    awful.key({ modkey }, "F4",
        function ()
            awful.util.spawn_with_shell("export LANGUAGE=en_US.UTF8;gvim")
        end),

    awful.key({ modkey }, "F5",
        function ()
            awful.util.spawn(terminal .. " -e tmux -u -2")
        end),

    awful.key({ modkey }, "F6",
        function ()
            awful.util.spawn(terminal .. " -e ncmpcpp")
        end),

    awful.key({ modkey }, "F7",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.util.spawn("screenkey")
            awful.util.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                            " -r 8 -i :0.0 -b:v 500k -pix_fmt rgb24 -y" ..
                            " -loop 0 -s 640x400 " .. home_dir ..
                            "/animated.gif")
        end),

    awful.key({ modkey }, "F8",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.util.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                            " -r 2 -i :0.0 -b:v 500k -pix_fmt rgb24 -y" ..
                            " -loop 0 -s 640x400 " .. home_dir ..
                            "/animated.gif")
        end),

    awful.key({ modkey }, "F9",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.util.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                            " -r 2 -i :0.0 -b:v 500k -pix_fmt rgb24 -y" ..
                            " -loop 0 -s 1440x900 " .. home_dir ..
                            "/animated.gif")
        end),

    awful.key({ modkey }, "F10",
        function ()
            awful.util.spawn("killall ffmpeg")
            awful.util.spawn("killall screenkey")
            awful.util.spawn("convert ephemeral:" .. home_dir ..
                             "/animated.gif -fuzz 7% -layers Optimize " ..
                             home_dir .. "/screencast.gif")
        end),

    awful.key({ modkey }, "F11",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.mkv")
            awful.util.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                            " -r 25 -i :0.0 -sameq " .. home_dir ..
                            "/screencast.mkv")
        end),

    awful.key({ modkey }, "F12",
        function ()
            awful.util.spawn("killall ffmpeg")
        end)

)

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
    awful.key({ modkey, "Control" }, "Return",
        function (c)
            c:swap(awful.client.getmaster())
        end),
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
        end),
    -- Move window to workspace left/right
    awful.key({ modkey, "Shift"   }, ",",
        function (c)
            local curidx = awful.tag.getidx(c:tags()[1])
            if curidx == 1 then
                c:tags({screen[mouse.screen]:tags()[9]})
            else
                c:tags({screen[mouse.screen]:tags()[curidx - 1]})
            end
        end),
    awful.key({ modkey, "Shift"   }, ".",
      function (c)
            local curidx = awful.tag.getidx(c:tags()[1])
            if curidx == 9 then
                c:tags({screen[mouse.screen]:tags()[1]})
            else
                c:tags({screen[mouse.screen]:tags()[curidx + 1]})
            end
        end),

    -- Show/hide client titlebar
    awful.key({ modkey, "Shift" }, "t", function (c)
       if   c.titlebar then awful.titlebar.remove(c)
       else awful.titlebar.add(c, { modkey = modkey }) end
    end),

    -- Show/hide border
    awful.key({ modkey }, "i", function (c)
       if   c.border_width == 0 then c.border_width = beautiful.border_width
       else c.border_width = 0 end
    end)

)

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
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- { rule = { class = "Screenkey" },
    --   properties = { floating = true, border_width = 0, height=120, width= 920 },
    --   callback = awful.placement.centered},
    { rule = { class = "Screenkey" },
      properties = { floating = true, border_width = 0 } },
    { rule = { class = "Gvim" },
      properties = { size_hints_honor = false } },
    { rule = { class = "URxvt" },
      properties = { size_hints_honor = false } },
    { rule = { class = "Xephyr" },
      callback = awful.placement.centered }
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebarv
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial
        -- position.
        if not c.size_hints.user_position and
            not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus",
    function(c)
        c.border_color = beautiful.border_focus
        -- c.opacity = 1
    end
)
client.add_signal("unfocus",
    function(c)
        c.border_color = beautiful.border_normal
        -- c.opacity = 0.9
    end
)
-- }}}

-- {{{ Disable startup-notification globally
-- if you are annoyed by the stopwatch/busy mouse cursor after starting
-- programs with 'awful.util.spawn'.
local oldspawn = awful.util.spawn
awful.util.spawn = function (s)
  oldspawn(s, false)
end
-- }}}

-- {{{ Folding for Vim
-- This fold the sections in vim, for a better handling
-- vim:foldmethod=marker
-- }}}
