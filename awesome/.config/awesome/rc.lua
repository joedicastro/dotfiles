--                                     _
--                          _ __ ___  | |_   _  __ _
--                         | '__/ __| | | | | |/ _` |
--                         | | | (__ _| | |_| | (_| |
--                         |_|  \___(_)_|\__,_|\__,_|
--
-- {{{ License
--
-- rc.lua, works with awesome 4.0
-- author: joedicastro <joe [at] joedicastro.com>
-- based on multiple rc.lua files from different awesome users
--
-- This work is licensed under the Creative Commons Attribution Share
-- Alike License: http://creativecommons.org/licenses/by-sa/3.0/
--
-- }}}

-- {{{ Load libraries
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup").widget
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
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Some initializations
-- set the local settings
os.setlocale('es_ES.UTF-8')
-- }}}

-- {{{ Variable definitions
-- Directories
home_dir = os.getenv("HOME")
cfg_dir = awful.util.get_configuration_dir()
theme_dir = cfg_dir .. "themes/"

-- Themes define colours, icons, font and wallpapers.
beautiful.init(theme_dir .. "itaca/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
editor = os.getenv("EDITOR") or "gvim"

-- Set the screen resolution for proper handling in various tasks.
scr_res = "1920x1200"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.max.fullscreen,
}
-- }}}

-- {{{ Menu
-- Create a main menu
mydebugmenu = {
    { "make rc_test.lua", cfg_dir .. "/awdt.py new" },
    { "edit rc_test.lua", editor .. " " .. cfg_dir .. "/rc_test.lua" },
    { "check rc_test.lua", cfg_dir .. "/awdt.py check" },
    { "start test FullHD", cfg_dir .. "/awdt.py start -t -d 1"},
    { "start test WXGA+", cfg_dir .. "/awdt.py start -t -s 1440x900 -d 2"},
    { "start default WXGA+", cfg_dir .. "/awdt.py start -s 1440x900 -d 3"},
    { "restart awesomes", cfg_dir .. "/awdt.py restart" },
    { "stop xephyr", cfg_dir .. "/awdt.py stop" },
    { "show debug.log", editor .. " -R " .. cfg_dir .. "/awdt.log" },
}

myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit configuration", editor .. " " .. awesome.conffile },
   { "debug", mydebugmenu },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

mymainmenu = awful.menu({
   items = {
      { "awesome", myawesomemenu, beautiful.awesome_icon },
      { "hotkeys", function() return false, hotkeys_popup.show_help end},
      { "open terminal", terminal }
   }
})
-- }}}

-- {{{ Wibar

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

-- Sound Volume {{{
mute = wibox.widget.textbox()
vicious.register(mute, vicious.widgets.volume, "$2", 2, "Master")
soundvol = wibox.widget.textbox()
vicious.register(soundvol, vicious.widgets.volume, "$1%", 2, "Master")
-- }}}

-- {{{ Textclock widget
mytextclock = wibox.widget.textclock(" %a %d %b %H:%M ", 15)
-- }}}

-- {{{ Space & Separator
space = wibox.widget.textbox()
space:set_text('     ')
-- }}}

-- }}}

-- {{{ Wibox itself
local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "α", "β", "γ", "δ", "ε", "ς", "ζ", "η", "θ"}, s, awful.layout.layouts[1])
    -- awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.focused)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s, height = "17" })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            space,
            mpdwidget,
            space,
            space,
            cputemp,
            space,
            cpuwidget,
            space,
            memwidget,
            space,
            netwidget,
            space,
            mute,
            soundvol,
            space,
            mytextclock,
            wibox.widget.systray(),
        },
    }
end)
-- }}}
-- }}}

-- {{{ Key bindings
-- {{{ Global Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "h",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "l",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({ "Mod1",           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ "Mod1",           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ "Mod1",           }, "j",     function () awful.tag.incwfact( 0.02)          end,
              {description = "increase master height factor", group = "layout"}),
    awful.key({ "Mod1",           }, "k",     function () awful.tag.incwfact(-0.02)          end,
              {description = "decrease master height factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "toogle", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),

    -- hide / show Wibox
    awful.key({ modkey }, "b",
       function ()
          local screen = awful.screen.focused()
          screen.mywibox.visible = not screen.mywibox.visible
       end,
       {description = "toggle wibox visibility", group = "layout"}),

    -- dmenu
    awful.key({ modkey }, "/",
        function()
            awful.util.spawn_with_shell(
                "export language=en_us.utf8;export path=$path:~/.bin;" ..
                "dmenu_run -b -i -fn " ..
                "'-*-dejavu sans mono-*-r-*-*-16-*-*-*-*-*-*-*' -p 'run:'")
        end,
        {description = "launch dmenu", group = "launcher"}),

     -- passmenu password copy to clipboard
    awful.key({ modkey }, ".",
        function()
            awful.util.spawn_with_shell(
                "export LANGUAGE=en_US.UTF8; " .. cfg_dir .. 
                "/passmenu 1 -sb darkred -p 'pass (password → clipboard):'")
        end,
        {description = "get passmenu password (clipboard)", group = "passwords"}),

    -- passmenu password type
    awful.key({ modkey, "Shift" }, ".",
        function()
            awful.util.spawn_with_shell(
                "export LANGUAGE=en_US.UTF8; " .. cfg_dir .. 
                "/passmenu 1 --type -sb darkred -p 'pass (password → type):'")
        end,
        {description = "get passmenu password (type)", group = "passwords"}),

     -- passmenu username copy to clipboard
    awful.key({ modkey }, ",",
        function()
            awful.util.spawn_with_shell(
                "export LANGUAGE=en_US.UTF8; " .. cfg_dir .. 
                "/passmenu 2 -sb darkgreen -p 'pass (username → clipboard):'")
        end,
        {description = "get passmenu username (clipboard)", group = "passwords"}),

    -- passmenu username type
    awful.key({ modkey, "Shift" }, ",",
        function()
            awful.util.spawn_with_shell(
                "export LANGUAGE=en_US.UTF8; " .. cfg_dir .. 
                "/passmenu 2 --type -sb darkgreen -p 'pass (username → type):'")
        end,
        {description = "get passmenu username (type)", group = "passwords"}),

  -- passmenu url copy to clipboard
    awful.key({ modkey }, ";",
        function()
            awful.util.spawn_with_shell(
                "export LANGUAGE=en_US.UTF8; " .. cfg_dir .. 
                "/passmenu 3 -sb purple -p 'pass (url → clipboard):'")
        end,
        {description = "get passmenu url (clipboard)", group = "passwords"}),

    -- passmenu url type
    awful.key({ modkey, "Shift" }, ";",
        function()
            awful.util.spawn_with_shell(
                "export LANGUAGE=en_US.UTF8; " .. cfg_dir .. 
                "/passmenu 3 --type -sb purple -p 'pass (url → type):'")
        end,
        {description = "get passmenu url (type)", group = "passwords"}),

    -- capture screen
    awful.key({ }, "Print",
        function ()
            awful.spawn("scrot -q 100")
        end,
        {description = "take a screenshot", group = "miscellaneous"}),

    -- lock screen
    awful.key({ modkey }, "y",
        function ()
            awful.spawn("dm-tool lock")
        end,
        {description = "lock screen", group = "miscellaneous"}),

    -- launch emacs
    awful.key({ modkey }, "e",
        function ()
            awful.spawn("emacsclient -c -a ''")
        end,
        {description = "launch emacs", group = "launcher"}),

    -- toggle Redshift (color temperature adjust)
     awful.key({ modkey }, "F9",
        function ()
            awful.spawn("pkill -USR1 redshift")
        end,
        {description = "toggle redshift", group = "miscellaneous"}),

   -- multimedia keys
     awful.key({ }, "XF86AudioPlay",
        function ()
            awful.spawn("mpc toggle")
        end,
        {description = "play/pause mpd", group = "multimedia"}),
     awful.key({ }, "XF86AudioStop",
        function ()
            awful.spawn("mpc stop")
        end,
        {description = "stop mpd", group = "multimedia"}),
     awful.key({ }, "XF86AudioPrev",
        function ()
            awful.spawn("mpc prev")
        end,
        {description = "previous song", group = "multimedia"}),
     awful.key({ }, "XF86AudioNext",
        function ()
            awful.spawn("mpc next")
        end,
        {description = "next song", group = "multimedia"}),
     awful.key({ }, "XF86AudioRaiseVolume",
        function ()
            awful.spawn("amixer sset Master 1%+")
        end,
        {description = "raise volume", group = "multimedia"}),
     awful.key({ }, "XF86AudioLowerVolume",
        function ()
            awful.spawn("amixer sset Master 1%-")
        end,
        {description = "lower volume", group = "multimedia"}),
     awful.key({ }, "XF86AudioMute",
        function ()
            awful.spawn("amixer sset Master toggle")
        end,
        {description = "mute volume", group = "multimedia"}),

    -- screen zoom in/out
     awful.key({ modkey }, "z",
        function ()
            awful.spawn("xrandr --output HDMI1 --mode 1024x768" ..
                            " --panning 1920x1200")
        end,
        {description = "zoom in", group = "miscellaneous"}),

     awful.key({ modkey, "Shift" }, "z",
        function ()
            awful.spawn("xrandr --output HDMI1 --mode 1920x1200" ..
                            " --panning 1920x1200")
        end,
        {description = "zoom out", group = "miscellaneous"}),

    -- record screencasts
    awful.key({ modkey }, "F1",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                           " -r 2 -i :0.0 -b:v 500k -pix_fmt rgb24 -y" ..
                           " -loop 0 -s 640x400 " .. home_dir ..
                           "/animated.gif")
        end,
        {description = "start gif LowRes", group = "screencast"}),

    awful.key({ modkey }, "F2",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                           " -r 2 -i :0.0 -b:v 500k -pix_fmt rgb24 -y" ..
                           " -loop 0 -s 1440x900 " .. home_dir ..
                           "/animated.gif")
        end,
        {description = "start gif HighRes", group = "screencast"}),

    awful.key({ modkey }, "F3",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                           " -r 2 -i :0.0 -b:v 500k -pix_fmt rgb24 -y" ..
                           " -loop 0 -s " .. scr_res .. " " .. home_dir ..
                           "/animated.gif")
        end,
        {description = "start gif FullRes", group = "screencast"}),

    awful.key({ modkey }, "F4",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.spawn("ffmpeg -f x11grab -s 957x1180" ..
                           " -r 2 -i :0.0+2,19 -b:v 500k -pix_fmt rgb24 -y" ..
                           " -loop 0 " .. home_dir ..  "/animated.gif")
        end,
        {description = "start gif left half", group = "screencast"}),

    awful.key({ modkey }, "F5",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.gif")
            awful.spawn("ffmpeg -f x11grab -s 860x588" ..
                           " -r 2 -i :0.0+2,19 -b:v 500k -pix_fmt rgb24 -y" ..
                           " -loop 0 " .. home_dir ..  "/animated.gif")
        end,
        {description = "start gif upper left quadrant", group = "screencast"}),

    awful.key({ modkey }, "F6",
        function ()
            awful.util.spawn("killall ffmpeg")
            awful.spawn("convert ephemeral:" .. home_dir ..
                           "/animated.gif -fuzz 7% -layers Optimize " ..
                           home_dir .. "/screencast.gif")
        end,
        {description = "stop gif recording", group = "screencast"}),

    awful.key({ modkey }, "F7",
        function ()
            awful.util.spawn_with_shell("rm " .. home_dir .. "/screencast.mkv")
            awful.spawn("ffmpeg -f x11grab -s " .. scr_res ..
                           " -r 25 -i :0.0 -qscale 0 " .. home_dir ..
                           "/screencast.mkv")
        end,
        {description = "stark mkv FullRes", group = "screencast"}),

    awful.key({ modkey }, "F8",
        function ()
            awful.spawn("killall ffmpeg")
        end,
        {description = "stop mkv recording", group = "screencast"})

)
-- }}}

-- {{{ Per client Key bindings
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey,           }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)
-- }}}

-- {{{ Tags
-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
        },
        class = {
          "Arandr",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Wpa_gui",
          "pinentry",
          "veromix",
          "MPlayer",
          "xtightvncviewer"},

        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- fulfill the screen, do not respect aspect ratio
    { rule_any = { class = {
                  "Emacs",
                  "Gvim",
                  "URxvt",
                  "XTerm"},
                 }, properties = { size_hints_honor = false } },

    -- Set Xephyr to always start centered
    { rule_any = { class = {
                      "pinentry",
                      "Xephyr"},
                 }, properties = { placement = awful.placement.centered } },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
        awful.placement.no_overlap(c)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Folding for Vim
-- This fold the sections in vim, for a better handling
-- vim:foldmethod=marker
-- }}}
