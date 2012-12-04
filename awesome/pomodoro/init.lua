local widget    = widget
local image     = image
local timer     = timer
local awful     = require("awful")
local naughty   = require("naughty")
local beautiful = require("beautiful")
local os        = os
local string    = string
local setmetatable = setmetatable

module("pomodoro")

-- pomodoro timer widget
pomodoro = {}
-- tweak these values in seconds to your liking
pomodoro.pause_duration = 5 * 60
pomodoro.work_duration = 25 * 60

local pomodoro_image_path = beautiful.pomodoro_icon or
                            awful.util.getdir("config") .."/pomodoro/pomodoro.png"
local pomodoro_sound_path = beautiful.pomodoro_sound or
                            awful.util.getdir("config") .."/pomodoro/complete.oga"


pomodoro.pre_text = ""
pomodoro.pause_title = "Pausa terminada."
pomodoro.pause_text = "Vuelve al trabajo!"
pomodoro.work_title = "Pomodoro terminado."
pomodoro.work_text = "Tomate un descanso!"
pomodoro.working = true
pomodoro.left = pomodoro.work_duration
pomodoro.widget = widget({ type = "textbox" })
pomodoro.icon_widget = widget({type="imagebox"})
pomodoro.icon_widget.image = image(pomodoro_image_path)
pomodoro.timer = timer { timeout = 1 }

function pomodoro:settime(t)
  if t >= 3600 then -- more than one hour!
    t = os.date("%X", t-3600)
  else
    t = os.date("%M:%S", t)
  end
  self.widget.text = pomodoro.pre_text .. "<span color='#729fcf'><b>" .. t .. 
                     "</b></span>"
end

function pomodoro:notify(title, text, duration, working)
  naughty.notify {
    -- bg = beautiful.bg_urgent,
    -- fg = beautiful.fg_urgent,
    title = title,
    text  = text,
    timeout = 15,
    icon = pomodoro.icon_widget.image,
    icon_size = 64,
    width = 300,
    -- normal_opacity = 0.9,
  }

  -- Use notify-send instead of naughty in Ubuntu
  -- awful.util.spawn_with_shell(string.format("notify-send '%s' '%s' -i %s",
  --                                           title, text, pomodoro_image_path))
  awful.util.spawn_with_shell(string.format("play %s -t alsa", pomodoro_sound_path))
  pomodoro.left = duration
  pomodoro:settime(duration)
  pomodoro.working = working
end

function pomodoro:init()
    pomodoro:settime(pomodoro.work_duration)
end

function get_buttons()
  return awful.util.table.join(
    awful.button({ }, 1, function()
      pomodoro.last_time = os.time()
      pomodoro.timer:start()
    end),
    awful.button({ }, 2, function()
      pomodoro.timer:stop()
    end),
    awful.button({ }, 3, function()
      pomodoro.timer:stop()
      pomodoro.left = pomodoro.work_duration
      pomodoro:settime(pomodoro.work_duration)
    end)
    )
end

pomodoro.widget:buttons(get_buttons())
pomodoro.icon_widget:buttons(get_buttons())

pomodoro_tooltip = awful.tooltip({
    objects = { pomodoro.widget, pomodoro.icon_widget},
    timer_function =
        function()
            if pomodoro.timer.started then
                if pomodoro.working then
                    return 'El trabajo termina en ' ..
                            os.date("%M:%S", pomodoro.left)
                else
                    return 'El descanso finaliza en ' ..
                            os.date("%M:%S", pomodoro.left)
                end
            else
                return 'Pomodoro no iniciado'
            end
            return 'Bad tooltip'
        end,
})

pomodoro.timer:add_signal("timeout", function()
  local now = os.time()
  pomodoro.left = pomodoro.left - (now - pomodoro.last_time)
  pomodoro.last_time = now

  if pomodoro.left > 0 then
    pomodoro:settime(pomodoro.left)
  else
    if pomodoro.working then
      pomodoro:notify(pomodoro.work_title, pomodoro.work_text,
        pomodoro.pause_duration, false)
    else
      pomodoro:notify(pomodoro.pause_title, pomodoro.pause_text,
        pomodoro.work_duration, true)
    end
    pomodoro.timer:stop()
  end
end)

return pomodoro
