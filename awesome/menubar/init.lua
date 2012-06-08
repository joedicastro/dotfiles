-- Advanced menu replacement similar to dmenu
-- @author Alexander Yakushev <yakushev.alex@gmail.com>
-- @license WTFPL version 2 http://sam.zoy.org/wtfpl/COPYING
-- @version 0.1.0

-- Grab environment we need
local capi = { widget = widget,
               screen = screen,
               image = image,
               client = client,
               wibox = wibox,
               screen = screen}
local setmetatable = setmetatable
local ipairs = ipairs
local table = table
local theme = require("beautiful")
local menu_gen = require("menubar.menu_gen")
local prompt = require("menubar.prompt")
local awful = require("awful")
local tonumber = tonumber
local io = io
local string = string
local mouse = mouse
local math = math
local keygrabber = keygrabber
local print = print

module("menubar")

current_item = 1
previous_item = nil
current_category = nil
shownitems = nil
cache_entries = true
show_categories = true

instance = { prompt = nil,
             widget = nil,
             wibox = nil }

common_args = { w = { layout = awful.widget.layout.horizontal.leftright },
                data = setmetatable({}, { __mode = 'kv' }),
                widgets = { imagebox = { },
                            textbox  = { ["margin"] = { ["left"]  = 5,
                                                        ["right"] = 5},
                                         ["bg_resize"] = true
                                } } }

g = { width = nil,
      height = 20,
      x = nil,
      y = nil }

local function colortext(s, c)
   return "<span color='" .. c .. "'>" .. s .. "</span>"
end

local function label(o)
   if o.focused then
      return
      colortext(o.name, awful.util.color_strip_alpha(theme.fg_focus)) or o.name,
      theme.bg_focus, nil, o.icon
   else
      return o.name, theme.bg_normal, nil, o.icon
   end
end

local function perform_action(o)
   if not o or o.empty then
      return true
   end
   if o.cat_id then
      current_category = o.cat_id
      local new_prompt = shownitems[current_item].name .. ": "
      previous_item = current_item
      current_item = 1
      return true, "", new_prompt
   elseif shownitems[current_item].cmdline then
      awful.util.spawn(shownitems[current_item].cmdline)
      hide()
      return true
   end
end

local function initialize()
   instance.wibox = capi.wibox({})
   instance.widget = new()
   instance.wibox.ontop = true
   instance.prompt = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
   instance.wibox.widgets = { instance.prompt,
                              instance.widget,
                              layout = awful.widget.layout.horizontal.leftright }
end

function refresh(use_cache)
   if not use_cache then
      menu_gen.generate()
   end

   if not awful.util.file_readable(menu_gen.output_filename) then
      error("Can't read generated file, menu_gen is broken")
   else
      categories = {}
      f = io.open(menu_gen.output_filename)
      f:read("*line")
      menu_entries = {}
      while true do
         local name = f:read("*line")
         if not name or name == menu_gen.eof then
            break
         end
         local cmdline = f:read("*line")
         local icon_path = f:read("*line")
         local category = tonumber(f:read("*line"))
         table.insert(menu_entries, { name = name, cmdline = cmdline,
                                      icon = icon_path ~= "" and capi.image(icon_path) or nil,
                                      category = category })
      end
   end
end

function show(screen)
   if not instance.wibox then
      initialize()
   elseif instance.wibox.screen then -- Menu already shown, exit
      return
   elseif not cache_entries then
      refresh()
   end

   -- Set position and size
   local scr = screen or mouse.screen or 1
   local scrgeom = capi.screen[scr].workarea
   local x = g.x or scrgeom.x
   local y = g.y or scrgeom.y
   instance.wibox.height = g.height or 20
   instance.wibox.width = g.width or scrgeom.width
   instance.wibox:geometry({x = x, y = y})

   current_item = 1
   current_category = nil
   menulist_update()
   prompt.run({ prompt = "Run app: " }, instance.prompt.widget, function(s) end, nil,
              awful.util.getdir("cache") .. "/history_menu", nil,
              hide,
              menulist_update,
              function(mod, key, comm)
                 if key == "Left" or (mod.Control and key == "j") then
                    current_item = math.max(current_item - 1, 1)
                    return true
                 elseif key == "Right" or (mod.Control and key == "k") then
                    current_item = current_item + 1
                    return true
                 elseif key == "BackSpace" then
                    if comm == "" and current_category then
                       current_category = nil
                       current_item = previous_item
                       return true, nil, "Run app: "
                    end
                 elseif key == "Escape" then
                    if current_category then
                       current_category = nil
                       current_item = previous_item
                       return true, nil, "Run app: "
                    end
                 elseif key == "Return" then
                    return perform_action(shownitems[current_item])
                 end
                 return false
              end)
   instance.wibox.screen = scr
end

function hide()
   keygrabber.stop()
   instance.wibox.screen = nil
end

local function nocase (s)
   s = string.gsub(s, "%a",
                   function (c)
                      return string.format("[%s%s]", string.lower(c),
                                           string.upper(c))
                   end)
   return s
end -- nocase

global_callback = nil

function menulist_update(query)
   local query = query or ""
   shownitems = {}
   local match_inside = {}

   -- We add entries that match from the beginning to the table
   -- shownitems, and those that match in the middle to the table
   -- match_inside.
   if show_categories then
      for i, v in ipairs(menu_gen.all_categories) do
         v.focused = false
         if not current_category and v.use then
            if string.match(v.name, nocase(query)) then
               if string.match(v.name, "^" .. nocase(query)) then
                  table.insert(shownitems, v)
               else
                  table.insert(match_inside, v)
               end
            end
         end
      end
   end

   for i, v in ipairs(menu_entries) do
      v.focused = false
      if not current_category or v.category == current_category then
         if string.match(v.name, nocase(query)) then
            if string.match(v.name, "^" .. nocase(query)) then
               table.insert(shownitems, v)
            else
               table.insert(match_inside, v)
            end
         end
      end
   end

   -- Now add items from match_inside to shownitems
   for i, v in ipairs(match_inside) do
      table.insert(shownitems, v)
   end

   if #shownitems > 0 then
      if current_item > #shownitems then
         current_item = #shownitems
      end
      shownitems[current_item].focused = true
   else
      table.insert(shownitems, { name = "&lt;no matches&gt;", icon = nil,
                                 empty = true })
   end

   awful.widget.common.list_update(common_args.w, nil, label,
                                   common_args.data,
                                   common_args.widgets, shownitems)
end

function new()
   if app_folders then
      menu_gen.all_menu_dirs = app_folders
   end
   refresh()
   -- Load categories icons and add IDs to them
   for i, v in ipairs(menu_gen.all_categories) do
      v.icon = (v.icon ~= nil) and capi.image(v.icon) or nil
      v.cat_id = i
   end
   menulist_update()
   return common_args.w
end

function set_icon_theme(theme_name)
   if string.sub(theme_name, 1, 1) == '/' then
      utils.icon_theme = theme_name
   else
      utils.icon_theme = '/usr/share/icons/' .. theme_name
   end
   menu_gen.lookup_category_icons()
end

setmetatable(_M, { __call = function(_, ...) return new(...) end })