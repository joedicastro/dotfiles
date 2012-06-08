-- Heinously stolen from awesome-freedesktop project.
-- https://github.com/terceiro/awesome-freedesktop
-- All attribution should go to Antonio Terceiro.

-- Grab environment

local io = io
local table = table
local ipairs = ipairs
local string = string

module("menubar.utils")

terminal = 'xterm'

default_icon = ""

icon_theme = nil

all_icon_sizes = {
   '128x128' ,
   '96x96',
   '72x72',
   '64x64',
   '48x48',
   '36x36',
   '32x32',
   '24x24',
   '22x22',
   '16x16'
}

icon_sizes = {}

local function file_exists(filename)
   local file = io.open(filename, 'r')
   local result = (file ~= nil)
   if result then
      file:close()
   end
   return result
end

function lookup_icon(icon_file)
   if not icon_file then
      return default_icon
   end
   if icon_file:sub(1, 1) == '/' and (icon_file:find('.+%.png') or icon_file:find('.+%.xpm')) then
      -- icons with absolute path and supported (AFAICT) formats
      return icon_file
   else
      local icon_path = {}
      local icon_theme_paths = {}
      if icon_theme then
         table.insert(icon_theme_paths, icon_theme .. '/')
         -- TODO also look in parent icon themes, as in freedesktop.org specification
      end
      table.insert(icon_theme_paths, '/usr/share/icons/hicolor/') -- fallback theme cf spec

      local isizes = {}
      for i, sz in ipairs(all_icon_sizes) do
         table.insert(isizes, sz)
      end

      for i, icon_theme_directory in ipairs(icon_theme_paths) do
         for j, size in ipairs(icon_file_sizes or isizes) do
            table.insert(icon_path, icon_theme_directory .. size .. '/apps/')
            table.insert(icon_path, icon_theme_directory .. size .. '/actions/')
            table.insert(icon_path, icon_theme_directory .. size .. '/devices/')
            table.insert(icon_path, icon_theme_directory .. size .. '/places/')
            table.insert(icon_path, icon_theme_directory .. size .. '/categories/')
            table.insert(icon_path, icon_theme_directory .. size .. '/status/')
         end
      end
      -- lowest priority fallbacks
      table.insert(icon_path, '/usr/share/pixmaps/')
      table.insert(icon_path, '/usr/share/icons/')

      for i, directory in ipairs(icon_path) do
         if (icon_file:find('.+%.png') or icon_file:find('.+%.xpm')) and file_exists(directory .. icon_file) then
            return directory .. icon_file
         elseif file_exists(directory .. icon_file .. '.xpm') then
            return directory .. icon_file .. '.xpm'
         elseif file_exists(directory .. icon_file .. '.png') then
            return directory .. icon_file .. '.png'
         end
      end
      return default_icon
   end
end

--- Parse a .desktop file
-- @param file The .desktop file
-- @param requested_icon_sizes A list of icon sizes (optional). If this list is given, it will be used as a priority list for icon sizes when looking up for icons. If you want large icons, for example, you can put '128x128' as the first item in the list.
-- @return A table with file entries.
function parse(file, requested_icon_sizes)
   local program = { show = true, file = file }
   for line in io.lines(file) do
      for key, value in line:gmatch("(%w+)=(.+)") do
         program[key] = value
      end
   end

   -- Don't show program if NoDisplay attribute is false
   if program.NoDisplay and string.lower(program.NoDisplay) == "true" then
      program.show = false
   end
   
   -- Only show the program if there is not OnlyShowIn attribute
   -- or if it's equal to 'awesome'
   if program.OnlyShowIn ~= nil and program.OnlyShowIn ~= "awesome" then
      program.show = false
   end

   -- Look up for a icon.
   if program.Icon then
      program.icon_path = lookup_icon(program.Icon)
   end

   -- Split categories into a table.
   if program.Categories then
      program.categories = {}
      for category in program.Categories:gfind('[^;]+') do
         table.insert(program.categories, category)
      end
   end

   if program.Exec then
      local cmdline = program.Exec:gsub('%%c', program.Name)
      cmdline = cmdline:gsub('%%[fuFU]', '')
      cmdline = cmdline:gsub('%%k', program.file)
      if program.icon_path then
         cmdline = cmdline:gsub('%%i', '--icon ' .. program.icon_path)
      else
         cmdline = cmdline:gsub('%%i', '')
      end
      if program.Terminal == "true" then
         cmdline = terminal .. ' -e ' .. cmdline
      end
      program.cmdline = cmdline
   end

   return program
end

--- Parse a directory with .desktop files
-- @param dir The directory.
-- @param icons_size, The icons sizes, optional.
-- @return A table with all .desktop entries.
function parse_dir(dir)
   local programs = {}
   local files = io.popen('find '.. dir ..' -maxdepth 1 -name "*.desktop"'):lines()
   for file in files do
      table.insert(programs, parse(file))
   end
   return programs
end
