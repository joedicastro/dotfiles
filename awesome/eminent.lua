----------------------------------------------------------------
-- Effortless wmii-style dynamic tagging.
----------------------------------------------------------------
-- Lucas de Vries <lucas@glacicle.org>
-- Licensed under the WTFPL version 2
--   * http://sam.zoy.org/wtfpl/COPYING
----------------------------------------------------------------
-- To use this module add:
--   require("eminent")
-- to the top of your rc.lua. 
--
-- That's it. Through magical monkey-patching, all you need to
-- do to start dynamic tagging is loading it.
--
-- Use awesome like you normally would, you don't need to
-- change a thing.
----------------------------------------------------------------

-- Grab environment
local ipairs = ipairs
local pairs = pairs
local awful = require("awful")
local table = table
local capi = {
    tag = tag,
    mouse = mouse,
    client = client,
    screen = screen,
    wibox = wibox,
    timer = timer,
    keygrabber = keygrabber,
}

-- Eminent: Effortless wmii-style dynamic tagging
module("eminent")

-- Grab the original functions we're replacing
local deflayout = nil
local orig = {
    new = awful.tag.new,
    viewidx = awful.tag.viewidx,

    taglist = awful.widget.taglist.new,
    label = awful.widget.taglist.label.all,
}

-- Return tags with stuff on them, mark others hidden
function gettags(screen)
    local tags = {}

    for k, t in ipairs(capi.screen[screen]:tags()) do
        if t.selected or #t:clients() > 0 then
            awful.tag.setproperty(t, "hide", false)
            table.insert(tags, t)
        else
            awful.tag.setproperty(t, "hide", true)
        end
    end

    return tags
end

-- Pre-create tags
awful.tag.new = function (names, screen, layout)
    deflayout = layout and layout[1] or layout
    return orig.new(names, screen, layout)
end

-- View tag by relative index
awful.tag.viewidx = function (i, screen)
    -- Hide tags
    local s = screen and screen.index or capi.mouse.screen
    local ctags = capi.screen[s]:tags()
    local tags = gettags(s)
    local sel = awful.tag.selected()

    -- Check if we should "create" a new tag
    local selidx = awful.util.table.hasitem(tags, sel)
    local tagidx = awful.util.table.hasitem(ctags, sel)

    -- Create a new tag if needed
    if selidx == #tags and i == 1 and #sel:clients() > 0 then
        -- Deselect all
        awful.tag.viewnone(s)

        if #ctags >= tagidx+1 then
            -- Focus next
            ctags[tagidx+1].selected = true
        else
            -- Create new
            local tag = capi.tag { name = ""..(tagidx+1) }
            tag.screen = s
            tag.selected = true
            awful.tag.setproperty(tag, "layout", deflayout)
        end
    else
        -- Call original
        orig.viewidx(i, screen)
    end
end

-- Taglist label functions
awful.widget.taglist.label.all = function (t, args)
    if t.selected or #t:clients() > 0 then
        return orig.label(t, args)
    end
end


-- Update hidden status
local function uc(c) gettags(c.screen) end
local function ut(s, t) gettags(s.index) end

capi.client.add_signal("unmanage", uc)
capi.client.add_signal("new", function(c)
    c:add_signal("property::screen", uc)
    c:add_signal("tagged", uc)
    c:add_signal("untagged", uc)
end)

for screen=1, capi.screen.count() do
    awful.tag.attached_add_signal(screen, "property::selected", uc)
    capi.screen[screen]:add_signal("tag::attach", ut)
    capi.screen[screen]:add_signal("tag::detach", ut)
end
