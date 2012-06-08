-- revelation.lua
--
-- Library that implements Expose like behavior.
--
-- @author Perry Hargrave resixian@gmail.com
-- @author Espen Wiborg espenhw@grumblesmurf.org
-- @author Julien Danjou julien@danjou.info
--
-- @copyright 2008 Espen Wiborg, Julien Danjou
--

local awful = require('awful')
local aw_rules = require('awful.rules')
local pairs = pairs
local setmetatable = setmetatable
local table = table
local capi = {
    tag = tag,
    client = client,
    keygrabber = keygrabber,
    mousegrabber = mousegrabber,
    mouse = mouse,
    screen = screen
}

local clientData = {} -- table that holds the positions and sizes of floating clients

module("revelation")

config = {
    -- Name of expose tag.
    tag_name = "Revelation",

    -- Match function can be defined by user.
    -- Must accept a `rule` and `client` and return `boolean`.
    -- The rule forms follow `awful.rules` syntax except we also check the
    -- special `rule.any` key. If its true, then we use the `match.any` function
    -- for comparison.
    match = {
        exact = aw_rules.match,
        any   = aw_rules.match_any
    },
}

-- Executed when user selects a client from expose view.
--
-- @param restore Function to reset the current tags view.
function selectfn(restore)
    return function(c)
        restore()
        -- Pop to client tag
        awful.tag.viewonly(c:tags()[1], c.screen)
        -- Focus and raise
        capi.client.focus = c
        c:raise()
    end
end

-- Tags all matching clients with tag t
-- @param rule The rule. Conforms to awful.rules syntax.
-- @param clients A table of clients to check.
-- @param t The tag to give matching clients.
function match_clients(rule, clients, t)
    local mf = rule.any and config.match.any or config.match.exact
    for _, c in pairs(clients) do
        if mf(c, rule) then
            -- Store geometry before setting their tags
            if awful.client.floating.get(c) then
                clientData[c] = c:geometry()
                awful.client.floating.set(c, false)
            end

            awful.client.toggletag(t, c)
            c.minimized = false
        end
    end
    return clients
end

-- Arrow keys and 'hjkl' move focus, Return selects, Escape cancels. Ignores
-- modifiers.
--
-- @param restore a function to call if the user presses escape
-- @return keyboardhandler
function keyboardhandler (restore)
    return function (mod, key, event)
        if event ~= "press" then return true end
        -- translate vim-style home keys to directions
        if key == "j" or key == "k" or key == "h" or key == "l" then
            if key == "j" then
                key = "Down"
            elseif key == "k" then
                key = "Up"
            elseif key == "h" then
                key = "Left"
            elseif key == "l" then
                key = "Right"
            end
        end

        --
        if key == "Escape" then
            restore()
            return false
        elseif key == "Return" then
            selectfn(restore)(capi.client.focus)
            return false
        elseif key == "Left" or key == "Right" or
            key == "Up" or key == "Down" then
            awful.client.focus.bydirection(key:lower())
        end
        return true
    end
end

-- Implement Exposé (ala Mac OS X).
--
-- @param rule A table with key and value to match. [{class=""}]
-- @param s The screen to consider clients of. [mouse.screen].
function expose(rule, s)
    local rule = rule or {class=""}
    local scr = s or capi.mouse.screen

    local t = awful.tag.new({config.tag_name},
                            scr,
                            awful.layout.suit.fair)[1]
    awful.tag.viewonly(t, t.screen)
    match_clients(rule, capi.client.get(scr), t)
    local function restore()
        awful.tag.history.restore()
        t.screen = nil
        capi.keygrabber.stop()
        capi.mousegrabber.stop()

        for _, c in pairs(capi.client.get(src)) do
            if clientData[c] then
                c:geometry(clientData[c]) -- Restore positions and sizes
                awful.client.floating.set(c, true)
            end
        end
    end

    capi.keygrabber.run(keyboardhandler(restore))

    
    local pressedMiddle = false
    capi.mousegrabber.run(function(mouse)
        local c = awful.mouse.client_under_pointer()
        if mouse.buttons[1] == true then
            selectfn(restore)(c)
            return false
        elseif mouse.buttons[2] == true and pressedMiddle == false and c ~= nil then -- is true whenever the button is down. 
            pressedMiddle = true -- extra variable needed to prevent script from spam-closing windows
            c:kill()
            return true
        elseif mouse.buttons[2] == false and pressedMiddle == true then
            pressedMiddle = false
        end

        return true
        --Strange but on my machine only fleur worked as a string.
        --stole it from
        --https://github.com/Elv13/awesome-configs/blob/master/widgets/layout/desktopLayout.lua#L175
    end,"fleur")
end

setmetatable(_M, { __call = function(_, ...) return expose(...) end })
