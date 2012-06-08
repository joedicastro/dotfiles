# revelation.lua

Provides Mac OSX like 'Expose' view of all clients.

## Use

### Installation
 (From user's awesome configuration directory, usually ~/.config/awesome)

 1. Clone repository:

        git clone https://bioe007@github.com/bioe007/awesome-revelation.git

 2. put near the top of your rc.lua require("revelation")
 3. Make a global keybinding for revelation in your rc.lua:

          awful.key({modkey}, "e", revelation)

    **NOTE:** Always double check this key binding syntax against the version of
    Awesome that you are using.

 4. Reload rc.lua and try the keybinding.

 It should bring all clients to the current tag and set the layout to fair. You
 can focus clients with __cursor__ or __hjkl__ keys then press __Enter__ or
 press the mouse right button to select or __Escape__ to abort.

 This is a modification of the original awesome library that implemented
 expose like behavior.

### Configuration
 Revelation's configuration is done through direct access to the module's
 `config` table.

 There are two basic settings, shown with default values:

    -- The name of the tag created for the 'exposed' view
    revelation.config.tag_name = 'Revelation'

    -- A table of matcher functions (used in client filtering)
    revelation.match.exact = awful.rules.match
    revelation.match.any   = awful.rules.match_any

 The rule matching functions must conform to `awful.rules.match` prototypes.

 For client matching rules, we follow the same syntax as awful.rules with one
 perk; if `rule.any == true`, then we call the `config.match.any` function.

### Examples
 All clients:

     awful.key({modkey}, "e", revelation)

 To match all urxvt terminals:

     awful.key({modkey}, "e", function()
                revelation({class="URxvt"})
             end)
 To match clients with class 'foo' or 'bar':

     awful.key({modkey}, "e", function()
                revelation({
                            class={"foo", "bar"},
                            any=true
                            })
            end)

## Credits

### Maintenance
    * Perry Hargrave <resixian@gmail.com>

### Contributions, many thanks!
    * Nikola Petrov <nikolavp@gmail.com>

### Original authors
    * Espen Wiborg <espenhw@grumblesmurf.org>
    * Julien Danjou <julien@danjou.info>

## License
 (c) 2008 Espen Wiborg, Julien Danjou
