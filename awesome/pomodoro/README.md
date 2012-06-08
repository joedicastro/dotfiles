# Pomodoro Widget

## Usage

    cd ~/.config/awesome
    git clone git://github.com/nikolavp/awesome-pomodoro.git pomodoro

### In you rc.lua:

    // insert after beautiful.init("...")
    local pomodoro = require("pomodoro")

    // customizations to the showed text or the time a pomodoro should take
    // look in init.lua for more info
    (...)

    //init the pomodoro object with the current customizations
    pomodoro.init()

At this point there are two widget you will want to use in your wibox:

*    pomodoro.widget - the main widget that will display the time of the current pomodoro.

*    pomodoro.icon_widget - the icon that you can use to display close to the text.

### Add it to your wibox

You can use:

* only the text widget:

        mywibox[s].widgets = {
            pomodoro.widget,
            mytextclock,
        }

* only the icon widget:

        mywibox[s].widgets = {
            pomodoro.icon_widget,
            mytextclock,
        }

* or you can use them both:

        mywibox[s].widgets = {
            pomodoro,widget, pomodoro.icon_widget,
            mytextclock,
        }

## Customization

If you want change the default icon, you can use beautiful:

    beautiful.pomodoro_icon = '/your/path/to/pomodoro/icon'

Check out the init.lua too. For example if you don't want the text "Pomodoro:"
before the actual timer issue 

    pomodoro.pre_text = "" 

before calling 

    pomodoro.init()

## License

Copyright 2010-2011 François de Metz, Nikolay Sturm(nistude), Nikola Petrov(nikolavp)

            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
                    Version 2, December 2004

    Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>

    Everyone is permitted to copy and distribute verbatim or modified
    copies of this license document, and changing it is allowed as long
    as the name is changed.

            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

    0. You just DO WHAT THE FUCK YOU WANT TO.
