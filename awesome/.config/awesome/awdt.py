#!/usr/bin/env python2
# encoding: utf-8

"""
awdt.py: a script to debug awesome wm configs in nested Xephyr sessions.

This script is a tool intended to help to debug Awesome wm configurations
in a safe manner. To this purpose uses the Xephyr X server to nest a X
session inside the current Awesome X session.

The original idea come from the mikar's bash awmtt script (Thanks mikar):

https://github.com/mikar/awmtt

Needs logger.py and check_execs.py from my Python Recipes repository at

https://github.com/joedicastro/python-recipes

"""

#==============================================================================
#    Copyright 2012 joe di castro <joe@joedicastro.com>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#==============================================================================


__author__ = "joe di castro <joe@joedicastro.com>"
__license__ = "GNU General Public License version 3"
__date__ = "2013-11-17"
__version__ = "0.6"

try:
    import os
    import sys
    from argparse import ArgumentParser, RawDescriptionHelpFormatter
    from re import findall
    from shutil import copy
    from subprocess import Popen, PIPE, STDOUT
    from tempfile import gettempdir
    from time import sleep
    from logger import Logger
    from check_execs import check_execs
except ImportError:
    # Checks the installation of the necessary python modules
    print((os.linesep * 2).join(["An error found importing one module:",
          str(sys.exc_info()[1]), "You need to install it", "Stopping..."]))
    sys.exit(-2)


def arguments():
    """Define the command line arguments for the script."""
    main_desc = """Debug awesome wm configurations in Xephyr sessions.

    Use `new` to create a new test config file cloned from your rc.lua
    Use `check` to test the Lua syntax on this file
    Use `start` to start a new awesome debug session
    Use `restart` to restart all awesome debug sessions
    Use `stop` to stop all awesome debug sessions
                   """

    parser = ArgumentParser(description=main_desc,
                            formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument("action", choices=["new", "check", "start", "restart",
                        "stop"], help="the action to perform")
    parser.add_argument("-t", dest="test", action="store_true", default=False,
                        help="use created test configuration file")
    parser.add_argument("-s", dest="screen", help="the screen resolution")
    parser.add_argument("-d", dest="display", help="the DISPLAY to use")
    parser.add_argument("-v", "--version", action="version",
                        version="%(prog)s {0}".format(__version__),
                        help="show program's version number and exit")
    return parser


def create_config_file(original_file, copy_file):
    """Create a config file as a copy of another configuration file."""
    copy(original_file, copy_file)


def main():
    """The script core."""
    # the files needed
    cfg_dir = os.path.expanduser("~/.config/awesome")
    rc_real = os.path.join(cfg_dir, "rc.lua")
    rc_test = os.path.join(cfg_dir, "rc_test.lua")
    rc_original = "/etc/xdg/awesome/rc.lua"
    xpids_tmp = os.path.join(gettempdir(), "xpids")
    apids_tmp = os.path.join(gettempdir(), "apids")
    log_file = os.path.join(cfg_dir, "awdt.log")

    args = arguments().parse_args()

    # get the current screen resolution
    xdpyinfo = Popen("xdpyinfo", stdout=PIPE).stdout.read()
    currres = findall(r"dimensions:\s*(\d+x\d+)\spixels", xdpyinfo)[0]

    # set defaults
    args.screen = args.screen if args.screen else currres
    args.test = rc_test if args.test else rc_original
    args.display = args.display if args.display else 1

    if args.action == "new":
        create_config_file(rc_real, rc_test)

    if args.action == "check":
        check = Popen("awesome -c {0} -k".format(rc_test).split(), stdout=PIPE,
                      stderr=STDOUT)
        check_out = check.stdout.readlines()
        Popen(["notify-send", "Lua sintax chek:", os.linesep.join(check_out)])

    if args.action == "start":
        # create test file if no exists
        if not os.path.exists(rc_test):
            create_config_file(rc_real, rc_test)

        # clean log in each debug session
        if not os.path.exists(apids_tmp):
            if os.path.exists(log_file):
                os.remove(log_file)

        log = Logger()
        log.filename = log_file
        log.header("https://github.com/joedicastro/dotfiles",
                   "This is a log from an Awesome wm's debug session")
        log.time("Start time")

        x_cmd = "Xephyr -ac -br -noreset -screen {0} :{1}".format(args.screen,
                                                                  args.display)
        aw_cmd = "awesome -c {0}".format(args.test)

        xserver = Popen(x_cmd.split(), stdout=PIPE, stderr=STDOUT)
        sleep(1)
        os.putenv("DISPLAY", ":{0}.0".format(args.display))
        awesome = Popen(aw_cmd.split(), stdout=PIPE, stderr=STDOUT)

        # save the process PIDs for kill them properly later. This way, no
        # matter how many awesome sessions do you start, all of them will be
        # reported to the log file. Also, awesome PIDs are used to restart each
        # one when is required
        with open(xpids_tmp, 'a+') as xpids:
            xpids.write(str(xserver.pid) + os.linesep)
        with open(apids_tmp, 'a+') as apids:
            apids.write(str(awesome.pid) + os.linesep)

        log.list("Parameters", ["Screen resolution: {0}".format(args.screen),
                                "Display: {0}".format(args.display),
                                "Configuration file: {0}".format(args.test)])
        log.list("Xephyr output", xserver.stdout.readlines())
        log.list("Awesome output", awesome.stdout.readlines())
        log.time("End time")
        log.free(os.linesep * 4)
        log.write(True)

    if args.action == "restart":
        with open(apids_tmp, 'r') as apids:
            for pid in apids.readlines():
                os.kill(int(pid), 1)

    if args.action == "stop":
        with open(xpids_tmp, 'r') as xpids:
            for pid in xpids.readlines():
                os.kill(int(pid), 9)
        os.remove(xpids_tmp)
        os.remove(apids_tmp)


if __name__ == '__main__':
    check_execs("Xephyr", "awesome")
    main()

#==============================================================================
# Changelog:
#==============================================================================
#
# 0.6:
#
# * Create a test configuration file if not exists
# * Use the original configuration file directly (always updated)
# * Change date format to ISO 8601 date format
#
# 0.5:
#
# * New log file per debug session
# * Improve argparse help
# * Better comments
#
# 0.4:
#
# * remove debug prints
# * clean code
#
# 0.3:
#
# * implement logging functions
# * check for executables
#
# 0.2:
#
# * argparse instead of sys.argv
# * create a new rc_test.lua file from original
# * restart the awesome session
#
# 0.1:
#
# * First attempt
#
#==============================================================================
