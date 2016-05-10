#!/usr/bin/env python2
# -*- coding: utf8 -*-

"""
    lock.py: Interrumpt the dunst notification service when the screen is
    locked via slimlock and resume it when is unlocked.
"""

__author__ = "joe di castro <joe@joedicastro.com>"
__license__ = "GNU General Public License version 3"
__date__ = "21/02/2013"
__version__ = "0.1"

try:
    import sys
    import os
    import subprocess
    import time
except ImportError:
    # Checks the installation of the necessary python modules
    print((os.linesep * 2).join(["An error found importing one module:",
          str(sys.exc_info()[1]), "You need to install it", "Stopping..."]))
    sys.exit(-2)


def main():
    """Main section"""
    subprocess.call("killall -SIGUSR1 dunst".split())
    subprocess.call('slimlock')
    while True:
        try:
            subprocess.check_output('pgrep slimlock'.split())
        except:
            subprocess.call("killall -SIGUSR2 dunst".split())
            sys.exit()
        time.sleep(2)


if __name__ == "__main__":
    main()

###############################################################################
#                                  Changelog                                  #
###############################################################################
#
# 0.1:
#
# * First attempt
#
