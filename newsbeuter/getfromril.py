#!/usr/bin/env python
# -*- coding: utf8 -*-

"""
    getfromril.py: Get the urls stored in Pocket (formerly Read it Later) &
    save them in a Org-mode file
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
__date__ = "20/06/2012"
__version__ = "0.2"

try:
    import sys
    import os
    import readitlater
    import ril_config as config
except ImportError:
    # Checks the installation of the necessary python modules
    print((os.linesep * 2).join(["An error found importing one module:",
    str(sys.exc_info()[1]), "You need to install it", "Stopping..."]))
    sys.exit(-2)
import os


def main():
    """Main section"""
    api = readitlater.API(config.RIL_APIKEY, config.RIL_USERNAME,
                          config.RIL_PASSWORD)

    items = api.get(state="unread")
    lista = items["list"]
    with open("ril_urls.org", "w") as output:
        output.write("* Read It Later URLs" + os.linesep)
        for i, k in lista.items():
            output.write("** {0}{1}".format(k['title'].encode("utf8"),
                        os.linesep))
            output.write("   [[{0}][Enlace]]{1}{1}".format(k['url'].
                        encode("utf8"), os.linesep))


if __name__ == "__main__":
    main()


###############################################################################
#                                  Changelog                                  #
###############################################################################
#
# 0.2:
#
# * Renamed (from get.py to getfromril.py)
#
# 0.1:
#
# * First attempt
#
