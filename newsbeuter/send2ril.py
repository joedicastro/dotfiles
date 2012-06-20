#!/usr/bin/env python
# -*- coding: utf8 -*-

"""
    send2ril.py: Send a new url to Pocket (formerly Read it Later)
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
__date__ = "18/06/2012"
__version__ = "0.1"

import sys
import readitlater
import ril_config as config


def main():
    """Main section"""
    api = readitlater.API(config.RIL_APIKEY, config.RIL_USERNAME,
                          config.RIL_PASSWORD)

    new = [{"url":sys.argv[1], "title":sys.argv[2]}]
    api.send(new=new)


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
