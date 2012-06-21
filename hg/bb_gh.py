#!/usr/bin/env python
# encoding: utf-8

"""
    bb_gh_sync.py: Mercurial hook to keep synced a repo to Bitbucket & GitHub.
"""

#==============================================================================
# This script maintain synced a repository to booth github and bitbucket sites,
# using only a local mercurial repository. To do this, makes use of hg-git, the
# paths defined in my local hg repo and the environment variable given by hg, to
# push to the site non described in the command line argument. This way, it's
# irrelevant which site I decided to push every time, booth are done by this
# hook.
##==============================================================================

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
__date__ = "23/04/2012"
__version__ = "0.1"


import os
from tempfile import gettempdir
from subprocess import call


def main():
    """Main section"""

    tmp_dir = gettempdir()
    lock_file = os.path.join(tmp_dir, "bb_gh.lock")

    # make sure that only runs once for each repository
    if not os.path.exists(lock_file):
        open(lock_file, "w").close()
        # if pushed to bitbucket, push to github too
        if os.environ['HG_ARGS'] == "push bitbucket":
            call(["/usr/bin/env", "hg", "push", "github"])
        # et viceversa...
        if os.environ['HG_ARGS'] == "push github":
            call(["/usr/bin/env", "hg", "push", "bitbucket"])
    else:
        os.remove(lock_file)


if __name__ == "__main__":
    main()
