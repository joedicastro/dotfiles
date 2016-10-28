# dotfiles
This is a repository with my configuration files, those that in Linux normally
are these files under the `$HOME` directory that are hidden and preceded by a
dot, AKA __dotfiles__

## Management

These configuration files are managed with the tool [GNU
Stow](https://www.gnu.org/software/stow) that is not intented originally for
this purpose but works pretty well.

To use this configurations you only have to clone this repository in a directory
of your home directory (`~/dotfiles` by default) and then use stow to "install"
them, choosing the ones that you want to use.

For example to use the Vim configuration you only have to do this (from the
dotfiles directory):

`$ stow vim`

and it would create a symlink for `~/.vim` and `~/.vimrc` to the same files/dirs
in the `~/dotfiles/vim` folder of this repository. Those symbolic links only
would be created if not exists previously, in that case I recommend to make a
backup of the original files first.

To remove a configuration is also very easy with stow:

`$ stow -D vim`

and it would remove the symbolic links.

And if you make a git pull to update this repository, you can "reinstall" the
configurations to update them. Stow would first remove the previous symbolic
links and then create the new ones.

`$ stow -R vim`

Stow is a package that you can find in the majority of Linux distributions and
make the process of test/manage these configurations easier.

## Content

That's the *current content* of this repository, and these are the more remarkable
files.

### `/awesome`

Configuration of [Awesome](http://awesome.naquadah.org/)

+ `.config/awesome/awdt.py` python script that allows me to test changes in the
  Awesome configuration. Starts a X session nested into the current session and
  served by Xephyr, with an test configuration (Awesome) running inside it.

+ `.config/awesome/prep.org` is a Org-mode format file in which I store the
  repositories, authors and licenses of the external libraries that I use in
  this configuration.

+ `.config/awesome/check_execs.py` & `/awesome/logger.py` are symbolic links to
  two python files that can be founded in this repository,
  <https://github.com/joedicastro/python-recipes>

### `/compton`

Configuration of [Compton](https://github.com/chjj/compton)

### `/dunst`

Configuration of [dunst](https://github.com/knopwob/dunst)

### `/emacs`

Configuration of [Emacs](http://www.gnu.org/software/emacs/)

### `/fontconfig`

Configuration of [fontconfig](http://www.freedesktop.org/wiki/Software/fontconfig)

### `/fonts`

The fonts that I use in my terminal, vim, etc ...

 - [Dejavu Sans Mono](http://dejavu-fonts.org) is a free public domain font and
   has the probably best Unicode support from all the monospaced fonts
   available.
 - Dejavu Sans Mono for Powerline is the same font adapted to use it with
   Powerline in Vim

### `/git`

Configuration of [git](http://git-scm.com/)

### `/gnupg`

Configuration of [GnuPG](https://www.gnupg.org/)

### `/gtk`

Configuration of the __Gtk__ theme to fix an error with the Gvim window

### `/hg`

Configuration of [Mercurial](http://mercurial.selenic.com/)

### `/livestreamer`

Configuration of [Livestreamer](https://github.com/chrippa/livestreamer)

### `/mpd`

Configuration of [mpd](http://mpd.wikia.com/wiki/Music_Player_Daemon_Wiki)

### `/ncmpcpp`

Configuration of [ncmpcpp](http://ncmpcpp.rybczak.net/)

### `/pylint`

Configuration of [Pylint](http://www.pylint.org/)

### `/ranger`

Configuration of [ranger](http://ranger.nongnu.org/)

### `/systemd`

Configuration of [systemd](https://www.freedesktop.org/wiki/Software/systemd/) user units and timers

### `/tmux`

Configuration of [tmux](http://tmux.sourceforge.net/)

### `/urxvt`

Configuration of [rxvt-unicode](http://software.schmorp.de/pkg/rxvt-unicode.html)

#### `/vim`

Configuration of [Vim](http://www.vim.org)

+ `.vimrc` the Vim configuration file
+ `.vim/README.md` is a summary of my Vim configuration customizations
+ `.vim/spell/` files needed for spelling
+ `.vim/UltiSnips/` my custom [UltiSnips][ulsns] snippets

  [ulsns]: https://github.com/SirVer/ultisnips

### `/vimperator`

Configuration of [Vimperator](http://www.vimperator.org/vimperator)

### `/xsession`

Configuration of the __X__ session

+ `.xprofile` bash script to setup the X session
+ `.config/user-dirs.dirs` default home user directories

### `/zathura`

Configuration of [zathura](http://pwmt.org/projects/zathura/)
