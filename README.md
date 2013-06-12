# dotfiles

Este es un repositorio de mis archivos de configuración, lo que en Linux
normalmente son los ficheros del `$HOME` que están ocultos y precedidos por un
punto.

## Contenido

Este es el contenido actual de este repositorio, reseñando los archivos
significativos.

### `/awesome`

Configuración de [Awesome](http://awesome.naquadah.org/)

+ `/awesome/awdt.py` script python que me permite testear cambios en la
  configuración de Awesome. Arranca una sesión X anidada dentro de la actual y
  servida por Xephyr, con un Awesome con una configuración de pruebas corriendo
  dentro de la misma

+ `/awesome/prep.org` archivo en formato Org-mode en el que anoto los
  repositorios, autores y licencias de las librerías externas que empleo en la
  configuración. Contiene además los ficheros necesarios para hacerlo funcionar
  conjuntamente con Gnome 2

+ `/awesome/check_execs.py` & `/awesome/logger.py` son enlaces simbólicos a dos
  ficheros python que pueden ser encontrados en este repositorio,
  <https://github.com/joedicastro/python-recipes>

### `/compton`

Configuración de [Compton](https://github.com/chjj/compton)

### `/dunst`

Configuración de [dunst](https://github.com/knopwob/dunst)

### `/emacs`

Configuración de [Emacs](http://www.gnu.org/software/emacs/) para usar 
[Org-mode](http://orgmode.org/) 

### `/fontconfig`

Configuración de [fontconfig](http://www.freedesktop.org/wiki/Software/fontconfig)

### `/git`

Configuración de [git](http://git-scm.com/)

### `/gtk`

Configuración del tema __Gtk__ a emplear y corregir un fallo en la ventana de
Gvim

### `/hg`

Configuración de [Mercurial](http://mercurial.selenic.com/)

+ `/hg/bb_gh.py` hook para Mercurial en Python para hacer `push` simultáneamente
  a el mismo repositorio en GitHub y en Bitbucket

### `/mpd`

Configuración de [mpd](http://mpd.wikia.com/wiki/Music_Player_Daemon_Wiki)

### `/ncmpcpp`

Configuración de [ncmpcpp](http://ncmpcpp.rybczak.net/)

### `/newsbeuter`

Configuración de [Newsbeuter](http://newsbeuter.org/)

+ `/newsbeuter/readitlater` API Python para Pocket (Formerly Read it Later)

+ `/newsbeuter/getfromril.py` script Python que emplea la API de Pocket para
   hacer una copia de seguridad de las urls guardadas en Pocket

+ `/newsbeuter/notify.py` script Python que genera las notificaciones emergentes
  que emite newsbeuter al acabar de actualizar las fuentes de noticias

+ `/newsbeuter/sen2ril.py` script Python que emplea la API de Pocket y me
  permite guardar marcadores en Pocket directamente desde Newsbeuter

### `/pentadactyl`

Configuración de [Pentadactyl](http://5digits.org/pentadactyl/)

### `/ranger`

Configuración de [ranger](http://ranger.nongnu.org/)

### `/tmux`

Configuración de [tmux](http://tmux.sourceforge.net/)

### `/tmuxinator`

Configuración de [Tmuxinator](https://github.com/aziz/tmuxinator)

### `/turses`

Configuración de [Turses](https://github.com/alejandrogomez/turses)

### `/urxvt`

Configuración de [rxvt-unicode](http://software.schmorp.de/pkg/rxvt-unicode.html)

#### `/vim`

Configuración de [Vim](http://www.vim.org)

+ `/vim/vimrc` es el archivo de configuración de Vim
+ `/vim/README.md` es un resumen de las opciones disponibles en mi configuracion
+ `/vim/spell/` son los archivos necesarios para la correccion ortografica
+ `/vim/UltiSnips/` son mis snippets personalizados para [UltiSnips][ulsns]

  [ulsns]: https://github.com/SirVer/ultisnips

### `/xsession`

Configuración de la sesión __X__

+ `/xsession/xinitrc` es un shell script que configura la sesión X

+ `/xsession/xmodmap.rc` es la configuración del mapeado del teclado

### `/zathura`

Configuración de [zathura](http://pwmt.org/projects/zathura/)
