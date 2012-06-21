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

### `/git`

Configuración de [git](http://git-scm.com/)

### `/hg`

Configuración de [Mercurial](http://mercurial.selenic.com/)

+ `/hg/bb_gh.py` hook para Mercurial en Python para hacer `push` simultáneamente
  a el mismo repositorio en GitHub y en Bitbucket

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

### `/urxvt`

Configuración de [rxvt-unicode](http://software.schmorp.de/pkg/rxvt-unicode.html)

### `/xsession`

Configuración de la sesión __X__

+ `/xsession/xinitrc` es un shell script que configura la sesión X

+ `/xsession/xmodmap.rc` es la configuración del mapeado del teclado
