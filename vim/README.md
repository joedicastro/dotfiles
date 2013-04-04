# Mi Configuración de Vim

El propósito de este documento es recopilar todas las opciones disponibles en mi
configuración para poner un poco de orden en la misma y servirme de recordatorio
de todo lo que tengo disponible. Evidentemente no pretendo replicar la ayuda de
vim ni de los plugins, solo destacar aquellas opciones que puedo necesitar en un
determinado momento. Del mismo modo puede servir de manual de instrucciones para
aquel que decida clonar esta configuración.

Debido a la naturaleza altamente "mutante" de mi configuración, este documento
estará sujeto del mismo modo a un numero elevado de modificaciones en el futuro.

A todo esto habría que sumarle todo lo que Vim aporta de serie, que no es poco.

> La tecla `<Leader>` la tengo mapeada a `,` y la tecla `<LocalLeader>` la tengo
mapeada a `\`


## Indice

 - [Esquemas de color](#esquemas-de-color)
     - [Temas oscuros](#temas-oscuros)
        - [vim-molokai256](#vim-molokai256)
        - [molokai](#molokai)
        - [badwolf](#badwolf)
     - [Temas claros](#temas-claros)
        - [github](#github)
        - [summerfruit256](#summerfruit256)
 - [Gestion de plugins](#gestion-de-plugins)
     - [Vundle](#vundle)
 - [Operaciones con ventanas](#operaciones-con-ventanas)
     - [scratch-utility](#scratch-utility)
     - [zoomwintab](#zoomwintab)
     - [vim-powerline](#vim-powerline)
 - [Edicion de texto](#edicion-de-texto)
     - [Desactivar el resaltado de la ultima busqueda](#desactivar-el-resultado-de-la-ultima-busqueda)
     - [Conmutar la visualizacion de numeros de linea](#conmutar-la-visualizacion-de-numeros-de-linea)
     - [Mostrar caracteres no imprimibles](#mostrar-caracteres-no-imprimibles)
     - [Abrir/cerrar pliegues](#abrircerrar-pliegues)
     - [Copiar/pegar](#copiarpegar)
     - [Edicion rapida de varios archivos](#edicion-rapida-de-varios-archivos)
     - [Revision de ortografia](#revision-de-ortografia)
     - [Guardar como root](#guardar-como-root)
     - [Guardado rapido](#guardado-rapido)
     - [Eliminar espacios al final de la linea](#eliminar-espacios-al-final-de-la-linea)
     - [Estadisticas de texto](#estadisticas-de-texto)
     - [vim-smartinput](#vim-smartinput)
     - [vim-speeddating](#vim-speeddating)
     - [vim-surround](#vim-surround)
     - [vim-repeat](#vim-repeat)
     - [vim-commentary](#vim-commentary)
     - [YankRing](#yankring)
     - [easydigraph](#easydigraph)
     - [Gundo](#gundo)
     - [vim-expand-region](#vim-expand-region)
     - [LoremIpsum](#loremipsum)
     - [vim-characterize](#vim-characterize)
     - [vim-transpose](#vim-transpose)
     - [vim-signature](#vim-signature)
 - [Exploracion de ficheros](#exploracion-de-ficheros)
     - [Ranger](#ranger)
     - [CtrlP](#ctrlp)
 - [Edicion de codigo](#edicion-de-codigo)
     - [Contar lineas de codigo](#contar-lineas-de-codigo)
     - [neocomplcache](#neocomplcache)
     - [jedi-vim](#jedi-vim)
     - [python-mode](#python-mode)
     - [indentLine](#indentline)
     - [vim-virtualenv](#vim-virtualenv)
     - [coveragepy](#coveragepy)
     - [tagbar](#tagbar)
     - [vimux](#vimux)
     - [TagmaTasks](#tagmatasks)
     - [UltiSnips](#ultisnips)
     - [Syntastic](#syntastic)
 - [DVCS:Git](#dvcsgit)
     - [Fugitive](#fugitive)
     - [vim-gitgutter](#vim-gitgutter)
     - [tig](#tig)
 - [Desarrollo Web](#desarrollo-web)
     - [HTML5](#html5)
     - [Sparkup](#sparkup)
     - [ColorV](#colorv)
 - [Markdown](#markdown)
     - [vim-markdown-extra-preview](#vim-markdown-extra-preview)
 - [Utilidades de Linux](#utilidades-de-linux)
     - [Ack](#ack)
     - [vim-eunuch](#vim-eunuch)
     - [DirDiff](#dirdiff)
 - [Internalizacion](#internalizacion)
     - [Traduccion de ficheros .po](#traduccion-de-ficheros-.po)
 - [Organizacion de tareas](#organizacion-de-tareas)
     - [vim-orgmode](#vim-orgmode)
     - [calendar](#calendar)
     - [utl](#utl)
     - [NrrwRgn](#nrrwrgn)


## Esquemas de color

### Temas oscuros

##### vim-molokai256

Este es el tema por defecto para consola, es el tema molokai adaptado para
terminal.

![molokai256][mlk256]

  [mlk256]: http://joedicastro.com/static/pictures/molokai256.png "vim-molokai256"

#### molokai

El tema por defecto para GVim, es practicamente identico a vim-molokai256, sirva
su imagen como referencia.

#### badwolf

![badwolf][bdwf]

  [bdwf]: http://joedicastro.com/static/pictures/badwolf.png "badwolf"

#### harlequin

![harlequin][hqn]

  [hqn]: http://joedicastro.com/static/pictures/harlequin.png "harlequin"


### Temas claros

#### github

![github][gh]

  [gh]: http://joedicastro.com/static/pictures/github.png "github"


#### summerfruit256

![summerfruit256][summ]

  [summ]: http://joedicastro.com/static/pictures/summerfruit256.png "summerfruit256"


## Gestion de Plugins

### Vundle

Un plugin para gobernarlos a todos! Me permite administrar el resto de los
plugins, el mismo incluido. A su vez lo tengo configurado para que se instale a
si mismo la primera vez que se ejecute vim con esta configuración (también
instala automáticamente el resto de plugins)

Funciona a través de comandos y de forma interactiva.

### Ayuda

`:h vundle.txt`, `:BundleDocs`

### Comandos

- `:BundleList`, muestra una ventana con la lista de todos los plugins
  instalados
- `:BundleInstall`, instala aquellos plugins que estén configurados en .vimrc
- `:BundleClean`, elimina los plugins que ya no estén en .vimrc
- `:BundleUpdate`, actualiza los plugins actualmente instalados
- `:BundleSearch [plugin]`, busca un plugin por su nombre en el repositorio de
  vimscripts
- `:Bundles`, muestra la lista de todos los plugins disponibles en el
  repositorio de vimscripts


## Operaciones con ventanas

### Atajos

- `<Leader>v` crea una nueva ventana vertical
- `<Leader>h` crea una nueva ventana horizontal
- `<C-H>` desplazamiento a la siguiente ventana a la izquierda
- `<C-J>` desplazamiento a la ventana inferior
- `<C-K>` desplazamiento a la ventana superior
- `<C-L>` desplazamiento a la siguiente ventana a la derecha
- `<Leader>m` cierra la ventana actual
- `<Leader>q` cierra la ventana QuickFix


### scratch-utility

![Scratch](http://joedicastro.com/static/pictures/scratch.gif "Scratch")

Nos crea un nuevo buffer temporal en el que no se guardara nada de lo que
editemos en ella, el contenido es descartado en cuanto cerramos la aplicación.
La ventana aparecera siempre encima de la ventana actual

__Atajo__ `<F8>` o `:Scratch` Mostrar/Ocultar la ventana Scratch


### zoomwintab

![zoom](http://joedicastro.com/static/pictures/zoomwintab.gif "zoom")

Hace zoom sobre una ventana, ocultando el resto. 

__Ayuda__ `:h zoomwintab.vim`

__Atajo__ `<Leader>z` o `:ZoomWinTabToggle`


### vim-powerline

![Powerline](http://joedicastro.com/static/pictures/powerline.gif "Powerline")

Es una linea de estado mejorada, mas agradable visualmente y preconfigurada para
mostrar bastante información útil sobre cada buffer (modo, tipo de fichero,
codificación, nombre, información de Git, ...).

__Ayuda__ `:h Powerline.txt`

__Comandos__ `:PowerlineClearCache` útil para cuando introducimos cambios en la
configuración de la misma y no se ven reflejados por culpa del cache.

## Edicion de texto

### Desactivar el resaltado de la ultima busqueda

![nohlsearch][nhs]

  [nhs]: http://joedicastro.com/static/pictures/nohlsearch.gif  "nohlsearch"

__Atajo__ `<Leader>sq`

### Conmutar la visualizacion de numeros de linea

![Conmutar numeros de linea][cnl]

  [cnl]: http://joedicastro.com/static/pictures/linenumbers.gif "Conmutar numeros de linea"

  Conmuta entre no mostrar los números de línea, mostrarlos relativos y
  mostrarlos absolutos.

  __Atajo__ `<Leader>l`

### Mostrar caracteres no imprimibles

![hiddenchars][hdc]

  [hdc]: http://joedicastro.com/static/pictures/hiddenchars.gif "mostrar caracteres no imprimibles"

__Atajo__ `<Leader>sh`


### Abrir/cerrar pliegues

![unfold][ufl]

  [ufl]: http://joedicastro.com/static/pictures/unfold.gif "abrir/cerrar pliegues"

__Atajo__ `<Space>`

### Copiar/pegar

__Atajos__

- `<Leader>y` copiar al portapapeles
- `<Leader>p` pegar desde el portapapeles
- `<Leader>P` conmutar el paste mode

### Edicion rapida de varios archivos

__Atajos__

- `<Leader>ev` edita el archivo `~/.vimrc` en una nueva ventana vertical
- `<Leader>es` edita el archivo de snippets globales
  (`~/.vim/UltiSnips/all.snippets`) en una nueva ventana vertical

### Revision de ortografia

__Atajos__

- `<Leader>ss` activa la corrección ortográfica en español
- `<Leader>se` activa la corrección ortográfica en ingles
- `<Leader>so` desactiva la corrección ortográfica
- `<Leader>sn` se desplaza a la siguiente palabra mal escrita

### Guardar como root

Permite guardar un archivo que solo tiene permisos para `root` sin necesidad de
ejecutar vim desde ese usuario o utilizando `$ sudo` y perder de ese modo las
ventajas de nuestra configuración.

__Comando__ `:w!!`

### Guardado rapido

Para guardar rápidamente un archivo sin tener que ejecutar el comando `:w`

__Atajo__ `<Leader>w`

### Eliminar espacios al final de la linea

![remove_trail](http://joedicastro.com/static/pictures/remove_trail_spaces.gif "eliminar espacios finales")


Elimina esos espacios que se quedan a veces al final de la linea y que suelen
ser casi siempre innecesarios y sin cometido alguno (excepto quizas en Markdown)

__Atajo__ `<Leader>rt`


### Estadisticas de texto

![text stats]( http://joedicastro.com/static/pictures/textstats.gif "estadisticas de texto")

Obtener el numero de lineas, palabras, caracteres y bytes (totales y de la
posición actual)

__Atajo__ `<Leader>st`

![frecuencia de palabras](
http://joedicastro.com/static/pictures/word_frecuency.gif "frecuencia de
palabras")

Obtener la frecuencia con la que aparece cada palabra para un texto dado, abre
una nueva ventana con las estadísticas.

__Atajo__ `<Leader>sw`

### vim-smartinput

![smartinput](http://joedicastro.com/static/pictures/smartinput.gif "smartinput")

Provee de autocompletado inteligente para pares de caracteres muy empleados en
programación como son __(), {}, [], "", '', ``__

El funcionamiento es muy sencillo, si escribimos el primero de este par de
caracteres, aparece automaticamente el segundo y el cursor se mueve al interior
de los mismos. Entonces escribimos lo que queremos y cuando acabemos solo
tenemos que introducir el segundo caracter. Sin en cambio solo quisieramos el
primero, bastaria con pulsar la tecla __Delete__

__Ayuda__ `:h smartinput.txt`

### vim-speeddating

![speeddating](http://joedicastro.com/static/pictures/speeddating.gif "speeddating")

Sirve para incrementar/decrementar de forma inteligente valores de fechas y
horas.  

__Ayuda__ `:speeddating.txt` 

__Atajos__

- `<C-A>` Incrementa el valor bajo el cursor una unidad
- `<C-X>` Decrementa el valor bajo el cursor una unidad
- `d<C-A>` Cambia la fecha/hora bajo el cursor a la hora actual en UTC
- `d<C-X>` Cambia la fecha/hora bajo el cursor a la hora actual en local

__Comandos__

- `:SpeedDatingFormat` Lista los formatos definidos
- `:SpeedDatingFormat!` Ayuda para los formatos soportados
- `:SpeedDatingFormat {format}` Define un formato nuevo
- `:SpeedDatingFormat! {format}` Eliminar un formato existente

### vim-surround

__Ayuda__ `:h surround.txt`

TODO

### vim-repeat

TODO

### vim-commentary

![commentary](http://joedicastro.com/static/pictures/commentary.gif "commentary")

Herramienta extremadamente sencilla para comentar/descomentar fragmentos de
texto/código. Simplemente tenemos que pulsar un atajo seguido de un movimiento
para comentar/descomentar o pulsar el atajo después de una selección visual. 

__Ayuda__ `:h commentary.txt`

__Atajo__ `<Leader>c` o `gc`

### YankRing

TODO

### easydigraph

TODO

### Gundo

TODO

### vim-expand-region

TODO

### LoremIpsum

TODO

### vim-characterize

TODO

### vim-transpose

TODO

### vim-signature

TODO

## Exploracion de ficheros

### Ranger

![ranger](http://joedicastro.com/static/pictures/ranger_vim.gif "ranger")

A través de esto atajo llamo al programa externo
[Ranger](http://joedicastro.com/productividad-linux-ranger.html) para navegar
por el sistema de ficheros y elegir el fichero que queremos editar.

__Atajo__ `<Leader>ra`

### CtrlP

TODO

## Edicion de codigo

### Contar lineas de codigo

![cloc](http://joedicastro.com/static/pictures/cloc.gif "cloc")

Ejecuta el programa externo `$ cloc` sobre el fichero y abre una nueva ventana
con el resultado.

__Atajo__ `<Leader>sc`

### neocomplcache

TODO

### jedi-vim

TODO

### python-mode

TODO    

### indentLine

TODO

### vim-virtualenv

TODO

### coveragepy

TODO

### tagbar

TODO

### vimux

TODO

### TagmaTasks

TODO

### UltiSnips

TODO

### Syntastic

TODO


## DVCS: Git

### Fugitive

TODO

### vim-gitgutter

TODO

### tig

TODO

## Desarrollo Web

### HTML5

TODO

### Sparkup

TODO

### ColorV

TODO


## Markdown

### vim-markdown-extra-preview

TODO

## Utilidades de Linux/Unix

### Ack

TODO

### vim-eunuch

TODO

### DirDiff

TODO

## Internalizacion

### Traduccion de ficheros .po

TODO

## Organizacion de tareas

### vim-orgmode

TODO    

### calendar

TODO 

### utl 

TODO

### NrrwRgn

TODO

























# ...Work in progress!
