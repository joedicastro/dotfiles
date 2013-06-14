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

    Esta configuración está basada en las de muchos otros, tantos que ni los
    recuerdo a todos y seria bastante injusto recordar a algunos y omitir a otros.
    Pero gracias a que muchos comparten generosamente su configuración, yo he
    llegado a la mía y sirva esta documento también de pequeña compensación por lo
    mucho que otros me han aportado.


## Indice

 - [Esquemas de color](#esquemas-de-color)
     - [Temas oscuros](#temas-oscuros)
        - [vim-molokai256](#vim-molokai256)
        - [molokai](#molokai)
        - [badwolf](#badwolf)
     - [Temas claros](#temas-claros)
        - [github256](#github256)
        - [summerfruit256](#summerfruit256)
 - [Gestion de plugins](#gestion-de-plugins)
     - [Vundle](#vundle)
     - [Actualizacion de plugins](#actualizacion-de-plugins)
 - [Operaciones con ventanas](#operaciones-con-ventanas)
     - [scratch-utility](#scratch-utility)
     - [zoomwintab](#zoomwintab)
     - [vim-powerline](#vim-powerline)
     - [winresizer](#winresizer)  
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
     - [utl](#utl)
 - [Edicion de codigo](#edicion-de-codigo)
     - [Contar lineas de codigo](#contar-lineas-de-codigo)
     - [neocomplete](#neocomplete)
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
     - [Editor hexadecimal](#editor-hexadecimal)
 - [Internalizacion](#internalizacion)
     - [Traduccion de ficheros .po](#traduccion-de-ficheros-.po)

## Esquemas de color

### Temas oscuros

##### vim-molokai256

Este es el tema por defecto para consola, es el tema molokai adaptado para
terminal.

![molokai256][mlk256]

  [mlk256]: http://joedicastro.com/static/pictures/molokai256.png "vim-molokai256"

*Repositorio:* <https://github.com/tomasr/molokai>

#### molokai

El tema por defecto para GVim, es practicamente identico a vim-molokai256, sirva
su imagen como referencia.

*Repositorio:* <https://github.com/joedicastro/vim-molokai256>

#### badwolf

![badwolf][bdwf]

  [bdwf]: http://joedicastro.com/static/pictures/badwolf.png "badwolf"

*Repositorio:* <https://github.com/sjl/badwolf>

#### harlequin

![harlequin][hqn]

  [hqn]: http://joedicastro.com/static/pictures/harlequin.png "harlequin"

*Repositorio:* <https://github.com/nielsmadan/harlequin>

### Temas claros

#### github256

![github][gh]

  [gh]: http://joedicastro.com/static/pictures/github.png "github"

*Repositorio:* <https://github.com/joedicastro/vim-github256>

#### summerfruit256

![summerfruit256][summ]

  [summ]: http://joedicastro.com/static/pictures/summerfruit256.png "summerfruit256"

*Repositorio:* <https://github.com/vim-scripts/summerfruit256.vim>

## Gestion de Plugins

### Vundle

Un plugin para gobernarlos a todos! Me permite administrar el resto de los
plugins, el mismo incluido. A su vez lo tengo configurado para que se instale a
si mismo la primera vez que se ejecute vim con esta configuración (también
instala automáticamente el resto de plugins)

Funciona a través de comandos y de forma interactiva.

__Ayuda__ `:h vundle.txt`, `:BundleDocs` <vimhelp:vundle.txt>

__Comandos__

- `:BundleList`, muestra una ventana con la lista de todos los plugins
  instalados
- `:BundleInstall`, instala aquellos plugins que estén configurados en .vimrc
- `:BundleClean`, elimina los plugins que ya no estén en .vimrc
- `:BundleUpdate`, actualiza los plugins actualmente instalados
- `:BundleSearch [plugin]`, busca un plugin por su nombre en el repositorio de
  vimscripts
- `:Bundles`, muestra la lista de todos los plugins disponibles en el
  repositorio de vimscripts

*Repositorio:* <https://github.com/gmarik/vundle>

### Actualizacion de Plugins

Vundle es una muy buena y potente herramienta que nos permite administrar de
forma muy sencilla nuestros plugins. Pero es una herramienta que conviene
utilizar con cuidado, sentido común y guardando unas ciertas precauciones, sobre
todo si usamos vim como herramienta profesional.

Dado que en mi caso y en el de muchos otros, instalamos y actualizamos varios
plugins directamente desde el repositorio git, esto nos expone a actualizaciones
inestables de los mismos.

Este es mi modo de actualizar los plugins que puede servir como guia u
orientación de como hacerlo sin que nuestro flujo de trabajo se vea
interrumpido.

Lo primero es aclarar que mi configuración de vim está situada en el directorio
`$HOME/dotfiles/vim` y utilizo enlaces simbólicos para que vim pueda localizar
la configuración que espera por defecto. En concreto lo tengo establecido de
esta manera:

- `~/.vim` es un enlace simbólico que apunta a `~/dotfiles/vim`
- `~/.vimrc` es un enlace simbólico que apunta a `~/dotfiles/vim/vimrc`

De modo que cuando quiero hacer una actualización de los plugins de vim, lo
único que tengo que hacer es crear una copia de la carpeta de vim en
`~/dotfiles` para tener un backup en caso de que algún plugin no funcione como
es debido.

De hecho, hago esto sin salir de Vim y con la ayuda de ranger, en sencillos
pasos:

 1. entro en ranger desde vim pulsando `<Leader>rt`
 2. voy a la carpeta `vim` en `~/dotfiles` y creo una copia pulsando `yy` y
 luego `pp` y me crea una copia llamada `vim_`
 3. actualizo los plugins de vim con `:BundleUpdate`
 4. sigo trabajando con normalidad
 5. si veo que algún plugin se ha vuelto inestable durante la actualización y lo
 necesito para seguir trabajando, simplemente borro la carpeta `~/dotfiles/vim`
 y renombro la copia en `~/dotfiles/vim_` a `~/dotfiles/vim`. Con salir de vim y
 volver a entrar tenemos una configuración probada y completamente funcional sin
 esfuerzo.

Usando este procedimiento o algo similar nos ahorramos muchos disgustos a la
hora de realizar las actualizaciones. Evidentemente, en caso de que solo falle
un plugin siempre podemos sustituir únicamente la carpeta de ese plugin dentro
de `~/dotfiles/vim/bundle` en lugar de toda la configuración de Vim.

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

*Repositorio:* <https://github.com/vim-scripts/scratch-utility>

### zoomwintab

![zoom](http://joedicastro.com/static/pictures/zoomwintab.gif "zoom")

Hace zoom sobre una ventana, ocultando el resto. 

__Ayuda__ `:h zoomwintab.vim` <vimhelp:zoomwintab.vim>

__Atajo__ `<Leader>z` o `:ZoomWinTabToggle`

*Repositorio:* <https://github.com/vim-scripts/zoomwintab.vim>

### vim-powerline

![Powerline](http://joedicastro.com/static/pictures/powerline.gif "Powerline")

Es una linea de estado mejorada, mas agradable visualmente y preconfigurada para
mostrar bastante información útil sobre cada buffer (modo, tipo de fichero,
codificación, nombre, información de Git, ...).

__Ayuda__ `:h Powerline.txt` <vimhelp:Powerline.txt>

__Comandos__ `:PowerlineClearCache` útil para cuando introducimos cambios en la
configuración de la misma y no se ven reflejados por culpa del cache.

*Repositorio:* <https://github.com/Lokaltog/powerline>

### winresizer

![winresizer](http://joedicastro.com/static/pictures/winresizer.gif "winresizer")

Es un plugin que sirve para redimensionar (cambiar de tamaño) muy fácilmente las
ventanas de vim.

__Atajos__

- `<C-E>` o `:WinResizerStartResize` activa el redimensionamiento de las ventanas
- `h` mueve el divisor de ventanas hacia la izquierda
- `l` mueve el divisor de ventanas hacia la derecha
- `j` mueve el divisor de ventanas hacia abajo
- `k` mueve el divisor de ventanas hacia arriba
- `<CR>` finaliza el redimensionado
- `q` cancela el redimensionado 

*Repositorio:* <https://github.com/jimsei/winresizer>


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

__Ayuda__ `:h smartinput.txt` <vimhelp:smartinput.txt>

*Repositorio:* <https://github.com/kana/vim-smartinput>

### vim-speeddating

![speeddating](http://joedicastro.com/static/pictures/speeddating.gif "speeddating")

Sirve para incrementar/decrementar de forma inteligente valores de fechas y
horas.  

__Ayuda__ `:speeddating.txt`  <vimhelp:speeddating.txt>

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

*Repositorio:* <https://github.com/tpope/vim-speeddating>

### vim-surround

![surround](http://joedicastro.com/static/pictures/surround.gif "surround")

Nos sirve para "envolver" un objeto de texto de vim con un par de caracteres o
etiquetas similares (parentesis, comillas, etiquetas HTML, ...). Tambien nos
permite cambiar o eliminar los ya existentes.

__Ayuda__ `:h surround.txt` <vimhelp:surround.txt>

__Atajos__

- `ys{motion or text-object}{char}` crear "envolvente" (*'your surround'*)
- `cs{orig_char}{dest_char}` cambiar "envolvente" (*'change surround'*)
- `ds{char}` eliminar "envolvente"  (*'delete surround'*)
- `S{char}`

> Si elegimos el primer miembro de un par, e.g '(', entonces nos crea el
> envolvente con un espacio entre el envolvente y la seleccion. Si elegimos el
> ultimo miembro, e.g. ')', entonces nos lo crea sin los espacios.

*Repositorio:* <https://github.com/tpope/vim-surround>

### vim-repeat

Este es un plugin muy sencillo creado por Tim Pope para dar soporte al operando
de Vim repetición `.` en la mayoría de sus plugins. En este caso da soporte a
*vim-speeddating*, *vim-surroud* y *vim-commentary*

__Ayuda__ `:h repeat.txt` <vimhelp:repeat.txt>

__Atajos__

- `.` repite la ultima operación una vez

*Repositorio:* <https://github.com/tpope/vim-repeat>

### vim-commentary

![commentary](http://joedicastro.com/static/pictures/commentary.gif "commentary")

Herramienta extremadamente sencilla para comentar/descomentar fragmentos de
texto/código. Simplemente tenemos que pulsar un atajo seguido de un movimiento
para comentar/descomentar o pulsar el atajo después de una selección visual. 

__Ayuda__ `:h commentary.txt` <vimhelp:commentary.txt>

__Atajo__ `<Leader>c` o `gc`

*Repositorio:* <https://github.com/tpope/vim-commentary>

### YankRing

![YankRing](http://joedicastro.com/static/pictures/yankring.gif "YankRing")

Es un plugin que pretende trasladar a Vim la opción de Emacs llamada "kill
ring". Este plugin consiste en una lista de los bloques de texto que han sido
previamente copiados/cortados/pegados en los registros/portapapeles.

__Ayuda__ `:h yankring.txt` <vimhelp:yankring.txt>

__Atajos__

Este plugin es muy completo, pero actualmente solo estoy empleando una opción de
las múltiples que soporta:

- `<Leader>i` conmuta la venta de YankRing que muestra las entradas disponibles

__Comandos__ 

- `:YRShow` muestra/oculta la ventana de YankRing 
- `:YRSearch {termino de busqueda/regex}` busca (se pueden emplear regex) en la lista
- `:YRToggle` activa/desactiva el plugin
- `:YRClear` limpia la lista

*Repositorio:* <https://github.com/vim-scripts/YankRing.vim>

### easydigraph

![easydigraph](http://joedicastro.com/static/pictures/easydigraph.gif "easydigraph")

Herramienta para insertar un dígrafo de forma bastante sencilla, sobre todo
cuando se trata de insertar varios simultáneamente.

__Ayuda__ `:h easydigraph.txt@en` <vimhelp:easydigraph.txt@en>

__Atajos__ 

- `<Leader>bb {motion}` convierte en dígrafo la selección efectuada con el
  movimiento. 

__Comandos__ 

- `:digraphs` muestra una tabla con todos los dígrafos disponibles y los
  caracteres necesarios para generarlos

*Repositorio:* <https://github.com/Rykka/easydigraph.vim>

### Gundo

![gundo](http://joedicastro.com/static/pictures/gundo.gif "gundo")

Sirve para hacer mas amigable la utilización del árbol de deshacer de Vim. De
esta manera podemos ver el árbol de los cambios realizados, previsualizar los
cambios que vamos a realizar y saber a donde vamos a retornar antes de deshacer
un cambio.

__Ayuda__ `:h gundo.txt` <vimhelp:gundo.txt>

__Atajos__ `<Leader>u` abre el interfaz de ventanas de Gundo

*Repositorio:* <https://github.com/sjl/gundo.vim>

### vim-expand-region

![expand-region](http://joedicastro.com/static/pictures/expand-region.gif "expand-region")

Nos sirve para incrementar/decrementar una seleccion visual de forma inteligente.

__Ayuda__ `:h vim-expand-region.txt` <vimhelp:vim-expand-region.txt>

__Atajos__

- `-` decrementa la seleccion visual
- `+` incrementa la seleccion visual

*Repositorio:* <https://github.com/terryma/vim-expand-region>

### LoremIpsum

![loremipsum](http://joedicastro.com/static/pictures/loremipsum.gif "loremipsum")

Sirve para generar texto aleatorio para rellenar borradores y pruebas de diseño,
muy usado en diseño web. Genera el famoso texto [Lorem Ipsum][lorem]

  [lorem]: http://es.wikipedia.org/wiki/Lorem_ipsum

__Ayuda__ `:h loremipsum.txt` <vimhelp:loremipsum.txt>

__Comandos__

- `:LoremIpsum {num palabras}` inserta el texto Lorem Ipsum de forma aleatoria

*Repositorio:* <https://github.com/vim-scripts/loremipsum>

### vim-characterize

![characterize](http://joedicastro.com/static/pictures/characterize.gif "characterize")

Muestra información ampliada sobre un carácter. Muestra el valor Unicode en
decimal, hexadecimal, octal, el nombre Unicode, la HTML entity, el codigo Emoji
y cualquier dígrafo disponible.

__Ayuda__ `:h characterize.txt` <vimhelp:characterize.txt>

__Atajo__ `ga` muestra la información sobre el carácter.


*Repositorio:* <https://github.com/tpope/vim-characterize>

### vim-transpose

![transpose](http://joedicastro.com/static/pictures/transpose.gif "transpose")

Sirve para transponer filas y columnas, que puede ser muy útil para editar
cierto tipo de ficheros (e.g. *csv*). Funciona con selecciones visuales.

__Ayuda__ `:h transpose.txt` <vimhelp:transpose.txt>

__Comandos__

- `:Transpose` hace la transposición por defecto
- `:TransposeCSV {separador} {delimitador}` hace la transposición teniendo en cuenta la separación por
  punto y coma o el separador que le especifiquemos y el delimitador
- `:TransposeTab` hace la transposición teniendo en cuenta los tabulados
- `:TransposeWords` hace la transposición por palabras e inserta una
  interrogación donde falte una
- `:TransposeInteractive` para transposiciones complejas

*Repositorio:* <https://github.com/salsifis/vim-transpose>

### vim-signature

![signature](http://joedicastro.com/static/pictures/signature.gif "signature")

Un plugin que sirve para conmutar, mostrar y navegar por los marcadores. Los
marcadores se muestran en la columna lateral de signos de Vim, a la izquierda de
los números de línea. 

__Ayuda__  `:h signature` <vimhelp:signature>

__Atajos__

- Marcadores alfabéticos

  - `m[a-zA-Z]` conmuta la marca y la muestra/oculta
  -  `m,`       activa el siguiente marcador disponible 
  -  `m<Space>` borra todos los marcadores 
  -  `]``      salta al marcador siguiente 
  -  `[``      salta al marcador previo 
  -  `]'`       salta al comienzo de la siguiente línea que tenga un marcador
  -  `['`       salta al comienzo de la anterior línea que tenga un marcador

- Marcadores simbólicos

  -  `m[0-9]`       activa el marcador simbólico correspondiente !@#$%^&*()
  -  `m<S-[0-9]>`   eliminar todos los marcadores iguales 
  -  `]-`           salta a la siguiente línea que tenga el mismo marcador
  -  `[-`           salta a la anterior línea que tenga el mismo marcador
  -  `m<BS>`        elimina todos los marcadores simbólicos

__Comandos__

- `:SignatureToggle` muestra/oculta los marcadores (seguirán activos aunque no
  se muestren)
- `:SignatureRefreshDisplay` refresca los marcadores en caso de ser necesario

*Repositorio:* <https://github.com/kshenoy/vim-signature>

## Exploracion de ficheros

### Ranger

![ranger](http://joedicastro.com/static/pictures/ranger_vim.gif "ranger")

A través de esto atajo llamo al programa externo
[Ranger](http://joedicastro.com/productividad-linux-ranger.html) para navegar
por el sistema de ficheros y elegir el fichero que queremos editar.

__Atajo__ `<Leader>ra`

### CtrlP

TODO: Añadir descripción a CtrlP

*Repositorio:* <https://github.com/kien/ctrlp.vim>

### utl 

![utl](http://joedicastro.com/static/pictures/utl.gif "utl")

Es un plugin que nos permite abrir URLs y enlaces a otro tipo de ficheros desde
vim. 

__Ayuda__ `:h utl_usr.txt` <vimhelp:utl_usr.txt>

__Atajo__ `<Leader>j` si usamos el atajo sobre un enlace se abrirá el destino
correspondiente en la aplicación que tengamos configurada

*Repositorio:* <https://github.com/vim-scripts/utl.vim>


## Edicion de codigo

### Contar lineas de codigo

![cloc](http://joedicastro.com/static/pictures/cloc.gif "cloc")

Ejecuta el programa externo `$ cloc` sobre el fichero y abre la ventana QuickFix 
con el resultado.

__Atajo__ `<Leader>sc`

### neocomplete

TODO: Añadir descripción a Neocomplete

*Repositorio:* <https://github.com/Shougo/neocomplete.vim>

### python-mode

TODO: Añadir descripción a python-mode    

*Repositorio:* <https://github.com/klen/python-mode>

### indentLine

![indentLine](http://joedicastro.com/static/pictures/indentline.gif "indentLine")

Sirve para mostrar lineas verticales en el código indentado (sangrado) con
espacios para marcar los niveles de indentado. Lo tengo desactivado por defecto.

__Ayuda__ `:h indentLine.txt` <vimhelp:indentLine.txt>

__Atajo__ `<Leader>sl` oculta/muestra las lineas guía

__Comandos__ 

- `:IndentLinesToggle` oculta/muestra las lineas guía
- `:IndentLinesReset {ancho}` redibuja las lineas guía, si se especifica el ancho (en
  espacios) se utilizara ese como espaciado entre niveles

*Repositorio:* <https://github.com/Yggdroot/indentLine>

### vim-virtualenv

TODO: Añadir descripción a Virtualenv

*Repositorio:* <https://github.com/jmcantrell/vim-virtualenv>

### coveragepy

TODO: Añadir descripción a coveragepy

*Repositorio:* <https://github.com/alfredodeza/coveragepy.vim>

### tagbar

![tagbar](http://joedicastro.com/static/pictures/tagbar.gif "tagbar")

Muestra una barra lateral con las etiquetas generadas por `ctags` para un
fichero de código fuente ordenadas por su "alcance" (scope). Desde esta barra
podemos navegar directamente a una etiqueta determinada en el fichero.

__Ayuda__ `:h tagbar.txt` <vimhelp:tagbar.txt>

__Atajo__ `<Leader>t` muestra/oculta la barra lateral

__Atajos en la barra lateral__ 

- `<Enter>` salta a la etiqueta en el fichero y cierra la barra
- `p` salta a la etiqueta en el fichero sin cerrar la barra y sin perder el foco
- `o` contrae/expande un pliege (scope)
- `s` ordena las etiquetas por nombre o por su orden de aparición en el fichero 
- `<Space>` Visualiza la linea en la que es definida la etiqueta en la linea de
  comandos
- `q` cierra la barra lateral
- `<F1>` muestra la ayuda para los atajos

*Repositorio:* <https://github.com/majutsushi/tagbar>

### vimux

![vimux](http://joedicastro.com/static/pictures/vimux.gif "vimux")

Sirve para interactuar entre vim y tmux. Básicamente permite enviar comandos a
un panel de tmux e interactuar con el sin perder el foco en Vim. Tal y como lo
tengo configurado, si no hay ningún otro panel abierto aparte del de Vim, se
abrirá uno debajo de este ocupando el 20% del espacio, en otro caso se
ejecutara en el panel abierto.

__Ayuda__ `:h vimux.txt` <vimhelp:vimux.txt>

__Atajos__

- `<Leader>xr` guarda el buffer actual, limpia el panel y ejecuta el contenido
  del buffer con `python2`
- `<Leader>xt` igual que el atajo anterior pero ejecuta el contenido precedido
  por el programa unix `time` para conocer el tiempo total empleado en su
  ejecucion.
- `<Leader>xp` igual que al atajo anterior pero empleando `pypy` en lugar de
  `python2`
- `<Leader>xc` llama a un prompt en la linea de comandos en el que podemos
  introducir el comando que queremos que se ejecute en el panel de tmux
- `<Leader>xl` repite el ultimo comando que se he ejecutado con vimux
- `<Leader>xs` interrumpe la ejecución del comando que hayamos lanzado con vimux
- `<Leader>xi` salta al panel donde se ha ejecutado el comando de vimux y entra
  en *copy mode*
- `<Leader>xz` cierra el panel donde se ha ejecutado el comando de vimux 

*Repositorio:* <https://github.com/benmills/vimux>

### TagmaTasks

![tagmatasks](http://joedicastro.com/static/pictures/tagmatasks.gif "TagmaTasks")

Visualiza las tareas pendientes para el buffer actual (o para una lista de
ficheros) y muestra marcas en la columna lateral izquierda de signos para cada una
de las tareas. Estas tareas se definen por medio de palabras clave en el buffer,
como __TODO__, __FIXME__, __NOTE__, __XXX__ y __COMBAK__, aunque se pueden
definir más.

__Ayuda__ `:h TagmaTasks.txt` <vimhelp:TagmaTasks.txt>

__Atajos__

- `<Leader>k` muestra/oculta la ventana de tareas
- `[t` salta a la tarea anterior 
- `]t` salta a la tarea siguiente 
- `[T` salta a la primera tarea
- `]T` salta a la última tarea

__Comandos__

- `:TagmaTasks {glob}` Genera la lista de tareas para el buffer actual o para
  una lista de ficheros definidos mediante *glob*
- `:TagmaTaskToggle` muestra/oculta la ventana de tareas y ejecuta
  automáticamente `:TagmaTasks` para el buffer actual

*Repositorio:* <https://github.com/LStinson/TagmaTasks>

### UltiSnips

TODO: Añadir descripción a UltiSnips 

*Repositorio:* <https://github.com/SirVer/ultisnips>

### Syntastic

Comprueba la sintaxis de numerosos lenguajes (python, ruby, lua, haskell, css,
html, js, json, ...) a través de herramientas externas*.

> *Estas herramientas necesitan estar instaladas para que el plugin funcione
correctamente.

Muestra los errores de sintaxis en la columna de signos de Vim, a la izquierda
de los números de línea. También muestra un resumen del numero de errores y la
localización del primero de ellos en la barra de estado (en este caso la de
Powerline)

__Ayuda__ `:h syntastic.txt` <vimhelp:syntastic.txt>

__Comandos__

- `:Errors` muestra una ventana con la lista de errores 
- `:SyntasticToggleMode` conmuta entre el modo activo y pasivo
- `:SyntasticCheck` ejecuta la comprobación manualmente, útil para cuando
  empleamos el modo pasivo. 

*Repositorio:* <https://github.com/scrooloose/syntastic>


## DVCS: Git

### Fugitive

TODO: Añadir descripción a Fugitive

*Repositorio:* <https://github.com/tpope/vim-fugitive>

### vim-gitgutter

![gitgutter](http://joedicastro.com/static/pictures/gitgutter.gif "gitgutter")

Muestra los cambios que se producen en el buffer con respecto al repositorio git
en el que se encuentra. Hace un git diff y muestra el estado de cada linea que
se ha cambiado/eliminado/añadido en la columna de signos de Vim a la izquierda
de los números de línea.

__Ayuda__ `:h gitgutter.txt` <vimhelp:gitgutter.txt>

*Repositorio:* <https://github.com/airblade/vim-gitgutter>

### tig

![tig](http://joedicastro.com/static/pictures/tig.gif "tig")

Con este atajo abrimos la aplicación externa [tig][tig] que es un interfaz
ncurses para git.

> Evidentemente esto solo funciona cuando te encuentras dentro de un repositorio
> git.

  [tig]: https://github.com/jonas/tig

__Atajo__ `<Leader>gt` abre la aplicación tig

*Repositorio:* <https://github.com/jonas/tig>

## Desarrollo Web

### HTML5

Proporciona funciones de autocompletado, sintaxis e indentación para HTML5. Para
ello tiene soporte de SVG, RDFa, microdata y WAI-AIRA.

*Repositorio:* <https://github.com/othree/html5.vim>

### Sparkup

![sparkup](http://joedicastro.com/static/pictures/sparkup.gif "sparkup")

Sparkup nos permite escribir archivos HTML más rápido, de manera más concisa y
de forma menos tediosa. Se basa en Zen Coding, por lo que toda la nomenclatura
que funciona con Zen Coding es valida para Sparkup.

__Ayuda__ `:h sparkup.txt` <vimhelp:sparkup.txt>

__Atajos__

- `<C-E>` ejecutar sparkup sobre la expresión bajo el cursor
- `<C-N>` saltar a la siguiente etiqueta/atributo vacío

La mejor forma de comprender como funciona es acceder a los ejemplos en la ayuda
de Vim, `:h sparkup-examples` <vimhelp:sparkup-examples> y si tienes
conocimientos de Python consultar el codigo en
`~/.vim/bundle/vim-sparkup/ftplugin/html/sparkup.py`

*Repositorio:* <http://github.com/joedicastro/vim-sparkup>

### ColorV
 
![colorv](http://joedicastro.com/static/pictures/colorv.gif "ColorV")

ColorV es el complemento perfecto para editar ficheros CSS a la hora de lidiar
con colores. No solo nos permite previsualizarlos en el fichero para saber que
color se corresponde con cada definición, si no que ademas nos provee de
herramientas para escoger colores (tanto en la consola como en modo gráfico),
esquemas de color, trabaja con varios espacios de color, etc. Tiene
prácticamente todo lo que se puede necesitar para la gestión del color, sin
envidiar a muchas herramientas profesionales.

__Ayuda__ `h: colorv.txt` <vimhelp:colorv.txt>

__Atajos__ 

- Visualizar colores

   - `<Leader>cv` muestra la ventana de ColorV
   - `<Leader>cw` muestra la ventana de ColorV con el color debajo del cursor
   - `<Leader>cpp` previsualiza los colores en el buffer actual

- Editar colores

   - `<Leader>ce` edita el color situado bajo de el cursor
   - `<Leader>cE` edita el color situado bajo de el cursor y cambia todas los
     colores similares en el mismo buffer (con confirmación previa)
   - `<Leader>cii` inserta un color empleando la ventana de ColorV. La segunda i
     puede ser sustituida por una `r` para insertar un color con nomenclatura
     RGB, una `m` para CMYK, etc... consultar la ayuda para más información

- Elegir colores

   - `<Leader>cn` muestra una ventana lateral con una lista de colores por
      nombre (colores Web del W3C)
   - `<Leader>cgh` muestra una ventana lateral con una lista de colores
      con el mismo tono que el situado bajo el cursor. La `h` puede ser cambiada
      para mostrar una lista de colores por saturación `s`, análogos `a`, ...
      consultar la ayuda para una lista completa
   - `<Leader>cd` muestra un selector de color gráfico (GUI)

- Elegir esquemas

   - `<Leader>css` elegir un esquema de color desde
     [Kuler](https://kuler.adobe.com) o [ColourLovers](http://www.colourlovers.com/)
   - `<Leader>csf` muestra los esquemas marcados como favoritos (`f` para marcar
     como favorito, `F` para desmarcarlo)
   - `<Leader>csn` crea un nuevo esquema
 

__Atajos en la ventana de ColorV__

- `z/Z` cambia el tamaño de la ventana
- `?` muestra los atajos disponibles ciclicamente
- `q` cierra la ventana

__Comandos__

- `:ColorvList {tipo} {num} {pasos}` Genera una lista de colores partiendo del
  que está bajo el cursor del tipo indicado (Hue, Saturation, ...) y con el
  numero y pasos indicados
- `:ColorVTurn2 {hex1} {hex2}` muestra una paleta de colores que varían el tono
  desde el primer color al segundo


*Repositorio:* <https://github.com/Rykka/colorv.vim>

## Markdown

### vim-markdown-extra-preview

![mep]( http://joedicastro.com/static/pictures/mep.gif "mep")

Es una utilidad que nos permite previsualizar el renderizado de un fichero
Markdown en el navegador, soporta además la extensión `extra` de Markdown.  El
fichero es renderizado con Python-Markdown, crea un fichero temporal html y lo
abre en una pestaña en el navegador. Usado en conjunto con algún plugin que
refresque la pestaña del navegador al cambiar el fichero html, conseguimos
previsualizar los cambios sin abandonar vim.

__Ayuda__ `:h vmep.txt` <vimhelp:vmep.txt>

__Atajos__

- `<LocalLeader>mp` previsualiza el buffer en una nueva pestaña
- `<LocalLeader>mr` refresca la pestaña ya abierta

__Comandos__

- `:Me` para previsualizar el buffer actual

- `:Mer` refresca la pestaña ya abierta anteriormente para el buffer actual


*Repositorio:* <http://github.com/joedicastro/vim-markdown-extra-preview>

## Utilidades de Linux/Unix

### Ack

![ack](http://joedicastro.com/static/pictures/ack.gif "ack")

Ejecuta el programa [Ack][ack] y mesta los resultados en la ventana QuickFix,
desde la que podemos abrir directamente en el buffer los distintos resultados.
Ack es una herramienta similar a grep pero enfocada a buscas en código fuente,
por lo que esta orientada principalmente a programadores.

  [ack]: http://betterthangrep.com/

__Ayuda__ `:h ack.txt` <vimhelp:ack.txt>

__Atajos__ 

- `<Leader>a` lanza el comando para que solo tengamos que introducir el
patrón de búsqueda y las opciones que queramos
- `<Leader>ag` lo mismo que el anterior pero solo busca en los ficheros que estan
  incluidos en el repositorio de git

__Comandos__

- `:AckFile` similar a `:Ack` pero busca ficheros por su nombre
- `:AckHelp` similar a `:Ack` pero busca dentro de la ayuda de Vim

__Atajos en la ventana QuickFix__ 

- `o` abre el fichero
- `go` abre el fichero pero el foco permanece en la ventana QuickFix
- `t` abre el fichero en una nueva pestaña
- `T` abre el fichero en una nueva pestaña pero no salta a ella
- `h` abre el fichero en una nueva ventana horizontal
- `H` abre el fichero en una nueva ventana horizontal pero no salta a ella
- `v` abre el fichero en una nueva ventana vertical
- `gv` abre el fichero en una nueva ventana vertical pero no salta a ella
- `q` cierra la ventana QuickFix

*Repositorio:* <https://github.com/mileszs/ack.vim>

### vim-eunuch 

Son una serie de comandos que llaman a diversos comandos UNIX que ayudan a
realizar ciertas tareas de forma mas sencilla.

> Ademas cuando se crea un nuevo fichero que comience con el shebang (`#!`), se
> llama automáticamente a `$ chmod +x` la primera vez que se guarde el fichero,
> por lo que pasa a tener permisos de ejecución.

__Ayuda__ `:h eunuch.txt` <vimhelp:eunuch.txt>

__Comandos__

- `:Unlink` o `:Remove` elimina el buffer y el fichero del disco. __Cuidado!__
- `:Move {file}` mueve un fichero en el disco y cambia el nombre del buffer
- `:Rename {file}` renombra el fichero en el disco y en el buffer
- `:Chmod {mode}` cambia los permisos del archivo
- `:Find {args}` ejecuta `$ find` con los argumentos dados
- `:Locate {args}` ejecuta `$ locate` con los argumentos dados
- `:SudoWrite {mode}` guarda el archivo empleando sudo (con permisos de root)
- `:W` guarda todas las ventanas abiertas (ventanas, no buffers)

*Repositorio:* <https://github.com/tpope/vim-eunuch>

### DirDiff

![DirDiff](http://joedicastro.com/static/pictures/dirdiff.gif "DirDiff")

Funciona de modo similar a vimdiff pero entre directorios en lugar de archivos.

__Comandos__

- `:DirDiff {A:directorio 1} {B: directorio 2}` muestra las diferencias entre
  los dos directorios
- `:DirDiffQuit` sale del modo DirDiff

*Repositorio:* <http://github.com/joedicastro/DirDiff.vim>

### Editor hexadecimal

![hex](http://joedicastro.com/static/pictures/hexman.gif "hex")

Este plugin en realidad utiliza la herramienta `xxd` para visualizar un fichero
de forma hexadecimal.

Este es un plugin a manejar con cuidado y sabiendo lo que se está haciendo.
Conviene ademas abrir el fichero en modo binario (`$ vim -b fichero`) y volver
siempre al modo ASCII (abandonar el modo binario) antes de guardar el fichero.

__Atajos__

- `<Leader>hm` entrar/salir del modo Hexadecimal
- `<leader>hd` elimina el caracter Hexadecimal bajo el cursor
- `<leader>hi` inserta un caracter ASCII antes del cursor
- `<leader>hg` ir al byte que le indiquemos (en hexadecimal)
- `<leader>hn` o `<Tab>` ir al siguiente byte
- `<leader>hp` o `<Shift><Tab>` ir al byte anterior
- `<leader>ht` mueve el cursor del area Hexadecimal al area ASCII y viceversa
- `?`          muestra la ayuda

*Repositorio:* <https://github.com/vim-scripts/hexman.vim>

## Internalizacion

### Traduccion de ficheros .po

![po](http://joedicastro.com/static/pictures/po.gif "po")

Es una utilidad para añadir sintaxis y algunos atajos para los ficheros `.po` de
traducción de cadenas (GNU gettext)

__Ayuda__ `:h po.txt` <vimhelp:po.txt>

__Atajos__

- `<LocalLeader>u` se desplaza a la siguiente cadena sin traducir
- `<LocalLeader>U` se desplaza a la anterior cadena sin traducir
- `<LocalLeader>c` copia la cadena `msgid` a `msgstr`
- `<LocalLeader>C` crea un comentario para esa entrada
- `<LocalLeader>d` elimina la cadena `msgstr` (solo en Insert mode)
- `<LocalLeader>f` se desplaza a la siguiente cadena "fuzzy" 
- `<LocalLeader>F` se desplaza a la anterior cadena "fuzzy" 
- `<LocalLeader>z` etiqueta la entrada "fuzzy" 
- `<LocalLeader>Z` elimina la etiqueta de la entrada "fuzzy"
- `<LocalLeader>s` muestra estadísticas `msgfmt` del fichero
- `<LocalLeader>e` navega a través de los errores `msgfmt` del fichero
- `<LocalLeader>t` introduce la información del traductor en la cabecera
- `<LocalLeader>T` introduce la información del equipo de traducción en la cabecera
- `<LocalLeader>W` formatea todo el fichero
- `gf` abre en otra ventana el fichero que está debajo del cursor


*Repositorio:* <https://github.com/vim-scripts/po.vim--gray>


# ...Work in progress!

TODO: Acabar con todos los TODOs  
FIXME: Reorganizar de forma coherente todos los mapeados para los atajos.  
FIXME: Realizar una revisión de calidad  
