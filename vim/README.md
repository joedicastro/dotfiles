# Mi Configuración de Vim

El propósito de este documento es recopilar todas las opciones disponibles en mi
configuración para poner un poco de orden en la misma y servirme de recordatorio
de todo lo que tengo disponible. Evidentemente no pretendo replicar la ayuda de
vim ni de los plugins, solo destacar aquellas opciones que puedo necesitar en un
determinado momento. Del mismo modo puede servir de una suerte de manual de
instrucciones para aquel que decida clonar esta configuración.

Debido a la naturaleza altamente "mutante" de mi configuración, este documento
estará sujeto del mismo modo a un numero elevado de modificaciones en el futuro.

A todo esto habría que sumarle todo lo que Vim aporta de serie, que no es poco.

> La tecla `<Leader>` la tengo mapeada a `,` y la tecla `<LocalLeader>` la tengo
mapeada a la tecla espaciadora

    DISCLAIMER & AGRADECIMIENTOS

    Evidentemente es necesario un conocimiento previo de Vim para sacarle todo
    el partido posible a esta configuración, del mismo modo que será necesario
    recurrir a la ayuda de ciertos plugins para familiarizarse con ellos más
    allá de las pautas que doy en este documento.

    Esta configuración está basada en las de muchos otros, tantos que ni los
    recuerdo a todos y seria bastante injusto recordar a algunos y omitir a
    otros. Pero gracias a que muchos comparten generosamente su configuración,
    yo he llegado a la mía y sirva esta documento también de pequeña
    compensación por lo mucho que otros me han aportado. Y gracias también a
    todos aquellos desarrolladores que crearon los plugins que incluyo (e incluí
    en el pasado) por que sin su maravillosa contribución y generosidad al
    compartirlos con el resto del mundo, esta configuración no sería posible.

## Gestion de Plugins

### Neobundle

Un plugin para gobernarlos a todos! Me permite administrar el resto de los
plugins, el mismo incluido. __A su vez lo tengo configurado para que se instale a
si mismo la primera vez que se ejecute vim con esta configuración (también
instala automáticamente el resto de plugins).__

Las ventajas de este plugin frente a otros similares como Vundle son las
siguientes:

- Permite usar plugins desde otras fuentes distintas a git (svn, hg, dir, ...)
- Permite establecer la revisión exacta que queremos emplear
- Permite marcar plugins como no actualizables
- Permite cargar los plugins bajo demanda, no al principio, para agilizar el
  arranque de Vim y el consumo de recursos
- Permite añadir multitud de opciones a cada plugin, como por ejemplo que se
  haga el `build` de forma automática si es necesario al instalar/actualizar
- Y muchas mas posibilidades, sobre todo si también usamos 'Vimproc' y 'Unite'
  del mismo autor

__Ayuda__ `:h neobundle.txt`  <vimhelp:neobundle.txt>

La mejor manera de usar NeoBundle es a través de Unite

__Unite__

- `<LocalLeader>n` o `:Unite menu:neobundle`, muestra un menú con las opciones
  de Neobundle

*Repositorio:* <https://github.com/Shougo/neobundle.vim>

### Actualizacion de Plugins

NeoBundle es una muy buena y potente herramienta que nos permite administrar de
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

#### Alternativa

Otra opción la tenemos a través de NeoBundle, que nos permite especificar la
revisión que queremos instalar de un plugin, e incluso nos permite decirle que
no se actualice nunca, si no nos interesa actualizarlo.


## Unite

Unite es una interfaz que unifica varios 'resultados de búsquedas' bajo un mismo
aspecto y siguiendo el comportamiento por defecto de Vim (modal). Es casi una
API sobre la que podemos construir nuestras propias soluciones. Sirve tanto para
abrir un fichero, como para cambiar de buffer, cambiar de esquema de color o
para hacer una búsqueda con regex (vimgrep, grep, Ack, ag, ...). Sirve hasta
para consultar los registros, ayuda, comandos... Resumiendo, es una navaja suiza
que bien empleada nos permite sustituir un varios plugins distintos por uno
solo (en este caso: CtrlP, Ack, YankRing).

> Una de las principales ventajas de Unite es que me permite subsanar uno de los
problemas que intentaba solucionar inicialmente con este documento, que es el
recordar todas las opciones y atajos que con tanto esfuerzo y tiempo he
incorporado a Vim. Es muy normal que tengamos disponible un plugin que usamos de
cuando en cuando, lo tengamos personalizado y cuando vayamos a emplearlo no
recordemos ni todas sus opciones, ni los atajos. Pues bien, con Unite es
sencillo crear un menú para ese plugin donde mostremos las opciones que tenemos
para el y los atajos que les hemos asignado y gracias otra vez a la magia de
Unite, ni siquiera necesitamos recordar el atajo a este menú, podemos buscarlo
dentro del indice de menús de Unite. Problema resuelto de forma rápida y
elegante.

El mayor inconveniente de Unite y a su vez una de sus mayores ventajas es que no
viene configurado apenas, dejando a nuestro gusto y responsabilidad el adaptarlo
a nuestro modo de trabajo.

### Fuentes y Menus

Yo he configurado Unite siguiendo dos vías distintas, por un lado lo utilizo
para acceder a fuentes directas (lo que Unite denomina *sources*) a través de un
atajo usando la tecla `<Leader>` y por otro lado llamando a menús usando la
tecla `<LocalLeader>`

- Las __fuentes__ llamadas directamente a través de un atajo con `<Leader>` las uso
  para aquellas tareas más comunes como abrir archivos, buscar dentro del
  fichero, hacer una búsqueda de archivos mediante regex (ag, ack, grep), etc.
  Un ejemplo de como abrir un archivo con Unite.

![unite_file](http://joedicastro.com/static/pictures/unite_file.gif "unite file")

- Los __menus__ los uso para agrupar las opciones bien por plugins o bien por
  funcionalidad. Ademas muestro los atajos en aquellas opciones que tengo
  mapeadas directamente, esto me permite no tener que consultar mi `~/.vimrc`
  para recordar cuales eran cuando me olvido de alguno. Un ejemplo que muestra
  las opciones que tengo disponibles para gestionar un repositorio git desde
  Vim.

![unite_git](http://joedicastro.com/static/pictures/unite_git.gif "unite_git")

También para no tener que recordar todos los menús que he creado, hay un menú
maestro que me permite acceder a cada uno de ellos y ver los atajos asociados a
cada uno de ellos. Unite posee un mecanismo que nos permite acceder a una lista
de menús generada automáticamente, y aunque en este caso ni se muestran
ordenados ni se pueden ver los atajos asociados directamente yo he creado las
descripciones de los menús de manera que se puedan ver los atajos. Es lo que el
comando `:menu` de Vim deberia haber sido: comodo, intuitivo y de fácil
navegación.

- `<LocalLeader>u` o `:Unite menu` muestra los menús disponibles


### Navegacion dentro de Unite

TODO: completar este apartado

*Repositorio:* <https://github.com/Shougo/unite.vim>

## Esquemas de color

Para cambiar de esquemas de color podemos hacerlo a través de Unite:

__Unite__

- `<LocalLeader>v` o `:Unite menu:varios` accedemos al menú *varios* donde
  podemos cambiar el esquema seleccionando la opción correspondiente
- `:Unite colorscheme -auto-preview` seleccionamos el esquema de la lista con
  previsualización del mismo

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

## Operaciones con ventanas

### Atajos

- `<C-H>` desplazamiento a la siguiente ventana a la izquierda
- `<C-J>` desplazamiento a la ventana inferior
- `<C-K>` desplazamiento a la ventana superior
- `<C-L>` desplazamiento a la siguiente ventana a la derecha

### Unite

El resto de opciones, incluidas las siguientes que muestro a continuación, se
pueden encontrar dentro del menú de Unite `navegacion`

`<LocalLeader>b` o `:Unite menu:navegacion` muestra el menú de navegación entre
ventanas, buffers y pestañas

### scratch-utility

![Scratch](http://joedicastro.com/static/pictures/scratch.gif "Scratch")

Nos crea un nuevo buffer temporal en el que no se guardara nada de lo que
editemos en ella, el contenido es descartado en cuanto cerramos la aplicación.
La ventana aparecerá siempre encima de la ventana actual

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

### Activar/desactivar el resaltado de busqueda

![nohlsearch][nhs]

  [nhs]: http://joedicastro.com/static/pictures/nohlsearch.gif  "nohlsearch"

__Atajo__ `<Leader>eq`

__Unite__ `<LocalLeader>e` Activa el menú de edición de texto donde esta
incluida esta acción

### Conmutar la visualizacion de numeros de linea

![Conmutar numeros de linea][cnl]

  [cnl]: http://joedicastro.com/static/pictures/linenumbers.gif "Conmutar numeros de linea"

  Conmuta entre no mostrar los números de línea, mostrarlos relativos y
  mostrarlos absolutos.

  __Atajo__ `<Leader>l`

__Unite__ `<LocalLeader>e` Activa el menú de edición de texto donde esta
incluida esta acción

### Mostrar caracteres no imprimibles

![hiddenchars][hdc]

  [hdc]: http://joedicastro.com/static/pictures/hiddenchars.gif "mostrar caracteres no imprimibles"

__Atajo__ `<Leader>eh`

__Unite__ `<LocalLeader>e` Activa el menú de edición de texto donde esta
incluida esta acción

### Abrir/cerrar pliegues

![unfold][ufl]

  [ufl]: http://joedicastro.com/static/pictures/unfold.gif "abrir/cerrar pliegues"

__Atajo__ <code>\\</code>

__Unite__ `<LocalLeader>e` Activa el menú de edición de texto donde esta
incluida esta acción

### Copiar/pegar

__Atajos__

- `<Leader>y` copiar al portapapeles
- `<Leader>p` pegar desde el portapapeles
- `<Leader>P` conmutar el paste mode

__Unite__ `<LocalLeader>e` Activa el menú de edición de texto donde esta
incluida esta acción

### Revision de ortografia

__Unite__

- `<LocalLeader>s` o `:Unite menu:ortografia` activa el menu con las opciones
  para la correccion ortografica

### Guardar como root

Permite guardar un archivo que solo tiene permisos para `root` sin necesidad de
ejecutar vim desde ese usuario o utilizando `$ sudo` y perder de ese modo las
ventajas de nuestra configuración.

__Comando__ `:w!!`

__Unite__ `<LocalLeader>o` Activa el menú de ficheros donde esta incluida esta
acción

### Guardado rapido

Para guardar rápidamente un archivo sin tener que ejecutar el comando `:w`

__Atajo__ `<Leader>w`

__Unite__ `<LocalLeader>o` Activa el menú de ficheros donde esta incluida esta
acción

### Eliminar espacios al final de la linea

![remove_trail](http://joedicastro.com/static/pictures/remove_trail_spaces.gif "eliminar espacios finales")


Elimina esos espacios que se quedan a veces al final de la linea y que suelen
ser casi siempre innecesarios y sin cometido alguno (excepto quizas en Markdown)

__Atajo__ `<Leader>et`

__Unite__ `<LocalLeader>e` Activa el menú de edición de texto donde esta
incluida esta acción


### Estadisticas de texto

![text stats]( http://joedicastro.com/static/pictures/textstats.gif "estadisticas de texto")

Obtener el numero de lineas, palabras, caracteres y bytes (totales y de la
posición actual)

__Atajo__ `<Leader>es`

__Unite__ `<LocalLeader>t` Activa el menú de edición de texto donde esta
incluida esta acción

![frecuencia de palabras](
http://joedicastro.com/static/pictures/word_frecuency.gif "frecuencia de
palabras")

Obtener la frecuencia con la que aparece cada palabra para un texto dado, abre
una nueva ventana con las estadísticas.

__Atajo__ `<Leader>ew`

__Unite__ `<LocalLeader>e` Activa el menú de edición de texto donde esta
incluida esta acción

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

### easydigraph

![easydigraph](http://joedicastro.com/static/pictures/easydigraph.gif "easydigraph")

Herramienta para insertar un dígrafo de forma bastante sencilla, sobre todo
cuando se trata de insertar varios simultáneamente.

__Ayuda__ `:h easydigraph.txt@en` <vimhelp:easydigraph.txt@en>

__Atajos__

- `<Leader>dd {motion}` convierte en dígrafo la selección efectuada con el
  movimiento.

__Comandos__

- `:digraphs` muestra una tabla con todos los dígrafos disponibles y los
  caracteres necesarios para generarlos

__Unite__ `<LocalLeader>e` Activa el menú de texto donde esta incluida esta
acción



*Repositorio:* <https://github.com/Rykka/easydigraph.vim>

### Gundo

![gundo](http://joedicastro.com/static/pictures/gundo.gif "gundo")

Sirve para hacer mas amigable la utilización del árbol de deshacer de Vim. De
esta manera podemos ver el árbol de los cambios realizados, previsualizar los
cambios que vamos a realizar y saber a donde vamos a retornar antes de deshacer
un cambio.

__Ayuda__ `:h gundo.txt` <vimhelp:gundo.txt>

__Atajos__ `<Leader>u` abre el interfaz de ventanas de Gundo

__Unite__ `<LocalLeader>i` Activa el menú de registros donde esta incluida esta
acción


*Repositorio:* <https://github.com/sjl/gundo.vim>

### LoremIpsum

![loremipsum](http://joedicastro.com/static/pictures/loremipsum.gif "loremipsum")

Sirve para generar texto aleatorio para rellenar borradores y pruebas de diseño,
muy usado en diseño web. Genera el famoso texto [Lorem Ipsum][lorem]

  [lorem]: http://es.wikipedia.org/wiki/Lorem_ipsum

__Ayuda__ `:h loremipsum.txt` <vimhelp:loremipsum.txt>

__Comandos__

- `:LoremIpsum {num palabras}` inserta el texto Lorem Ipsum de forma aleatoria

__Unite__ `<localleader>e` activa el menú de texto donde esta incluida esta
acción

*Repositorio:* <https://github.com/vim-scripts/loremipsum>

### vim-characterize

![characterize](http://joedicastro.com/static/pictures/characterize.gif "characterize")

Muestra información ampliada sobre un carácter. Muestra el valor Unicode en
decimal, hexadecimal, octal, el nombre Unicode, la HTML entity, el codigo Emoji
y cualquier dígrafo disponible.

__Ayuda__ `:h characterize.txt` <vimhelp:characterize.txt>

__Atajo__ `ga` muestra la información sobre el carácter.

__Unite__ `<localleader>e` activa el menú de texto donde esta incluida esta
acción

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
  -  <code>]`</code>      salta al marcador siguiente
  -  <code>[`</code>      salta al marcador previo
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

__Unite__

- `<localleader>b` o `:Unite menu:busquedas` activa el menu busquedas donde
  podemos buscar todas las marcas del buffer

*Repositorio:* <https://github.com/kshenoy/vim-signature>

## Exploracion de ficheros

### Ranger

![ranger](http://joedicastro.com/static/pictures/ranger_vim.gif "ranger")

A través de esto atajo llamo al programa externo
[Ranger](http://joedicastro.com/productividad-linux-ranger.html) para navegar
por el sistema de ficheros y elegir el fichero que queremos editar.

__Atajo__ `<Leader>r`

__Unite__

- `<localleader>o` o `:Unite menu:ficheros` activa el menu ficheros donde
  podemos acceder tambien a ranger

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

Ejecuta el programa externo `$ cloc` sobre el fichero y muestra el resultado en
Unite.

__Unite__ `<localleader>p` o `:Unite menu:code` abre el menú de herramientas de
código donde esta incluida esta función

### neocomplete

![neocomp](http://joedicastro.com/static/pictures/neocomp.gif "neocomp")

Autocompleta palabras clave, métodos, ... con solo escribir las primeras letras.
Bien usado permite agilizar mucho la escritura de código o texto.
Neocomplete es un plugin que mejora el autocompletado por defecto de Vim, con
búsqueda con lógica difusa (fuzzy) al mismo tiempo que se escribe. Esta plagado
de opciones y es completamente personalizable.

__Ayuda__ `:h neocomplete.txt` <vimhelp:neocomplete.txt>

__Atajos__

- `<CR>`    inserta la palabra seleccionada
- `<C-N>`   nos desplaza a la palabra inferior en la lista de opciones
- `<C-P>`   nos desplaza a la palabra superior en la lista de opciones

__Comandos__

- `:NeocompleteToggle` activa/desactiva Neocomplete en el buffer actual

*Repositorio:* <https://github.com/Shougo/neocomplete.vim>

### python-mode

TODO: Añadir descripción a python-mode

*Repositorio:* <https://github.com/klen/python-mode>

### indentLine

![indentLine](http://joedicastro.com/static/pictures/indentline.gif "indentLine")

Sirve para mostrar lineas verticales en el código indentado (sangrado) con
espacios para marcar los niveles de indentado. Lo tengo desactivado por defecto.

__Ayuda__ `:h indentLine.txt` <vimhelp:indentLine.txt>

__Atajo__ `<Leader>L` oculta/muestra las lineas guía

__Unite__ `<localleader>p` o `:Unite menu:code` muestra el menú de herramientas
de código donde se encuentra esta opción

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

La funcionalidad de este plugin la he sustituido con una busqueda grep a traves
de Unite.

__Unite__ `<LocalLeader>f` abre el menu de busquedas donde tenemos acceso a esta
funcon

__Atajo__ `<Leader>;` efectua la busqueda de tareas

### UltiSnips

![ulti](http://joedicastro.com/static/pictures/ulti.gif "ulti")

Ultisnips es un plugin para gestionar Snippets, el mas avanzado y potente que
conozco para Vim.  Los snippets son porciones de código o texto en las que
cierta parte es declarada como variable y el resto como fija y nos ayudan a no
tener que teclear una y otra vez las mismas porciones de texto/código.
Simplemente invocamos el snippet con el identificador y el texto fijo es
insertado automáticamente, dejando aquellas partes declaradas como variables
para ser rellenadas de forma interactiva. Se puede apreciar mejor en la imagen
el funcionamiento de los mismos.

Ultisnips trae por defecto algunos snippets predefinidos para varios lenguajes y
algunos globales. La mejor característica de Ultisnips es que nos permite
definir los nuestros propios con un nivel de control y automatismo que ningún
otro plugin nos ofrece. Es lo suficiente complejo para no entrar aquí en
detalles, es necesario leerse detenidamente la ayuda para comprenderlo.
Destacaría de todos modos que nos permite emplear comandos externos (shell,
vimscript y Python) dentro de los mismos o que podemos usarlos con selecciones
visuales, así como anidar snippets o usar transformaciones de texto.

__Ayuda__ `:h UltiSnips.txt` <vimhelp:UltiSnips.txt>

__Atajos__

- `<Tab>` precedido por el identificador nos lanza el snippet
- `<C-J>` nos desplaza al siguiente campo a rellenar
- `<C-K>` nos desplaza al anterior campo a rellenar

En el directorio `./UltiSnips` guardo mis snippets personalizados.

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

TODO: Completar esta seccion

__Ayuda__ `:h fugitive.txt` <vimhelp:fugitive.txt>

__Unite__ `<LocalLeader>g`

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

__Unite__ `<LocalLeader>c` o `:Unite menu:colorv` nos abre el menu de *colorv*
donde se encuentran todas estas opciones

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

__Unite__

- `<localleader>k` o `:Unite menu:markdown` nos abre el menu markdown

__Comandos__

- `:Me` para previsualizar el buffer actual

- `:Mer` refresca la pestaña ya abierta anteriormente para el buffer actual


*Repositorio:* <http://github.com/joedicastro/vim-markdown-extra-preview>

## Utilidades de Linux/Unix

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

- `<F6>` entrar/salir del modo Hexadecimal
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

- `/u` se desplaza a la siguiente cadena sin traducir
- `/U` se desplaza a la anterior cadena sin traducir
- `/c` copia la cadena `msgid` a `msgstr`
- `/C` crea un comentario para esa entrada
- `/d` elimina la cadena `msgstr` (solo en Insert mode)
- `/f` se desplaza a la siguiente cadena "fuzzy"
- `/F` se desplaza a la anterior cadena "fuzzy"
- `/z` etiqueta la entrada "fuzzy"
- `/Z` elimina la etiqueta de la entrada "fuzzy"
- `/s` muestra estadísticas `msgfmt` del fichero
- `/e` navega a través de los errores `msgfmt` del fichero
- `/t` introduce la información del traductor en la cabecera
- `/T` introduce la información del equipo de traducción en la cabecera
- `/W` formatea todo el fichero
- `gf` abre en otra ventana el fichero que está debajo del cursor


*Repositorio:* <https://github.com/vim-scripts/po.vim--gray>


# ...Work in progress!

TODO: Acabar con todos los TODOs

FIXME: Reorganizar de forma coherente todos los mapeados para los atajos.

FIXME: Realizar una revisión de calidad

FIXME: Actualizar algunas capturas de pantalla
