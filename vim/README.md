# Mi Configuración de Vim

El propósito de este documento es recopilar todas las opciones disponibles en mi
configuración para poner un poco de orden en la misma y servirme de recordatorio
de todo lo que tengo disponible. Evidentemente no pretendo replicar la ayuda de
vim ni de los plugins, solo destacar aquellas opciones que puedo necesitar en un
determinado momento. Del mismo modo puede servir de manual de instrucciones para
aquel que decida clonar esta configuración.

Debido a la naturaleza altamente "mutante" de mi configuración, este documento
estará sujeto del mismo modo a un numero elevado de modificaciones en el futuro.

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
     - [Insertar fecha/hora actual](#insertar-fechahora-actual)
     - [Estadisticas de texto](#estadisticas-de-texto)
 - [Exploracion de ficheros](#exploracion-de-ficheros)
     - [Ranger](#ranger)
 - [Edicion de codigo](#edicion-de-codigo)
     - [Contar lineas de codigo](#contar-lineas-de-codigo)
     


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
- `Ctrl + h` desplazamiento a la siguiente ventana a la izquierda
- `Ctrl + j` desplazamiento a la ventana inferior 
- `Ctrl + k` desplazamiento a la ventana superior 
- `Ctrl + l` desplazamiento a la siguiente ventana a la derecha
- `<Leader>m` cierra la ventana actual
- `<Leader>q` cierra la ventana QuickFix

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
  
__Atajo__ `Space`

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


### Insertar fecha/hora actual

__Atajos__ 

- `<Leader>is` inserta un timestamp (e.g. *mar abr  2 19:47:02 CEST 2013*)
- `<Leader>ih` inserta la hora actual   
- `<Leader>id` inserta el día actual    
- `<Leader>if` inserta el día y la hora actuales

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


## Exploracion de ficheros

### Ranger

![ranger](http://joedicastro.com/static/pictures/ranger_vim.gif "ranger")

A través de esto atajo llamo al programa externo
[Ranger](http://joedicastro.com/productividad-linux-ranger.html) para navegar
por el sistema de ficheros y elegir el fichero que queremos editar.

__Atajo__ `<Leader>ra`

## Edicion de codigo

### Contar lineas de codigo

![cloc](http://joedicastro.com/static/pictures/cloc.gif "cloc")

Ejecuta el programa externo `$ cloc` sobre el fichero y abre una nueva ventana
con el resultado.

__Atajo__ `<Leader>sc`














# ...Work in progress!














