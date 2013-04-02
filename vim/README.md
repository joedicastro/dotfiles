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


## Vundle

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

- `<Leader>v`, crea una nueva ventana vertical
- `<Leader>h`, crea una nueva ventana horizontal
- `Ctrl + h`, desplazamiento a la siguiente ventana a la izquierda
- `Ctrl + j`, desplazamiento a la ventana inferior 
- `Ctrl + k`, desplazamiento a la ventana superior 
- `Ctrl + l`, desplazamiento a la siguiente ventana a la derecha
- `<Leader>m`, cierra la ventana actual

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

# ...Work in progress!














