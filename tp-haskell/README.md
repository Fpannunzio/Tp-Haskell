# Functional

Necesario tener instalado freeglut3

para esto, en linux:

`sudo apt install freeglut3-dev`

en windows, descargar la version MinGw de:

`https://www.transmissionzero.co.uk/software/freeglut-devel/`

agregar a la variable path el .dll x64 que se encuentra en la carpeta bin del archivo comprimido

```console
$ stack init
$ stack build
$ stack exec functional
```
