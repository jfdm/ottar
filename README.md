Óttar
=====

                                                        ___  _   _ 
                                                       / _ \| |_| |_ __ _ _ __ 
                                                      | | | | __| __/ _` | '__|
                                                      | |_| | |_| || (_| | | 
                                                       \___/ \__|\__\__,_|_| 



Ottar is a tool written in `Haskell` to parse informal security protocol narrations into other formats.
Currently the following transformations are implemented.

* `latex`
* `ottar`

The last of which is an identity transformation.
The Ottar Grammar file details the syntax.
Examples can be found in the example folder.

Ottar should be considered Academic Quality Code, and should be realeased under the [CRAPL](http://matt.might.net/articles/crapl/) license, however, BSD will do fine.
Please not this is a work in progress and YMMV.
I also aim to fix the syntax in the near future...

## Building

It is assumed that you have installed [Haskell](http://www.haskell.org/platform/).

To build and install `ottar` run the following commands in the projects root directory:

    $ cabal configure
    $ cabal build
    $ cabal install

These should hopefully install ottar on `cabal`'s `bin` path.
If there is time I shall try and place this on Hackage.

## Using

    $ ottar --help
    $ ottar --to latex file.ottar
    $ ottar --to ottar file.ottar
    $ ottar --to latex file.ottar -o file
