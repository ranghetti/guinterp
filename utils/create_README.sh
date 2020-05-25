#!/bin/bash

# Script used to automatically generate README.Rmd from index.Rmd
# (replacing internal links with complete links to the online documentation
# at http://guinterp.ranghetti.info)

# Launch the script from the main sen2r directory

sed -e "s/](\([^\(\)]*\)\.md)/](http:\/\/guinterp.ranghetti.info\/\1\.html)/g" index.Rmd > README.Rmd
