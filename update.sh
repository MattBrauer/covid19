#!/bin/bash
git pull
git submodule update --remote --recursive
Rscript -e "rmarkdown::render('covid19data.Rmd')"
