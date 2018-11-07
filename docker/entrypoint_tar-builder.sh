#!/bin/bash                                                                                                                                        

# configure environment
export R_LIBS_USER=/tmp/R_user_lib
rm -rf $R_LIBS_USER
mkdir -p $R_LIBS_USER

export TAR_OPTIONS="--no-same-owner -v"

# run from workspace
cd /workspace

R -q -e "roxygen2::roxygenise('BFE_RShiny/flamingo')"

R CMD Rd2pdf BFE_RShiny/flamingo 1> Rd2pdf.log 2>&1
R CMD build BFE_RShiny/flamingo

# Return tar without version num
mv $(find -name flamingo_*.tar.gz) flamingo.tar.gz
