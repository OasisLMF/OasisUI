#!/usr/bin/env bash
R -q -e "install.packages('devtools', repos = 'https://cloud.r-project.org/')"
R -q -e "install.packages('shiny', repos = 'https://cloud.r-project.org/')"
R -q -e "install.packages('htmltools', repos = 'https://cloud.r-project.org/')"
R -q -e "install.packages('httr', repos = 'https://cloud.r-project.org/')"
R -q -e "install.packages('DBI', repos = 'https://cloud.r-project.org/')"
R -q -e "install.packages('xml2', repos = 'https://cloud.r-project.org/')"
R -q -e "install.packages('shinyjs', repos = 'https://cloud.r-project.org/')"
R -q -e "install.packages('jsonlite', repos = 'https://cloud.r-project.org/')"
R -q -e "install.packages('DT', repos = 'https://cloud.r-project.org/')"
R -q -e "install.packages('logging', repos = 'https://cloud.r-project.org/')"
R -q -e "roxygen2::roxygenise('flamingo')"
rm -rf *.Rcheck
rm -rf *.pdf
rm -rf .Rd2pdf*
rm -rf *.log
R CMD Rd2pdf flamingo 1> Rd2pdf.log 2>&1
R CMD build flamingo
R CMD check flamingo_0.0.7.tar.gz
