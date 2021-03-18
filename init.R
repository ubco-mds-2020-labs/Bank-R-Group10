# R script to run author supplied code, typically used to install additional R packages
# contains placeholders which are inserted by the compile script
# NOTE: this script is executed in the chroot context; check paths!

r <- getOption('repos')
r['CRAN'] <- 'http://cloud.r-project.org'
options(repos=r)

# ======================================================================

# packages go here
install.packages(c('remotes','dash','rjson','ggthemes',
                   'GGally','tidyverse','here','readr',
                   'ggplot2','purrr','dplyr','ggiraph'))

remotes::install_github("facultyai/dash-bootstrap-components@r-release")