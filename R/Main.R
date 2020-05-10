################################################################################
# This is the main script for the calculus where all paths to folders and data
# are defined and all functions are executed

pack <- c('tidyverse','openxlsx')  # Define packages to be used
install.packages( pack )           # Install packages
library( "tidyverse" )             # Load packages
library( "openxlsx" )

github <- "C:/Users/hwieland/Github workspace/"  # Define path to your local github repos

# Define set of important paths
path <<- list("repo" = paste0(github,"GEM/"),
              "input" = paste0(github,"GEM/input/"),
              "output" = paste0(github,"GEM/output/"),
              "r" = paste0(github,"GEM/R/"),
              "exio" = paste0(github,"GEM/input/EXIOBASE/") )

fun <- list("exio" = paste0(path$r,"Compile_EXIOBASE.R"))  # Define functions to be used
lapply(fun,source)                                         # Load all functions

remove(github,fun)         # Remove objects not needed anymore

MRIO <- Compile_EXIOBASE(2008,"PxP")



