# ===============================================================================
# SETUP 
# ===============================================================================

# Library
library(rstudioapi) # setup
library(tidyverse)  # data processing
library(magrittr)
library(reshape2)
library(ranger)     # machine learning
library(cowplot)    # plotting
library(egg)
library(ggpubr)


# Set Working Directory
setwd(dirname(getActiveDocumentContext()$path))


# List of functions
functions <- list.files(path = 'functions/', pattern = "[.]R$", 
                        full.names=TRUE, recursive = TRUE)

# Load functions
for (i in (1:length(functions))) {
  print(functions[i])
  source(functions[i])
}


# clean env
remove(functions)
remove(i)
