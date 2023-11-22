###  Load libraries  ---------------------------------------------------
print("Load R libraries")

lib_nm <- c("ggplot2", "data.table", "dplyr", "tidyr", "tidyverse", "modelr", "klaR",
            "GGally", "randomForest")

sapply(lib_nm, function(x) library(x, character.only = T))

print("Libraries have been loaded")