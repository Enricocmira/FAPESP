######################################################
## Código: Painel GSADF                             ##
## Aluno: Enrico Campos de Mira                     ##
## Orientador: Wilfredo Maldonado                   ##
######################################################


###############################
## Preparing the environment ##
###############################


# Cleaning the environment 
rm(list=ls())   # remove all files from workspace
graphics.off()  # clear all graphs

# Packag4es
library(dplyr)
library(exuber)
library(ggplot2)

##########
## Data ##
##########

# Import data
data = readxl::read_excel("C:/Users/enric/Downloads/Fipezap (3).xlsx", 
                          sheet = "Página2", col_types = c("date", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric"))

# Select the index

index = rep(0,nrow(data))

for (i in 1:ncol(data)) {
  index[i] = which(data[,i] != 0)[1]
}

# Find the index of 01/01/2018
index = max(index)

# Select the data
data = data[index:nrow(data),]

###############
# Panel GSADF #
###############

# Estimate the GSADF statistic
panel_gsadf = radf(data)

# Critical Value
cv = radf_sb_cv(data)

# Correct the attributes
attributes(panel_gsadf)$index = 1:nrow(data)
attributes(cv)$index = 1:nrow(data)

# Ploting
autoplot(panel_gsadf,cv)



