######################################################
## Title: Estimação da preferência pela propriedade ##
## imobiliária e bolhas especulativas no setor:     ##
## Um Estudo Aplicado ao Brasil                     ##
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

# Select the city
data = data[,c("Data","Preço de Venda Brasil","Preço do Aluguel Brasil")]
index <- which(data$`Preço do Aluguel Brasil` != 0)[1]

data <- data[index:nrow(data), ]

# Indentifing the integration order
aTSA::adf.test(data[[2]])
aTSA::adf.test(data[[3]])
aTSA::adf.test(diff(data[[2]]))
aTSA::adf.test(diff(data[[3]]))

# Engle-Granger Cointegration test 
aTSA::coint.test(data[[2]], data[[3]])


trace=urca::ca.jo(cbind(as.ts(data[[2]]),as.ts(data[[3]])), type="trace", ecdet="none", spec="longrun")
urca::summary(trace)

# Johassen Cointegration test (eigen value)
eigen=urca::ca.jo(cbind(as.ts(data[[2]]),as.ts(data[[3]])), type="eigen", spec="longrun",ecdet="none")
urca::summary(eigen)


###################################
# Multiple Bubble for Real Prices #
###################################

# Create the Ratio Collumn
data$Ratio= data[[2]]/data[[3]] 

# Data for real price test
data_price = select(data,1, 2)

# Estimate the GSADF statistic
gsadf = exuber::radf(data = data_price)

# Critical Value
cv = exuber::radf_wb_cv2(data_price)

# Correct the attributes
attributes(gsadf)$index = 1:nrow(data_price)
attributes(cv)$index = 1:nrow(data_price)

# Distribution
distr = exuber::radf_wb_distr2(data_price)

# Bubble episodes
ep = exuber::datestamp(gsadf,cv = cv, sig_lvl = 90)

# Bubble diagnostics
exuber::diagnostics(gsadf,cv = cv,sig_lvl = 90)

#Ploting
exuber::autoplot(gsadf, cv = cv, sig_lvl = 90)
exuber::autoplot2(gsadf, cv = cv, sig_lvl = 90)



###########################################
# Multiple Bubble for Price to Rent Ratio #
###########################################

# Data for ratio test
data_ratio = select(data,Data, Ratio)

# Estimate the GSADF statistic
gsadf = exuber::radf(data = data_ratio)

# Critical Value
cv = exuber::radf_wb_cv2(data_ratio)

# Correct the attributes
attributes(gsadf)$index = 1:nrow(data_ratio)
attributes(cv)$index = 1:nrow(data_ratio)

# Distribution
distr = exuber::radf_wb_distr2(data_ratio)

# Bubble episodes
ep = exuber::datestamp(gsadf,cv = cv, sig_lvl = 90)

# Bubble diagnostics
exuber::diagnostics(gsadf,cv = cv,sig_lvl = 90)

#Ploting
exuber::autoplot(gsadf, cv = cv, sig_lvl = 90)
exuber::autoplot2(gsadf, cv = cv, sig_lvl = 90)
