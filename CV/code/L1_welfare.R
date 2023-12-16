#-----------------------------
# Autor: Edinson Tolentino
# Proyecto : expenditure analysis (Peru)
#-----------------------------
rm(list=ls())

#-------------------------------------------------------------
# Carpetas de informacion
#-------------------------------------------------------------
ruta   <- "C:/Users/et396/Dropbox"
base   <- "/BASES/ENAHO"
codigo <- "/Scripts"
out    <- "/Docencia/Impacto/ML/L1/Data"
tabla  <- "/Docencia/Impacto/ML/L1/Tablas"

#-------------------------------------------------------------
# Importar librerias
#-------------------------------------------------------------
paquetes_set <- c("readstata13", "dplyr", "tidyverse", "Hmisc",
                  "survey", "stargazer", "texreg", "sjPlot", 
                  "ggplot2", "caTools","caret","MLmetrics","boot")
#install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

# Import 
#-------------------------------------------------------------
# Import database
#-------------------------------------------------------------
data <- readRDS(file = paste0(ruta,"/",out,"/","base_pobreza.rds"))
data %>%  dim()
data %>%  names()
data %>%  summary()
data %>%  str()

Hmisc::describe(data$lnrgasto)
Hmisc::describe(data$rpoverty_f)

# select dataset to model
dataset <- data %>% 
  dplyr::select(lnrgasto, rgasto ,rpoverty, rpoverty_f, lnr6, redad, rneduca_rprimaria, 
                rneduca_rsecundaria,rneduca_rsuperior, rneduca_rtecnica, 
                relectricidad, rpeople, r6,
                rcod_hogar)
dataset %>%  dim()
dataset %>%  names()

plot(rgasto~r6, data=dataset)
plot(rgasto~redad, data=dataset)

#-------------------------------------------------------------
# Test and train dataset
#---------------------------------------------------
# 75% of the sample size
# install.packages("caTools")
# require(caTools)
set.seed(101) 
sample = sample.split(dataset$rcod_hogar, SplitRatio = .75)
data_train = subset(dataset, sample == TRUE)
data_test  = subset(dataset, sample == FALSE)

data_train %>% dim()
data_test %>%  dim()

#-------------------------------------------------------------
# Models (ols)
#-------------------------------------------------------------
data_train %>%  names()
data_train %>%  str()
m0 <- glm(rgasto  ~ redad + rneduca_rprimaria + rneduca_rtecnica 
         + rneduca_rsuperior + rpeople + relectricidad  , 
         data=dataset)
summary(m0)

m1 <- lm(rgasto  ~ redad + rneduca_rprimaria + rneduca_rtecnica 
               + rneduca_rsuperior + rpeople + relectricidad  , 
         data=dataset)
summary(m1)
mod1 <- list(m1)
Hmisc::describe(dataset$rgasto)

# Make tex file
tabla1 <-  texreg(mod1, label = "tab:1",
                  #custom.coef.names = c(),
                  custom.model.names = c("OLS model"),
                  caption = "Model OLS",
                  float.pos = "h", 
                  return.string = TRUE, 
                  bold = 0.05, 
                  stars = c(0.01, 0.05, 0.1),
                  #custom.note = "Coefficients with $p < 0.05$ in \\textbf{bold}.",
                  digits = 3, 
                  leading.zero = FALSE, 
                  omit.coef = "Inter")
tabla1
# export to latex
write(tabla1, file = paste0(ruta,"/",tabla,"/","tabla_m1.tex"))

## The Validation Set Approach
plot(rgasto~redad, data=data_test)
data_train %>%  dim()
data_test %>%  dim()

set.seed(1)
train <- sample(20676, 6893)
###
lm.fit <- lm(rgasto ~ redad, data = dataset, subset = train)
###
attach(dataset)
mean((rgasto - predict(lm.fit, dataset))[-train]^2)
###
lm.fit2 <- lm(rgasto ~ poly(redad, 2), data = dataset, 
              subset = train)
mean((rgasto - predict(lm.fit2, dataset))[-train]^2)
lm.fit3 <- lm(rgasto ~ poly(redad, 3), data = dataset, 
              subset = train)
mean((rgasto - predict(lm.fit3, dataset))[-train]^2)

##########

set.seed(2)
train <- sample(20676, 6893)
###
lm.fit <- lm(rgasto ~ redad, data = dataset, subset = train)
###
attach(dataset)
mean((rgasto - predict(lm.fit, dataset))[-train]^2)
###
lm.fit2 <- lm(rgasto ~ poly(redad, 2), data = dataset, 
              subset = train)
mean((rgasto - predict(lm.fit2, dataset))[-train]^2)
lm.fit3 <- lm(rgasto ~ poly(redad, 3), data = dataset, 
              subset = train)
mean((rgasto - predict(lm.fit3, dataset))[-train]^2)

## Leave-One-Out Cross-Validation
# more time
# library(boot)
# glm.fit <- glm(rgasto ~ redad, data = dataset)
# cv.err <- cv.glm(dataset, glm.fit)
# cv.err$delta

###
# more time
# cv.error <- rep(0, 3)
# for (i in 1:3) {
#   glm.fit <- glm(rgasto ~ poly(redad, i), data = dataset)
#   cv.error[i] <- cv.glm(dataset, glm.fit)$delta[1]
# }
# cv.error

###
## $k$-Fold Cross-Validation
### less time
set.seed(17)
degree = 1:5
cv.error.5 <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(rgasto ~ poly(redad, i), data = dataset)
  cv.error.5[i] <- cv.glm(dataset, glm.fit, K = 3)$delta[1]
}
plot(degree,cv.error.5, type='o')
cv.error.5
