#-----------------------------
# Autor: Edinson Tolentino
# Proyecto : poverty analysis (Peru)
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
                  "ggplot2", "caTools","caret","MLmetrics")
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

table(data$rpoverty)
str(data$rpoverty)
Hmisc::describe(data$rpoverty)
Hmisc::describe(data$rpoverty_f)

# select dataset to model
dataset <- data %>% 
  dplyr::select(rpoverty, rpoverty_f, lnr6, redad, rneduca_rprimaria, rneduca_rsecundaria,
                rneduca_rsuperior, rneduca_rtecnica, relectricidad, rpeople, 
                rcod_hogar)
dataset %>%  dim()

#-------------------------------------------------------------
# Test and train dataset
#---------------------------------------------------
# 75% of the sample size
# install.packages("caTools")
# require(caTools)
set.seed(101) 
sample = sample.split(dataset$rcod_hogar, SplitRatio = .75)
train = subset(dataset, sample == TRUE)
test  = subset(dataset, sample == FALSE)

train %>% dim()
test %>%  dim()

#-------------------------------------------------------------
# Models (logit)
#-------------------------------------------------------------
train %>%  names()
train %>%  str()
logit_1 <- glm(rpoverty_f  ~ lnr6 + redad + rneduca_rprimaria + rneduca_rtecnica 
               + rneduca_rsuperior + rpeople + relectricidad  , 
               family = "binomial", data=train)
summary(logit_1)
prop.table(table(train$rpoverty_f)) # Proporcion de hogares pobres
Hmisc::describe(train$rpoverty_f)

# Make tex file
tabla1 <-  texreg(list(logit_1), label = "tab:1",
               #custom.coef.names = c(),
               custom.model.names = c("Logit model"),
               caption = "Model Logit",
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
write(tabla1, file = paste0(ruta,"/",tabla,"/","tabla_1.tex"))

# make predictions
logit_1_prob <- predict(logit_1, type = "response")
logit_1_prob[1:10]
logit_1_prob %>%  summary()

# creamos la prediccion de pobreza
# table(train$rpoverty_f)
logit_proverty_prob <- rep(0,nrow(train))
logit_proverty_prob[logit_1_prob > .5] = 1

table(train$rpoverty, logit_proverty_prob)
mean(logit_proverty_prob == train$rpoverty) 
#table(logit_proverty_prob, train$rpoverty_f)
#(18399+4397)/nrow(train)


#--------------------------------------------------------------
# Specify type of training method used and the number of folds
# library(caret)
set.seed(1985)
ctrlspecs <- trainControl(method="cv", 
                          number=5, 
                          savePredictions="all",
                          classProbs=TRUE)

m1 <- train(rpoverty_f ~ lnr6 + redad + rneduca_rprimaria 
                + rneduca_rtecnica + rneduca_rsuperior 
                + rpeople + relectricidad,
                data=train,method="glm",
                family=binomial, 
            trControl=ctrlspecs)
summary(m1)
print(m1)
varImp(m1)      # importance of varibales

# Predict outcome using model from training data based on testing data
pred_logit <- predict(m1, newdata=test)
confusionMatrix(data=pred_logit, test$rpoverty_f)

# final model
modelo_final <- m1$finalModel
summary(m1)

## CV: Cross-Validation
## -------------------------------------
CV_AUC_folds <- list()
CV_AUC_folds <- m1$resample[,c("Resample", "Accuracy")]
CV_AUC_folds

CV_AUC <- list()
CV_AUC <- mean(CV_AUC_folds$Accuracy)
CV_AUC

## Indicators GINI
CV_GINI_folds <- rep(0,0)
for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  gini_fold <- 
    m1$pred %>%
    filter(Resample == fold)
  gini <- Gini(gini_fold$obs, gini_fold$Pobre)
  CV_GINI_folds[fold] <- as.numeric(gini)
}
CV_GINI <- mean(CV_GINI_folds)

## summary results ----------------
summary_results <- data.frame("CV AUC"= unlist(CV_AUC),
                              "CV GINI" = unlist(CV_GINI))
modelos <- c('Logit M1')
rownames(summary_results) <- modelos
summary_results

