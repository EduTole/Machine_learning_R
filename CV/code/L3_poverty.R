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
Tablas <- "/Docencia/Impacto/ML/L1/Tablas"

# Function ---------------------------------
source(paste0(ruta,base,codigo,"/","ml_support_function.R"))


#-------------------------------------------------------------
# Importar librerias
#-------------------------------------------------------------
# carga 
#library(readstata13)
#?readstata13
#library(tidyverse)
#library(dplyr)
#library(tidyverse)
# factor de expansion
#install.packages("Hmisc")
#library(Hmisc)
#install.packages("survey")
#library(survey)
#install.packages("stargazer") 
#library(stargazer)
#install.packages("labelled")
#library(labelled)

paquetes <- c("haven", "purrr", "margins", "skimr", "httr","kableExtra",
              "Hmisc", "cowplot", "gmodels", "lspline", "sandwich",
              "modelsummary", "rattle", "caret", "pROC", "ranger", "rpart",
              "partykit","rpart.plot","MLmetrics")
#install.packages(paquetes)
lapply(paquetes, library, character.only=TRUE)

# ------
# Crtl + shicft + C comentarios grupales
# library(haven)
# #library(glmnet)
# library(purrr)
# #install.packages("margins")
# library(margins)
# library(skimr)
# #install.packages("httr")
# library(httr)
# #install.packages("kableExtra")
# library(kableExtra)
# library(Hmisc)
# #install.packages("cowplot")
# library(cowplot)
# #install.packages("gmodels")
# library(gmodels) 
# library(lspline)
# library(sandwich)
# library(modelsummary)
# 
# #install.packages("rattle")
# library(rattle)
# library(caret)
# #install.packages("pROC")
# library(pROC)
# #install.packages("ranger")
# library(ranger)
# library(rpart)
# #install.packages("partykit")
# library(partykit)
# library(rpart.plot)



#-------------------------------------------------------------
# Import database
#-------------------------------------------------------------
data <- readRDS(file = paste0(ruta,"/",out,"/","base_pobreza.rds"))
data %>%  dim()
data %>%  names()
data %>%  summary()

# we change our target variable
table(data$rpoverty)
Hmisc::describe(data$rpoverty)
Hmisc::describe(data$rpoverty_f)

#data <- data %>% mutate(rpobre = ifelse(rpoverty=="Pobre",1,0))
#data$rpobre <- data$rpoverty
#table(data$rpobre)

# informacion de variables
interactions1 <- c("redad*redad","rneduca*rpeople")

# Variables
X1 <- c("lnrgasto","redad")
X2 <- c("lnrgasto","rpeople","redad","rneduca","rmujer")
X3 <- c("lnrgasto","rpeople","redad","rneduca","rmujer",interactions1)

#-------------------------------------------------------------
# Separate database
#-------------------------------------------------------------
set.seed(13505)
train_indices <- as.integer(createDataPartition(data$rpoverty, p = 0.75, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

# Descripte analysis
Hmisc::describe(data$rpoverty_f)
Hmisc::describe(data_train$rpoverty_f)
Hmisc::describe(data_holdout$rpoverty_f)


# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)

# Train Logit Models ----------------------------------------------
logit_model_vars <- list("Logit M1" = X1, "Logit M2" = X2, "Logit M3" = X3)

CV_RMSE_folds <- list()
logit_models  <- list()

for (model_name in names(logit_model_vars)) {
  
  features <- logit_model_vars[[model_name]]
  
  set.seed(13505)
  glm_model <- train(
    formula(paste0("rpoverty_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = "binomial",
    trControl = train_control
  )
  
  logit_models[[model_name]] <- glm_model
  # Calculate RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]
  
}

# Draw ROC Curve and calculate AUC for each folds --------------------------------
# library(pROC)
CV_AUC_folds <- list()
# 
for (model_name in names(logit_models)) {
# #library(dplyr)
# 
  auc <- list()
  model <- logit_models[[model_name]]
     for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
       cv_fold <-
         model$pred %>%
         filter(Resample == fold)
       roc_obj <- roc(cv_fold$obs, cv_fold$NoPobre)
       auc[[fold]] <- as.numeric(roc_obj$auc)
     }
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                              "AUC" = unlist(auc))
 }

# For each model: average RMSE and average AUC for models ----------------------------------

CV_RMSE <- list()
CV_AUC <- list()

for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

nvars          <- lapply(logit_models, FUN = function(x) length(x$coefnames))
logit_summary1 <- data.frame("Number of predictors" = unlist(nvars),
                              "CV RMSE" = unlist(CV_RMSE),
                              "CV AUC" = unlist(CV_AUC))
logit_summary1

# format latex -----------------------------
kable(x = logit_summary1, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
       linesep = "", col.names = c("Number of predictors","CV RMSE","CV AUC")) %>%
  cat(.,file= paste0(ruta,"/",Tablas,"/", "logit_summary1.tex"))

# format html -----------------------------
kable(x = logit_summary1, format = "html", booktabs=TRUE,  digits = 3, row.names = TRUE,
       linesep = "", col.names = c("Number of predictors","CV RMSE","CV AUC")) %>%
   cat(.,file= paste0(ruta,"/",Tablas,"/", "logit_summary1.htm"))

# Eleccion de mejor modelo ------------------------------
best_logit_no_loss <- logit_models[["X3"]]

logit_predicted_probabilities_holdout    <- predict(best_logit_no_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_no_loss_pred"] <- logit_predicted_probabilities_holdout[,"Pobre"]
RMSE(data_holdout[, "best_logit_no_loss_pred", drop=TRUE], data_holdout$rpoverty)



data_holdout$rpoverty
logit_predicted_probabilities_holdout

#library(readr)
#data_clean <- read.csv("C:/Users/et396/Downloads/cs_bisnode_panel.csv")
data_clean <- read.csv("C:/Users/et396/Downloads/bisnode_firms_clean.csv")
data_clean %>%  dim()
#data_clean %>%  head()

str(data_clean$default)
str(data_clean$default_f)
table(data_clean$default_f)
