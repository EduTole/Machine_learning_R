# Aplicacion de la ecuacion de mincer
rm(list=ls())
# Ruta de carpeta 
setwd("C:\\Users\\et396\\Dropbox\\Aplicacion")
 
# Paquetes para instalar y poder usarlos
paquetes_set <- c("readstata13", "dplyr", "tidyverse", "Hmisc",
                  "survey", "stargazer", "texreg", "sjPlot", 
                  "ggplot2", "caTools","caret","MLmetrics","boot")
# install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

data <- read.csv("mod_500_2021.csv")
data %>% head()

# Graficos

educacion_df <- data %>% 
  group_by(rneduca) %>% 
  summarise(ingresos= mean(r6))

ggplot(educacion_df, aes(x=rneduca, y=ingresos))+
  geom_col()

ggplot(educacion_df, aes(x=rneduca, y=ingresos))+
  geom_col()+
  geom_text(aes(label=ingresos))

educacion_df <- data %>% 
  group_by(rneduca) %>% 
  summarise(ingresos= round(mean(r6),0))

ggplot(educacion_df, aes(x=rneduca, y=ingresos))+
  geom_col()+
  geom_text(aes(label=ingresos), vjust = 1.6, color="white")

# Segun sexo
educacion_df <- data %>% 
  group_by(rneduca, rmujer) %>% 
  summarise(ingresos= round(mean(r6),0))

ggplot(educacion_df, aes(x=rneduca, y=ingresos))+
  geom_col(aes(color = rmujer, fill = rmujer),
           position = position_dodge(0.8), width = 0.7)+
  geom_text(aes(label=ingresos), vjust = -1.8, color="black")

ggplot(educacion_df, aes(x=rneduca, y=ingresos))+
  geom_col(aes(color = rmujer, fill = rmujer),
           position = position_dodge(0.8), width = 0.7)+
  geom_text(aes(label=ingresos, group=rmujer), position = position_dodge(0.8),
            vjust = -0.3, size=3.5, color="black")+
  xlab("Niveles de educacion")+
  ylab("Ingresos laborales mensuales (S/.)")


# Modelo de regresion
data %>% names()
data <- data %>%  mutate(redadsq=redad*redad)
m1 <- lm(lnr6~ redad+ redadsq+ rmujer, data= data)
summary(m1)

# split datasets
set.seed(101) 
sample = sample.split(data$rid_persona, SplitRatio = .75)
data_train = subset(data, sample == TRUE)
data_test  = subset(data, sample == FALSE)

data_train %>% dim()
data_test %>%  dim()

# train
m1t <- lm(lnr6~ redad+ redadsq+ rmujer, data= data_train)
summary(m1t)
?predict

# Prediccion y test
yhat <- predict(m1t, newdata = data_test)
rmse <- sqrt(sum((exp(yhat)-data_test$r6)^2)/length(data_test$r6))
c(RMSE = rmse , R2= summary(m1t)$r.squared)


