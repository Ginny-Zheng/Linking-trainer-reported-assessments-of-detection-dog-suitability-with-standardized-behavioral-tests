library(readxl)
library(ranger)
library(gplots)
library(dplyr)
library(psych)
library(tidyr)
library('caret') #confusionMatrix
library("pROC") #roc
library(tidyverse)

setwd("C:/Users/15305/Box/Desktop/research/Xu/DCSITE/task 2.2")
data <- read_excel("time variant and invariant data.xlsx")
View(data)

## ------------------- Prediction of CST vs final disposition -----------------
data$final_disposition <- factor(data$final_disposition, 
                                 levels = c("Washout", "Sale Quality"))

scores <- data[, c(2:49, 55)]
t3_idx <- 1:12
t6_idx <- 13:24
t10_idx <- 25:36
t12_idx <- 37:48

## -----------------------  12-month ---------------------------------------
data_model <- scores[, t12_idx]
names(data_model)[12] <- "Trainability"
names(data_model)

fit <- lm(Trainability~., data = data_model)
summary(fit)

fit_reduced <- lm(Trainability~ Hunt.Time12 + 
                    Surfaces.Time12 +
                    People.Time12  +
                    `Work/Effort.Time12` +
                    Excitability.Time12, 
                  data = data_model)
summary(fit_reduced)

pred_data <- data.frame(true = data_model$Trainability, 
                        pred = predict(fit_reduced, newdata = data_model),
                        group = data$final_disposition)
cor(pred_data$true, pred_data$pred, method = 'pearson')
cor(pred_data$true, pred_data$pred, method = 'spearman')

data$CST.Time12 <- pred_data$pred

## CST vs trainability score
ggplot(pred_data, aes(x = true, y = pred)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE) + theme_classic()+
  ggtitle(paste("CST vs Trainability Scores, 5 selected treats", 
                round(cor(predict(fit_reduced, newdata = data_model), 
                          data_model$Trainability), 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=14, face="bold"),
    axis.text.y = element_text(size=14, face="bold")) 

## CST vs Final disposition
cst_3 <- data.frame(ID = 1:180,
                    dogID = data$DogID,
                    CST = predict(fit_reduced, newdata = data_model),
                    final_disposition = data$final_disposition)
cst3_ordered <- cst_3 %>% arrange(CST)

ggplot(cst3_ordered, aes(x = 1:180, y = CST)) + theme_bw() +
  geom_point(aes(colour = final_disposition)) + 
  geom_hline(yintercept = 3.2)

## cut-off of CST
pred.class <- ifelse(cst_3$CST > 3.2,'Sale Quality', 'Washout')
## Confusion matrix and statistics
confusionMatrix(data = factor(pred.class, 
                              levels = c("Washout", "Sale Quality")), 
                reference = data$final_disposition, 
                mode = "everything", positive = "Sale Quality")

table(cst_3$final_disposition[which(cst_3$CST>3)])


## -----------------------  10-month ---------------------------------------
data_model <- scores[, t10_idx]
names(data_model)[12] <- "Trainability"
names(data_model)

fit <- lm(Trainability~., data = data_model)
summary(fit)

fit_reduced <- lm(Trainability~ `Focus.on.Toy/Reward.Time10` + 
                    Independence.Time10 +
                    People.Time10  +
                    `Vehicles.&.Urban.Clutter.Time10` +
                    Excitability.Time10, 
                  data = data_model)
summary(fit_reduced)

pred_data <- data.frame(true = data_model$Trainability, 
                        pred = predict(fit_reduced, newdata = data_model),
                        group = data$final_disposition)
cor(pred_data$true, pred_data$pred, method = 'pearson')
cor(pred_data$true, pred_data$pred, method = 'spearman')

data$CST.Time10 <- pred_data$pred

## CST vs trainability score
ggplot(pred_data, aes(x = true, y = pred)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE) + theme_classic()+
  ggtitle(paste("CST vs Trainability Scores, 5 selected treats", 
                round(cor(predict(fit_reduced, newdata = data_model), 
                          data_model$Trainability), 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=14, face="bold"),
    axis.text.y = element_text(size=14, face="bold")) 

## CST vs Final disposition
cst_3 <- data.frame(ID = 1:180,
                    dogID = data$DogID,
                    CST = predict(fit_reduced, newdata = data_model),
                    final_disposition = data$final_disposition)
cst3_ordered <- cst_3 %>% arrange(CST)

ggplot(cst3_ordered, aes(x = 1:180, y = CST)) + theme_bw() +
  geom_point(aes(colour = final_disposition)) + 
  geom_hline(yintercept = 3)

## cut-off of CST
pred.class <- ifelse(cst_3$CST > 3,'Sale Quality', 'Washout')
## Confusion matrix and statistics
confusionMatrix(data = factor(pred.class, 
                              levels = c("Washout", "Sale Quality")), 
                reference = data$final_disposition, 
                mode = "everything", positive = "Sale Quality")


## -----------------------  6-month ---------------------------------------
data_model <- scores[, t6_idx]
names(data_model)[12] <- "Trainability"
names(data_model)

fit <- lm(Trainability~., data = data_model)
summary(fit)

fit_reduced <- lm(Trainability~ Physical.Possessiveness.of.Toy.Time6 + 
                    Independence.Time6 +
                    Surfaces.Time6  +
                    `Work/Effort.Time6` +
                    Excitability.Time6, 
                  data = data_model)
summary(fit_reduced)

pred_data <- data.frame(true = data_model$Trainability, 
                        pred = predict(fit_reduced, newdata = data_model),
                        group = data$final_disposition)
cor(pred_data$true, pred_data$pred, method = 'pearson')
cor(pred_data$true, pred_data$pred, method = 'spearman')

data$CST.Time6 <- pred_data$pred

## CST vs trainability score
ggplot(pred_data, aes(x = true, y = pred)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE) + theme_classic()+
  ggtitle(paste("CST vs Trainability Scores, 5 selected treats", 
                round(cor(predict(fit_reduced, newdata = data_model), 
                          data_model$Trainability), 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=14, face="bold"),
    axis.text.y = element_text(size=14, face="bold")) 

## CST vs Final disposition
cst_3 <- data.frame(ID = 1:180,
                    dogID = data$DogID,
                    CST = predict(fit_reduced, newdata = data_model),
                    final_disposition = data$final_disposition)
cst3_ordered <- cst_3 %>% arrange(CST)

ggplot(cst3_ordered, aes(x = 1:180, y = CST)) + theme_bw() +
  geom_point(aes(colour = final_disposition)) + 
  geom_hline(yintercept = 2.8)

## cut-off of CST
pred.class <- ifelse(cst_3$CST > 2.8,'Sale Quality', 'Washout')
## Confusion matrix and statistics
confusionMatrix(data = factor(pred.class, 
                              levels = c("Washout", "Sale Quality")), 
                reference = data$final_disposition, 
                mode = "everything", positive = "Sale Quality")

## -----------------------  3-month ---------------------------------------
data_model <- scores[, t3_idx]
names(data_model)[12] <- "Trainability"
names(data_model)

fit <- lm(Trainability~., data = data_model)
summary(fit)

fit_reduced <- lm(Trainability~ Physical.Possessiveness.of.Toy.Time3 + 
                    Independence.Time3 +
                    Hunt.Time3  +
                    `Work/Effort.Time3` +
                    Excitability.Time3, 
                  data = data_model)
summary(fit_reduced)

pred_data <- data.frame(true = data_model$Trainability, 
                        pred = predict(fit_reduced, newdata = data_model),
                        group = data$final_disposition)
cor(pred_data$true, pred_data$pred, method = 'pearson')
cor(pred_data$true, pred_data$pred, method = 'spearman')

data$CST.Time3 <- pred_data$pred

## CST vs trainability score
ggplot(pred_data, aes(x = true, y = pred)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE) + theme_classic()+
  ggtitle(paste("CST vs Trainability Scores, 5 selected treats", 
                round(cor(predict(fit_reduced, newdata = data_model), 
                          data_model$Trainability), 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=14, face="bold"),
    axis.text.y = element_text(size=14, face="bold")) 

## CST vs Final disposition
cst_3 <- data.frame(ID = 1:180,
                    dogID = data$DogID,
                    CST = predict(fit_reduced, newdata = data_model),
                    final_disposition = data$final_disposition)
cst3_ordered <- cst_3 %>% arrange(CST)

ggplot(cst3_ordered, aes(x = 1:180, y = CST)) + theme_bw() +
  geom_point(aes(colour = final_disposition)) + 
  geom_hline(yintercept = 2.6)

## cut-off of CST
pred.class <- ifelse(cst_3$CST > 2.6,'Sale Quality', 'Washout')
## Confusion matrix and statistics
confusionMatrix(data = factor(pred.class, 
                              levels = c("Washout", "Sale Quality")), 
                reference = data$final_disposition, 
                mode = "everything", positive = "Sale Quality")

library(writexl)
write_xlsx(data, path = "Data with CST.xlsx")
