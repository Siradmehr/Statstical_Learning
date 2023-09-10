library(data.table)
library(ggplot2)
data = fread('/Users/apple/Downloads/diabetes_012.csv')
data_cpy = fread('/Users/apple/Downloads/diabetes_012.csv')
data_cpy2 = fread('/Users/apple/Downloads/diabetes_012.csv')
data_cpy$Diabetes_012 <- NULL
data$Diabetes_012 = as.factor(data$Diabetes_012)
ggplot(data, aes(x=Age, y=Diabetes_012,color = Diabetes_012)) +
  geom_boxplot(alpha = 0.75)

data$one = 1
ds = data[,.(n = sum(one)),.(Diabetes_012,NoDocbcCost)]
ds[,n_total := sum(n),.(NoDocbcCost)]
ds[,n_percent := n/n_total]
ggplot(ds,aes(as.factor(NoDocbcCost),n_percent,fill = Diabetes_012))+
  geom_bar(stat = 'identity')
ds2 = data[,.(n= sum(one)),.(Diabetes_012,HighChol)]
ds2[,n_total := sum(n),.(HighChol)]
ds2[,n_percent := n/n_total]
ggplot(ds2,aes(as.factor(HighChol),n_percent,fill = Diabetes_012))+
  geom_bar(stat = 'identity')
##Q2
library(Hmisc)
library(corrplot)
library(psych)
library(psy)
library(gridExtra)
library(ggplot2)
library(caret)
library(factoextra)
library(maptools)
library(dplyr)

data_df <- data_cpy[sample(1:nrow(data),14000,replace=F), ]
sprintf("The new dataset has %s observations with a total of %s features.", nrow(data_df), ncol(data_df))
preprocess <- preProcess(data_df, method=c("center", "scale"))
data_norm <- predict(preprocess, data_df)
sprintf("The dataset contain %s missing values .",sum(is.na(data_norm)))
data_norm[] <- lapply(data_norm, as.numeric)
cor_matrix <- cor(data_norm)
corrplot(cor_matrix, type = "upper", order = "hclust", tl.col = "black", tl.cex = 0.6)
diabetes_pca <- prcomp(data_norm, center=FALSE, scale=FALSE)
summary(diabetes_pca)
fviz_eig(diabetes_pca,  choice='eigenvalue',barfill = "goldenrod4",barcolor = "goldenrod4")
plot(summary(diabetes_pca)$importance[3,], ylab="Cumulative Variance", main="Cumulative Variance")

diabetes_eigVal <- get_eigenvalue(diabetes_pca)
diabetes_eigVal[, -1]

diabetes_eigVal[, 1]
scree.plot(diabetes_eigVal[,1], title = "EigenValue", type = "E",  simu = "P")

fviz_pca_var(diabetes_pca, col.var="contrib",axes = c(1, 2))+
  labs(title="PC1 and PC2") +
  scale_color_gradient2(low="#77AA55", mid="#BB0066", 
                        high="black", midpoint=5)
fviz_pca_var(diabetes_pca, col.var="contrib",axes = c(3, 4))+
  labs(title="PC3 and PC4") +
  scale_color_gradient2(low="#77AA55", mid="#BB0066", 
                        high="black", midpoint=5)
PC_1_Contrib <- fviz_contrib(diabetes_pca, "var", axes=1, fill = "goldenrod4",color = "goldenrod4") 
PC_2_Contrib <- fviz_contrib(diabetes_pca, "var", axes=2, fill = "goldenrod4",color = "goldenrod4") 
PC_3_Contrib <- fviz_contrib(diabetes_pca, "var", axes=3, fill = "goldenrod4",color = "goldenrod4")
PC_4_Contrib <- fviz_contrib(diabetes_pca, "var", axes=4, fill = "goldenrod4",color = "goldenrod4")
PC_5_Contrib <- fviz_contrib(diabetes_pca, "var", axes=5, fill = "goldenrod4",color = "goldenrod4") 
PC_6_Contrib <- fviz_contrib(diabetes_pca, "var", axes=6, fill = "goldenrod4",color = "goldenrod4")
PC_7_Contrib <- fviz_contrib(diabetes_pca, "var", axes=7, fill = "goldenrod4",color = "goldenrod4")

grid.arrange(PC_1_Contrib,PC_2_Contrib,PC_3_Contrib)
grid.arrange(PC_4_Contrib,PC_5_Contrib,PC_6_Contrib)
grid.arrange(PC_7_Contrib)

diabetes_pca_r<-principal(data_norm, nfactors=11, rotate="varimax")
plot(diabetes_pca_r$complexity, diabetes_pca_r$uniqueness)
pointLabel(diabetes_pca_r$complexity, diabetes_pca_r$uniqueness, labels=names(diabetes_pca_r$uniqueness), cex=0.8) 
abline(h=c(0.4), lty=3, col=2)
abline(v=c(1.9), lty=3, col=2)

library(ggplot2)
library(ggvis)
library(corrplot)
library(caTools)
library(ROCR)

smp_size <- floor(0.75 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

library(nnet)
model <- multinom (Diabetes_012 ~ Age + PhysActivity + Sex + HvyAlcoholConsump + BMI + HighBP + Stroke , data = data_cpy2)
model2 <- multinom (Diabetes_012 ~ . , data = data_cpy2)
summary(model)
summary(model2)

train$ClassPredicted <- predict(model, newdata = train, "class")
train$ClassPredictedall <- predict(model2, newdata = train, "class")

test$ClassPredicted <- predict(model, newdata = test, "class")
test$ClassPredictedall <- predict(model2, newdata = test, "class")

tab <- table(train$Diabetes_012, train$ClassPredicted)
tab2 <- table(train$Diabetes_012, train$ClassPredictedall)
tab3 <- table(test$Diabetes_012, test$ClassPredicted)
tab4 <- table(test$Diabetes_012, test$ClassPredictedall)
sprintf("The train dataset ACC after PCA: %s",round((sum(diag(tab))/sum(tab))*100,2))
sprintf("The train dataset ACC before PCA %s:",round((sum(diag(tab2))/sum(tab2))*100,2))
sprintf("The test dataset ACC after PCA: %s",round((sum(diag(tab3))/sum(tab3))*100,2))
sprintf("The test dataset ACC before PCA: %s",round((sum(diag(tab4))/sum(tab4))*100,2))