###APPLIED MULTIVARIATE DATA ANALYSIS
###FINAL PROJECT SPRING 2021 - PART 1
###MODEL CREATION
###READMISSION PREDICTION OF DIABETES PATIENTS
###KAUSHIKI CHAUHAN

library(GGally)
library(corrplot)
library(ggplot2)
library(ggfortify)  #interpret PCA objects with autoplot
library(caret)

##################################DATA FETCHING############################################
filename <- "C://Workspace//Rowan Class//Applied Multivariate Data Analysis//Final Project//Part1//dataset_diabetes//cleaned_diabetic_data.csv"
data <- read.csv(filename, header = TRUE)
head(data)
summary(data)
str(data)
dim(data)


#########################UNSUPERVISED LEARNING##################################
############KMEANS CLUSTERING
dim(data)
library(cluster)
library(dplyr)
autoplot(clara(data[-33], 2), frame = TRUE)

#Tried pam cluster. It didn't work since my dataset has records around 100K 
#and PAM doesn't work with records more than 65536 in dataset 
autoplot(pam(data[,-33], 2), frame = TRUE, frame.type = 'norm')

#Another clustering method - Correct one
#install.packages("factoextra")
library(factoextra)
data.scaled <- scale(data[,-33])
km <- kmeans(data.scaled, 2, nstart = 10)
data.frame(data$readmitted, km$cluster)
fviz_cluster(km, data[, -33], ellipse.type = "norm")

####################################CORRELATION#########################################

#Feature Selection using Correlation and PCA
#Correlation Matrix

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#chart.Correlation(df2, histogram = TRUE, pch = 19)

library(mlbench)
library(caret)
# set.seed(100)
# rpartMod <- train(data$readmitted ~ ., data=data, method="rpart")
# rpartImp <- varImp(rpartMod, scale=FALSE)
# rpartImp

correlationMatrix <- cor(data[,c(1:32)])
print(correlationMatrix)
#find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)
#print indexes of highly correlated attributes
print(highlyCorrelated)

par("mar")
par(mar=c(1,1,1,1))
c <- cor(data, method = "pearson", use = "pairwise.complete.obs")
corrplot(c, type = "full", method = "circle", order = "hclust")

#pairs(df2, pch = 19, col = "#990000")
ggcorr(data[,-33], method = c("pairwise","pearson"), label = TRUE, 
       label_size = 3, label_round = 2)

graphics.off()

#PCA
Z <- scale(data[,-33])
pca <- prcomp(Z, scale = TRUE)
summary(pca)
names(pca)
pca$center
pca$scale
pca$rotation[1:5,1:4]
dim(pca$x)

round((pca$sdev^2),3)

#Eigen value is 1 till PC15

biplot(pca, scale = TRUE)

library(ggfortify)
autoplot(pca, data = data, colour = "readmitted", 
         loadings = TRUE, loadings.label = TRUE)

par (mar=c(5,5,5,5) , # space around graph
     cex.main=2 , # title font Size
     cex.lab=1.5 , # axes labels Font
     cex.axis= 1 # Font Size for axes Numbers
)
screeplot(pca, type = "lines", lwd = 1, col ="red")

####PCA- 27 variables explain about 90% of variance in dataset. We reduced 33 to 27 variables 

data.color<-ifelse(test = data$readmitted == 1, yes = "blue", no = "red")
pc <- princomp(data[,1:32], cor = TRUE,scores = TRUE)
summary(pc, loadings = TRUE, cutoff = 0.0)
score.cor<-predict(pc, newdata = data)
common.limits<-c(min(score.cor[,1:2]), max(score.cor[,1:2]))

autoplot(pc, data = data, colour = "readmitted", loadings = TRUE, 
         loadings.label = TRUE)

#Variables with high variances
#Number of variables selected whose loadings is equal to or larger than 0.3 
#in 1st and 2nd PCs or Eigen values >= 1

screeplot(pc, type = "lines", lwd = 1, col ="red")

names(pc)
pc$loadings[,1:3]*100
pc$scale

biplot(pc, scale = TRUE)

#########PCA - To be kept
library(FactoMineR)
library(ggrepel)
res.pca <- PCA(data[,1:32],  graph = FALSE)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 10))
# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)
# Extract the results for variables
var <- get_pca_var(res.pca)
var

head(var$coord)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


###Factor Analysis
#factanal() produces maximum likelihood factor analysis
z <- scale(data[,1:32], scale = TRUE)
out <- factanal(z, factors = 3, scores = "regression")
print(out, digits = 2, cutoff = 0.1, sort = TRUE)

#Ideally, Uniqueness should be small
#Loading matrix lambda
L <- out$loadings
round(rowSums(L^2), 3)

# plot factor 1 by factor 2
load <- out$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(data),cex=.7, col = "red")

autoplot(load, label = TRUE, label.size = 3,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)

graphics.off()

#Find moderate to high loadings greater than 0.3

#New variable selection after PCA, Factor loadings
library(tidyverse)
variable_selected <- c("race","age","admission_type_id","discharge_disposition_id",
                       "admission_source_id","time_in_hospital","num_procedures",
                       "num_medications","number_emergency","number_inpatient",
                       "number_diagnoses","max_glu_serum","A1Cresult","glipizide",
                       "insulin","change","diabetesMed","readmitted")

new_data <- data[variable_selected]
head(new_data)
dim(new_data)

par("mar")
par(mar=c(1,1,1,1))
c <- cor(new_data, method = "pearson", use = "pairwise.complete.obs")
corrplot(c, type = "full", method = "circle", order = "hclust")


##############################DATASET SPLITTING#############################
#####Splitting data into Training and Testing dataset
library(caTools)
set.seed(123)

split <-  sample.split(new_data$readmitted, SplitRatio = 0.7)
training_set <- subset(new_data, split == TRUE)
test_set <- subset(new_data, split == FALSE)
dim(training_set)
dim(test_set)


########################SUPERVISED LEARNING#################################
####Logistic Regression
model_glm <- glm(formula = readmitted ~ ., family = binomial(link = 'logit'), 
                 data = training_set)
summary(model_glm)

library(car)
anova(model_glm, test = 'Chisq')
#AIC value of model_glm is 47174. Let's create another model by removing less
#significant variable 

model2_glm <- glm(formula = readmitted ~ age+admission_type_id+discharge_disposition_id+
                  time_in_hospital+num_procedures+num_medications+number_emergency+
                  number_inpatient+number_diagnoses+insulin+diabetesMed,
                  family = binomial(link = 'logit'), data = training_set)
summary(model2_glm)
#AIC value = 47176

#Let's consider null hypothesis - second model is better than first model. 
#If p<0.05 - reject H0, or p>0.05 - failed to reject H0
anova(model_glm, model2_glm, test = 'Chisq')

#p<0.05 - reject null hypothesis. Thus, first model is better than second

#exponents of Logistic model so we can interpret the magnitude of coefficients
round(exp(coef(model_glm)),3)

#Training Data
pi.hat <- predict(object = model_glm, type = "response")
head(pi.hat)

#Test data
pi.hat.v<-predict(object = model_glm, newdata = test_set, type = "response")
head(pi.hat.v)

cutoff <- 0.5
pred.readmitted_train <- ifelse(test = pi.hat > cutoff, 1, 0)
pred.readmitted_test <- ifelse(test = pi.hat.v > cutoff, 1, 0)

#Accuracy
summarize.class<-function(original, classify) {
  class.table<-table(original, classify)
  numb<-rowSums(class.table)
  prop<-round(class.table/numb,4)
  overall<-round(sum(diag(class.table))/sum(class.table),4)
  list(class.table = class.table, prop = prop, overall.correct = overall)
}

summarize.class(original = training_set$readmitted, classify = pred.readmitted_train)
summarize.class(original = test_set$readmitted, classify = pred.readmitted_test)

#Some numbers in the testing set were never predicted. So, used table method to
#find out the differences and used factor() in both test and predicted set
table(pred.readmitted_test)
table(test_set$readmitted)

cm_train <- confusionMatrix(factor(pred.readmitted_train),factor(training_set$readmitted))
cm_train
cm <- confusionMatrix(factor(pred.readmitted_test),factor(test_set$readmitted))
cm

#Confusion matrix training set
ggplot(as.data.frame(cm_train$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Class_1","Class_2")) +
  scale_y_discrete(labels=c("Class_2","Class_1"))

#Confusion matrix test set
ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("0","1")) +
  scale_y_discrete(labels=c("1","0"))

####Receiver Operating Characteristics (ROC) Curve

library(ROCR)   #This is a S4 package

save.pred<-prediction(predictions = pi.hat.v, labels = test_set$readmitted)
slotNames(save.pred)
head(save.pred@fp[[1]]) #Number of false positives for a cut-off
tail(save.pred@fp[[1]]) #Number of false positives for a cut-off
head(save.pred@cutoffs[[1]]) #Corresponding cut-offs
tail(save.pred@cutoffs[[1]]) #Corresponding cut-offs

save.perf<-performance(prediction.obj = save.pred, "sens", "fpr")
slotNames(save.perf)

par (mar=c (5,5,5,5) , # space around graph
     cex.main=2 , # title font Size
     cex.lab=1.5 , # axes labels Font
     cex.axis= 1 # Font Size for axes Numbers
)

plot(save.perf, colorize = TRUE, main = "ROC Curve", )
grid()


library(pROC)
test_prob = predict(model_glm, newdata = test_set, type = "response")
test_roc = roc(test_set$readmitted ~ test_prob, plot = TRUE, print.auc = TRUE)

#Corresponding Analysis
res.ca <- CA(data[,1:32], graph = FALSE)
get_ca_row(res.ca)
get_ca_col(res.ca)

fviz_ca_biplot(res.ca, repel = TRUE)


