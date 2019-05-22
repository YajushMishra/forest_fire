library(caret)
library(dplyr)
forestfiresA=read.csv('C:\\Users\\YAJUSH\\Downloads\\forestfiresA.csv',stringsAsFactors = TRUE)
summary(forestfiresA$area)
hist(forestfiresA$area,col=rainbow(6))
forestfiresA
head(forestfiresA)
str(forestfiresA)
hist(forestfiresA$X)
hist(forestfiresA$y)
hist(forestfiresA$temp)
hist(forestfiresA$wind)
hist(forestfiresA$area)
hist(forestfiresA$rainfall)
hist(forestfiresA$FFMC)
hist(forestfiresA$DMC)
hist(forestfiresA$DC)
hist(forestfiresA$ISI)
hist(forestfiresA$day)
hist(forestfiresA$Seasons)
boxplot(forestfiresA$X)
boxplot(forestfiresA$Y)
boxplot(forestfiresA$day)
boxplot(forestfiresA$FFMC)
boxplot(forestfiresA$DMC)
boxplot(forestfiresA$DC)
boxplot(forestfiresA$ISI)
boxplot(forestfiresA$temp)
boxplot(forestfiresA$wind)
boxplot(forestfiresA$rain)
boxplot(forestfiresA$RH)
boxplot(forestfiresA$area)
boxplot(forestfiresA$Seasons)
boxplot(forestfiresA$day)
#we will take logarithmic transformation of area,dc,dmc,wind,rh,y, also filteration need to be done for area, and rain as rain has mostly all the values equal to 0.00 which is insignificant and so is with area.
#we need to select the attributes for data training for that we will do clustering
# assign the data from column 9-12 (features) to variable x, and the class (area) to variable y
names(forestfiresA)
A = forestfiresA[1:2]
B=forestfiresA[5:8]
C=forestfiresA[9:12]
D = forestfiresA[3:4]
y=forestfiresA$area
#Create k means model (You need to put the number how many cluster you want, in this case I use 6 because we want  classes)
area1 <- kmeans(y,6)
area1
summary(area1)
#correlation-
cor(forestfiresA)
mat<-cor(forestfiresA)
plot(forestfiresA)
library(corrplot)
corrplot(mat)
corrplot(mat,method="circle")
corrplot(mat,method="ellipse")
corrplot(mat,method="pie")
corrplot(mat,method="color")
corrplot(mat,method="shade")

corrplot(mat,order="AOE",method = "color",addCoef.col="blue")
cor.test(forestfiresA$area,forestfiresA$temp)






#After we know the result,  for error and missing data, so we need to compare the clustering result with the area/classes forestfires data.
hist(area1$cluster,col = rainbow(6))
#plots
plot(forestfires$temp,area1$cluster,col = rainbow(1))
plot(forestfires$temp,forestfires$area,col = rainbow(1))
correlation1<-cor(A, y = area1$cluster, use = "everything",method = c("pearson", "kendall", "spearman"))
correlation2<-cor(B, y = area1$cluster, use = "everything",method = c("pearson", "kendall", "spearman"))
correlation3<-cor(C, y = area1$cluster, use = "everything",method = c("pearson", "kendall", "spearman"))
correlation1
correlation2
correlation3
# result :poor correlation with y,RH  
#good correlation with x,FFMC,DMC,DC
#strong correlation with ISI,wind
#very strong correlation with temp,rain
#plot function

ggplot(forestfires, aes( forestfires$X,forestfires$DMC, color = area1$cluster)) + geom_point()
ggplot(forestfires, aes( forestfires$X,forestfires$DMC, color = area1$cluster)) + geom_point()
plot(forestfires$X,area1$cluster, col = area1$cluster)
plot(forestfires$Y,area1$cluster, col = area1$cluster)
plot(forestfires$temp,area1$cluster,col = area1$cluster)
plot(forestfires$rain,area1$cluster,col = area1$cluster)
plot(forestfires$Y,area1$cluster,col = area1$cluster)
plot(forestfires$wind,area1$cluster,col = area1$cluster)
plot(forestfires$FFMC,area1$cluster,col = area1$cluster)
plot(forestfires$DMC,area1$cluster,col = area1$cluster)
plot(forestfires$ISI,area1$cluster,col = area1$cluster)
plot(forestfires$RH,area1$cluster,col = area1$cluster)
dim(forestfires$rain)

#dataselection and filteration;
#seasons('dec'=1,'jan'=1,'feb'=1,'mar'=2,'apr'=2,'may'=2,'june'=3,'july'=3,'aug'=3,'sep'=4,'oct'=4,'nov'=4)
names(forestfires)
newforestfires<-as.data.frame(forestfiresA%>%select(ISI,wind,temp,rain,area,Seasons,X,FFMC,DMC,DC)%>%filter(area>0,area<800,rain<1,temp>=5,DMC<230,ISI>0,ISI<50,FFMC>=80,DC>0,X>0,wind<9,Seasons>1))
names(newforestfires)
names(forestfiresA)
hist(forestfiresA$FFMC)
boxplot(forestfiresA$FFMC)
hist(newforestfires$FFMC)
boxplot(newforestfires$FFMC)

#LOGARITHMIC TRANSFORMATION OF AREA,FFMC,X

area_2<-log(newforestfires$area)
FFMC_2<-log(newforestfires$wind)
X_2<-log(newforestfires$X)

hist(area_2)
hist(newforestfires$area)
hist(FFMC_2)
hist(forestfiresA$FFMC)
hist(newforestfires$FFMC)
hist(newforestfires$X)
boxplot(newforestfires$FFMC)
hist(FFMC_2)
boxplot(FFMC_2)
#X DISTRIBUTION WAS MUCH BETTER WITHOUT LOGARITHMIC TRANSFORMATION SO WE WONT BE TAKING LOG OF X FOR TRAINING AND TESTING.

#MUTATION OF DATA:
boxplot.stats(area_2)
boxplot.stats(newforestfires$area)
boxplot.stats(FFMC_2)
newforestfires1<-mutate(newforestfires,area_log=area_2,FFMC_log=FFMC_2)

#new work
forestfiresA=read.csv('C:\\Users\\YAJUSH\\Downloads\\forestfiresA.csv',stringsAsFactors = TRUE)
newforestfires<-as.data.frame(forestfiresA%>%select(ISI,wind,temp,rain,area,Seasons,X,FFMC,DMC,DC)%>%filter(area>0,area<800,rain<1,temp>=5,DMC<230,ISI>0,ISI<50,FFMC>=80,DC>0,X>0,wind<9,Seasons>1))
area_2<-log(newforestfires$area*100,150)
FFMC_2<-log(newforestfires$wind)
newforestfires1<-mutate(newforestfires,area_log=area_2,FFMC_log=FFMC_2)
set.seed(100)
intrain <- createDataPartition(y =newforestfires1$area_log, p= 0.7, list = FALSE)
training <- newforestfires1[intrain,]
testing <- newforestfires1[-intrain,]
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
training<-training[,c("ISI","wind","temp","Seasons","FFMC_log","area_log","DMC","DC","X")]
testing<-testing[,c("ISI","wind","temp","Seasons","FFMC_log","area_log","DMC","DC","X")]
#svm  model
svm_fit<-svm(area_log ~.,data=training, gamma = '1.316872e-06', cost = 100, kernel = "linear")
plot(svm_fit)
svm_prediction <- predict(svm_fit,newdata=testing) 
svm_prediction
plot(testing$area_log,svm_prediction,xlim=c(0,3),ylim=c(0,3),xlab="actual values",ylab="predicted value",main="SVM MODEL")
abline(0,1, col=2)

#knn model
library(e1071)
knn_fit <- train(area_log~.,data = training, method = "knn",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
knn_fit
#plot accuracy vs K Value graph 
plot(knn_fit)
# estimate skill of knn on the validation dataset
predictions <- predict(knn_fit, testing)
plot(testing$area_log,svm_prediction,xlim=c(0,3),ylim=c(0,3),xlab="actual values",ylab="predicted value",main="KNN MODEL")
abline(0,1, col=2)
#model evaluation and comparison
library(caret)
library(mlbench)
control<-trainControl(method = "repeatedcv",number = 10,repeats = 3)
set.seed(7)
modelSvm<-train(area_log~.,data=training,method = "svmRadial",trControl = control)
set.seed(7)
modelknn<-train(area_log~.,data=training,method = "knn",trControl= control)
results<-resamples(list(SVM=modelSvm,knn=modelknn))
summary(results)
bwplot(results)
dotplot(results)
