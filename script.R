library(readr)
dataset <- read.table("D:/Desktop/datamining/banco.csv",header = TRUE, sep=";")
train <- read.table("D:/Desktop/datamining/train.csv",header = TRUE, sep=";")
test <- read.table("D:/Desktop/datamining/test.csv",header = TRUE, sep=";")

## medidas e graficos
lines(density(dataset$G3))
boxplot(dataset$priceidx, main = "Indice de pre�os do consumidor", col="green")
boxplot(dataset$confiidx, main = "Indice de confian�a do consumidor", col="green")
boxplot(dataset$euribor3m, main = "Taxa de juros de empr�stimos interbanc�rios na zona do Euro", col="green")
hist(main="Taxa de juros na Zona do Euro", xlab="%", ylab="Frequ�ncia", col="green",  dataset$euribor3m)
hist(main="Indice de pre�o ao consumidor", xlab="Indice", ylab="Frequ�ncia", col="green",  dataset$priceidx)
hist(main="Indice de confian�a do consumidor", xlab="Indice", ylab="Frequ�ncia", col="green",  dataset$cons.conf.idx)
boxplot(main="Dias do ultimo contato", xlab="Dias", ylab="Frequ�ncia", col="green",  dataset$pdays)
plot(dataset$y)
hist(main="Idade dos consumidores", xlab="Anos", ylab="Frequ�ncia", col="green",  dataset$age)



library(e1071)
## separa��o dos dados em treinamento e teste
set.seed(123)
smp_size <- floor(0.75 * nrow(dataset))
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)
train <- dataset[train_ind, ]
test <- dataset[-train_ind,]
xTrain <- subset(train, select=-y)
yTrain <- train$y
xTest <- subset(test, sleect=-y)
yTest <- test$y
## treinamento da svm
svm_model <- svm(y ~ ., data=train,kernel="polynomial",cost = 1, gamma = 0.1, degree = 2, coef0 = 1)
summary(svm_model)

##previsao svm
pred <- predict(svm_model,xTest)
##matriz de confus�o svm
table(pred,yTest)
##tempo de execu��o svm
system.time(pred <- predict(svm_model,xTest))

write.csv(train, file="train.csv")
write.csv(test, file="test.csv")


## arvore de decis�o
library(rpart)
fit <- rpart(y ~ ., data=train, method="class")
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
Prediction
##calculo da precis�o da arvore
c <- 0
for(i in 1:length(Prediction)){
  if(as.vector(Prediction)[i] == as.vector(test[['y']])[i]){
    c <- c + 1
  }
}
c/1030
## tempo de execuc�o
system.time(Prediction <- predict(fit, test, type = "class"))
## matriz de confus�o da arvore
table(Prediction, yTest)