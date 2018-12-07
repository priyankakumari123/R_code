#.....Project#6....#
#..(a)....#
library(ISLR)
str(OJ)
View(OJ)
oj.newdata = OJ[,-1]

#....(a)...# Create training and test sets -->
set.seed(2)
train =1:870
length(train)
OJ.test = OJ[-train, ]
OJ.train = OJ[train, ]

#....(b)...#
install.packages("e1071")
library(e1071)

#support vector classifier..
svmfit <- svm(OJ$Purchase~ ., data = oj.newdata, subset= train, kernel = "linear", cost = 0.1, scale = FALSE)
summary(svmfit)

# Fit SV classifiers and perform cross-validation (default: 10-fold CV) -->
set.seed(1)
tune.out <- tune(svm, Purchase~ ., data = OJ[train,], kernel = "linear", 
                 ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)
# best cost is 10
bestmod=tune.out$best.model
bestmod
names(tune.out)
pred <- predict(bestmod, newdata=OJ.test)
table(pred,OJ.test$Purchase)
#length(pred)
#..(c)..Using support vector machine...
svmfit2 <- svm(OJ$Purchase ~ ., kernel = "polynomial", data = oj.newdata,subset=train, degree = 2,cost=10)
summary(svmfit2)
tune.out2 <- tune(svm, Purchase~ ., data = OJ[train,], kernel = "polynomial", 
                 ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out2)

bestmod2=tune.out2$best.model
bestmod2
pred2 <- predict(bestmod2, newdata=OJ.test)
table(pred2,OJ.test$Purchase)
#..(d)..svm with radial kernel...
svmfit3 <- svm(OJ$Purchase ~ ., data = oj.newdata,subset=train, kernel = "radial", gamma = 0.5, cost = 0.1)
summary(svmfit3)
set.seed(1)
tune.out3 <- tune(svm, Purchase~ ., data = OJ[train,], kernel = "radial", 
                 ranges = list(cost = 10^seq(-2, 1, by = 0.25), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out3)
bestmod3=tune.out3$best.model
bestmod3
pred3 <- predict(bestmod3, newdata=OJ.test)
table(pred3,OJ.test$Purchase)
