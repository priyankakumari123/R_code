
#.....Project#5...Priyanka kumari...


planet = read.csv(file.choose(),header= T)
sum(is.na(planet))
str(planet)
View(planet)

#..... Part (a): Exploratory analysis .....#
# Pairwise plots
pairs(planet, main="Pairwise plot of planet Data",pch=19) ## seems positively correlated

# Correlation matrix
cor(planet)

# Histogram
par(mfrow = c(1, 3))
hist(planet$Mass)
hist(planet$Eccentricity)
hist(planet$Period)

# Boxplot
par(mfrow = c(1, 3))
boxplot(planet$Mass,xlab="Mass")
boxplot(planet$Eccentricity,xlab="Eccentricity")
boxplot(planet$Period,xlab="Period")

#...summary statistics...#
summary(planet)

#Centering & scaling predictors
x= planet
x.sc= scale(x)
#...(d)....#
# mean and sd on original scale-->
apply(planet, 2, mean)
apply(planet, 2, sd)

# Hierarchical clustering with complete linkage and  Euclidean distance -->

hc.complete <- hclust(dist(x.sc), method = "complete")
plot(hc.complete, main = "HC treewith Complete Linkage and Euclidean distance", xlab = "", sub = "", 
     cex = 0.7)

# Simulate data with p = 3 features and cluster -->
set.seed(3)
hc.clusters = cutree(hc.complete, 3)
dd <- as.dist(1 - cor(t(x.sc)))
plot(hc.complete, main = "H.Clustering with Scaled Features and correlation based distance", xlab = "", sub = "", 
     cex = 0.7, col=hc.clusters)
pairs(planet, main="Pairwise plot of three cluster in  planet Data",col=hc.clusters, pch=hc.clusters) 

# Cut the dendograms to determine cluster labels..#
cutree(hc.complete, 3)

#....(e)...k-mean...#
library(knitr)
# Simulate data on 3 features with three clusters -->

set.seed(2)
km.out = kmeans(x.sc,3,nstart=30)
km.clusters = km.out$cluster
table(km.clusters,hc.clusters)


#......question 2 ..........#

women = read.csv(file.choose(),header= T)
str(women)
View(women)
#..... Part (a): Exploratory analysis .....#
# Pairwise plots
pairs(women[,-1], main="Pairwise plot of Women Data",pch=19) ## seems positively correlated

# Correlation matrix
cor(women[,-1]) ##### error saying must be numeric

#...summary statistics...#
summary(women)
#Centering & scaling predictors
states <- row.names(women)
states
# Look at mean and sd -->
apply(w, 2, mean)
apply(w, 2, sd)
w= women[,-1]
rownames(w)=women$country
w.sc = scale(w, center = TRUE, scale = TRUE)
# Perform PCA  -->
pca <- prcomp(w, center = T, scale = T)
names(pca)
# Check center and scale -->
pca$center
pca$scale
# Get the loading matrix -->
pca$rotation                

 # Get the score matrix -->$
dim(pca$x)
head(pca$x) 
# 50*4 matrix has as its columns the principal component score vectors. That is, the kth column #is the kth principal component score vector.
sort(pca$x[,1])

# Check the calculation -->
x.std <- apply(w, 2, function(x){(x-mean(x))/sd(x)})
max(abs(pca$x - (x.std %*% pca$rotation)))

# Check the covariance matrix of the scores -->
round(cov(pca$x), 3)

# Display a biplot the results (shows both pc scores and loading vectors) -->
pca$rotation
#correlation between variables and PC1
pca$sdev[1] * pca$rotation[,1]
#correlation between variables and PC2
pca$sdev[2] * pca$rotation[,2]
biplot(pca, scale=0) 
### The scale =0 argument to ensures that the arrows are scaled to scale=0 , represent the #loadings;

# Display the biplot after changing the signs of loadings and scores -->
# (the way in Chapter 10 of the book) -->
  
pca$rotation <- -pca$rotation
pca$x <- -pca$x
pca$rotation
biplot(pca, scale=0)

 # Compute the proportion of variance explained (PVE) -->
pve <- pc.var/sum(pc.var)
pve
cumsum(pve)
# Scree plot  -->
  
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')

# Plot of cumulative PVE -->
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')
#......#..(d)...
summary(pca)
#...........Question 3.........#

library(ISLR)
library(tree)
str(OJ)
View(OJ)
oj.newdata = OJ[,-1]

#....(a)...# Create training and test sets -->
set.seed(2)
train =1:870
length(train)
OJ.test = OJ[-train, ]

#...(b)...#...Fit a tree to training data -->
tree.OJ<- tree(OJ$Purchase~ . , oj.newdata, subset=train)
plot(tree.OJ)
tree.plot= text(tree.OJ,pretty=0)
summary(tree.OJ)
tree.OJ


# Get predictions on the test set -->
tree.pred <- predict(tree.OJ,OJ.test, type = "class")
summary(tree.pred)
1 - mean(OJ.test$Purchase == tree.pred)
table(OJ.test$Purchase, tree.pred)

#....(c)....#

# Perform cost complexity pruning by CV, guided by misclassification rate -->
set.seed(3)
cv.oj <- cv.tree(tree.OJ, FUN = prune.misclass)
cv.oj

# Computation of pruned tree -->
prune.OJ <- prune.misclass(tree.OJ, best = 4)
tree.pred2 <- predict(prune.OJ,OJ.test, type = "class")
table(OJ.test$Purchase,tree.pred2)
1 - mean(OJ.test$Purchase == tree.pred2)

# Get the pruned tree of the best size -->
prune.oj <- prune.misclass(tree.OJ, best = 7)

# Plot the pruned tree -->
plot(prune.oj)
text(prune.oj, pretty = 0)



# Plot the estimated test error rate -->
par(mfrow = c(1, 2))
plot(cv.oj$size, cv.oj$dev, type = "b")
plot(cv.oj$k, cv.oj$dev, type = "b")

# Get the best size 
cv.oj$size[which.min(cv.oj$dev)]


 # Get predictions on the test set -->
oj.pred <- predict(prune.oj,OJ.test, type = "class")
# Get the confusion matrix  -->
table(oj.pred, OJ.test$Purchase)
#test mse
1 - mean(OJ.test$Purchase == oj.pred)

#...summary of pruned  tree..#
summary(prune.oj)


#....(d)....#
library(randomForest)
# Perform bagging (i.e., random forest with m = p = 13) -->
set.seed(1)
bag.oj <- randomForest(OJ$Purchase~ . , oj.newdata, subset = train, 
                           mtry = 17, ntree = 1000, importance = TRUE)
bag.oj

# Estimate test error rate -->

yhat.bag <- predict(bag.oj, newdata = OJ.test)
#mean((yhat.bag - OJ.test$Purchase)^2) #regression
1 - mean(OJ.test$Purchase == yhat.bag) #classification

#.....(e)...#
# Grow a random forest  -->
# (Default m = p/3 for regression and sqrt(p) for classification) -->
set.seed(1)
rf.oj <- randomForest(OJ$Purchase~ ., data = oj.newdata, subset = train, 
                          mtry = 4, ntree = 1000, importance = TRUE)
rf.oj
# Estimate test error rate -->
yhat.rf = predict(rf.oj,OJ.test, subset = test, 
                  mtry = 4, ntree = 1000, importance = TRUE)
1-mean(yhat.rf ==OJ.test$Purchase)
# table(yhat.rf,OJ.test$Purchase)

# Get variable importance measure for each predictor -->
importance(rf.oj)

varImpPlot(rf.oj)

##....(e)...#

library(gbm)

# Fit a boosted regression tree -->
set.seed(1)
z = ifelse(OJ$Purchase[train]=="MM",0,1)
boost.oj <- gbm(z~ ., data = oj.newdata[train,], distribution = "bernoulli", 
                    n.trees = 1000, interaction.depth = 1)

 # Get the relative influence plot -->
summary(boost.oj)


# Get the partial dependence plot of selected variables on response -->
  
par(mfrow = c(1, 2))
plot(boost.oj, i = "LoyalCH")
plot(boost.oj, i = "PriceDiff")

 # Estimate test error rate for the boosted model -->
  
yhat.boost <- predict(boost.oj, newdata = OJ.test, 
                      n.trees = 1000, type="response")
z.hat <- ifelse(yhat.boost >=0.5, "CH", "MM")
1-mean(z.hat==OJ.test$Purchase)

# Perform boosting with a different value of lambda -->

boost.oj <- gbm(z ~ ., data = oj.newdata[train,], distribution = "bernoulli", 
                    n.trees = 1000, interaction.depth = 1, shrinkage = 0.01, verbose = F)
yhat.boost <- predict(boost.oj, newdata = OJ.test, 
                      n.trees = 1000,type="response")
z.hat <- ifelse(yhat.boost >=0.5, "CH", "MM")
1-mean(z.hat==OJ.test$Purchase)

#...(e)....KNN approach..#

library(class)

set.seed(1)
oj.newdata$Store7 = ifelse(oj.newdata$Store7=="No",0,1)
mod.train <- knn(oj.newdata[train,], oj.newdata[train,], OJ$Purchase[train], k = 16, prob=T)
mod.test =  knn(oj.newdata[train,], oj.newdata[-train,], OJ$Purchase[train], k = 16, prob=T)

#...test error rate
table(mod.test,OJ.test$Purchase)
1-mean(mod.test==OJ.test$Purchase)


ks <- c(seq(1, 50, by = 1), seq(50, 200, by = 5))

nks <- length(ks)
err.rate.train <- numeric(length = nks)
err.rate.test <- numeric(length = nks)
names(err.rate.train) <- names(err.rate.test) <- ks

for (i in seq(along = ks)) {
  set.seed(1)
  mod.train <- knn(oj.newdata[train,], oj.newdata[train,], OJ$Purchase[train], k = ks[i])
  set.seed(1)
  mod.test <- knn(oj.newdata[train,], oj.newdata[-train,], OJ$Purchase[train], k = ks[i])
  err.rate.train[i] <- 1 - sum(mod.train == OJ$Purchase[train])/length(OJ$Purchase[train])
  err.rate.test[i] <- 1 - sum(mod.test == OJ$Purchase[-train])/length(OJ$Purchase[-train])
}


plot(ks, err.rate.train, xlab = "Number of nearest neighbors", ylab = "Error rate", 
     type = "b", ylim = range(c(err.rate.train, err.rate.test)), col = "blue", pch = 20)
lines(ks, err.rate.test, type="b", col="purple", pch = 20)
legend("bottomright", lty = 1, col = c("blue", "purple"), legend = c("training", "test"))

result <- data.frame(ks, err.rate.train, err.rate.test)

result[err.rate.test == min(result$err.rate.test), ]

