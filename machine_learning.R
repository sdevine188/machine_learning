## pml week 3
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

data <- iris
head(iris)
names(iris)
table(iris$Species)

## seperate into training and test
in_train <- createDataPartition(data$Species, p = .7, list = FALSE)
training <- data[in_train, ]
testing <- data[-in_train, ]
str(training)
str(testing)

ggplot(data = training, aes(x = Petal.Width, y = Sepal.Width, color = Species)) + geom_point()

mod_fit1 <- train(Species ~ ., method = "rpart", data = training)
mod_fit1
mod_fit1$finalModel

plot(mod_fit1$finalModel, uniform = TRUE, main = "Classification Tree")
text(mod_fit1$finalModel, use.n = TRUE, all = TRUE, cex = .8)

install.packages("rattle")
library(rattle)

fancyRpartPlot(mod_fit1$finalModel)

predict(mod_fit1, newdata = testing)

## bagging lecture - boostrap aggregating
## bagging estimates will have lower variable but similiar bias than individual estimates

## ordering ozone (outcome variable) just to show clearly how bagging works
library(ElemStatLearn)
data(ozone, package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone), ]
head(ozone)

## create empty ll matrix to fill in for loop
ll <- matrix(NA, nrow = 10, ncol = 155)

## bootstrap resample with replacement of ozone rows, in ascending order,
## then fit a loess model of temperature on each ozone sample dataset
## then predict that model on newdata which is just a column 1:155 representing range of ozone values
for(i in 1:10){
        ss <- sample(1:dim(ozone)[1], replace = T)
        ozone0 <- ozone[ss, ]
        ozone0 <- ozone0[order(ozone$ozone), ]
        loess0 <- loess(temperature ~ ozone, data = ozone0, span = .2)
        ll[i, ] <- predict(loess0, newdata = data.frame(ozone = 1:155))
}

## can do bagging automatically in "train" function of caret package
## specify method = bagEarth, or treebag, or bagFDA

## now plot ozone and temperature
## then add in grey loess line for each individual model fit, independent 1:155 ozone value on x axis, and predicted temp on y axis
## then add in red loess line that is the mean of the predicted temperature for each 1:155 ozone value
plot(ozone$ozone, ozone$temperature, pch = 19, cex = .5)
for(i in 1:10){
        lines(1:155, ll[i, ], col = "grey", lwd = 2)
        lines(1:155, apply(ll, 2, mean), col = "red", lwd = 2)
}


## random forests lecture
## random forests and boosting are two most accurate algorithms.  downside of rf is that it can be tough to interpret
## random forests is extension of bagging - you also bootstrap resample your training data, predict the model, then avg the models
## but the difference is that on each resampling of the training data, you sample only a random subset of the variables
## this produces many random regression trees, and the final model is an avg of these many tree model predictions

# random forest in layman's terms http://www.quora.com/Random-Forests/How-do-random-forests-work-in-laymans-terms

data(iris)
library(ggplot2)
library(caret)
inTrain <- createDataPartition(y = iris$Species, p = .7, list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

# mod_fit <- train(Species ~ ., data = training, method = "rf", prox = TRUE)
# another option for rf model that uses k-fold cross-validation instead of default bootstrap resampling
# also limits k to 3, instead of default 10
# also limits ntree from default of 500 to 10
# for info on ntree and mtry
# see http://stackoverflow.com/questions/13956435/setting-values-for-ntree-and-mtry-for-random-forest-regression-model
# this can speed up rf model processing time (I think)
# for more, see http://topepo.github.io/caret/training.html#control
mod_fit <- train(Species ~ ., data = training, method = "rf", trControl = trainControl(method = "cv"), number = 3, ntree = 10)
mod_fit

## in mod_fit output, the "mtry" column of the tuning parameters is the index number of the random tree it built (i think)
## can look at specific tree, for instance the second tree (mtry = 2)
# how to read getTree output
# http://stackoverflow.com/questions/14996619/random-forest-output-interpretation
getTree(mod_fit$finalModel, k = 2) 

# info on rf variable importance
# two methods: Out-of-Bag error rate, and decrease in node impurities when split on variable (intuitive) 
# for decrease in impurity method, RSS is used for regression rf, and Gini index decrease used for classification rf
# http://stats.stackexchange.com/questions/95839/gini-decrease-and-gini-impurity-of-children-nodes
?importance

## can plot the "class centers" of outcome variable on an x-y plot of two predictor variables
irisP <- classCenter(training[ , c(3, 4)], training$Species, mod_fit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, color = Species, data = training)
p + geom_point(aes(x = Petal.Width, y = Petal.Length, color = Species), size = 5, shape = 4, data = irisP)

## can predict on the testing set
pred <- predict(mod_fit, testing)
testing$pred_right <- pred == testing$Species
table(pred, testing$Species)

## can see which predictions you missed
qplot(Petal.Width, Petal.Length, color = pred_right, data = testing)
