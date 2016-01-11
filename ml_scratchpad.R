library(dplyr)
library(stringr)
library(ggplot2)
library(caret)
library(missForest)


setwd("G:/PNP/Performance Measurement/master_data")
datafilename <- list.files()[str_detect(list.files(), "master_data_20")]

md <- read.csv(datafilename, stringsAsFactors = FALSE) 
md2 <- md[ , c(2, 3, 4, 5, 6, 29, 40, 30, 46, 47, 50)]
md3 <- na.omit(md2)

md3$Status <- as.factor(md3$Status)

set.seed(5)
inTrain <- createDataPartition(y = md3$Status, p = .7, list = FALSE)
training <- md3[inTrain, ]
testing <- md3[-inTrain, ]

# special Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
# factor Appr.Desc has new levels Mandated/Demo, Technical Assistance
bad <- which(testing$Appr.Desc == "Mandated/Demo")
bad2 <- which(testing$Appr.Desc == "Technical Assistance")
bad3 <- c(bad, bad2)
testing <- testing[ -bad3, ]

mod_fit1 <- train(Status ~ ., data = training, method = "rf", trControl = trainControl(method = "cv"), number = 3, ntree = 10, na.action = na.omit)
mod_fit1
mod_fit1$finalModel

pred <- predict(mod_fit1, testing)
testing$pred_right <- pred == testing$Status

confusionMatrix(pred, testing$Status)

# subset data
md3 <- md[ , c(2, 3, 4, 5, 6, 29, 40, 30, 46, 47, 50)]

# find number of NA's per column
na_col <- sapply(1:ncol(md3), function(x) sum(is.na(md3[ , x])))
na_df <- rbind(names(md3), na_col)

# only columns 3, 4, 6, 8, 9, 10 contain NA's, so ditch others before using missForest
md3_full <- md3[ , -c(3, 4, 6, 8, 9, 10)]
md3_na <- md3[ , c(3, 4, 6, 8, 9, 10)]

# try imputing NAs instead of removing
# convert character columns to factors
char_col <- sapply(md3, class) == "character"
# char_col <- sapply(1:ncol(md3), function(x) class(md3[ , x])) == "character"
md3[ , char_col] <- lapply(md3[ , char_col], as.factor)

# impute NAs with missForest
md3_na_imp <- missForest(md3_na, ntree = 10)
md3_na_imp_df <- md3_na_imp$ximp

# recombine the md3_full with md3_na_imp_df
md3_final <- cbind(md3_full, md3_na_imp_df)

# build rf model
set.seed(5)
inTrain3 <- createDataPartition(y = md3_final$Status, p = .7, list = FALSE)
training3 <- md3_final[inTrain3, ]
testing3 <- md3_final[-inTrain3, ]

mod_fit3 <- train(Status ~ ., data = training3, method = "rf", trControl = trainControl(method = "cv"), number = 3, ntree = 10)

# impute NAs
# error: Can not handle categorical predictors with more than 53 categories
# must first convert the columns with > 53 factor levels into dummies
str(md3_na)
summary(md3)
# Appr.Desc & Appl.State.Abbr
for(i in unique(md3$Appr.Desc)){
        dummy_name <- str_c("dummy_Appr.Desc_", i, sep = "")
       md3[ , paste(dummy_name)] <- ifelse(md3$Appr.Desc == i, 1, 0)
}


# once dummies created, can delete the factors with > 53 levels
md3 <- md3[ , !(colnames(md3) %in% c("Appr.Desc", "Appl.State.Abbr"))]
md3_imp <- missForest(md3)




# test of missForest imputation
library(missForest)
set.seed(81)
iris.mis <- prodNA(iris, noNA = 0.2)
summary(iris.mis)
iris.mis$Species <- as.character(iris.mis$Species)

iris.imp <- missForest(iris.mis)
iris.imp$OOBerror

iris_imp_df <- iris.imp$ximp

