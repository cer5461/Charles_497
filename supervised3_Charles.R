#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())
# load required libraries
install.packages("kernlab") 
install.packages("e1071")
library(e1071)
library(kernlab)
library(dplyr)

library(caret)

# set working directory
setwd("C:/Users/kevin/Documents/GitHub/TAD_2021/R lessons/")


#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds")
table(news_data$category)

# let's work with 2 categories
# pipe is pre processing and dividing the data all in one go 
# here we filter the day such that we are only looking at data 
# that has observations in the the categories weird news and good news 
# then we select, which allows us to look at only specific columns: headline and category 
news_samp <- news_data %>% filter(category %in% c("WEIRD NEWS", "GOOD NEWS")) %>% select(headline, category) %>% setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "WEIRD NEWS"])
head(news_samp$text[news_samp$class == "GOOD NEWS"])

# some pre-processing (the rest will let dfm do)
# gsub is a way of doing regular expresssions 
### we define pattern that we are looking for and we replace it with nothing
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- dplyr::recode(news_samp$class,  "WEIRD NEWS" = "weird", "GOOD NEWS" = "good")

# what's the distribution of classes?
# the accuracy of any class is highly related to the distribution of the classes
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
# set.seed is a way to replicate the randomization step
# it allows us to recreate the same exact randomization sometime in the future
set.seed(1984)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL


#----------------------------------------
# 4. Support Vector Machine (SVM) using Caret ---
#----------------------------------------
library(caret)
library(quanteda)

# create document feature matrix
# the dfm in quanteda is a special object that does not trivially interact with other packages 
# by converting to matrix, we are stripping some of the special quanteda dfm things 
# so that we can use dfm in the caret workflow 
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% convert("matrix")

# A. the caret package has it's own partitioning function
set.seed(1984)
#data partition is a vector of ids that we define as the training set 
#we are putting p = .8 (80%) of our data in the training set 
ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.2, list = FALSE, times = 1)
#take matrix of term frequencies (new_dfm) and transform it into a data frame
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
#then create a vector of labels and transform it from a character vector into a factor vector
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
#do the same thing for test set, but with (-) which means "not"
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "none")
#trctrl <- trainControl(method = "LOOCV", p = 0.8)
?trainControl
# C. train model (caret gives us access to even more options)
# see: https://topepo.github.io/caret/available-models.html

# svm - linear
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)

# svm - radial
svm_mod_radial <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trctrl)

svm_radial_pred <- predict(svm_mod_radial, newdata = test_x)
svm_radial_cmat <- confusionMatrix(svm_radial_pred, test_y)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]], "\n",
  "SVM-Radial Accuracy:",  svm_radial_cmat$overall[["Accuracy"]]
)


####Be sure to save it as a new file, with a new filename!

# 1. Re-run the analysis with a smaller training set and larger test set. Does the accuracy go up or down?
#svm_linear
#svm_linear 
## accuracy with p=.8 is 77.4
## accuracy with p=.2 is 75.09

#svm_radial 
## accuracy with p=.8 is 66.87
## accuracy with p=.2 is 65.87

