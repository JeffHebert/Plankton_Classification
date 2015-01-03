## Starter Code for Analyzing Plankton Data

## Created by Jeff Hebert 12/30/14
## Kaggle Competition: http://www.kaggle.com/c/datasciencebowl

## This is basic starter code to create a model to classify plankton
## based on image dimensions and image density.
## This code should produce a results file with public score of 3.397518.
## Surely you can come up with a better model!


## Instructions
## 1. Download and unzip the competition data
## 2. Set the data_dir variable to the path to the data directory
## 3. Run the code. It will take 30-50 minutes, limited by IO of the thousands of files
## 4. Customize the extract_stats function to add your custom variables
## 4a. Remember to add new variables to train_data and test_data


## Hints
## Use cross validation to get a better model.
## Please let me know if you find this starter code helpful.
## Remember to use Rprof() and microbenchmark() when your code slows down.
## Remember to use some form of version control and rename your submission files.


## ==============================
## Main variables
## ==============================


## Set the path to your data here
data_dir <- "./data"

train_data_dir <- paste(data_dir,"/train", sep="")
test_data_dir <- paste(data_dir,"/test", sep="")



## ==============================
## Load packages
## ==============================

## These packages are available from CRAN

library(jpeg)
library(randomForest)


## ==============================
## Define Functions
## ==============================

## Handy function to display a greyscale image of the plankton
im <- function(image) image(image, col = grey.colors(32))


## Function to extract some simple statistics about the image
## UPDATE THIS to do any calculations that you think are useful
extract_stats <- function(working_image = working_image){
    #Invert the image to calculate density
    image <- working_image < mean(working_image)
    im_length <- nrow(image)
    im_width <- ncol(image)
    im_density <- mean(image)
    im_ratio <- im_length / im_width 
    return(c(length=im_length,width=im_width,density=im_density,ratio=im_ratio))    
}

## Function to calculate multi-class loss on train data
mcloss <- function (y_actual, y_pred) {
    dat <- rep(0, length(y_actual))
    for(i in 1:length(y_actual)){
        dat_x <- y_pred[i,y_actual[i]]
        dat[i] <- min(1-1e-15,max(1e-15,dat_x))
    }
    return(-sum(log(dat))/length(y_actual))
}

## ==============================
## Read training data
## ==============================


## Create empty data structure to store summary statistics from each image file
train_data <- data.frame(class = character(), filename = character(),lenght=numeric(),width=numeric(),density=numeric(),ratio=numeric(), stringsAsFactors = FALSE)


## Get list of classes
classes <- list.dirs(train_data_dir, full.names = FALSE)
classes <- setdiff(classes,"")

## Read all the image files and calculate training statistics

for(classID in classes){
    # Get list of all the examples of this classID
    train_file_list <- list.files(paste(train_data_dir,"/",classID,sep=""))
    train_cnt <- length(train_file_list)
    working_data <- data.frame(class = rep("a",train_cnt), filename = "a",lenght=0,width=0,density=0,ratio=0, stringsAsFactors = FALSE)
    idx <- 1
    #Read and process each image
    for(fileID in train_file_list){
        working_file <- paste(train_data_dir,"/",classID,"/",fileID,sep="")
        working_image <- readJPEG(working_file)
        
        # Calculate model statistics
        
        ## YOUR CODE HERE ##
        working_stats <- extract_stats(working_image)
        working_summary <- array(c(classID,fileID,working_stats))
        working_data[idx,] <- working_summary
        idx <- idx + 1
    }
    train_data <- rbind(train_data,working_data)
    cat("Finished processing",classID, '\n')
}




## ==============================
## Create Model
## ==============================

## We need to convert class to a factor for randomForest
## so we might as well get subsets of x and y data for easy model building
y_dat <- as.factor(train_data$class)
x_dat <- train_data[,3:6]

plankton_model <- randomForest(y = y_dat, x = x_dat)



# Compare importance of the variables
importance(plankton_model)


## Check overall accuracy... 24%, not very good but not bad for a simplistic model
table(plankton_model$predicted==y_dat)
#  FALSE  TRUE 
#  22959  7377

## Make predictions and calculate log loss
y_predictions <- predict(plankton_model, type="prob")

ymin <- 1/1000
y_predictions[y_predictions<ymin] <- ymin

mcloss(y_actual = y_dat, y_pred = y_predictions)
# 3.362268


## ==============================
## Read test data and make predictions
## ==============================



## Read all the image files and calculate training statistics
## This should take about 10 minutes, with speed limited by IO of the thousands of files

    # Get list of all the examples of this classID
    test_file_list <- list.files(paste(test_data_dir,sep=""))
    test_cnt <- length(test_file_list)
    test_data <- data.frame(image = rep("a",test_cnt), lenght=0,width=0,density=0,ratio=0, stringsAsFactors = FALSE)
    idx <- 1
    #Read and process each image
    for(fileID in test_file_list){
        working_file <- paste(test_data_dir,"/",fileID,sep="")
        working_image <- readJPEG(working_file)
        
        # Calculate model statistics
        
        ## YOUR CODE HERE ##
        working_stats <- extract_stats(working_image)
        working_summary <- array(c(fileID,working_stats))
        test_data[idx,] <- working_summary
        idx <- idx + 1
        if(idx %% 10000 == 0) cat('Finished processing', idx, 'of', test_cnt, 'test images', '\n')
    }


## Make predictions with class probabilities
test_pred <- predict(plankton_model, test_data, type="prob")
test_pred[test_pred<ymin] <- ymin

## ==============================
## Save Submission File
## ==============================

## Combine image filename and class predictions, then save as csv
submission <- cbind(image = test_data$image, test_pred)
submission_filename <- paste(data_dir,"/submission03.csv",sep="")
write.csv(submission, submission_filename, row.names = FALSE)


