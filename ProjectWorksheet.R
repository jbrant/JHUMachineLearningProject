library(RCurl)

training.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

## Read in training data
training.data <- read.csv(training.url)

## Read in testing data
testing.data <- read.csv(testing.url)

dim(training.data)