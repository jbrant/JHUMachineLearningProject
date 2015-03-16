library(RCurl)
library(ggplot2)
library(caret)
library(randomForest)
library(pander)

##############################
# Loading and Summary Analysis
##############################

training.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

## Read in training data
training.data <- read.csv(training.url, na.strings = c("", "NA"))

## Read in testing data
testing.data <- read.csv(testing.url, na.strings = c("", "NA"))

## Observe the dimensionality of the training and test sets
dim.data.frame <- rbind.data.frame(dim(training.data), dim(testing.data))
rownames(dim.data.frame) <- c("Training Data", "Testing Data")
colnames(dim.data.frame) <- c("# of Observations", "# of Variables")

## Note the number of observations for the 5 different ways
## in which the exercise could be performed
ggplot(training.data, aes(classe)) + 
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Observations per Class", x = "Classe", y = "Observations") +
  theme(plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

##############
# Partitioning
##############

## Set seed for reproducibility
set.seed(54321)

## Partition the training set into a training and cross-validation set (85%/15%)
training.indexes <- createDataPartition(training.data$classe, p = 0.85, list = FALSE)
training.set <- training.data[training.indexes,]
cross.validation.set <- training.data[-training.indexes,]

##############################
# Cleaning and Pre-Processing
##############################

## Remove predictors that have near zero variance
near.zero.var <- nearZeroVar(training.set)
training.set <- training.set[, -near.zero.var]

## The following columns have no predictive relevance, so remove them
irrelevant.cols <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                     "cvtd_timestamp", "new_window", "num_window")
training.set <- training.set[, !names(training.set) %in% irrelevant.cols]

## Subset the training set to columns that have no NAs 
## (this actually yielded better results than imputation)
training.set <- training.set[,colSums(is.na(training.set)) == 0]

###############################
# Model Training and Prediction
###############################

## Train the model using random forests using 100 trees
model <- randomForest(classe ~ ., 
                      data = training.set, 
                      importance = TRUE, 
                      recursive = TRUE,                      
                      ntree = 100)

## Run training
training.results <- predict(model, training.set)
print(confusionMatrix(training.results, training.set$classe))

## Run cross-validation
cross.validation.results <- predict(model, cross.validation.set)
print(confusionMatrix(cross.validation.results, cross.validation.set$classe))

############
# Submission
############

## Run the model on the testing dataset
testing.results <- as.character(predict(model, testing.data))

## Define the function supplied in the assignment instructions
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

## Invoke the above function to generate the answer file
pml_write_files(testing.results)