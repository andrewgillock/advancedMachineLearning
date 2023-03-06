library(keras)
library(tensorflow)
library(tidyverse)

og <- read.csv('preprocessed.csv')
View(og)
getwd()
setwd("/Users/apgillock/Dropbox/MIS 382N Advanced Machine Learning/Project")

train <- read.csv('train.csv')
train <- train[sample(1:nrow(train)), ]
train_y <- train %>% select(readmitted) %>% as.matrix()
train_x <- train %>% select(-readmitted) %>% scale() %>% normalize()
dim(train_x) <- c(51471, 51)

test <- read.csv('test.csv')
test_y <- test %>% select(readmitted)
test_x <- test %>% select(-readmitted) %>% scale() %>% normalize()

dim(train_x)



rm(my_model)
my_model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = 'relu', input_shape = c(51)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 8, activation = 'relu') %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')

my_model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)

my_model <- my_model %>% 
  fit(x = train_x, y = train_y,
      epochs = 100,
      batch_size = 64,
      validation_split = 0.25
  )


my_model %>% evaluate(test_x, test_y)

hat_y <- my_model %>% 
  predict_classes(test_y)





train <- read.csv('train.csv')
test <- read.csv('test.csv')
full <- data.frame(rbind(train, test))
head(full)


rm(my_model)
my_model <- keras_model_sequential() %>%
  layer_dropout(rate = 0.15, input_shape = c(51))
  layer_dense(units = 8, activation = 'relu') %>% 
  layer_dense(units = 16, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'sigmoid')
summary(my_model)
my_model %>% compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)
my_model %>% 
  fit(x = train_x, y = train_y,
      epochs = 30,
      batch_size = 25
  )

hmm <- read.csv('ogpreprocessed.csv')
hmm %>% group_by(readmitted) %>% count()
