library(tidyverse)
library(ForecastFramework)
#theme_set(theme_bw())
library(reticulate)
use_python("/Users/gcgibson/anaconda/bin/python2.7")


source_python("main.py")

data <- "./training_data.txt"

#ext_data <- Nick puts in test data
#ext_data_outcome <- Nick puts in true test data

#test.data <- train_dl(data,adjacency_matrix,train_size,valid_size,model_name,save_name)


#forecasts <- test(data,adjacency_matrix,train_size,valid_size,model_name,save_name,NULL,NULL)
