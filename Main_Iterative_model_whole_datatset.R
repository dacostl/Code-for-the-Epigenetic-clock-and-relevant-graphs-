#1 Load required packages
library(tidyverse)    # read_csv, mutate, ggplot
library(tidymodels)   # recipe(), workflow()
library(rstatix)      # cor_test()
library(broom)        # tidy()


create_model <- function(iteration) {
  
  input_filename <- paste0(c("Entire_dataset_",iteration,".csv"), collapse = "")
  output_filename <- paste0(c("Entire_dataset_",(iteration+1),".csv"), collapse = "")
  qc_metrics <- paste0(c("Median_error_cor_test_table_",iteration,".csv"), collapse = "")
  kept_cpgs <- paste0(c("kept_cpgs_from_model_",iteration,".csv"), collapse = "")
  training_graph <- paste0(c("training_graph_",iteration,".svg"), collapse = "")
  testing_graph <- paste0(c("testing_graph_",iteration,".svg"), collapse = "")
  
  
  # 2 Importing data and data transformation
  read_csv(input_filename)-> Entire_dataset
  trafo= function(x,adult.age=20) { x=(x+1)/(1+adult.age); y=ifelse(x<=1, log( x),x-1);y }
Entire_dataset%>% mutate(fage = trafo(age))->Mutated_Data_subset
  
  # 3 Define model & recipe & workflow
  linear_reg(penalty = 0.0226, mixture = 0.5) %>% set_engine("glmnet") -> linear_regression_model
  My_recipe <- recipe(fage ~ ., data = Mutated_Data_subset) %>% update_role(age, new_role = "ID")
  my_workflow <- workflow() %>% add_recipe( My_recipe) %>% add_model(linear_regression_model)
  
  
  
  # 4 Splitting and fitting data
  set.seed(128)
  Mutated_Data_subset %>% initial_split(prop = 0.9) ->Mutated_model_data_subset
  my_workflow %>% fit(training(Mutated_model_data_subset)) -> fitted_workflow_train
  
  
  #  5. Generate predictions
  prediction_of_training_data_subset <- fitted_workflow_train %>%predict(new_data = training(Mutated_model_data_subset)) %>%bind_cols(training(Mutated_model_data_subset)) %>% select(.pred, fage) 
  
  prediction_of_testing_data_subset <- fitted_workflow_train %>% predict(new_data = testing(Mutated_model_data_subset)) %>%bind_cols(testing(Mutated_model_data_subset)) %>% select(.pred, fage) 
  
  #: 6 Conversion of transformed (fage and .pred) back into the original scale(AGe, .pred_class)
  
  anti.trafo <- function(x,adult.age=20) { ifelse(x<0, (1+adult.age)*exp(x)-1, (1+adult.age)*x+adult.age) }
  prediction_of_testing_data_subset%>% mutate(Age = anti.trafo(fage), .pred_class = anti.trafo(.pred))->Converted_prediction_vs_actual_testing
  prediction_of_training_data_subset%>% mutate(Age = anti.trafo(fage), .pred_class = anti.trafo(.pred))->Converted_prediction_vs_actual_training
  
  
  #7  Median error & correlation coefficient of testing data
  
  Converted_prediction_vs_actual_testing%>% cor_test(.pred_class,Age)->correlation_test
  
  Converted_prediction_vs_actual_testing$Absolute_difference <- abs(Converted_prediction_vs_actual_testing$.pred_class - Converted_prediction_vs_actual_testing$Age)
  median(Converted_prediction_vs_actual_testing$Absolute_difference,na.rm=TRUE)->Median_error_testing
  
  #8 Median error & correlation coefficient of training data
  
  Converted_prediction_vs_actual_training%>% cor_test(.pred_class,Age)->correlation_train
  
  Converted_prediction_vs_actual_training$Absolute_difference <- abs(Converted_prediction_vs_actual_training$.pred_class - Converted_prediction_vs_actual_training$Age)
  median(Converted_prediction_vs_actual_training$Absolute_difference,na.rm=TRUE)->Median_error_training
  
  #7b-8b Tabulate  correlation coefficients and the median error (qc_metrics)
  
  Median_error_cor_test_table<- tibble("Evaluation metric"  = c("Correlation coefficient", "Median error"), Training_data = c(correlation_train$cor, Median_error_training), Testing_data = c(correlation_test$cor, Median_error_testing) )
  Median_error_cor_test_table %>%write.csv(qc_metrics)
  
  
  #9 Plots 
  
  ggplot(Converted_prediction_vs_actual_training, aes(x = .pred_class, y = Age)) +
    geom_point(colour = "#CC79A7") +
    geom_abline() +
    xlab("Predicted Age") + ylab("Actual Age") +
    ggtitle(paste0(
      "Actual Age vs Predicted Age (Training Data)\n",
      "Correlation: ", round(correlation_train$cor, 2),
      " / Median Error: ", round(Median_error_training, 2)
    ))
  ggsave(filename = training_graph,device = "svg")
  
  
  Converted_prediction_vs_actual_testing %>%
    ggplot(aes(x = .pred_class, y = Age)) +
    geom_point(colour = "#009E73") +
    geom_abline() +
    xlab("Predicted Age") +
    ylab("Actual Age") +
    ggtitle(paste0(
      "Actual Age vs Predicted Age (Testing Data)\n",
      "Correlation: ", round(correlation_test$cor, 2),
      " / Median Error: ", round(Median_error_testing, 2)
    ))
  ggsave(filename = testing_graph,device = "svg")
  
  #10 Retaining CpGs with a coefficient of 0
  
  broom::tidy(fitted_workflow_train)->Coefficients
  Coefficients %>% filter(estimate==0)->CpGs_with_zero_coefficients
  Coefficients %>% filter(estimate!=0)->relevant_CpGs_coefficients
  
  #11 Saving outputs to Model_1 file 
  
  #CpGs_with_zero_coefficients%>%write.csv("CpGs_with_zero_coefficients.csv")
  relevant_CpGs_coefficients%>%write.csv(kept_cpgs)
  
  # Filtered original data CpGs with 0 coefficients (but containing the methylation data, rather than the coefficients)
  
  Entire_dataset%>%select(!any_of(relevant_CpGs_coefficients$term))->New_starting_data
  New_starting_data%>%write.csv(output_filename)
  
}

iteration_value <- 1

while(TRUE) {
  create_model(iteration_value)
  print(paste("Running iteration",iteration_value))
  iteration_value <<- iteration_value+1
}

