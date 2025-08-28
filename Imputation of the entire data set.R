#Contains 
#Code used to impute the entire dataset
#Workflow & recipe used 

# 2 Importing data and data transformation
getwd()
setwd("/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing")
read_csv("starting_data_transposed_GSM_ID.csv")-> Entire_starting_dataset
trafo= function(x,adult.age=20) { x=(x+1)/(1+adult.age); y=ifelse(x<=1, log( x),x-1);y }
Entire_starting_dataset%>% mutate(fage = trafo(age))->Entire_mutated_dataset


# 3 Define model & recipe & workflow

ED_My_recipe <- recipe(fage ~ ., data = Entire_mutated_dataset) %>% update_role(age, new_role = "ID")
ED_ratio_recipe <- ED_My_recipe%>%step_impute_knn(all_predictors(),neighbors = 5)

prep(ED_ratio_recipe)->ED_prep_recipe 
bake(ED_prep_recipe, new_data = NULL) ->Entire_imputed_data 
Entire_imputed_data%>%select(-"sample_id")->Entire_imputed_data
Entire_imputed_data %>%write.csv("Entire_dataset_1.csv")



