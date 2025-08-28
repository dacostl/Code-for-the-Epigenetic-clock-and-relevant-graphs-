# Contains 
# QC metric graphs (Median error & correlation coefficients) 
# Iteration wise comparison plots 
# Dataframe shortening and aggregation 
# Table containing the number of CpGs used in each iteration 
########################################################

library(tidyverse)

read_qc_data <- function(Iteration) {

  print(paste("Iteration",Iteration))
  paste0("tables/Median_error_cor_test_table_",Iteration,".csv") -> file_name
  
  read_delim(file_name) |>
    select(-1) |>
    pivot_wider(
      names_from=`Evaluation metric`,
      values_from=c(Training_data,Testing_data)
    ) |>
    add_column(Iteration=Iteration) |>
    select(Iteration,everything()) -> this_data
  
  
  paste0("tables/Entire_dataset_",Iteration,".csv") -> file_name

  count_fields(file_name, tokenizer_csv(), n_max = 1) -> rows_in_data

  this_data |>
    add_column(Rows_in_Data = rows_in_data-1) -> this_data
  
  return(this_data)
}
#Aggregating and specifying limits o dataset you want to observe 

lapply(1:100,read_qc_data) -> qc_data
lapply(1:1500,read_qc_data) -> qc_data_1500
lapply(1:200,read_qc_data) -> qc_data_200
lapply(1:15000,read_qc_data) -> qc_data_15000

do.call(bind_rows,qc_data) -> qc_data
do.call(bind_rows,qc_data_1500) -> qc_data_1500
do.call(bind_rows,qc_data_200) -> qc_data_200

qc_data |>
  write_tsv("aggregated_qc_data.tsv")

read_delim("aggregated_qc_data.tsv") -> qc_data
setwd
#Plots
#Correlation coefficient testing  
qc_data |>
  ggplot(aes(x=Iteration, y=`Testing_data_Correlation coefficient`)) +
  geom_line(colour="#009E73")


   
qc_data |>
  ggplot(aes(x=Iteration, y=Rows_in_Data)) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim=c(0,30000))




###############################################################################################################################
#Testing and Training data plots (Full view and Focused view according to where data plateau)- dependant on q-data aggregation 
###############################################################################################################################

# Iteration-wise comparison of Training and Testing Correlation - max100 iterations(focused view) given that plateu occurs at this number of iterations 
ggplot(data = qc_data, aes(x = Iteration)) +
  geom_line(aes(y = `Testing_data_Correlation coefficient`, colour = "Testing data")) +
  geom_line(aes(y = `Training_data_Correlation coefficient`, colour = "Training data")) +
  scale_colour_manual( "",
                       breaks = c("Testing data", "Training data"),
                       values = c("Testing data" = "#CC79A7", "Training data" = "#009E73")) +
  xlab("Iteration number") +
  ylab("Correlation coefficient") + ggtitle("Iteration-wise comparison of orrelation coefficients across Training and Testing data")
#theme minimal()-makes background white  


# Iteration-wise comparison of Training and Testing Median error- max 1500 iterations(full view)

####plateau occurs at about 200
ggplot(data = qc_data_1500, aes(x = Iteration)) +
  geom_line(aes(y =`Testing_data_Median error`, colour = "Testing data")) +
  geom_line(aes(y = `Training_data_Median error`, colour = "Training data")) +
  scale_colour_manual( "",
                       breaks = c("Testing data", "Training data"),
                       values = c("Testing data" = "#CC79A7", "Training data" = "#009E73")) +
  xlab("Iteration number") +
  ylab("Median error") + ggtitle("Iteration-wise comparison of Median error across Training and Testing data")

#Iteration-wise comparison of Training and Testing Median error-max 200 iterations(focused view)
ggplot(data = qc_data_200, aes(x = Iteration)) +
  geom_line(aes(y =`Testing_data_Median error`, colour = "Testing data")) +
  geom_line(aes(y = `Training_data_Median error`, colour = "Training data")) +
  scale_colour_manual( "",
                       breaks = c("Testing data", "Training data"),
                       values = c("Testing data" = "#CC79A7", "Training data" = "#009E73")) +
  xlab("Iteration number") +
  ylab("Median error") + ggtitle("Iteration-wise comparison of Median error across Training and Testing data")

###################################################################################################################
# Testing Data only graphs
####################################################################################################################


# Correlation Coefficient of Testing Data by Iteration" max100 iterations(focused view) given that plateu occurs at this number of iterations 
ggplot(data = qc_data, aes(x = Iteration)) +
  geom_line(aes(y = `Testing_data_Correlation coefficient`, colour = "Testing data")) +
  scale_colour_manual( "",
                       breaks = c("Testing data"),
                       values = c("Testing data" = "#CC79A7")) +
  xlab("Iteration number") +
  ylab("Correlation coefficient") + ggtitle("Correlation Coefficient of Testing Data by Iteration") 


#Median error of Testing Data by Iteration" -max 200 iterations(focused view)
ggplot(data = qc_data_200, aes(x = Iteration)) +
  geom_line(aes(y =`Testing_data_Median error`, colour = "Testing data")) +
  scale_colour_manual( "",
                       breaks = c("Testing data"),
                       values = c("Testing data" = "#CC79A7")) +
  xlab("Iteration number") +
  ylab("Median error") + ggtitle("Median error of Testing Data by Iteration")


points

qc_data |>
  ggplot(aes(x=Iteration, y=Rows_in_Data)) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim=c(0,30000))
  


##################################################################
# Tabulate number of cpgs used in each iteration 
##################################################################

column_counts_entire_dataset<- sapply(files, function(file_entire_dataset) {
  data <- read_csv(file_entire_dataset)  
  ncol(data) 
})
resulting_table <- data.frame(column_counts_entire_dataset)
resulting_table2 <- tibble(
  file_name = basename(names(column_counts)),
  column_count = as.integer(column_counts),
  cycle = str_extract(basename(names(column_counts)), "(?<=Entire_dataset_)\\d+")
) %>%
  mutate(cycle = as.integer(cycle)) %>%
  arrange(cycle)

resulting_table2 %>%ggplot(aes(x=cycle,y=column_count))

# View sorted file names
print(file_df)

column_counts <- sapply(files, function(file) {
  data <- read_csv(file)  
  ncol(data) 
})




