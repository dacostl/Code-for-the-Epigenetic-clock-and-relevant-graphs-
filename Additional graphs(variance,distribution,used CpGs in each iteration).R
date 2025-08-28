# Contains 
#Graph+table showing the number of Cpgs used in each iteration, 
#Distribution of methylation levels for the CpGs used in each cycle
#Distribution of methylation levels for the CpGs in the entire dataset
#Distribution of variance for first set of selected CpGs vs those selected in the other iterations
#########################################################################################################


 Step 1: List all files starting with 'Entire_dataset_'
file_entire_dataset2 <- list.files(
  path = "/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing/tables",
  pattern = "^Entire_dataset_.*\\.csv$",
  full.names = TRUE
)

column_counts <- sapply(file_entire_dataset2, function(file) {
  data <- read_csv(file)
  ncol(data)
})

resulting_table2 <- tibble(
  file_path = file_entire_dataset2,
  file_name = basename(file_entire_dataset2),
  column_count = as.integer(column_counts),
  cycle = str_extract(basename(file_entire_dataset2), "(?<=Entire_dataset_)\\d+")
) %>%
  mutate(cycle = as.integer(cycle)) %>%
  arrange(cycle)

print(resulting_table2)

###########################################################################
#    Table with the number of kept CpGs selected in each iteration cycle
##########################################################################

library(tidyverse)

# List all files starting with 'kept_model_cpg_'
files <- list.files(
  path = "/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing/tables",
  pattern = "^kept_model_cpg_.*\\.csv$",
  full.names = TRUE
)

# Count columns in each file
column_counts <- sapply(files, function(file) {
  data <- read_csv(file)
  ncol(data)
})

# Create a data frame with file names and column counts

resulting_table <- tibble(
  file_name = basename(names(column_counts)),
  column_count = as.integer(column_counts),
  cycle = str_extract(basename(names(column_counts)), "(?<=kept_model_cpg_)\\d+")
) %>%
  mutate(cycle = as.integer(cycle)) %>%
  arrange(cycle)

                
as.numeric(resulting_table$cycle)->resulting_table$cycle
as.numeric(resulting_table$column_count)->resulting_table$column_count

resulting_table %>% ggplot(aes(x=resulting_table$cycle,y=resulting_table$column_count)+geom_point()+geom_col(fill = "pink") +labs(title = "Number of CpGs Used in Each Model", x = "Iteration",y = "Number of CpGs"))

# Ensure the columns are numeric
resulting_table$cycle <- as.numeric(resulting_table$cycle)
resulting_table$column_count <- as.numeric(resulting_table$column_count)

###########################################################################
#    Graph showing the number of kept cpgs selected in each iteration cycle
###########################################################################

resulting_table%>%slice(1:200, )->filtered_resulting_table
ggplot(filtered_resulting_table, aes(x = cycle , y =column_count)) +
  geom_line( col = "chocolate") +
  labs(
    title = "Number of CpGs Used in Each Iteration",
    x = "Iteration",
    y = "Number of CpGs"
  ) 
##############################################################################
#    Distribution of methylation levels for the CpGs used in each cycle
##############################################################################

read_csv("/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing/tables/kept_model_cpg_1.csv")->Distribution_graph_Iter1
Distribution_graph_Iter1%>%pivot_longer(everything(),names_to = "cpg",values_to = "methylation")%>% ggplot(aes(methylation))+
  geom_density(fill="indianred")+xlab("Methylation Levels") +
  ylab("Density") + ggtitle("Methylation Profile of CpGs Selected in Iteration 1 ") 


read_csv("/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing/tables/kept_model_cpg_10.csv")->Distribution_graph_Iter10
Distribution_graph_Iter10%>%pivot_longer(everything(),names_to = "cpg",values_to = "methylation")%>% ggplot(aes(methylation))+
  geom_density(fill="indianred")+xlab("Methylation Levels") +
  ylab("Density") + ggtitle("Methylation Profile of CpGs Selected in Iteration 10 ")



read_csv("/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing/tables/kept_model_cpg_20.csv")->Distribution_graph_Iter20
Distribution_graph_Iter20%>%pivot_longer(everything(),names_to = "cpg",values_to = "methylation")%>% ggplot(aes(methylation))+
  geom_density(fill="indianred")+xlab("Methylation Levels") +
  ylab("Density") + ggtitle("Methylation Profile of CpGs Selected in Iteration 20 ")

read_csv("/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing/tables/kept_model_cpg_40.csv")->Distribution_graph_Iter40
Distribution_graph_Iter40%>%pivot_longer(everything(),names_to = "cpg",values_to = "methylation")%>% ggplot(aes(methylation))+
  geom_density(fill="indianred")+xlab("Methylation Levels") +
  ylab("Density") + ggtitle("Methylation Profile of CpGs Selected in Iteration 40 ")


read_csv("/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing/tables/kept_model_cpg_60.csv")->Distribution_graph_Iter60
Distribution_graph_Iter60%>%pivot_longer(everything(),names_to = "cpg",values_to = "methylation")%>% ggplot(aes(methylation))+
  geom_density(fill="indianred")+xlab("Methylation Levels") +
  ylab("Density") + ggtitle("Methylation Profile of CpGs Selected in Iteration 60 ")



read_csv("/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing/tables/kept_model_cpg_80.csv")->Distribution_graph_Iter80
Distribution_graph_Iter80%>%pivot_longer(everything(),names_to = "cpg",values_to = "methylation")%>% ggplot(aes(methylation))+
  geom_density(fill="indianred")+xlab("Methylation Levels") +
  ylab("Density") + ggtitle("Methylation Profile of CpGs Selected in Iteration 80 ")

read_csv("/bi/group/bioinf/LauraCosta/Epigenetic_Clocks/Whole dataset processing/tables/kept_model_cpg_100.csv")->Distribution_graph_Iter100
Distribution_graph_Iter100%>%pivot_longer(everything(),names_to = "cpg",values_to = "methylation")%>% ggplot(aes(methylation))+
  geom_density(fill="indianred")+xlab("Methylation Levels") +
  ylab("Density") + ggtitle("Methylation Profile of CpGs Selected in Iteration 100 ")


##############################################################################
#    Distribution of methylation levels for the CpGs used in the entire dataset
##############################################################################



  Distribution_graph_entire_dataset %>%
    pivot_longer(starts_with("cg"), names_to = "cpg", values_to = "methylation") %>%
    ggplot(aes(x = methylation)) +
    geom_density(fill = "navyblue") +
    xlab("Methylation Levels") +
    ylab("Density") +
    ggtitle("Methylation Profile of CpGs in Entire Dataset") 


##############################################################################
#    Distribution of methylation levels for the CpGs used in the entire dataset
##############################################################################

Distribution_graph_entire_dataset %>%
  pivot_longer(starts_with("cg"), names_to = "cpg", values_to = "methylation") %>%
  ggplot(aes(x = methylation)) +
  geom_density(fill = "navyblue") +
  xlab("Methylation Levels") +
  ylab("Density") +
  ggtitle("Methylation Profile of CpGs in Entire Dataset") 


####################################################################################
# Variance of first set of selected CpGs vs those selected in the other iterations
####################################################################################

Distribution_graph_entire_dataset |>
  rename(sample=1) |>
  pivot_longer(
    cols=-sample,
    names_to="CpG",
    values_to="methylation"
  ) |>
  group_by(CpG) |>
  summarise(
    stdev = sd(methylation),
    methylation = mean(methylation)
  ) |>
  ungroup() |>
  mutate(first_model = if_else(CpG %in% colnames(kept_model_cpg_1), "First_Model","Other")) |>
  arrange(desc(first_model)) |>
  ggplot(aes(x=methylation, y=stdev, colour=first_model)) +
  geom_point() +
  scale_colour_manual(values=c("red2","#333333")) +
  coord_cartesian(ylim=c(0,0.4))+ggtitle("Distribution of variance in the CpGs used in each cycle")

kept_model_cpg_1 |>
  pivot_longer(
    cols=everything(),
    names_to="CpG",
    values_to="methylation"
  ) |>
  ggplot(aes(x=methylation)) +
  geom_density()



