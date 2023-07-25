#R script to generate the plots when both the variables are categorical

#Load the required packages
library(ggplot2)
library(patchwork)
library(dplyr)

#Function to generate a bar plot with two categorical variables and no stratification variables
plotBothCategoricalNoStr <- function(var_1, var_2,visual){
  

  #Filter the data to extract rows with only one race in race_detail
  if(var_1 == "race_detail" || var_2 =="race_detail"){
    sample_info <- sample_info[!grepl(",",sample_info$race_detail),]
  }
  
  #If else block to decide between sample size and sample proportion
  if(visual == "Sample size"){
    
    #Bar plot using ggplot
    caplotNoStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
      geom_bar() +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample size") +
      guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  }else if (visual == "Sample proportion"){
    
    #Calculation of Sample proportions for proportions plot
    #Create a table of frequency between the two selected variables
    frequency <- table(sample_info[[var_1]],sample_info[[var_2]])
    
    #Convert the table into a dataframe
    frequency<-as.data.frame(frequency)
    
    #Filter the data to ignore rows that have 0 frequency
    frequency<-frequency[frequency$Freq!=0,]
    
    #Calculate the total counts of each group using summarize function 
    grouped_counts <- frequency %>%
      group_by(Var1) %>%
      summarize(total_counts = sum(Freq))
    
    #Merge the total counts dataframe with the frequency dataframe
    merged_df <- merge(frequency, grouped_counts, by = c("Var1"))
    
    #Add a new column for proportions and calculate the proportions
    merged_df$proportions <- merged_df$Freq / merged_df$total_counts
    
    #Add a new column for percentage and calculate the percentage
    merged_df$per <- paste(round(merged_df$proportions * 100,2),"%")
    
    #Order the data in the decreasing order for correct data mapping in plot
    my_ordered_data <- merged_df[order(merged_df$Var2,decreasing = TRUE), ]
    
    
    #Bar plot using the processed data
    caplotNoStr <- ggplot(my_ordered_data,aes(Var1, weight = Freq)) +
      geom_bar(aes(fill = Var2), position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = ifelse(proportions < .05, NA, per), y = proportions), position = position_fill(vjust = 0.5)) +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample proportion") +
      guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  }
  
  #Return the plot object
  return(caplotNoStr)
}



#Function to generate a counts plot with two categorical variables and no stratification variables
plotBothCategoricalCount <- function(var_1,var_2,visual){
  
  #Filter the data to extract rows with only one race in race_detail
  if(var_1 == "race_detail" || var_2 =="race_detail"){
    sample_info <- sample_info[!grepl(",",sample_info$race_detail),]
  }
  
  #If else block to decide between sample size and sample proportion
  if(visual == "Sample size"){
    
    #Counts plot using ggplot
    caplotCount <- ggplot(sample_info,aes(get(var_1),get(var_2))) +
      geom_count() +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
      scale_size(name = "Sample Size")
    
    #Adding labels to the counts
    caplotCount <- caplotCount + geom_text(data = ggplot_build(caplotCount)$data[[1]], 
              aes(x, y, label = n),vjust = -0.8)
    
  }else if (visual == "Sample proportion"){
    
    #Calculation of Sample proportions for proportions plot
    #Create a table of frequency between the two selected variables
    frequency <- table(sample_info[[var_1]],sample_info[[var_2]])
    
    #Convert the table into a dataframe
    frequency<-as.data.frame(frequency)
    
    #Filter the data to ignore rows with 0 Frequency
    frequency<-frequency[frequency$Freq!=0,]
    
    #Calculate the total counts of each group using Summarize function
    grouped_counts <- frequency %>%
      group_by(Var1) %>%
      summarize(total_counts = sum(Freq))
    
    #Merge the total counts dataframe with the frequency dataframe
    merged_df <- merge(frequency, grouped_counts, by = c("Var1"))
    
    #Add a new column and calculate the proportions
    merged_df$proportions <- merged_df$Freq / merged_df$total_counts
    
    #Add a new column and calculate the percentages
    merged_df$per <- paste(round(merged_df$proportions * 100,2),"%")
    
    #Order the dataframe in the decreasing order for correct mapping of data
    my_ordered_data <- merged_df[order(merged_df$Var2,decreasing = TRUE), ]

    #Counts plot using ggplot
    caplotCount <- ggplot(my_ordered_data,aes(Var1,Var2, weight = proportions)) +
      geom_count() +
      geom_text(aes(label = per), hjust = 0.35, vjust = -1.5)+
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2], title = paste0("Grouped by ",variable_info$varShow[variable_info$variables == var_1])) +
      scale_size_area(max_size = 10, name = "Sample proportion") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))


    #Calculation of Sample proportions for proportions plot
    #Create a table of frequency between the two selected variables
    frequency <- table(sample_info[[var_1]],sample_info[[var_2]])
    
    #Convert the table into a dataframe
    frequency<-as.data.frame(frequency)
    
    #Filter the dataframe to ignore the rows with 0 frequency
    frequency<-frequency[frequency$Freq!=0,]
    
    #Calculate the total counts of each group using summarize function
    grouped_counts <- frequency %>%
      group_by(Var2) %>%
      summarize(total_counts = sum(Freq))
    
    #Merge the total counts dataframe with the frequency dataframe
    merged_df <- merge(frequency, grouped_counts, by = c("Var2"))
    
    #Add a new column and calculate the proportions
    merged_df$proportions <- merged_df$Freq / merged_df$total_counts
    
    #Add a new column and calculate the percentage
    merged_df$per <- paste(round(merged_df$proportions * 100,2),"%")
    
    #Order the data in a decreasing order for correct mapping of data points
    my_ordered_data <- merged_df[order(merged_df$Var2,decreasing = TRUE), ]

    #Counts plot using ggplot
    caplotCount <- caplotCount +ggplot(my_ordered_data,aes(Var1,Var2, weight = proportions)) +
      geom_count() +
      geom_text(aes(label = per), hjust = 0.35, vjust = -1.5)+
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2], title = paste0("Grouped by ",variable_info$varShow[variable_info$variables == var_2])) +
      scale_size_area(max_size = 10, name = "Sample proportion") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  #Return the ggplot object
  return(caplotCount)

}

#Function to generate a counts plot with two categorical variables and one stratification variables
plotBothCategoricalCountOneStr <- function(var_1,var_2,str,visual){
  
  #Filter the data to extract rows with only one race in race_detail
  if(var_1 == "race_detail" || var_2 =="race_detail" || str == "race_detail"){
    sample_info <- sample_info[!grepl(",",sample_info$race_detail),]
  }
  
  #Filter the data to remove samples that have OtherUnknown as value in race_major
  if(str == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }

  #If else block to decide between sample size and sample proportion
  if(visual == "Sample size"){
    
    #Calculate the total counts for each combination of the selected variables
    sample_info_count <- sample_info %>% count(sample_info[[var_1]],sample_info[[var_2]],sample_info[[str]])
    
    #Name the columns of the dataframe appropriately
    colnames(sample_info_count) <- c(var_1,var_2,str,"count")
    
    #Counts plot using ggplot
    caplotCount <- ggplot(sample_info,aes(get(var_1),get(var_2))) +
      geom_count() +
      facet_wrap(~get(str))+
      geom_text(data = sample_info_count,aes(get(var_1),get(var_2),label = count), vjust = -0.9) +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
      scale_size(name = "Sample Size")
    
  }else if (visual == "Sample proportion"){
    
    
    #Calculate the proportions to plot the data
    #Create a table of frequency for each unique combination of the selected variables
    frequency <- table(sample_info[[var_1]],sample_info[[var_2]],sample_info[[str]])
    
    #Convert the table into a dataframe
    frequency<-as.data.frame(frequency)
    
    #Remove the rows with 0 frequency
    frequency<-frequency[frequency$Freq!=0,]
    
    #Calculate the total counts of each required group using summarize function
    grouped_counts <- frequency %>%
      group_by(Var1,Var3) %>%
      summarize(total_counts = sum(Freq))
    
    #Merge the grouped counts dataframe with the frequency dataframe
    merged_df <- merge(frequency, grouped_counts, by = c("Var1","Var3"))
    
    #Add a new column and calculate the proportions
    merged_df$proportions <- merged_df$Freq / merged_df$total_counts
    
    #Add a new column and calculate the percentages
    merged_df$per <- paste(round(merged_df$proportions * 100,2),"%")
    
    #Order the data in decreasing order for correct mapping of data points
    my_ordered_data <- merged_df[order(merged_df$Var2,decreasing = TRUE), ]
    
    #Counts plot using ggplot
    caplotCount <- ggplot(my_ordered_data,aes(Var1,Var2, weight = proportions)) +
      geom_count() +
      geom_text(aes(label = per), hjust = 0.35, vjust = -1.5)+
      facet_wrap(~Var3) +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2], title = paste0("Grouped by ",variable_info$varShow[variable_info$variables == var_1])) +
      scale_size_area(max_size = 10, name = "Sample proportion") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    #Calculate the proportions to plot the data
    #Create a table of frequency for each unique combination of the selected variables
    frequency <- table(sample_info[[var_1]],sample_info[[var_2]],sample_info[[str]])
    
    #Convert the table into a dataframe
    frequency<-as.data.frame(frequency)
    
    #Remove the rows with 0 frequency
    frequency<-frequency[frequency$Freq!=0,]
    
    #Calculate the total counts of each required group using summarize function
    grouped_counts <- frequency %>%
      group_by(Var2,Var3) %>%
      summarize(total_counts = sum(Freq))
    
    #Merge the grouped counts dataframe with the frequency dataframe
    merged_df <- merge(frequency, grouped_counts, by = c("Var2","Var3"))
    
    #Add a new column and calculate the proportions
    merged_df$proportions <- merged_df$Freq / merged_df$total_counts
    
    #Add a new column and calculate the percentages
    merged_df$per <- paste(round(merged_df$proportions * 100,2),"%")
    
    #Order the data in decreasing order for correct mapping of data points
    my_ordered_data <- merged_df[order(merged_df$Var2,decreasing = TRUE), ]

    
    #Counts plot using ggplot
    caplotCount <- caplotCount +ggplot(my_ordered_data,aes(Var1,Var2, weight = proportions)) +
      geom_count() +
      geom_text(aes(label = per), hjust = 0.35, vjust = -1.5)+
      facet_wrap(~Var3)+
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2], title = paste0("Grouped by ",variable_info$varShow[variable_info$variables == var_2])) +
      scale_size_area(max_size = 10, name = "Sample proportion") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  #Return the plot object
  return(caplotCount)
  
}


#Function to generate a bar plot with two categorical variables and one stratification variable
plotBothCategoricalOneStrfacet <- function(var_1,var_2,str_1,visual){
  
  #Filter the data to remove samples that have OtherUnknown as value in race_major
  if(str_1 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  
  #Filter the data to remove samples that have Unknown as value in TPN
  if(str_1 == "TPN"){
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  #Filter the data to extract rows with only one race in race_detail
  if(str_1 == "race_detail" || var_1 == "race_detail" || var_2 =="race_detail"){
    sample_info <- sample_info[!grepl(",",sample_info$race_detail),]
  }
  
  #If else block to decide between sample size and sample proportions
  if(visual == "Sample size"){
    
    #Bar plot using ggplot
    caplotOneStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
      geom_bar() +
      facet_wrap(~get(str_1)) +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample size") +
      guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
    
  }else if(visual == "Sample proportion"){
    
    #Calculate the proportions to plot the data
    #Create a table of frequency for each unique combination of the selected variables
    frequency <- table(sample_info[[var_1]],sample_info[[var_2]],sample_info[[str_1]])
    
    #Convert the table into a dataframe
    frequency<-as.data.frame(frequency)
    
    #Remove the rows with 0 frequency
    frequency<-frequency[frequency$Freq!=0,]
    
    #Calculate the total counts of each required group using summarize function
    grouped_counts <- frequency %>%
      group_by(Var1, Var3) %>%
      summarize(total_counts = sum(Freq))
    
    #Merge the grouped counts dataframe with the frequency dataframe
    merged_df <- merge(frequency, grouped_counts, by = c("Var1", "Var3"))
    
    #Add a new column and calculate the proportions
    merged_df$proportions <- merged_df$Freq / merged_df$total_counts
    
    #Add a new column and calculate the percentages
    merged_df$per <- paste(round(merged_df$proportions * 100,2),"%")
    
    #Order the data in decreasing order for correct mapping of data points
    my_ordered_data <- merged_df[order(merged_df$Var2,decreasing = TRUE), ]
    
    #Bar plot using ggplot
    caplotOneStr <- ggplot(my_ordered_data,aes(Var1, weight = Freq)) +
      geom_bar(aes(fill = Var2), position = "fill") +
      facet_wrap(~Var3) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = ifelse(proportions < .05, NA, per), y = proportions), position = position_fill(vjust = 0.5))+
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample proportion") +
      guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  }
  
  #Return the plot object
  return(caplotOneStr)
}


#Function to generate a bar plot with two categorical variables and two stratification variables
plotBothCategoricalTwoStrfacet <- function(var_1,var_2,str_1,str_2){
  
  #Filter the data to extract rows with only one race in race_detail
  if(str_1 == "race_detail" || str_2 == "race_detail" || var_1 == "race_detail" || var_2 =="race_detail"){
    sample_info <- sample_info[!grepl(",",sample_info$race_detail),]
  }
  
  #Bar plot using ggplot
  caplotTwoStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    geom_bar() +
    facet_wrap(~get(str_1)+get(str_2)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Count") +
    guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  
  
  
  #Return the plot object
  return(caplotTwoStr)
}


