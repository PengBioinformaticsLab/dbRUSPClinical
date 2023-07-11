#R script to generate the plots when both the variables are categorical

#Load the required packages
library(ggplot2)
library(patchwork)

#Function to generate a bar plot with two categorical variables and no stratification variables
plotBothCategoricalNoStr <- function(var_1, var_2,visual){
  


  if(visual == "Sample size"){
    caplotNoStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
      geom_bar() +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample size") +
      guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  }else if (visual == "Sample proportion"){
    
    frequency <- table(sample_info[[var_1]],sample_info[[var_2]])
    frequency<-as.data.frame(frequency)
    frequency<-frequency[frequency$Freq!=0,]
    grouped_counts <- frequency %>%
      group_by(Var1) %>%
      summarize(total_counts = sum(Freq))
    merged_df <- merge(frequency, grouped_counts, by = c("Var1"))
    merged_df$proportions <- merged_df$Freq / merged_df$total_counts
    merged_df$per <- paste(round(merged_df$proportions * 100,2),"%")
    my_ordered_data <- merged_df[order(merged_df$Var2,decreasing = TRUE), ]
    
    # frequency_divided <- prop.table(frequency, margin = 1)
    # row_sums <- rowSums(frequency)
    # col_sums <- colSums(frequency)
    # rows_to_remove <- row_sums == 0
    # col_to_remove <- col_sums == 0
    # frequency <- frequency[!rows_to_remove,!col_to_remove]
    # frequency_divided <- prop.table(frequency, margin = 1)
    # frequency <- as.data.frame(frequency)
    # frequency_df <- as.data.frame(frequency_divided)
    # frequency_df$per <- paste(round(frequency_df$Freq * 100, 2),"%")
    # frequency_df$Count <- frequency$Freq
    # my_ordered_data <- frequency_df[order(frequency_df$Var2,decreasing = TRUE), ]
    
    caplotNoStr <- ggplot(my_ordered_data,aes(Var1, weight = Freq)) +
      geom_bar(aes(fill = Var2), position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = ifelse(proportions < .05, NA, per), y = proportions), position = "fill")+
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample proportion") +
      guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
    
    
    # caplotNoStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    #   geom_bar(position = "fill") +
    #   theme_light() +
    #   labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample proportion") +
    #   guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  }
  return(caplotNoStr)
}



#Function to generate a count plot with two categorical variables and no stratification variables
plotBothCategoricalCount <- function(var_1,var_2,visual){
  
  if(visual == "Sample size"){
    caplotCount <- ggplot(sample_info,aes(get(var_1),get(var_2))) +
      geom_count() +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2]) +
      scale_size(name = "Sample Size")
  }else if (visual == "Sample proportion"){
    
    caplotCount <- ggplot(sample_info,aes(get(var_1),get(var_2)))+
      geom_count(aes(size = after_stat(prop), group = get(var_1))) +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2], title = paste0("Grouped by ",variable_info$varShow[variable_info$variables == var_1])) +
      scale_size_area(max_size = 10, name = "Sample proportion") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
    
    caplotCount <- caplotCount + ggplot(sample_info,aes(get(var_1),get(var_2)))+
      geom_count(aes(size = after_stat(prop), group = get(var_2))) +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2], title = paste0("Grouped by ",variable_info$varShow[variable_info$variables == var_2])) +
      scale_size_area(max_size = 10, name = "Sample proportion") +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  return(caplotCount)

}


#Function to generate a jitter plot with two categorical variables and no stratification variables
plotBothCategoricalJitter <- function(var_1,var_2){
  
  caplotJitter <- ggplot(sample_info,aes(get(var_1),get(var_2))) +
    geom_jitter() +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = variable_info$varShow[variable_info$variables == var_2])
  
  
  
  
  return(caplotJitter)
  
}


#Function to generate a mosaic plot with two categorical variables and no stratification variables
plotBothCategoricalMosaic <- function(var_1, var_2){
  
  

  caplotmosaic <- mosaicplot(get(var_2)~get(var_1),data = sample_info, 
                             main = "Mosaic plot",
                             xlab = variable_info$varShow[variable_info$variables==var_2], 
                             ylab = variable_info$varShow[variable_info$variables==var_1],
                             color = TRUE)
  
  
  
  
  return(caplotmosaic)
}


#Function to generate a bar plot with two categorical variables and one stratification variable
plotBothCategoricalOneStrfacet <- function(var_1,var_2,str_1,visual){
  
  if(str_1 == "race_major"){
    sample_info <- subset(sample_info, race_major != "OtherUnknown")
  }
  
  if(str_1 == "TPN"){
    sample_info <- subset(sample_info, TPN != "Unknown")
  }
  
  if(visual == "Sample size"){
    # bar plot using ggplot2
    caplotOneStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
      geom_bar() +
      facet_wrap(~get(str_1)) +
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample size") +
      guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  }else if(visual == "Sample proportion"){
    
    frequency <- table(sample_info[[var_1]],sample_info[[var_2]],sample_info[[str_1]])
    frequency<-as.data.frame(frequency)
    frequency<-frequency[frequency$Freq!=0,]
    grouped_counts <- frequency %>%
      group_by(Var1, Var3) %>%
      summarize(total_counts = sum(Freq))
    merged_df <- merge(frequency, grouped_counts, by = c("Var1", "Var3"))
    merged_df$proportions <- merged_df$Freq / merged_df$total_counts
    merged_df$per <- paste(round(merged_df$proportions * 100,2),"%")
    my_ordered_data <- merged_df[order(merged_df$Var2,decreasing = TRUE), ]
    
    caplotOneStr <- ggplot(my_ordered_data,aes(Var1, weight = Freq)) +
      geom_bar(aes(fill = Var2), position = "fill") +
      facet_wrap(~Var3) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(aes(label = ifelse(proportions < .05, NA, per), y = proportions), position = "fill")+
      theme_light() +
      labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample proportion") +
      guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
    
    
    # # bar plot using ggplot2
    # caplotOneStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    #   geom_bar(position = "fill") +
    #   facet_wrap(~get(str_1)) +
    #   theme_light() +
    #   labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Sample proportion") +
    #   guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  }
  
  
  
  
  return(caplotOneStr)
}


#Function to generate a bar plot with two categorical variables and two stratification variables
plotBothCategoricalTwoStrfacet <- function(var_1,var_2,str_1,str_2){
  
  # bar plot using ggplot2
  caplotTwoStr <- ggplot(sample_info,aes(x = get(var_1), fill = get(var_2))) +
    geom_bar() +
    facet_wrap(~get(str_1)+get(str_2)) +
    theme_light() +
    labs(x = variable_info$varShow[variable_info$variables == var_1], y = "Count") +
    guides(fill=guide_legend(title=variable_info$varShow[variable_info$variables==var_2]))
  
  
  
  
  return(caplotTwoStr)
}


