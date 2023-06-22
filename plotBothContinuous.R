library(ggplot2)

plotBothContinuous <- function(var_1, var_2, str_1, str_2){
  #cplot <- plot(sample_info[[var_1]],sample_info[[var_2]], xlab = var_1, ylab = var_2, main = "Correlation plot")
  
  # Create data vectors
  x <- sample_info[[var_1]]  # First variable
  y <- sample_info[[var_2]]  # Second variable
  
  # Calculate the correlation coefficient, ignoring missing values
  cor_coef <- cor(x, y, use = "complete.obs")
  
  # Plot the correlation using ggplot2
  cplot <- ggplot(sample_info, aes(var_1,var_2)) +
    geom_point() +
    geom_smooth() +
    theme_light() +
    labs(title = paste("Correlation:", cor_coef), x = var_1, y = var_2)
  
  return(cplot)
}

ggplot(sample_info, aes(GA,BW,colour = race_major)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~sex) +
  theme_light() +
  labs(title = paste("Correlation:", cor(sample_info$GA,sample_info$BW, use = "complete.obs")))


ggplot(sample_info, aes(Age_group,BW_group,color = race_major)) +
  geom_jitter(height = 2, width = 2) +
  facet_wrap(~sex)
  theme_light()

ggplot(sample_info, aes(Age_group,BW_group,color = race_major)) +
  geom_count() +
  facet_wrap(~sex) +
  theme_light()


#ggplot(sample_info, aes(Age_hr,BW_group,color = race_major)) +
  #geom_col() +
  #facet_wrap(~sex) +
  #theme_light()


ggplot(sample_info, aes(BW_group,Age_hr,color = race_major)) +
  geom_boxplot() +
  facet_wrap(~sex) +
  theme_light()

ggplot(sample_info, aes(BW_group,Age_hr,color = race_major)) +
  geom_dotplot() +
  facet_wrap(~sex) +
  theme_light()

ggplot(sample_info, aes(BW_group,Age_hr,color = race_major)) +
  geom_violin(scale = "area") +
  facet_wrap(~sex) +
  theme_light()
