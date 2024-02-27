# Load packages
library(tidyverse)

# theme
theme_set(theme_bw()) 

options(warn = -1)

fig = function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}
set.seed(42)
###########################################

train = read.csv("C://Users//wesle//Documents//R//AED//train.csv", sep = ",")
test = read.csv("C://Users//wesle//Documents//R//AED//test.csv", sep = ",")
meta = read.csv("C://Users//wesle//Documents//R//AED//metadata.csv", sep = ",")

data = function(x, test=F){
  
  x = x %>% mutate_all(~ifelse(.x == -999, NA_real_, .x))
  
  for(j in 1:nrow(meta)){
    
    if(str_detect(meta[j, 2], "^Qualitativo")){
      
      if(str_detect(meta[j, 2], "Qualitativo ordinal"))
        {
        x[,j] = factor(x %>%  pull(j), ordered=T)
        }
      else if(str_detect(meta[j, 2], "Qualitativo nominal"))
        {
        x[,j] = as.factor(x %>% pull(j))
        }   
    }
  }
  if(test==F){
    x$y = as.factor(x$y)
  }
  
  return(x)
}

train = data(train)

#data types
num_features= train %>% select_if(is.numeric) %>% colnames()
cat_features= train %>% select_if(is.factor) %>% select(-id) %>% colnames()

##### Data Visualization ######
fig(14, 4)
DataExplorer::plot_intro(train, ggtheme = theme_bw())

train %>% sample_n(1000) %>% visdat::vis_dat()

