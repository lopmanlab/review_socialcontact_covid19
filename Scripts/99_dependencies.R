package_list <- c(
"readxl",
"dplyr",
"kableExtra",
"forestplot",
"ggalt",
"plotly",
"forcats",
"ggpubr",
"viridis",
"tidyr",
"hrbrthemes",
"viridis",
"ggpubr",
"gridExtra"
  
)



#install dependencies
if(F){
  install.packages(package_list)
}

lapply(package_list, function(x) library(x, character.only = T))

rm(package_list)
