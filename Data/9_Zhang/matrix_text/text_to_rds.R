## Below code converts matrices in text files into RDS
## Text files were extracted directly from supplementary information from PDFs

## For matrices where the copied version is a vector in text form and needs to be reshaped into a matrix with column and row names added
setwd("C:/Users/cliu369/Box Sync/5.Literature/1.Social contact patterns Lit Review 2020/0_2020rapidreview/Data/11_Zhang/matrix_text")

txt_matrix <- function(df) {
                  df99<- df[,1]
                  df99 <- t(matrix(data=df99, nrow=17, ncol=16))
                  age_cat <- df99[,1]
                  df99<- as.numeric(df99[,2:17])
                  df99<- t(matrix(data=df99, nrow=16,ncol=16))
                  
                  colnames(df99) <- age_cat
                  rownames(df99) <-age_cat
                  
                  return(df99)
}

saveRDS(txt_matrix(read.table("shenzhen_outbreak.txt")),"shenzhen_outbreak.RDS")
saveRDS(txt_matrix(read.table("shenzhen_post.txt")),"shenzhen_post.RDS")
saveRDS(txt_matrix(read.table("changsha_base.txt")),"changsha_base.RDS")
saveRDS(txt_matrix(read.table("changsha_outbreak.txt")),"changsha_outbreak.RDS")
saveRDS(txt_matrix(read.table("changsha_post.txt")),"changsha_post.RDS")
saveRDS(txt_matrix(read.table("shanghai_post.txt")),"shanghai_post.RDS")
saveRDS(txt_matrix(read.table("wuhan_post.txt")),"wuhan_post.RDS")


## For matrices where the copied version is a matrix in text

df<-read.table("wuhan_base.txt")
colnames(df) <- c("part_age",df[,1])
saveRDS(df,"wuhan_base.RDS")

df<-read.table("wuhan_outbreak.txt")
colnames(df) <- c("part_age",df[,1])
saveRDS(df,"wuhan_outbreak.RDS")

df<-read.table("shanghai_base.txt")
colnames(df) <- c("part_age",df[,1])
saveRDS(df,"shanghai_base.RDS")

df<-read.table("shanghai_oubtreak.txt")
colnames(df) <- c("part_age",df[,1])
saveRDS(df,"shanghai_outbreak.RDS")

# Attempt to melt variables in preparation for matrix changes
melt(df, varnames = c("part_age", "cont_age"), value.name = "contacts")

