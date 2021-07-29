## Code to create figure 4
# Load packages and data
source("Scripts/99_dependencies.R")
stratified <- read_excel("Data/Data.xlsx",sheet = "Strat_change") #Load stratified data

## Plot themes
theme<- theme_classic()+
  theme(plot.title = element_text(hjust=0.5, face="bold",size=14),
        axis.title.x=element_text(size=9),
        axis.text = element_text(size=14),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())

theme_red <- theme(axis.title.x = element_blank(),
                   axis.title.y = element_blank())

## Get legend
p <- stratified[stratified$SN==1 & stratified$Strata=="age", ]  %>% 
  select(strata_cat, mean_precovid, mean_lckdown, mean_post, mean_post2) %>%tidyr::gather(measure, mean, -strata_cat) %>%
  mutate(measure = factor(measure, levels=c("mean_precovid","mean_lckdown","mean_post","mean_post2"))) %>%
  ggplot() +geom_point(aes(x=mean,y=strata_cat, color=measure), size =3) +
  scale_colour_manual(values = c("#2171B5","black","grey60","#C6DBEF"), name = "Mean contact",
                      labels = c("Pre-COVID","Initial mitigation","1-month post-relax","2+ month post-relax"))+

  theme_bw()+theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))

legend <- get_legend(p)


## Small replacement for viz purposes
stratified$mean_lckdown[stratified$strata_cat=="0-6"&stratified$SN==121]<- 2.4
stratified$mean_lckdown[stratified$strata_cat=="60+"&stratified$SN==121]<- 2.2
stratified$mean_lckdown[stratified$strata_cat=="0-6"&stratified$SN==122]<- 2.6


# Short script to get maximum number of contacts in any strata
SN <- c(1,2,31,33,35,36,37,4,6,7,10,111,112,121,122,32,12)
max<-data.frame(name=rep(NA,length(SN)), max = rep(NA, length(SN)))
for (i in 1:length(SN)){
  study <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="age", ] %>% 
    select(mean_precovid, mean_lckdown, mean_post, strata_cat)
  max$name[[i]] <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="age", ]$name[1]
  max$max[[i]]<- max(study$mean_precovid, na.rm=T)
}


## Script for fig 4
age_plots <- list()
age_red <- list()


for (i in 1:length(SN)){
  name <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="age", ]$name[1]
  study <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="age", ] %>% 
    select(mean_precovid, mean_lckdown, mean_post,mean_post2, strata_cat)
  
  if(max(study$mean_precovid, na.rm=T)<=18.1){
    max <-18.3
  } else if (max(study$mean_precovid, na.rm=T)<=28.5) {
    max<- 29
  } else {
    max <- 35
  }
  
  study<- study %>% gather(measure, mean, -strata_cat) %>%
          mutate(measure = factor(measure, levels=c("mean_precovid","mean_lckdown","mean_post","mean_post2"))) 
    
    age_plots[[i]] <- ggplot() +
      # reshape the data frame & get min value so you can draw an eye-tracking line (this is one geom)
      geom_segment(
        data = study %>%
          filter(!is.na(mean))%>%
          group_by(strata_cat) %>% 
          top_n(-1) %>% 
          slice(1) %>%
          ungroup(),
        aes(x = 0, xend = mean, y = strata_cat, yend = strata_cat),
        linetype = "dotted", size = 0.5, color = "gray86"
      ) +
      # reshape the data frame & get min/max strata_category meanues so you can draw the segment (this is another geom)
      geom_segment(data=study%>%
          filter(!is.na(mean)) %>%
          group_by(strata_cat) %>% 
          summarise(start = range(mean)[1], end = range(mean)[2]) %>% 
          ungroup(),
        aes(x = start, xend = end, y = strata_cat, yend = strata_cat),
        color = "gray80", size = 1
      ) +
      # reshape the data frame & plot the points
      geom_point(
        data = study,
        aes(mean, strata_cat, color = measure), 
        size = 3
      ) +
      # i just extended the scale a bit + put axis on top; choose aesthetics that work 
      # for you
      scale_x_comma(position = "bottom", breaks = seq(0,max,by=4), labels = seq(0,max,by=4),limits = c(0, max)) +
      #scale_colour_manual(values = c("#2171B5","black","grey60","#C6DBEF"), name = "Mean contact",
      scale_colour_manual(values = c("gray42","black","grey60","gray85"), name = "Mean contact",
                          labels = c("Pre-COVID","Initial mitigation","1-month post-relax","2+ month post-relax"))+
      #scale_color_ipsum(name = "A real legend title") +
      labs(
        x = NULL, y = NULL,
        title = paste(name)
      ) +
      theme +
      theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))

}

## Percent reduction between lockdown and pre-lockdown
for (i in 1:length(SN)){
  name <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="age", ]$name[1]
  study <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="age", ]
  age_red[[i]] <- study%>%
  ggplot(aes(x=round(perc_red_lckdown*100,digits=2), y=strata_cat)) + 
  geom_point(col="tomato2", size=4) +   # Draw points
  labs(title=study$name[1]) +
  scale_x_continuous(limits=c(30,95))+
  theme_bw() + theme_red
#theme(axis.text=element_text(size=12))
}
# xlab("Mean contacts per person per day")

blank <- grid.rect(gp=gpar(col="white")) ## place holder for ggarrange


png("Plot/fig4_agestrat_gray.png",height = 12,width = 13, units = 'in',res=700)
annotate_figure(
          ggarrange(age_plots[[12]],age_plots[[13]],age_plots[[14]],age_plots[[15]],
          age_plots[[5]],age_plots[[2]],age_plots[[3]],age_plots[[16]], 
           age_plots[[1]],age_plots[[6]],age_plots[[8]],age_plots[[4]],  
           age_plots[[10]],age_plots[[9]], age_plots[[7]], age_plots[[17]],
          age_plots[[11]],
          common.legend = T, 
          #legend.grob = legend,
          ncol = 4,nrow=5),
          bottom = text_grob("Mean daily contact rate", face = "bold", size=17),
          left = text_grob("Age groups", rot=90, face = "bold", size=17))
dev.off()

## Percent reduction in contacts
png("Plot/SI_agestrat_perc.png",height = 8,width = 12, units = 'in',res=600)
annotate_figure(ggarrange(age_red[[1]],age_red[[2]],age_red[[3]],age_red[[4]],
          age_red[[5]],age_red[[6]],age_red[[7]],age_red[[8]],
          age_red[[9]],age_red[[10]],age_red[[11]],age_red[[12]],age_red[[13]],
          age_red[[14]], age_red[[15]], age_red[[16]], ncol = 4,nrow=4),
          bottom = text_grob("% reduction in mean daily contact rate", face = "bold", size=17),
          left = text_grob("Age groups", rot =90, face = "bold", size = 17))
dev.off()

