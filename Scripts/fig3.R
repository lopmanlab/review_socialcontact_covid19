## Code to create figure 4
# Load packages and data
source("Scripts/99_dependencies.R")
stratified <- read_excel("Data/Data.xlsx",sheet = "Strat_change") #Load stratified data

## Plot themes
##lkjasdlkajsd
theme<- theme_classic()+
  theme(plot.title = element_text(hjust=0.5, face="bold",size=10),
        axis.title.x=element_text(size=8),
        #axis.title.y = element_text(size-8),
        #plot.background=element_rect(fill="#f7f7f7"),
        #panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
#theme(axis.text=element_text(size=8))

theme_red <- theme(axis.title.x = element_blank(),
                   axis.title.y = element_blank())

## Get legend
p <- stratified[stratified$SN==2 & stratified$Strata=="place", ]  %>% 
  select(strata_cat, mean_precovid, mean_lckdown, mean_post, mean_post2) %>%tidyr::gather(measure, mean, -strata_cat) %>%
  mutate(measure = factor(measure, levels=c("mean_precovid","mean_lckdown","mean_post","mean_post2"))) %>%
  ggplot() +geom_point(aes(x=mean,y=strata_cat, color=measure), size =3) +
  scale_colour_manual(values = c("#2171B5","black","grey60","#C6DBEF"), name = "Mean contact",
                      labels = c("Pre-lockdown","Lockdown","1-month post-lockdown","2+ month post-lockdown"))+
  
  theme_bw()+theme(legend.text = element_text(size = 10), legend.title = element_text(size = 10))

legend <- get_legend(p)

# Short script to get maximum number of contacts in any strata
SN <- unique(stratified[stratified$Strata=="place","SN"])
SN <- as.vector(unlist(SN$SN))

max<-data.frame(name=rep(NA,length(SN)), max = rep(NA, length(SN)))
for (i in 1:length(SN)){
  study <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="place", ] %>% 
    select(mean_precovid, mean_lckdown, mean_post, strata_cat)
  max$name[[i]] <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="place", ]$name[1]
  max$max[[i]]<- max(study$mean_precovid, na.rm=T)
}


## Script for fig 4
loc_plots <- list()
loc_red <- list()


for (i in 1:length(SN)){
  name <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="place", ]$name[1]
  study <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="place", ] %>% 
    select(mean_precovid, mean_lckdown, mean_post,mean_post2, strata_cat)

  study<- study %>% gather(measure, mean, -strata_cat) %>%
    mutate(measure = factor(measure, levels=c("mean_precovid","mean_lckdown","mean_post","mean_post2"))) 
  
  
  if(max(study$mean, na.rm=T)<=7){
    max <-7
  } else if (max(study$mean, na.rm=T)<=9) {
    max<- 9
  } else if (max(study$mean, na.rm=T) <=13) {
    max <- 13
  } else {
    max <- 25
  }
  
  loc_plots[[i]] <- ggplot() +
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
    scale_colour_manual(values = c("#2171B5","black","grey60","#C6DBEF"), name = "Mean contact",
                        labels = c("Pre-lockdown","Lockdown","1-month post-lockdown","2+ month post-lockdown"))+
    #scale_color_ipsum(name = "A real legend title") +
    labs(
      x = NULL, y = NULL,
      title = paste(name)
    ) +
    theme +
    theme(legend.position = "right")
  
}

## Percent reduction between lockdown and pre-lockdown
for (i in 1:length(SN)){
  name <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="place", ]$name[1]
  study <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="place", ]
  loc_red[[i]] <- study%>%
    filter(!is.na(perc_red_lckdown)) %>% 
    ggplot(aes(x=round(perc_red_lckdown*100,digits=2), y=strata_cat)) + 
    geom_point(col="tomato2", size=4) +   # Draw points
    labs(title=study$name[1]) +
    scale_x_continuous(limits=c(0,100))+
    theme_bw() + theme_red
  #theme(axis.text=element_text(size=12))
}
# xlab("Mean contacts per person per day")

blank <- grid.rect(gp=gpar(col="white"))

png("Plot/fig3_locstrat.png",height = 11,width = 14, units = 'in',res=600)
annotate_figure(
  ggarrange(loc_plots[[11]], loc_plots[[12]],loc_plots[[9]],loc_plots[[1]],
            loc_plots[[3]],loc_plots[[14]],loc_plots[[2]], loc_plots[[4]],
            loc_plots[[5]],loc_plots[[7]],loc_plots[[6]], blank,
            loc_plots[[8]], loc_plots[[13]], loc_plots[[10]],
            legend = "right",legend.grob = legend, ncol = 4,nrow=4),
  bottom = text_grob("Mean daily contact", face = "bold", size=17),
  left = text_grob("Contact location", rot=90, face = "bold", size=17))
dev.off()

## Percent reduction in contacts
png("Plot/SI_locstrat_perc.png",height = 8,width = 12, units = 'in',res=600)
annotate_figure(
ggarrange(loc_red[[11]],loc_red[[12]],loc_red[[9]],loc_red[[1]],
          loc_red[[3]],loc_red[[14]],loc_red[[2]],loc_red[[4]],
          loc_red[[5]],loc_red[[7]],loc_red[[6]],blank,
          blank, loc_red[[13]], loc_red[[10]], ncol = 4,nrow=4),
  bottom = text_grob("% reduction in mean daily contact",face="bold", size=17),
  left = text_grob("Contact location", rot=90, face = "bold", size=17))
dev.off()

