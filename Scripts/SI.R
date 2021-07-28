## Code to create figure 4
# Load packages and data
source("Scripts/99_dependencies.R")
stratified <- read_excel("Data/Data.xlsx",sheet = "Strat_change") #Load stratified data

## Table for gender
gender <- stratified %>% filter(Strata == "gender")

gender_tab <- gender %>% 
  select(contains(c("name","strata_cat","mean","IQR"))) %>%       ## select relevant columns with col names match
  
  mutate_if(is.numeric,round,1) %>%                               ## round numerical columns to one digit
  
  mutate(mean_precovid = paste(mean_precovid,"(",IQRlo_precovid,"-",IQRhi_precovid,")", sep = ""),    ##paste means with IQRS
         mean_lckdown = paste(mean_lckdown,"(",IQRlo_lckdown,"-",IQRhi_lckdown,")", sep = ""),       
         mean_post = paste(mean_post,"(",IQRlo_post,"-",IQRhi_post,")", sep = ""),
         mean_post2 = paste(mean_post2,sep = "")) %>%
  
  select(mean_precovid, mean_lckdown, mean_post,mean_post2, strata_cat,name) %>%    
 # Pivot longer, cols 1:4, names_to and values_to determine column name of variables that pivoted to long and the value name
  
  pivot_longer(1:4, names_to = "period", values_to = "mean" ) %>%         

  ## Repivot so that each column is gender/period
  pivot_wider(names_from = c(strata_cat, period),
              values_from = mean)  %>%
   mutate_all(funs(ifelse(.=="NA"|.=="NA(NA-NA)","-",.))) %>%
   mutate_all(funs(gsub("\\(NA-NA)","",.)))

gender_tab<- gender_tab[,c(1:2,6,3,7,4,8)]

write.csv(gender_tab, "gender_strat.csv")

## Changes in contacts stratified by HH size
## Code to create figure 4
# Load packages and data
source("Scripts/99_dependencies.R")
stratified <- read_excel("Data/Data.xlsx",sheet = "Strat_change") #Load stratified data

## Plot themes
theme<- theme_classic()+
  theme(plot.title = element_text(hjust=0.5, face="bold",size=13),
        axis.title.x=element_text(size=8),
        axis.text = element_text(size=13),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())


theme_red <- theme(axis.title.x = element_blank(),
                   axis.title.y = element_blank())

## Get legend
p <- stratified[stratified$SN==2 & stratified$Strata=="hhsize", ]  %>% 
  select(strata_cat, mean_precovid, mean_lckdown, mean_post, mean_post2) %>%
  tidyr::gather(measure, mean, -strata_cat) %>%
  mutate(measure = factor(measure, levels=c("mean_precovid","mean_lckdown","mean_post","mean_post2"))) %>%
  ggplot() +geom_point(aes(x=mean,y=strata_cat, color=measure), size =3) +
  scale_colour_manual(values = c("#2171B5","black","grey60","#C6DBEF"), name = "Mean contact",
                      labels = c("Pre-COVID","Lockdown (Spring 2020)","1-month post-relax","2+ month post-relax"))+
  
  theme_bw()+theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))

legend <- get_legend(p)

# Short script to get maximum number of contacts in any strata
SN <-c(1,2,6,7,122,111,121,112,12)



max<-data.frame(name=rep(NA,length(SN)), max = rep(NA, length(SN)))
for (i in 1:length(SN)){
  study <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="hhsize", ] %>% 
    select(mean_precovid, mean_lckdown, mean_post, strata_cat)
  max$name[[i]] <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="hhsize", ]$name[1]
  max$max[[i]]<- max(study$mean_precovid, na.rm=T)
}


## Script for fig 4
hh_plots <- list()
hh_red <- list()


for (i in 1:length(SN)){
  name <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="hhsize", ]$name[1]
  study <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="hhsize", ] %>% 
    select(mean_precovid, mean_lckdown, mean_post,mean_post2, strata_cat)
  
  study<- study %>% gather(measure, mean, -strata_cat) %>%
    mutate(measure = factor(measure, levels=c("mean_precovid","mean_lckdown","mean_post","mean_post2"))) 
  
  
  if(max(study$mean, na.rm=T)<=12){
    max <-12
  } else {
    max <- 25.5
  }
  
  hh_plots[[i]] <- ggplot() +
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
                        labels = c("Pre-COVID","Initial mitigation","1-month post-relax","2+ month post-relax"))+
    #scale_color_ipsum(name = "A real legend title") +
    labs(
      x = NULL, y = NULL,
      title = name
    ) +
    theme +
    theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))
  
}

## Percent reduction between lockdown and pre-lockdown
for (i in 1:length(SN)){
  name <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="hhsize", ]$name[1]
  study <- stratified[stratified$SN==SN[[i]] & stratified$Strata=="hhsize", ]
  hh_red[[i]] <- study%>%
    filter(!is.na(perc_red_lckdown)) %>% 
    ggplot(aes(x=round(perc_red_lckdown*100,digits=2), y=strata_cat)) + 
    geom_point(col="tomato2", size=4) +   # Draw points
    labs(title=name) +
    scale_x_continuous(limits=c(0,100))+
    theme_bw() + theme_red
  #theme(axis.text=element_text(size=12))
}
# xlab("Mean contacts per person per day")

blank <- grid.rect(gp=gpar(col="white")) ## place holder for ggarrange

png("Plot/SI_hhstrat.png",height = 11,width = 16, units = 'in',res=800)
annotate_figure(
  ggarrange(hh_plots[[9]], hh_plots[[3]],hh_plots[[8]],
            hh_plots[[7]],hh_plots[[2]],hh_plots[[1]], 
            hh_plots[[5]],hh_plots[[6]],hh_plots[[4]], common.legend = T),
  # legend = "top",legend.grob = legend, ncol = 4,nrow=4),
  bottom = text_grob("Mean daily contact rate", face = "bold", size=17),
  left = text_grob("Household size", rot=90, face = "bold", size=17))
dev.off()

## Percent reduction in contacts
png("Plot/SI_hhstrat_perc.png",height = 8,width = 12, units = 'in',res=600)
annotate_figure(
  ggarrange(hh_red[[7]],hh_red[[3]],hh_red[[8]],
            hh_red[[5]],hh_red[[2]],hh_red[[1]],
            hh_red[[6]],hh_red[[4]], ncol = 3,nrow=3),
  bottom = text_grob("% reduction in mean daily contact rate",face="bold", size=17),
  left = text_grob("Household size", rot=90, face = "bold", size=17))
dev.off()

