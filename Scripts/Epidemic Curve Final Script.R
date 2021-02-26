#EPI CURVE PANEL FINAL

data.src<-file.path('/Users/jjetter280/Desktop/CountryData/Epi Curves')

covid_global<- read.csv(file.path(data.src,"WHO-COVID-19-global-data.csv"),header = FALSE)
names(covid_global)<- covid_global[1,]

#DATA SUBSETS
belgium<-subset(covid_global, Country=="Belgium")
china<-subset(covid_global, Country=="China")
france<-subset(covid_global, Country=="France")
germany<-subset(covid_global, Country=="Germany")
greece<-subset(covid_global, Country=="Greece")
italy<-subset(covid_global, Country=="Italy")
kenya<-subset(covid_global, Country=="Kenya")
luxem<-subset(covid_global, Country=="Luxembourg")
nether<-subset(covid_global, Country=="Netherlands")
SA<-subset(covid_global, Country=="South Africa")
UK<-subset(covid_global, Country=="The United Kingdom") 
US<-subset(covid_global, Country=="United States of America")

#PLOTS
library(ggplot2)
library(zoo)


#BELGIUM
rownames(belgium) <- seq(length=nrow(belgium))
belgium<-belgium[59:273,]
rownames(belgium) <- seq(length=nrow(belgium))
belgium$Date_reported<-factor(belgium$Date_reported, levels=belgium$Date_reported)
belgium$New_cases<-as.numeric(belgium$New_cases)

belgium$New_cases<-ifelse(belgium$New_cases <0,0,belgium$New_cases)
belgium$New_cases<-ifelse(is.na(belgium$New_cases),0,belgium$New_cases)

belgium$NewCases7d_Avg<- zoo::rollmean(belgium$New_cases, k=7, fill=NA)
summary(belgium$NewCases7d_Avg)

Bel_plot<- ggplot(belgium, aes(x=Date_reported, y=NewCases7d_Avg)) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) + 
  theme_bw() + theme(legend.position="none") + labs(title="Belgium") +
  scale_y_continuous("Number New Cases", limits = c(0,2500), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-day Average New COVID-19 Cases", "National Lockdown Start/End", "Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=belgium, aes( xintercept=12),linetype="solid",size=0.25, colour="red") + 
  geom_vline(data=belgium, aes(xintercept=65),linetype="solid",size=0.25, colour="red") + 
  #annotate("text", label="Lockdown End", colour="red", x=67, y=1250, angle=90, size=3) +
  annotate("rect", xmin=13, xmax=65, ymin=0, ymax=2500, alpha=.1, fill="red")+
  annotate("rect", xmin=55, xmax=61, ymin=0, ymax=2500, fill="blue", alpha=.2)+ annotate(
    "text", label="Colletti", x=58, y=1500, colour="black", size=3, angle=90)+
  annotate(
    "text", label="Del Fava", x=33, y=1500, colour="black", size=3, angle=90) +
  annotate("rect", xmin=30, xmax=36, ymin=0, ymax=2500, alpha=.2, fill="blue")+
  annotate( "text", label="Coletti 2", x=99, y=1500, colour="black", size=3, angle=90)+
  annotate("rect", xmin=96, xmax=102, ymin=0, ymax=2500, alpha=.2, fill="blue") +
  annotate( "text", label="Coletti 3", x=141, y=1500, colour="black", size=3, angle=90)+
  annotate("rect", xmin=138, xmax=144, ymin=0, ymax=2500, alpha=.2, fill="blue")




#CHINA
rownames(china) <- seq(length=nrow(china))
china<-china[1:273,]
rownames(china) <- seq(length=nrow(china))
china$Date_reported<-factor(china$Date_reported, levels=china$Date_reported)
china$New_cases<-as.numeric(china$New_cases)

china$New_cases<-ifelse(china$New_cases <0,0,china$New_cases)
china$New_cases<-ifelse(is.na(china$New_cases),0,china$New_cases)

china$NewCases7d_Avg<- zoo::rollmean(china$New_cases, k=7, fill=NA)
summary(china$NewCases7d_Avg)


Ch_plot<- ggplot(china) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) + 
  theme_bw() + theme(legend.position="none") + labs(title=expression("China"^2)) +
  scale_y_continuous("Number New Cases", limits = c(0,5000), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/03/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=china, aes(xintercept=21),linetype="solid", colour="darkgoldenrod1",size=0.25) + 
  geom_vline(data=china, aes(xintercept=97),linetype="solid",colour="darkgoldenrod1",size=0.25) + 
  annotate("rect", xmin=21, xmax=97, ymin=0, ymax=5000, alpha=.2, fill="darkgoldenrod1") + 
  annotate("text", label="Zhang 1", angle=90, x=35, y=3500, colour="black", size=3) +
  annotate("rect", xmin=32, xmax=38, ymin=0, ymax=5000, alpha=.2, fill="blue") +
  annotate( "text",  label="Zhang 2", x=66, y=3500, size=3,angle=90,colour="black") +
  annotate("text",  label="Zhang 3", x=130, y=3500, size=3,angle=90,colour="black") +
  annotate("rect", xmin=59, xmax=78, ymin=0, ymax=5000, alpha=.2, fill="blue") +
  annotate("rect", xmin=126, xmax=134, ymin=0, ymax=5000, alpha=.2, fill="blue") 



#FRANCE
rownames(france) <- seq(length=nrow(france))
france<-france[59:273,]
rownames(france) <- seq(length=nrow(france))
france$Date_reported<-factor(france$Date_reported, levels=france$Date_reported)
france$New_cases<-as.numeric(france$New_cases)

france$New_cases<-ifelse(france$New_cases <0,0,france$New_cases)
france$New_cases<-ifelse(is.na(france$New_cases),0,france$New_cases)

france$NewCases7d_Avg<- zoo::rollmean(france$New_cases, k=7, fill=NA)
summary(france$NewCases7d_Avg)

Fr_plot<- ggplot(france) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y= NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) + 
  theme_bw() + theme(legend.position="none") + labs(title="France") +
  scale_y_continuous("Number New Cases", limits = c(0,12000), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1)))+ 
  geom_vline(data=france, aes(xintercept=17),linetype="solid",colour="red",size=0.25) + 
  geom_vline(data=france, aes(xintercept=72),linetype="solid", colour="red",size=0.25) + 
  annotate("rect", xmin=17, xmax=72, ymin=0, ymax=12000, alpha=.1, fill="red") +
  annotate("text", label="Del Fava", x=33, y=6000,angle=90, colour="black", size=3)+
  annotate("rect", xmin=30, xmax=36, ymin=0, ymax=12000, alpha=.2, fill="blue") +
  annotate("rect", xmin=49, xmax=59, ymin=0, ymax=12000, alpha=.2, fill="blue") + 
  annotate("text", label="Bosetti", x=54, y=5000, size=3,angle=90,colour="black") 




#GERMANY
rownames(germany) <- seq(length=nrow(germany))
germany<-germany[59:273,]
rownames(germany) <- seq(length=nrow(germany))
germany$Date_reported<-factor(germany$Date_reported, levels=germany$Date_reported)
germany$New_cases<-as.numeric(germany$New_cases)

germany$New_cases<-ifelse(germany$New_cases <0,0,germany$New_cases)
germany$New_cases<-ifelse(is.na(germany$New_cases),0,germany$New_cases)

germany$NewCases7d_Avg<- zoo::rollmean(germany$New_cases, k=7, fill=NA)
summary(germany$NewCases7d_Avg)


Ger_plot<- ggplot(germany) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y= NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Germany") +
  scale_y_continuous("Number New Cases", limits = c(0,6500), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End", "Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=germany, aes(xintercept=23),linetype="solid",size=0.25, colour="red") + 
  geom_vline(data=germany, aes(xintercept=71),linetype="solid",size=0.25, colour="red") + 
  annotate("rect", xmin=23, xmax=71, ymin=0, ymax=6500, alpha=.1, fill="red") +
  annotate("rect", xmin=23, xmax=29, ymin=0, ymax=6500, alpha=.2, fill="blue") + 
  annotate("text", label="Del Fava", x=26, y=4500, angle=90,colour="black", size=3)



#GREECE
rownames(greece) <- seq(length=nrow(greece))
greece<-greece[59:273,]
rownames(greece) <- seq(length=nrow(greece))
greece$Date_reported<-factor(greece$Date_reported, levels=greece$Date_reported)
greece$New_cases<-as.numeric(greece$New_cases)

greece$New_cases<-ifelse(greece$New_cases <0,0,greece$New_cases)
greece$New_cases<-ifelse(is.na(greece$New_cases),0,greece$New_cases)

greece$NewCases7d_Avg<- zoo::rollmean(greece$New_cases, k=7, fill=NA)
summary(greece$NewCases7d_Avg)


Gr_plot<- ggplot(greece) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) + 
  theme_bw() + theme(legend.position="none") + labs(title="Greece") +
  scale_y_continuous("Number New Cases", limits = c(0,400), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=greece, aes(xintercept=23),linetype="solid",colour="red", size=0.25) + 
  geom_vline(data=greece, aes(xintercept=57),linetype="solid",colour="red",size=0.25) + 
  annotate("rect", xmin=23, xmax=57, ymin=0, ymax=400, alpha=.1, fill="red") +
  annotate( "text", label="Sypsa", x=34.5, y=275,angle=90,colour="black", size=3) +
  annotate("rect", xmin=31,xmax=38,ymin=0,ymax=400,alpha=.2,fill="blue")



#ITALY
rownames(italy) <- seq(length=nrow(italy))
italy<-italy[59:273,]
rownames(italy) <- seq(length=nrow(italy))
italy$Date_reported<-factor(italy$Date_reported, levels=italy$Date_reported)
italy$New_cases<-as.numeric(italy$New_cases)

italy$New_cases<-ifelse(italy$New_cases <0,0,italy$New_cases)
italy$New_cases<-ifelse(is.na(italy$New_cases),0,italy$New_cases)

italy$NewCases7d_Avg<- zoo::rollmean(italy$New_cases, k=7, fill=NA)
summary(italy$NewCases7d_Avg)


It_plot<- ggplot(italy) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) + 
  theme_bw() + theme(legend.position="none") + labs(title="Italy") +
  scale_y_continuous("Number New Cases", limits = c(0,6000), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=italy, aes(xintercept=11),linetype="solid", colour="red",size=0.25) + 
  geom_vline(data=italy, aes(xintercept=65),linetype="solid", colour="red",size=0.25) + 
  annotate("rect", xmin=30, xmax=36, ymin=0, ymax=6000, alpha=.2, fill="blue")+ 
  annotate("text", label="Del Fava", x=33, y=3000,angle=90, colour="black", size=3)  +
  annotate("rect", xmin=11, xmax=65, ymin=0, ymax=6000, alpha=.1, fill="red")



#KENYA
rownames(kenya) <- seq(length=nrow(kenya))
kenya<-kenya[59:273,]
rownames(kenya) <- seq(length=nrow(kenya))
kenya$Date_reported<-factor(kenya$Date_reported, levels=kenya$Date_reported)
kenya$New_cases<-as.numeric(kenya$New_cases)

kenya$New_cases<-ifelse(kenya$New_cases <0,0,kenya$New_cases)
kenya$New_cases<-ifelse(is.na(kenya$New_cases),0,kenya$New_cases)

kenya$NewCases7d_Avg<- zoo::rollmean(kenya$New_cases, k=7, fill=NA)
summary(kenya$NewCases7d_Avg)


Ken_plot<- ggplot(kenya) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Kenya") +
  scale_y_continuous("Number New Cases", limits = c(0,800), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=kenya, aes(xintercept=37),linetype="solid",colour="darkgoldenrod1", size=0.25) + 
  geom_vline(data=kenya, aes(xintercept=129),linetype="solid",colour="darkgoldenrod1",size=0.25) + 
  annotate("rect", xmin=37, xmax=129, ymin=0, ymax=800, alpha=.2, fill="darkgoldenrod1") +
  annotate("rect", xmin=62, xmax=90, ymin=0, ymax=800, alpha=.2, fill="blue") +
  annotate("text", label="Quaife", x=76, y=500,size=3,angle=90, colour="black") 


#LUXEMBOURG
rownames(luxem) <- seq(length=nrow(luxem))
luxem<-luxem[59:273,]
rownames(luxem) <- seq(length=nrow(luxem))
luxem$Date_reported<-factor(luxem$Date_reported, levels=luxem$Date_reported)
luxem$New_cases<-as.numeric(luxem$New_cases)

luxem$New_cases<-ifelse(luxem$New_cases <0,0,luxem$New_cases)
luxem$New_cases<-ifelse(is.na(luxem$New_cases),0,luxem$New_cases)

luxem$NewCases7d_Avg<- zoo::rollmean(luxem$New_cases, k=7, fill=NA)
summary(luxem$NewCases7d_Avg)

Lux_plot<- ggplot(luxem) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Luxembourg") +
  scale_y_continuous("Number New Cases", limits = c(0,200), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=luxem, aes(xintercept=16),linetype="solid",colour="red",size=0.25) + 
  geom_vline( data=luxem, aes(xintercept=51),linetype="solid",colour="red",size=0.25) + 
  annotate("rect", xmin=16, xmax=51, ymin=0, ymax=200, alpha=.1, fill="red") +
  annotate("text", label="Latsuzbaia 1", x=58, y=100, angle=90, colour="black", size=3) +
  annotate("rect",xmin=24, xmax=63, ymin=0, ymax=200, alpha=.2, fill="blue")+
  annotate( "text", label="Latsuzbaia 2", x=110.5, y=100, angle=90,colour="black",size=3) + 
  annotate("rect", xmin=104, xmax=117, ymin=0, ymax=200, alpha=.2, fill="blue") 
  #annotate( "text", label="Latsuzbaia 3", x=47, y=130, angle=90,colour="black", size=3) +
  #annotate("rect", xmin=46, xmax=48, ymin=0, ymax=200, alpha=.2, fill="blue") +
  #annotate("text", label="Latsuzbaia 4", x=62, y=130, angle=90,colour="black", size=3) + 
  #annotate("rect", xmin=61, xmax=63, ymin=0, ymax=200, alpha=.2, fill="blue")


#NETHERLANDS
rownames(nether) <- seq(length=nrow(nether))
nether<-nether[59:273,]
rownames(nether) <- seq(length=nrow(nether))
nether$Date_reported<-factor(nether$Date_reported, levels=nether$Date_reported)
nether$New_cases<-as.numeric(nether$New_cases)

nether$New_cases<-ifelse(nether$New_cases <0,0,nether$New_cases)
nether$New_cases<-ifelse(is.na(nether$New_cases),0,nether$New_cases)

nether$NewCases7d_Avg<- zoo::rollmean(nether$New_cases, k=7, fill=NA)
summary(nether$NewCases7d_Avg)


Neth_plot<- ggplot(nether) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Netherlands") +
  scale_y_continuous("Number New Cases", limits = c(0,3000), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=nether, aes(xintercept=16),linetype="solid",size=0.25, colour="red") + 
  geom_vline(data=nether, aes(xintercept=72),linetype="solid",size=0.25, colour="red") + 
  annotate("rect", xmin=16, xmax=72, ymin=0, ymax=3000, alpha=.1, fill="red") +
  annotate("rect", xmin=32, xmax=61, ymin=0, ymax=3000,alpha=.2, fill="blue")+
  annotate("text", label="Backer", x=48, y=2000,angle=90,
           colour="black", size=3) +
  annotate("rect", xmin=30, xmax=36, ymin=0, ymax=3000, alpha=.2, fill="blue")+
  annotate("text", label="Del Fava", x=33, y=2000, colour="black", angle=90, size=3) +
  annotate("rect", xmin=93, xmax=122, ymin=0, ymax=3000, alpha=.2, fill="blue")+
  annotate("text", label="Backer 2", x=110, y=2000, colour="black", angle=90, size=3)


#SOUTH AFRICA
rownames(SA) <- seq(length=nrow(SA))
SA<-SA[59:273,]
rownames(SA) <- seq(length=nrow(SA))
SA$Date_reported<-factor(SA$Date_reported, levels=SA$Date_reported)
SA$New_cases<-as.numeric(SA$New_cases)

SA$New_cases<-ifelse(SA$New_cases <0,0,SA$New_cases)
SA$New_cases<-ifelse(is.na(SA$New_cases),0,SA$New_cases)

SA$NewCases7d_Avg<- zoo::rollmean(SA$New_cases, k=7, fill=NA)
summary(SA$NewCases7d_Avg)



SA_plot<- ggplot(SA) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="South Africa") +
  scale_y_continuous("Number New Cases", limits = c(0,13000), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=nether, aes(xintercept=27),linetype="solid",size=0.25, colour="red") + 
  geom_vline(data=nether, aes(xintercept=62),linetype="solid",size=0.25, colour="red") + 
  annotate("rect", xmin=27, xmax=62, ymin=0, ymax=13000, alpha=.1, fill="red") +
  annotate("rect", xmin=95, xmax=138, ymin=0, ymax=13000,alpha=.1, fill="blue")+
  annotate("text", label="McCreesh 1", x=116.5, y=7000,angle=90,colour="black", size=3) +
  annotate("rect", xmin=138, xmax=170, ymin=0, ymax=13000, alpha=.2, fill="blue")+
  annotate("text", label="McCreesh 2", x=154, y=7000, colour="black", angle=90, size=3)



#UK
rownames(UK) <- seq(length=nrow(UK))
UK<-UK[59:273,]
rownames(UK) <- seq(length=nrow(UK))
UK$Date_reported<-factor(UK$Date_reported, levels=UK$Date_reported)
UK$New_cases<-as.numeric(UK$New_cases)

UK$New_cases<-ifelse(UK$New_cases <0,0,UK$New_cases)
UK$New_cases<-ifelse(is.na(UK$New_cases),0,UK$New_cases)

UK$NewCases7d_Avg<- zoo::rollmean(UK$New_cases, k=7, fill=NA)
summary(UK$NewCases7d_Avg)


UK_plot<- ggplot(UK) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) +
  geom_line(aes(x=0, y= 0, group=1, colour="red")) + 
  theme_bw() + theme(legend.position="none") + labs(title="UK") +
  scale_y_continuous("Number New Cases", limits = c(0,7000), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                    "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red",'darkgoldenrod1'),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=UK, aes(xintercept=23),linetype="solid",size=0.25, colour="red") + 
  geom_vline(data=UK, aes(xintercept=71),linetype="solid",colour="red",size=0.25) + 
  annotate("rect", xmin=23, xmax=71, ymin=0, ymax=7000, alpha=.1, fill="red") +
  annotate("rect", xmin=30, xmax=36, ymin=0, ymax=7000, alpha=.2, fill="blue") + 
  annotate("text", label="Del Fava", x=33, y=4000, angle=90, colour="black", size=3) +
  annotate("rect", xmin=24, xmax=27, ymin=0, ymax=7000, alpha=.2, fill="blue")+
  annotate("text", label="Jarvis", x=25, y=5000, angle=90,  colour="black", size=3) 



#US
rownames(US) <- seq(length=nrow(US))
US<-US[59:273,]
rownames(US) <- seq(length=nrow(US))
US$Date_reported<-factor(US$Date_reported, levels=US$Date_reported)
US$New_cases<-as.numeric(US$New_cases)

US$New_cases<-ifelse(US$New_cases <0,0,US$New_cases)
US$New_cases<-ifelse(is.na(US$New_cases),0,US$New_cases)

US$NewCases7d_Avg<- zoo::rollmean(US$New_cases, k=7, fill=NA)
summary(US$NewCases7d_Avg)


US_plot<- ggplot(US) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="red")) +
  geom_line(aes(x=Date_reported, y=NewCases7d_Avg, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="US") +
  scale_y_continuous("Number New Cases", limits = c(0,70000), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("03/01/20","05/01/20","07/01/20",
                                   "09/01/20")) +
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("7-Day Average New COVID-19 Cases", "National Lockdown Start/End", "Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) +
  annotate("rect", xmin=22, xmax=39, ymin=0, ymax=70000, alpha=.2, fill="blue") +
  annotate("text", label="Feehan 1", x=30, y=46000,  colour="black",   size=3, angle=90) + 
  annotate("rect", xmin=109, xmax=115, ymin=0, ymax=70000, alpha=.2, fill="blue") +
  annotate("text", label="Feehan 2", x=112, y=46000,  colour="black",   size=3, angle=90) +
  annotate("rect", xmin=195, xmax=210, ymin=0, ymax=70000, alpha=.2, fill="blue") +
  annotate("text", label="Feehan 3", x=202.5, y=46000,  colour="black",   size=3, angle=90) +
  annotate("rect", xmin=15, xmax=62, ymin=0, ymax=70000, alpha=.2, fill="darkgoldenrod1") +
  geom_vline( data=US,  aes(xintercept=62), linetype="solid",colour="darkgoldenrod1", size=0.25) + 
  geom_vline(data=US, aes(xintercept=15),linetype="solid",colour="darkgoldenrod1",size=0.25) 


#PANEL
library(lemon)

grid_arrange_shared_legend(Bel_plot,Ch_plot,Fr_plot,Ger_plot,Gr_plot,
                           It_plot,Ken_plot,Lux_plot,Neth_plot,
                           SA_plot,UK_plot,US_plot, nrow=4,ncol=3,
                           top="Supplement XX: 7-Day Average New COVID-19 Cases and Survey Periods During Country Lockdowns January-September 2020")



