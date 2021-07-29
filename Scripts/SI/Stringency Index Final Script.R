#Stringency Index Panel Final

data.src<-file.path('/Users/jjetter280/Desktop/CountryData/Stringency Indices')

stringency<- read.csv(file.path(data.src,"covid-stringency-index.csv"),header = FALSE)
names(stringency)<-stringency[1,]

#SUBSET DATA
belgium<-subset(stringency, Entity=="Belgium")
china<-subset(stringency, Entity=="China")
france<-subset(stringency, Entity=="France")
germany<-subset(stringency, Entity=="Germany")
greece<-subset(stringency, Entity=="Greece")
luxem<-subset(stringency, Entity=="Luxembourg")
italy<-subset(stringency, Entity=="Italy")
kenya<-subset(stringency, Entity=="Kenya")
SA<-subset(stringency, Entity=="South Africa")
nether<-subset(stringency, Entity=="Netherlands")
UK<-subset(stringency, Entity=="United Kingdom")
US<-subset(stringency, Entity=="United States")

#STRINGENCY INDEX PLOTS
library(ggplot)

belgium<-belgium[1:366,]
rownames(belgium)<-seq(length=nrow(belgium))
belgium$stringency_index<-as.numeric(belgium$stringency_index)
belgium$Date<-factor(belgium$Date, level=belgium$Date)

Bel_plot<- ggplot(belgium) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Belgium")+
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20")) +
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End"))+
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period", "National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1))+
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=belgium, aes( xintercept=74),linetype="solid",size=0.25, colour="red") + 
  geom_vline(data=belgium,  aes(xintercept=126),linetype="solid",size=0.25, colour="red") + 
  annotate("rect", xmin=74, xmax=126, ymin=0, ymax=100, alpha=.1, fill="red")+
  annotate("rect", xmin=93, xmax=122, ymin=0, ymax=100, fill="blue", alpha=.2)+ annotate(
    "text", label="Colletti 1", x=110, y=50, colour="black", size=3, angle=90)+
  annotate(
    "text", label="Del Fava", x=93.5, y=70, colour="black", size=3, angle=90) +
  annotate("rect", xmin=90, xmax=97, ymin=0, ymax=100, alpha=.2, fill="blue")+
  annotate( "text", label="Coletti 2", x=160, y=50, colour="black", size=3, angle=90)+
  annotate("rect", xmin=157, xmax=163, ymin=0, ymax=100, alpha=.2, fill="blue") +
  annotate( "text", label="Coletti 3", x=202, y=50, colour="black", size=3, angle=90)+
  annotate("rect", xmin=199, xmax=205, ymin=0, ymax=100, alpha=.2, fill="blue")


#CHINA
china<-china[1:366,]
rownames(china)<-seq(length=nrow(china))
china$stringency_index<-as.numeric(china$stringency_index)
china$Date<-factor(china$Date, level=china$Date)


Ch_plot<- ggplot(china, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="China") +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "yellow")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20"))+
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period",
                              "Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=china, aes(xintercept=24),linetype="solid", colour="darkgoldenrod1", size=0.25) + 
  geom_vline(data=china, aes(xintercept=100),linetype="solid",colour="darkgoldenrod1",size=0.25) + 
  annotate("rect", xmin=24, xmax=100, ymin=0, ymax=100, alpha=.2, fill="darkgoldenrod1") + 
  annotate("text", label="Zhang 1", angle=90,x=37.5, y=40, colour="black", size=3) +
  annotate("rect", xmin=35, xmax=41, ymin=0, ymax=100, alpha=.2, fill="blue") +
  annotate( "text",  label="Zhang 2", x=72, y=40, size=3,angle=90,colour="black") +
  #annotate("text",  label="/Changsa", x=73, y=90, size=3,angle=90,colour="black") +
  annotate("rect", xmin=62, xmax=81, ymin=0, ymax=100, alpha=.2, fill="blue") +
  annotate("rect", xmin=129, xmax=137, ymin=0, ymax=100, alpha=.2, fill="blue")+
  annotate( "text",  label="Zhang 3", x=133, y=40, size=3,angle=90,
            colour="black") 



#FRANCE
france<-france[1:366,]
rownames(france)<-seq(length=nrow(france))
france$stringency_index<-as.numeric(france$stringency_index)
france$Date<-factor(france$Date, level=france$Date)


Fr_plot<- ggplot(france, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="France") +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20"))+
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period",
                              "Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=france, aes(xintercept=78), linetype="solid",colour="red",size=0.25) + 
  geom_vline(data=france, aes(xintercept=132),linetype="solid", colour="red",size=0.25) + 
  annotate("rect", xmin=78, xmax=132, ymin=0, ymax=100, alpha=.1, fill="red") +
  annotate("text", label="Del Fava", x=93.5, y=50,angle=90, 
           colour="black", size=3)+
  annotate("rect", xmin=90, xmax=97, ymin=0, ymax=100, alpha=.2, fill="blue") +
  annotate(
    "rect", xmin=110, xmax=120, ymin=0, ymax=100, alpha=.2, fill="blue") + annotate(
      "text", label="Bosetti", x=115, y=50, size=3,angle=90,
      colour="black") 


#GERMANY
germany<-germany[1:366,]
rownames(germany)<-seq(length=nrow(germany))
germany$stringency_index<-as.numeric(germany$stringency_index)
germany$Date<-factor(germany$Date, level=germany$Date)


Ger_plot<- ggplot(germany, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Germany") +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20"))+
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period",
                              "Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) +
  geom_vline(data=germany, aes(xintercept=82),linetype="solid",size=0.25, colour="red") + 
  geom_vline(data=germany, aes(xintercept=127),linetype="solid",size=0.25, colour="red") + 
  annotate("rect", xmin=82, xmax=127, ymin=0, ymax=100, alpha=.1, fill="red") +
  annotate("rect", xmin=83, xmax=89, ymin=0, ymax=100, alpha=.2, fill="blue") + 
  annotate("text", label="Del Fava", x=86, y=40, angle=90,
           colour="black", size=3)




#GREECE
greece<-greece[1:366,]
rownames(greece)<-seq(length=nrow(greece))
greece$stringency_index<-as.numeric(greece$stringency_index)
greece$Date<-factor(greece$Date, level=greece$Date)


Gr_plot<- ggplot(greece, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Greece")+
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01"))+
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period",
                              "Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1)))+ 
  geom_vline(data=greece, aes(xintercept=83),linetype="solid",colour="red",size=0.25) + 
  geom_vline(data=greece, aes(xintercept=117),linetype="solid",colour="red",size=0.25) + 
  annotate("rect", xmin=83, xmax=117, ymin=0, ymax=100, alpha=.1, fill="red") +
  annotate("text", label="Sypsa", x=94.7, y=55,angle=90,colour="black", size=3) +
  annotate("rect", xmin=91,xmax=98,ymin=0,ymax=100,alpha=.2,fill="blue")



#ITALY
italy<-italy[1:366,]
rownames(italy)<-seq(length=nrow(italy))
italy$stringency_index<-as.numeric(italy$stringency_index)
italy$Date<-factor(italy$Date, level=italy$Date)


It_plot<- ggplot(italy, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Italy") +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20")) +
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period",
                              "Regional Lockdown Period'"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1))) + 
  geom_vline(data=italy, aes(xintercept=71),linetype="solid", colour="red",size=0.25) + 
  geom_vline(data=italy, aes(xintercept=124),linetype="solid", colour="red",size=0.25) + 
  annotate("rect", xmin=89, xmax=96, ymin=0, ymax=100, alpha=.2, fill="blue")+ 
  annotate("text", label="Del Fava", x=92.5, y=60,angle=90, colour="black", size=3) +
  annotate("rect", xmin=71, xmax=124, ymin=0, ymax=100, alpha=.1, fill="red")




#KENYA
kenya<-kenya[1:366,]
rownames(kenya)<-seq(length=nrow(kenya))
kenya$stringency_index<-as.numeric(kenya$stringency_index)
kenya$Date<-factor(kenya$Date, level=kenya$Date)


Ken_plot<- ggplot(kenya, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Kenya")+
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20")) +
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period",
                              "Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1)))+
  geom_vline(data=kenya, aes(xintercept=97),linetype="solid",colour="darkgoldenrod1",size=0.25) + 
  geom_vline(data=kenya, aes(xintercept=189),linetype="solid",colour="darkgoldenrod1",size=0.25) + 
  annotate("rect", xmin=97, xmax=189, ymin=0, ymax=100, alpha=.2, fill="darkgoldenrod1") +
  annotate("rect", xmin=122, xmax=152, ymin=0, ymax=100, alpha=.2, fill="blue") +
  annotate(
    "text", label="Quaife", x=137, y=55,size=3,angle=90, 
    colour="black") 




#LUXEMBOURG
luxem<-luxem[1:366,]
rownames(luxem)<-seq(length=nrow(luxem))
luxem$stringency_index<-as.numeric(luxem$stringency_index)
luxem$Date<-factor(luxem$Date, level=luxem$Date)


Lux_plot<- ggplot(luxem, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Luxembourg")+
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20")) +
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown
                              Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1)))+ 
  geom_vline(data=luxem, aes(xintercept=76),linetype="solid",colour="red",size=0.25) + 
  geom_vline(data=luxem, aes(xintercept=111),linetype="solid",colour="red",size=0.25) + 
  annotate("rect", xmin=76, xmax=111, ymin=0, ymax=100, alpha=.1, fill="red") +
  annotate("text", label="Latsuzbaia 1", x=103.5, y=50, angle=90, colour="black", size=3) +
  annotate("rect",xmin=85, xmax=122, ymin=0, ymax=100, alpha=.2, fill="blue")+
  annotate("text", label="Latsuzbaia 2", x=171.5, y=50, angle=90, colour="black",size=3) + 
  annotate("rect", xmin=164, xmax=177, ymin=0, ymax=100, alpha=.2, fill="blue") 


#NETHERLANDS
nether<-nether[1:366,]
rownames(nether)<-seq(length=nrow(nether))
nether$stringency_index<-as.numeric(nether$stringency_index)
nether$Date<-factor(nether$Date, level=nether$Date)

Neth_plot<- ggplot(nether, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="Netherlands")+
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=12, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20")) +
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End",
                                 "Regional Lockdown Start/End'")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period","Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1)))+ 
  geom_vline(data=nether, aes(xintercept=76),linetype="solid",size=0.25, colour="red") + 
  geom_vline(data=nether, aes(xintercept=132),linetype="solid",size=0.25, colour="red") + 
  annotate("rect", xmin=76, xmax=132, ymin=0, ymax=100, alpha=.1, fill="red") +
  annotate("rect", xmin=92, xmax=121, ymin=0, ymax=100,alpha=.2, fill="blue")+
  annotate("text", label="Backer", x=106.5, y=30,angle=90,colour="black", size=3) +
  annotate("rect", xmin=89, xmax=96, ymin=0, ymax=100, alpha=.2, fill="blue")+
  annotate("text", label="Del Fava", x=92.5, y=70, colour="black", angle=90, size=3) +
  annotate("rect", xmin=153, xmax=182, ymin=0, ymax=100, alpha=.2, fill="blue")+
  annotate("text", label="Backer 2", x=167.5, y=30, colour="black", angle=90, size=3)







#SOUTH AFRICA
SA<-SA[1:366,]
rownames(SA)<-seq(length=nrow(SA))
SA$stringency_index<-as.numeric(SA$stringency_index)
SA$Date<-factor(SA$Date, level=SA$Date)


SA_plot<- ggplot(SA, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="South Africa")+
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20")) +
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period",
                              "Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1)))+ 
  geom_vline(data=nether, aes(xintercept=87),linetype="solid",size=0.25, colour="red") + 
  geom_vline(data=nether, aes(xintercept=122),linetype="solid",size=0.25, colour="red") + 
  annotate("rect", xmin=87, xmax=122, ymin=0, ymax=100, alpha=.1, fill="red") +
  annotate("rect", xmin=155, xmax=198, ymin=0, ymax=100,alpha=.1, fill="blue")+
  annotate("text", label="McCreesh 1", x=176.5, y=40,angle=90,colour="black", size=3) +
  annotate("rect", xmin=198, xmax=230, ymin=0, ymax=100, alpha=.2, fill="blue")+
  annotate("text", label="McCreesh 2", x=214, y=40, colour="black", angle=90, size=3)




#UK
UK<-UK[1:366,]
rownames(UK)<-seq(length=nrow(UK))
UK$stringency_index<-as.numeric(UK$stringency_index)
UK$Date<-factor(UK$Date, level=UK$Date)


UK_plot<- ggplot(UK, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="UK")+
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20")) +
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period",
                              "Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1)))+ 
  geom_vline(data=UK, aes(xintercept=83),linetype="solid", size=0.25, colour="red") + 
  geom_vline(data=UK, aes(xintercept=131),linetype="solid",colour="red",size=0.25) + 
  annotate("rect", xmin=83, xmax=131, ymin=0, ymax=100, alpha=.1, fill="red") +
  annotate("rect", xmin=89, xmax=96, ymin=0, ymax=100, alpha=.2, fill="blue") + 
  annotate("text", label="Del Fava", x=92.5, y=30, angle=90, colour="black", size=3) +
  annotate("rect", xmin=84, xmax=87, ymin=0, ymax=100, alpha=.2, fill="blue")+
  annotate("text", label="Jarvis", x=85.5, y=70, angle=90,  colour="black", size=3) 



#US
US<-US[1:366,]
rownames(US)<-seq(length=nrow(US))
US$stringency_index<-as.numeric(US$stringency_index)
US$Date<-factor(US$Date, level=US$Date)


US_plot<- ggplot(US, aes(Date, stringency_index)) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="dodgerblue4")) + 
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="red")) +
  geom_line(aes(x=Date, y= stringency_index, group=1, colour="darkgoldenrod1")) +
  theme_bw() + theme(legend.position="none") + labs(title="US")+
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "blue")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "red")) +
  geom_ribbon(aes(x=27, ymin=0, ymax=0, fill= "darkgoldenrod1")) +
  scale_y_continuous("Stringency Index", limits = c(0,100), expand=c(0,0)) +
  scale_x_discrete("Date", breaks=c("01/01/20","03/01/20","05/01/20",
                                    "07/01/20","09/01/20","11/01/20")) +
  theme(axis.text.x=element_text(size=7))+
  scale_colour_manual(name=NULL, 
                      values = c("dodgerblue4","red","darkgoldenrod1"),
                      labels = c("Stringency Index","National Lockdown Start/End","Regional Lockdown Start/End")) +
  scale_fill_manual(name = NULL, 
                    labels= c("Survey Period","National Lockdown Period",
                              "Regional Lockdown Period"),
                    values = adjustcolor(c("blue","red","darkgoldenrod1"), alpha.f = 0.1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.1)))+
  annotate("rect", xmin=88, xmax=99, ymin=0, ymax=100, alpha=.2, fill="blue") +
  annotate("text", label="Feehan 1", x=93.5, y=35,  colour="black",   size=3, angle=90) + 
  annotate("rect", xmin=169, xmax=175, ymin=0, ymax=100, alpha=.2, fill="blue") +
  annotate("text", label="Feehan 2", x=172, y=35,  colour="black",   size=3, angle=90) +
  annotate("rect", xmin=255, xmax=270, ymin=0, ymax=100, alpha=.2, fill="blue") +
  annotate("text", label="Feehan 3", x=262.5, y=35,  colour="black",   size=3, angle=90)+
  annotate("rect", xmin=76, xmax=122, ymin=0, ymax=100, alpha=.2, fill="darkgoldenrod1") +
  geom_vline(data=US, aes(xintercept=76),linetype="solid",colour="darkgoldenrod1",size=0.25) + 
  geom_vline(data=US, aes(xintercept=122),linetype="solid",colour="darkgoldenrod1", size=0.25)




#COMBINED PLOT
library(lemon)

grid_arrange_shared_legend(Bel_plot,Ch_plot,Fr_plot,Ger_plot,Gr_plot,
                           It_plot,Ken_plot,Lux_plot,Neth_plot,SA_plot,
                           UK_plot,US_plot, nrow=4,ncol=3,
                           top="Figure 2: Composite Government Stringency Indices and Survey Periods During COVID-19 Pandemic January 2020-December 2020")









