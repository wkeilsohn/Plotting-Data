#### William Keilsohn
#### Ploting Data

### Load Packages
library(ggplot2)
library(plyr)

### Plots Algae biomass as a function of rock shelter

## Bar Chart
ALGD<-na.omit(ALGD)
ALGD$year<-as.factor(ALGD$year)
ALGD2<-ddply(ALGD, .(year, month), summarize, mean = mean(biomass), 
             sd = sd(biomass), se = sd(biomass) / sqrt(length(biomass)))
ALGD3<-ddply(ALGD, .(exposure, position), summarize, mean = mean(biomass), 
             sd = sd(biomass), se = sd(biomass) / sqrt(length(biomass)))

Plot1<-ggplot(ALGD3, aes(x = exposure, y = mean, fill = position))+
  geom_bar(stat="identity", position = position_dodge(), width = 0.5)+
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), width = 0.2, position = position_dodge(0.5))+
  scale_x_discrete("", labels = c("Exposed", "Sheltered")) + scale_y_continuous(expression 
                                                                                ("Algae Biomass"~ (kg/m ^2)), expand = c(0,0))+
  theme_classic()
theme1<-theme(axis.text.y = element_text(colour = "black", size = 20),
              axis.title.y = element_text(colour = "black", size = 20),
              axis.title.x = element_text(colour = "black", size = 20), 
              axis.text.x =element_text(colour = "black", size = 20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18), 
              legend.key.size = unit(2,"line"))
Plot1<-Plot1+theme1
Plot1<-Plot1+scale_fill_manual(values = c("darkorange3", "goldenrod2", "darkgreen"),
                               name = "Intertidal Level", labels = c("Low", "Middle", "High"))
Plot1

## Point plot, with error bars and yearly divisions
AGLD4<-ddply(ALGD, .(exposure, year, season), summarize, mean = mean(biomass), 
             sd = sd(biomass), se = sd(biomass) / sqrt(length(biomass)))


Plot2<-ggplot(AGLD4, aes(x=exposure, y=mean, shape = year))+
  geom_point(size=3, position = position_dodge(1))+
  #scale_shape_manual(values = c(1,8,16))+
  geom_errorbar(aes(ymax = mean+se, ymin = mean-se), width = 0.2, position = position_dodge(1))+
  scale_x_discrete("Exposure", labels=c("Exposed","Sheltered")) + 
  scale_y_continuous(expression("Algae Biomass"~ (kg/m ^2)))+
  theme_classic()
theme2<- theme(axis.text.y = element_text(colour = "black", size = 10), 
               axis.title.y = element_text(colour = "black", size = 10),
              axis.title.x = element_text(colour = "black", size = 10),
              axis.text.x =element_text(colour = "black", size = 10), 
              legend.text=element_text(size=8), 
              legend.title=element_text(size=10),
              legend.key.size = unit(4,"line"))
Plot2<-Plot2+theme2
Plot2<-Plot2+ scale_shape_manual(values = c(1,8,16),
                                 name =" ",
                                  labels=c("Year 2013", "Year 2014", "Year 2015"))
Plot2<-Plot2+facet_wrap(~season)
Plot2<-Plot2+theme(legend.position="right",
                   strip.text.x = element_text(size = 10),
                   strip.background = element_blank()) 
Plot2

### Bar chart with stacked yearly divisions
AGLD5<-ddply(ALGD, .(season, year, position), summarize, mean = mean(biomass), 
             sd = sd(biomass), se = sd(biomass) / sqrt(length(biomass)))

Plot3<-ggplot(AGLD5, aes(x = season, y = mean))+
  geom_bar(stat = "identity", aes(fill=position))+
  scale_x_discrete("Season", labels = c("Autumm", "Spring","Summer"))+
  scale_y_continuous(expression ("Algae Biomass"~ (kg/m ^2)), expand = c(0,0))+
  theme_classic()+
  theme1+
  scale_fill_manual(values = c("darkorange3", "goldenrod2", "darkgreen"),
                    name = "Intertidal Level", labels = c("Low", "Middle", "High"))+
  facet_wrap(~year)+
  theme(legend.position = "right",
        strip.text.x = element_text(size = 20),
        strip.background = element_blank())
Plot3



