
library("cowplot")
library("devtools")
library("ggplot2")
library("ggpubr")
library("grid")
library("gridExtra")
library("lattice")
library("marelac")
library("scales")
library("ggpubr")

#Updated csv manually through FWC Commercial landings website
landings <- read.csv("data/landings.csv", header= T)


stland<-ggplot(data=landings, aes(x=Year, y=State_landings))+
  geom_line(size=2)+
  labs(x= "Year", y= "State Landings") +
  scale_x_continuous(limits=c(1986,2018), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016,2018)) +
  scale_y_continuous()+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


sttrip<-ggplot(data=landings, aes(x=Year, y=State_trips))+
  geom_line(size=2)+
  labs(x= "Year", y= "State Trips")+
  scale_x_continuous(limits=c(1986,2018), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016,2018)) +
  scale_y_continuous()+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


stpertrip<-
  ggplot(data=landings, aes(x=Year, y=State_per_trip))+
  geom_line(size=2)+
  labs(x= "Year", y= "State Per Trip")+
  scale_x_continuous(limits=c(1986,2018), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016,2018)) +
  scale_y_continuous()+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


apstland<-
  ggplot(data=landings, aes(x=Year, y=Apalach_landings)) +
  geom_line(size=2) +
  labs(x= "Year", y= "Apalachicola Landings") +
  scale_x_continuous(limits=c(1986,2018), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016,2018)) +
  scale_y_continuous()+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



apsttrip<-
  ggplot(data=landings, aes(x=Year, y=Apalach_trips))+
  geom_line(size=2)+
  labs(x= "Year", y= "Apalachicola Trips")+
  scale_x_continuous(limits=c(1986,2018), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016,2018))+
  scale_y_continuous()+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



apstpertrip<-
  ggplot(data=landings, aes(x=Year, y=Apalach_per_trip))+
  geom_line(size=2)+
  labs(x= "Year", y= "Apalachicola Per Trip")+
  scale_x_continuous(limits=c(1986,2018), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016,2018)) +
  scale_y_continuous()+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)




swstland<-
  ggplot(data=landings, aes(x=Year, y=Suw_landings))+
  geom_line(size=2)+
  labs(x= "Year", y= "Suwannee Sound Landings")+
  scale_x_continuous(limits=c(1986,2018), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016,2018)) +
  scale_y_continuous()+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



swsttrip<-
  ggplot(data=landings, aes(x=Year, y=Suw_trips))+
  geom_line(size=2)+
  labs(x= "Year", y= "Suwannee Sound Trips") +
  scale_x_continuous(limits=c(1986,2018), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016,2018)) +
  scale_y_continuous()+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



swstpertrip<-
  ggplot(data=landings, aes(x=Year, y=Suw_per_trip))+
  geom_line(size=2)+
  labs(x= "Year", y= "Suwannee Sound Per Trip")+
  scale_x_continuous(limits=c(1986,2018), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016,2018)) +
  scale_y_continuous()+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


alllandslines86_18<-
ggdraw() +
  draw_plot(swstland, x=0.34, y=0, width=0.3, height=0.30 ) +
  draw_plot(swsttrip, x=0.34, y=0.30, width=0.3, height=0.30 ) +
  draw_plot(swstpertrip, x=0.34, y=0.60, width=0.3, height=0.30 ) +
  draw_plot(stland, x=0, y=0, width=0.3, height=0.30 ) +
  draw_plot(sttrip, x=0, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(stpertrip, x=0, y=0.6, width=0.3, height=0.30 ) +
  draw_plot(apstland, x=0.67, y=0, width=0.3, height=0.30 ) +
  draw_plot(apsttrip, x=0.67, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(apstpertrip, x=0.67, y=0.6, width=0.3, height=0.30 )
ggsave("pic/alllandslines86_12.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')


suw_plots86_18<-plot_grid(swstland,swsttrip,swstpertrip, ncol=1)
ggsave("pic/suw_plots86_18.tiff", units="in", width=8, height=12, dpi=300, compression = 'lzw')


