library("tidyverse")
library("lubridate")
library("waterData")
library("dplyr")
library("ggplot2")


source("script/dailyFunctions.R")


# Water discharge carpentry (dynamically updating local discharge file)
station = '02323500' 
stinfo  = siteInfo(station)
dis <- read_rds("rmd/data/dis.rds")
if (max(dis$dates) < (Sys.Date() - 5)) {
  sdate <- max(dis$dates) + 1
  newdis <- importDVs(staid = station, code = '00060', stat = '00003', 
                      sdate= as.character(sdate))
  dis <- bind_rows(dis, newdis) %>%
    distinct() # In case there's repeated rows due to code failure
  write_rds(dis, "rmd/data/dis.rds")
}


dis$Date<- ymd(dis$dates)

class(dis$Date)

period<-
ggplot(data=dis, aes(x=year(Date), y=val)) +
  stat_summary(fun.y=mean, geom= "line") +
  xlab("Date") +
  ylab("Daily Mean River Discharge (cfs)") +
  ggtitle("A. Suwannee River Discharge 1950-2018") +
  scale_y_continuous(limit=c(0,20000))+
  theme(legend.position=("top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13,face="bold"),
        plot.title =element_text(size=13, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1, face= "bold", size=15),
        axis.text.y = element_text(face= "bold", size=15),
        legend.title=element_blank())

setwd("T:/Oyster Project/project_task/t8_proj_admin/quarterly_report/figure")
ggsave("period.png", dpi=300, width= 10, height =6)

class(dis$Date)

#period1718<-

ggplot(data=dis, aes(x=Date, y=val)) +
  geom_line()+
  xlab("Date") +
  ylab("Daily Mean River Discharge (cfs)") +
  ggtitle("B. Suwannee River Discharge 2017-2018") +
  scale_y_continuous(limit=c(0,20000))+
  scale_x_date( limits = as.Date(c('2017-01-01','2018-12-30')))+
  #geom_smooth(se=FALSE) +
  theme(legend.position=("top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13,face="bold"),
        plot.title =element_text(size=13, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1, face= "bold", size=15),
        axis.text.y = element_text(face= "bold", size=15),
        legend.title=element_blank())

setwd("T:/Oyster Project/project_task/t8_proj_admin/quarterly_report/figure")
ggsave("period1718.png", dpi=300, width= 10, height =6)
  