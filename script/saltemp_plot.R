library("tidyverse")
library("lubridate")
library("waterData")
library("dplyr")
library("ggplot2")

source("script/dailyFunctions.R")

# Sensor water quality carpentry
wq <- read.csv("data/wq.csv", header= T) %>%
  filter(Site != 0)
wq$Date <- wq$Date %>% ymd_hms %>% round_date(unit="hour")
wq$Site <- factor(wq$Site, levels = c("6", "1", "7", "5", "2", "8","4", "3", "9"))

# Water discharge carpentry (dynamically updating local discharge file)
station = '02323500' 
stinfo  = siteInfo(station)
dis <- read_rds("data/dis.rds")
if (max(dis$dates) < (Sys.Date() - 5)) {
  sdate <- max(dis$dates) + 1
  newdis <- importDVs(staid = station, code = '00060', stat = '00003', 
                      sdate= as.character(sdate))
  dis <- bind_rows(dis, newdis) %>%
    distinct() # In case there's repeated rows due to code failure
  write_rds(dis, "data/dis.rds")
}

# Define start and end date of the plot
####ALWAYS CHANGE THIS DATE TO THE LAST UPLOAD TIME############################
startDate <- ymd_hms("2017-08-16 00:00:00")
endDate <- ymd_hms("2018-10-04 23:00:00")


# Create sequence of dates (so that NA is recognized by geom_line) and build a table of
# salinity and temperature (remove other irrelevant ones)
d <- seq(startDate, endDate, by = "hour")
sal_temp <- expand(wq, Site, Date=d) %>%
  left_join(wq, by=c("Site" = "Site", "Date" = "Date")) %>%
  select(Site, Date, Salinity, Temperature)
sal_temp$d2 <- date(sal_temp$Date)

# Calculate mean, min and max daily Sal and Temp
# Note: tidyr's gather is in use here, by converting the "wide" table to
# "long" one, we can use the aes(color=Measure) in ggplot
# Note2: I've created functions in dailyFunction.R to make calculating daily mean, min,
# max easy. The function allows certain proportions of NA here. In this example, 0.75
# of the daily numbers can be NA, and we'll still calculate the meant, min, max
sal_temp_summ <- sal_temp %>%
  gather(key = "Measure", value = "Value", Salinity, Temperature) %>%
  group_by(Site, d2, Measure) %>%
  summarise(meanVal = dailyMean(Value, 0.75), 
            minVal = dailyMin(Value, 0.75), 
            maxVal = dailyMax(Value, 0.75)) %>%
  ungroup()

dis2 <- dis %>%
  mutate(Datetime = paste(dates, " 12:00:00") %>% ymd_hms()) %>%
  filter(Datetime >= startDate & Datetime <= endDate)

dis3 <- dis2 %>%
  expand(Site=unique(sal_temp_summ$Site), dates) %>%
  left_join(dis2) %>%
  mutate(Measure = "Discharge", minVal = -20, maxVal = val/1000 - 20) %>%
  select(Site, d2 = dates, Measure, minVal, maxVal)

std_summ <- bind_rows(sal_temp_summ, dis3)


saltemp<-ggplot() +
  geom_ribbon(data=dis2, aes(x=dates, ymax=val/1000 - 20, ymin=-20), 
              alpha=0.5, fill= "#56B4E9") +
  geom_line(data=std_summ, aes(x=d2, y=meanVal, color=Measure), size=1.2) +
  geom_ribbon(data=std_summ, aes(x=d2, ymax=maxVal, ymin=minVal, fill=Measure), 
              alpha=0.5) +
  xlab("Date") +
  scale_y_continuous(name = "Salinity(ppt) & Temperature (C)", 
                     limits=c(-20,40), 
                     breaks = seq(0, 40, 10),
                     sec.axis = sec_axis(~(.+20), 
                                         name = "River Discharge (1,000 cfs)
                                         ",
                                         breaks = seq(0, 20, 10))) +
  scale_x_date(date_breaks = "2 months", date_labels = "%y-%m-%d", expand = c(0, 0)) +
  
  scale_fill_manual(values = c("#56B4E9", "#0072B2", "#D55E00")) +
  
  scale_color_manual(values = c("#56B4E9", "#0072B2", "#D55E00")) +
  
  theme(legend.position=("top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13,face="bold"),
        plot.title =element_text(size=13, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1, face= "bold"),
        axis.text.y = element_text(face= "bold"),
        legend.title=element_blank()) +
  
  guides(color = guide_legend(override.aes = list(linetype = c(0, 1,1)))) +
  
  facet_wrap(~ Site, ncol=3, labeller = label_both)


ggsave("fig/saltemp_plot.png", dpi=300, width= 12, height =10)

