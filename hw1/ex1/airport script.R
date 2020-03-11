library(mosaic)
library(tidyverse)
library(knitr)

ABIA <- read.csv("~/GitHub/SDS323_Spring2020/ex1/ABIA.csv")
#new variable total delay
ABIA = ABIA %>% mutate(totdelay = ArrDelay + DepDelay + 
                         CarrierDelay + WeatherDelay + NASDelay + SecurityDelay +LateAircraftDelay)

#omit NA
ABIA_edit <- na.omit(ABIA)

#fix labels
ABIA_edit <- mutate(ABIA_edit, DayOfWeek = 
                      factor(DayOfWeek,levels = c(1, 2, 3, 4, 5, 6, 7),
                             labels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
ABIA_edit <- mutate(ABIA_edit, Month = 
                      factor(Month,levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                             labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")))

#Cut CRSDepTime in Factors
ABIA_edit = ABIA_edit %>%
  mutate(tod_cat = cut(CRSDepTime, 
                       c(0000, 0100, 0200, 0300, 0400, 0500, 0600, 0700, 0800, 0900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400),
                       labels = c("12am-1am", "1am-2am", "2am-3am", "3am-4am", "4am-5am", "5am-6am", "6am-7am", "7am-8am", "8am-9am", "9am-10am", "10am-11am", "11am-12pm", "12pm-1pm", "1pm-2pm", "2pm-3pm", "3pm-4pm", "4pm-5pm", "5pm-6pm", "6pm-7pm", "7pm-8pm", "8pm-9pm", "9pm-10pm", "10pm-11pm", "11pm-12am")))


#plot avg total delay by day of week
bydow = ABIA_edit %>%
  group_by(DayOfWeek) %>%
  summarize(avg.delay = sum(totdelay)/n())
bydow

ggplot(data=bydow, aes(x=DayOfWeek, y=avg.delay)) +
  geom_bar(stat='identity') +
  labs(title= "Delays by Days of the Week", 
       x = "Day of the Week",
    y = "Average Delays (minutes)") + 
  coord_flip()

#plot avg total delay by time of day
bytod = ABIA_edit %>%
  group_by(tod_cat) %>%
  summarize(avg.delay = sum(totdelay)/n())
bytod

ggplot(data=bytod, aes(x=reorder(tod_cat, avg.delay), y=avg.delay)) +
  geom_bar(stat='identity') +
  labs(title= "Delays by Time of Departure", 
       x = "Scheduled Departure Time",
       y = "Average Delays (minutes)") + coord_flip()


#plot avg total delay by month
bym = ABIA_edit %>%
  group_by(Month) %>%
  summarize(avg.delay = sum(totdelay)/n())
bym

ggplot(data=bym, aes(x=Month, y=avg.delay)) +
  geom_bar(stat='identity') +
  labs(title= "Delays by Month", 
       x = "Month",
       y = "Average Delays (minutes)") + coord_flip()

#plot flight distance by days of the week
distance = ABIA_edit %>%
  group_by(DayOfWeek) %>%
  summarize(avg.distance = sum(Distance)/n())
distance

ggplot(data=distance, aes(x=DayOfWeek, y=avg.distance)) +
  geom_bar(stat='identity') +
  labs(title= "Average Distance Traveled by Days of the Week", 
       x = "Day of the Week",
       y = "Average Distance (miles)") + coord_flip()

