
# When should I plant my garden?


library(ggplot2)
library(weathercan)
library(dplyr)
library(lubridate)


weathercan::stations_search(name = "Fort St. John")

fsj.recent <- weather_dl(station_ids = 50837, interval= "day") #defaults to full range of dates

str(fsj.recent)

unique(fsj.recent$year)




#plot minimum daily temps
ggplot(fsj.recent)+
  geom_line(aes(x=date, y=min_temp), col="blue")+
  geom_hline(yintercept=5, col="blue")



# Can I safely plants my tomatoes outside on May long weekend?

#tool to find Vicoria Day long weekend year by year:
yr <- 2014
dates <- as.character(seq(ymd(paste(yr,"-05-18")),ymd(paste(yr,"-05-24")),by = "days"))
wday(dates, label = T, abbr = F)
data.frame(dates, wday=wday(dates,label = T, abbr = F))

may.long <- ymd("2012-05-21","2013-05-20","2014-05-19",
                "2015-05-18","2016-05-23", "2017-05-22", "2018-05-21","2019-05-20")
(may.long.df <- data.frame(year=2012:2019, date = may.long))




# what were the temps on Victoria day?


min_temp_on_may_long <- may.long.df %>% 
  left_join(fsj.recent, by=("date"))

min_temp_on_may_long$min_temp

plot_maytemps <- ggplot(data=min_temp_on_may_long)+
  geom_point(aes(x=year.y, y=min_temp))+
  geom_hline(yintercept=5, col="blue")+
  theme_bw()

plot_maytemps




library(cowplot)
library(magick)
ggdraw() +
  draw_plot(plot_maytemps)+
  draw_image("256px-Tomaten_tomatoes_pomodori.jpg", scale=0.3, hjust=-0.35, vjust=.2) 




# okay, but maybe this year is a warm one! How is 2020 shaping up?

# look to see cumulative degree days with mean temps over 5 degrees 
  # since the start of the year :

str(fsj.recent)
range(fsj.recent$date)

fsj.dd <- fsj.recent %>% 
  select(date, year, month, day, max_temp, mean_temp, min_temp) %>% 
  group_by(year) %>% 
  mutate(jday = yday(date), month = as.numeric(month), day= as.numeric(day),
         over.5deg = 0) %>% 
  mutate(over.5deg=ifelse(!is.na(mean_temp) & mean_temp>=5, mean_temp-5,over.5deg)) %>%
  mutate(cumulative.dd = cumsum(over.5deg)) %>% 
  filter(month %in% 4:5)

fsj.dd


#plot cumulative degree days >= 5deg C

ggplot(fsj.dd)+
  geom_line(aes(x=jday, y=cumulative.dd, col=cumulative.dd), size=2)+
  geom_vline(xintercept=yday(ymd("2020-05-11")))+
  scale_color_gradient("cumulative\n degrees",low="blue", high="red") +
  facet_wrap(~year) +
  labs(title="Cumulative degree days >=5 deg C, April-May", y="")
 



#.... in the end... no answers were found :(

  


  
  
  
  