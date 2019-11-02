library(tidyverse)
library(skimr)
library(lubridate)
library(riem)
library(ggpubr)

# data from TidyTuesday
df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")


# if tail twitching is a sign of happiness, then on whic day fo th emonth are they happiest?

# pull NYC weather data for observations 
weather <- riem_measures("NYC", date_start = "2018-10-06", date_end = "2018-10-21")

# extract day, create summary of mean temp each day in NYC
# tmpf is Air Temp in F, typically at 2 meters
weather <- weather %>%
  mutate(day = day(valid)) %>%
  group_by(day) %>%
  summarize(mean_temp = mean(tmpf, na.rm = TRUE))

# silly assumption: 
# tail_twitches, kuks, quaas = "danger/unhappy", 
# moan and eating = "happy" (in squirrel..., see this explanation 
# https://www.washingtonpost.com/local/learn-to-speak-squirrel-in-four-easy-lessons/2012/04/09/gIQAV8Jr6S_story.html

# question, does daily temp affect the happiness ratio in squirrels?

# define ratio/happy_quo
df <- df %>% 
  mutate(day = day(mdy(date))) %>%
  group_by(day) %>%
  summarize(sum_twitch = sum(tail_twitches),
            sum_eat = sum(eating), 
            sum_quaa = sum(quaas),
            sum_kuk = sum(kuks), 
            sum_moan = sum(moans)) %>%
  group_by(day) %>%
  mutate(happy_quo = sum(sum_moan,sum_eat)/sum(sum_twitch, sum_quaa, sum_kuk) - 1)

# join df and weather by day
df <- df %>%
  inner_join(weather) 


# plot happy_quo by temp
plot <- ggplot(df, aes(x = mean_temp, y = happy_quo, fill = happy_quo < 0)) +
  geom_col() +
  scale_fill_manual(breaks = c(TRUE, FALSE), labels = c("positive", "negative"), values=c("blue", "red"))+
  labs(x = "Mean temperature (Fahrenheit)",
       y = "Happiness Quotient (Happy : Unhappy)\n", 
       title = "Happiness in NYC Squirrels v. Average Daily Temperature",
       subtitle = "Observations in 3018 individual animals in October, 2018",
       caption = "
       Happy indicators: Moan, Foraging Behavior; Unhappy indicators: Tail-twitch, Quaa, Kuk\n
       Weather data collected from Automated Surface Observing System (ASOS) stations in NYC", 
       fill = "Happiness Quotient")+
  xlim(45, 75)+
  theme(plot.title = element_text(face = "bold", size = 18, color = "black"),
        plot.subtitle = element_text(face = "italic", size = 15),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15, face = "bold", color = "black"), 
        plot.caption = element_text(size = 11, face = "italic", color = "black", hjust = 0.5), 
        legend.position = "top", 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))


ggsave('happiness_plot.tiff', plot, height = 5, width = 9, units= 'in', dpi = 150)

