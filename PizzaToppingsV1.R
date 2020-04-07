#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
# Project:      MakeoverMonday 2020 Week 13: Does Pineapple Belong on Pizza?
# Programmer:   Angela The 
# Data:         YouGov Poll: Feb 26-28, 2017 (n=3683 GB Adults)
# URL:          https://query.data.world/s/tan5cwzw7qocji4ulcen5udbeyuciw
# Resources:    Cleveland Dot Plots: https://uc-r.github.io/cleveland-dot-plots
#               ggplot2: https://ggplot2.tidyverse.org/reference/theme.html 
#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------


#------------------------[0] Install Packages
library(Hmisc)

library(ggplot2)
library(tidyr)
library(dplyr)

library(ggthemes)
library(colourpicker)
library(gganimate)
library(plotly)

library(httr)
library(readxl)


#----------------------- [1] Set WD and Load Pizza Data
setwd("~/Desktop/Data for Journal/Current Project/2020:W13 Pizza")
pizza <- read_xlsx("PizzaWeek.xlsx")


#----------------------- [2] Edit Data

#Add var -- Type of Topping: Vegetarian vs. Meat
pizza$Type <- ifelse (
  (pizza$Topping %in% c("Ham", "Chicken", "Pepperoni",
                         "Bacon", "Beef", "Tuna", "Pork", 
                         "Anchovies", "Something else"))
  , "Meat", "Vegetarian")
  
#MAIN dataframe: Pizza1
pizza1 <- pizza %>%
  gather('Male', 'Female', key="Gender", value="GrpPct") %>%      #Separate rows for gender
  arrange(desc(Total)) %>%    #Arrange rows by descending total percent
  filter(Topping != "Something else")   #Remove "Something else" topping

# New DS: toppings w/ percent differences over 10% btwn genders
sigdiff <- pizza1 %>% 
  spread(Gender, GrpPct) %>% 
  group_by(Topping) %>% 
  mutate(Max = max(Female, Male),
         Min = min(Female, Male),
         Diff = Max - Min) %>% 
  arrange(desc(Diff)) %>%
  filter(Diff > 10)

# New DS: filter main data to only include toppings where gender Diff > 10%
highlight <- filter(pizza1, Topping %in% sigdiff$Topping)

#New DS: Right Labels
right_label <- pizza1 %>%
  group_by(Topping) %>%
  arrange(desc(GrpPct)) %>%
  dplyr::top_n(1) %>%
  ungroup(Topping) %>%
  filter(Topping %in% sigdiff$Topping)

# New DF: Labels
plot_label <- sigdiff %>%
  select(Topping, GrpPct = Max, Diff) %>%
  dplyr::right_join(right_label)


#----------------------- [3] Cleveland Dot Plot / by Gender

p <- 
  #Create base plot
  ggplot(data=pizza1, aes(x=GrpPct, y=reorder(Topping, Total))) + 
  geom_line(aes(group=Topping), alpha=0.3) +
  geom_point(aes(color=Gender), size=3, alpha=0.3) +
  
  
  #Indicates which toppings should be highlighted (Diff>10%)
  geom_line(data=highlight, aes(group=Topping)) +
  geom_point(data=highlight, aes(color= Gender), size=5) +
  
  
  #Add Diff% label for highlighted toppings
  geom_text(data = plot_label, aes(color = Gender, 
                                   label = paste0("+", Diff, "%")),
            size = 3, hjust = -.5)


pizzaplot <- p + 
  #Scale
  scale_color_discrete(labels = c("Female", "Male")) +
  scale_x_continuous(expand = c(0.02, 0), 
                     limits = c(10, 70),
                     breaks = seq(10, 70, by = 10)) +
  scale_y_discrete(expand = c(0, 1)) +
  
  #Titles & axis titles
  labs(title = "Pizza Topping Favorability by Gender",
       subtitle = "In a Feb 2017 YouGov poll conducted among 3683 British adults, people were asked whether they 
       \nlike certain toppings on pizza. Out of the 18 toppings, 7 toppings had favorability differences 
       \nof greater than 10% between men and women.",
       caption = "YouGov (Feb 26-28, 2017): https://yougov.co.uk/topics/politics/articles-reports/2017/03/06/does-pineapple-belong-pizza",
       y = "Topping",
       x = "Percent") +
  
  #Theme
  theme_minimal() +
  theme(axis.title = element_text(size=12, face="bold", margin = margin(l=10, b=10, t=10, r=10)),

        #Panel
        panel.grid.major.x = element_line(),
        panel.grid.minor = element_blank(),
        
        #Legend
        legend.title = element_blank(),
        legend.margin = margin(b=20),
        legend.box.margin = margin(t=30, b=20),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.justification = c(0.5,0.5), 
        legend.position = c(0.875, 1.025),
        legend.background = element_blank(),
        legend.direction="horizontal",
        
        #Plot text
        text = element_text(family = "Palatino", size=14),
        plot.title = element_text(size = 22, face="bold", margin = margin(b = 20, t = 10), hjust = 0),
        plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 30), hjust = 0, lineheight = 0.5),
        plot.caption = element_text(size = 7, margin = margin(t = 10, b = 5), color = "grey70", hjust = 0))
pizzaplot


#Export Graph to PNG fille
ppi <- 300
png("PizzaPlot200406.png", width=8.5*ppi, height=11*ppi, res=ppi)
plot(pizzaplot)
dev.off()


