library(tidyverse)
library(ggplot2)
library(devtools)
#devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
names(df)
table(df$scouting_period)
df=fb_player_scouting_report("https://fbref.com/en/players/83d074ff/Alexis-Mac-Allister",pos_versus = "primary")
df1=df %>% group_by(StatGroup) %>% filter(scouting_period=='2023-2024 Premier League') %>% arrange(desc(Percentile))
rownames(df1)=NULL
player_df = df1[c(8,42,47,77,13,14,32,26,2,11,15,5),] #values in order
View(player_df)
player_df$index = 1:12 #creating index for plotting values in order(clockwise)
View(player_df)
player_df = player_df %>% 
  mutate(type=case_when(
    index %in% 1:4 ~ 'Attacking',
    index %in% 5:8 ~ 'Possession',
    index %in% 9:12 ~ 'Defending'
  )) #mutate: creating new variables #case when - Equivalent of SQL "case when" statement

#Rotating axis text in a plot
#angle of axis text
temp = (360/(length(player_df$index))/2)
myAng = seq(-temp, -360+temp, length.out = length(player_df$index))
#every label
ang=ifelse(myAng < -90, myAng+180, myAng)
ang=ifelse(ang < -90, ang+180, ang)

#color
color1 <- "#034694"
color2 <- "grey"
color3 <- "gold"

ggplot(data=player_df, aes(x=reorder(Statistic, index), y=Percentile, label=Percentile, fill=type))+
    geom_bar(data=player_df, width=1,
             color='oldlace',
             stat='identity')+
  #wrap bar chart as around polar center
  coord_polar()+
  # filling the whole polar chart
  geom_bar(aes(y=100,fill=type),stat = 'identity',width=1,alpha=0.5)+
  #add & customize lines that barder whole pizza
  geom_hline(yintercept = seq(0,100,by=100),
             color='oldlace',
             size=1)+
  #add & customize lines between each pizza slices
  geom_vline(xintercept = seq(0.5, 12, by=1),
             color='oldlace',
             size=0.5)+
  #add percentile labels
  geom_label(color='gray20',fill='oldlace',size=2.5,fontface='bold',family='Comic Sans MS',
             show.legend = FALSE)+
  #manually set the colors of bars (3 here fore each group of stat, attaching,possesion,defending)
  scale_fill_manual(values = c(color1,color2,color3))+
  #theme manipulation to customize plot
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "oldlace", color="oldlace"),
        legend.title = element_blank(),
        legend.text = element_text(colour = "gray20", family = "Comic Sans MS", face = "bold"),
        legend.key.size = unit(.5, "cm"),
        legend.box.spacing = unit(0, "mm"),
        plot.title = element_text(hjust = .5, colour = "gray20", face = "bold", size = 16, family = "Comic Sans MS"),
        plot.subtitle = element_text(hjust = .5, colour = "gray20", size = 8, family = "Comic Sans MS"),
        plot.background = element_rect(fill = "oldlace", color="oldlace"),
        panel.background = element_rect(fill = "oldlace", color="oldlace"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold", size = 6.8, colour = "gray20"),
        axis.title = element_blank(),
        axis.text.x = element_text(face = "bold", size = 7, family = "Comic Sans MS")) +
  # add title and subtitle
  labs(title = "Alexis Mac Allister Scouting Report",
       subtitle = "reference: worldfootballR", x = NULL, y = NULL)

