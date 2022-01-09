library(tidyverse)
library(glue)
library(ggtext)
library(showtext)

setwd('Users/Mohamed/Documents/Virtual_Water_Project')


colorvalues = c('#4E5166','#7C90A0', '#B5AA9D', '#B9B7A7','#747274', 
               '#C1292E', '#FB3640','#512500', '#512500','#874000',
               '#F5CB5C','#242423')

-----------------------------
# read the data 
ds <- read_csv('../Output/ds.csv')

ds <- ds %>% filter(!City %in% c('Colombo', 'Gampaha',
                                 'Kalutara', 'Trincomalee',
                                 'Matara'))

B <- ds %>% 
  filter(Type == "Bleu water") %>%
  group_by(City) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  head(20) %>%
  rename('percent_Bleu' = value)
  
G <- ds %>% 
  filter( Type == "Grey water") %>%
  group_by(City) %>%
  summarise(value = sum(value)) %>%
  filter(City %in% B$City) %>%
  rename('percent_Grey' = value)

one <- merge(B, G, by = 'City') %>%
  mutate(bump_Bleu = if_else(percent_Bleu < percent_Grey,
                               percent_Bleu - 27,
                               percent_Bleu + 27),
         bump_Grey = if_else(percent_Bleu < percent_Grey,
                                percent_Grey + 27,
                                percent_Grey - 27)) %>%
  mutate_if(is.numeric, round)
  


# make the plot 

one %>% 
  pivot_longer(cols = -City, names_to=c(".value", "type"),
               names_sep = '_') %>%
  mutate(City = factor(City, levels = rev(one$City))) %>%
  ggplot(aes(x=percent, y=City, color=type)) +
  geom_line(color="#e6e6e6", size=1.75, show.legend = FALSE) +
  geom_point(size=2, show.legend = FALSE) +
  geom_text(aes(label=glue("{percent} liters"), x=bump),
            size=3, show.legend = FALSE) +
  labs(x=NULL, y=NULL,
       title="Top 20 Global Southern cities virtual water", subtitle = 'Liters per capita/year',
       caption="<i>Base: 187 global southern cities virtual water project across 24 countries")+
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=20)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", size=0.1),
    panel.grid.major.y = element_line(color="gray", size=0.1, linetype="dotted")
  ) +
  scale_color_manual(name=NULL,
                     breaks=c("Grey", "Bleu"),
                     values=c("#727272", "#15607a"),
                     labels=c("Grey", "Bleu"))


ggsave("../Figures/fig_01.tiff", width=10, height=6)


-------------------------------------
  
ds %>%
  filter(City %in% c(one$City)) %>%
  group_by(City, Sector, Type) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x = value, y = City, fill = Sector)) +
  geom_bar(stat = 'identity', position = 'stack') +
  labs(x=NULL, y=NULL,
       title="Top 20 Global Southern cities virtual water decomosition by major consumption categories", subtitle = 'Liters per capita/year',
       caption="<i>Base: 187 global southern cities virtual water project across 24 countries") +
  facet_grid(~Type, scale = 'free') +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=20)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", size=0.1),
    panel.grid.major.y = element_line(color="gray", size=0.1, linetype="dotted")
  )
  
ggsave("../Figures/fig_02.tiff", width=10, height=6)
 
-----------------------------------
  
ds %>%
  group_by(Sector, Type) %>%
  summarise(value = mean(value)) %>%
  ggplot(aes(x = Type, y  = value, fill = Sector)) +
  geom_bar(stat = 'identity', position = 'fill') +
  coord_flip() + 
  labs(x=NULL, y=NULL,
       title="Global Southern cities virtual water decomposition", subtitle = 'Average (%) - Liters per capita/year',
       caption="<i>Base: 187 global southern cities virtual water project across 24 countries") + 

  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=20)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", size=0.1),
    panel.grid.major.y = element_line(color="gray", size=0.1, linetype="dotted")
  )
  
ggsave("../Figures/A_Graphical_abstract.tiff", width=10, height=6)
  
---------------------------------------------------------

