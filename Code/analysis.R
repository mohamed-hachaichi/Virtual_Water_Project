library(tidyverse)
library(glue)
library(ggtext)
library(showtext)
library(ggstatsplot)
library(gapminder)
library(reshape2)

setwd('Users/Mohamed/Documents/Virtual_Water_Project')


colorvalues = c('#011627','#3F172B', '#7C172E', '#5DD39E','#9C8185', 
               '#41EAD4', '#9FF5E8','#F71735', '#FF9F1C','#E5E5E5')

-----------------------------
# read the data 
ds <- read_csv('../Output/ds.csv')

class <- read_csv('../Data/index.csv')

ds <- merge(ds, class, by = 'City')

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
  

### figure 1

b <- ds %>% 
  filter(Type == "Bleu water") %>%
  group_by(City) %>% 
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  mutate(type = 'Blue water')

g <- ds %>% 
  filter( Type == "Grey water") %>%
  group_by(City) %>%
  summarise(value = sum(value)) %>%
  mutate(type = 'Grey water')

d <- rbind(g, b)

d <- merge(d, class, by = 'City')


## for reproducibility
set.seed(123)

## plot

ggbetweenstats(
  data  = d,
  x     = type,
  y     = value,
  type = "p",
  conf.level = 0.99,
  package = "ggsci",
  palette = "nrc_npg",
  xlab = '',
  ylab = 'Virtual water (Liters/year)',
  title = "Distribution of the water footprints across Global Southern countries",
  outlier.tagging = TRUE,
  outlier.label = City,
)

ggsave('../Figures/Figure_02.png', dpi = 350, height = 12, width = 18, units = 'cm')

p1 <- ggbetweenstats(
  data  = d %>% filter(type == "Grey water"),
  x     = Category,
  y     = value,
  type = "p",
  conf.level = 0.99,
  package = "ggsci",
  palette = "nrc_npg",
  xlab = '',
  ylab = 'Grey virtual water (Liters/year)',
  title = "Distribution of Grey water footprint",
  outlier.tagging = TRUE,
  outlier.label = City,
)

p2 <- ggbetweenstats(
  data  = d %>% filter(type == "Blue water"),
  x     = Category,
  y     = value,
  type = "p",
  conf.level = 0.99,
  package = "ggsci",
  palette = "nrc_npg",
  xlab = '',
  ylab = 'Blue virtual water (Liters/year)',
  title = "Distribution of Blue water footprint",
  outlier.tagging = TRUE,
  outlier.label = City
)


p3 <- ggbetweenstats(
  data  = d %>% filter(type == "Grey water", Continent != 'Europe'),
  x     = Continent,
  y     = value,
  type = "p",
  conf.level = 0.99,
  package = "ggsci",
  palette = "nrc_npg",
  xlab = 'Type of water footprint',
  ylab = 'Virtual water (Liters/year)',
  title = "Distribution of Grey water footprint"
)

p4 <- ggbetweenstats(
  data  = d %>% filter(type == "Blue water", Continent != 'Europe'),
  x     = Continent,
  y     = value,
  type = "p",
  conf.level = 0.99,
  package = "ggsci",
  palette = "nrc_npg",
  xlab = 'Type of water footprint',
  ylab = 'Virtual water (Liters/year)',
  title = "Distribution of Blue water footprint"
)

## combining the individual plots into a single plot
combine_plots(
  list(p1, p2),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = "Comparison of Grey and Blue water footprints"
  )
)

ggsave('../Figures/Figure_03.png', dpi = 350, height = 16, width = 30, units = 'cm')


### through continents 

combine_plots(
  list(p3, p4),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    title = "Comparison of Grey and Blue water footprints"
  )
)

ggsave('../Figures/Figure_04.png', dpi = 350, height = 16, width = 30, units = 'cm')


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
       title="Top 20 Global Southern cities virtual water", subtitle = 'Liters per capita/year') +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=0)),
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


ggsave("../Figures/Figure_05.tiff", width=10, height=6)


-------------------------------------

ds %>%
  filter(City %in% c(one$City)) %>%
  group_by(City, Sector, Type) %>%
  summarise(value = sum(value)) %>%
  filter(City == "Beijing" & Type == "Grey water") %>%
  mutate(percent = (value / 622.8432) * 100) %>%
  select(City, Sector, percent)
  
    
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
    plot.title = element_text(face="bold", margin= margin(b=0)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", size=0.1),
    panel.grid.major.y = element_line(color="gray", size=0.1, linetype="dotted")
  ) + scale_fill_manual(values = colorvalues)
  
ggsave("../Figures/Figure_06.tiff", width=10, height=6)
 

--------------------------------------------

  
  
  
    
ds %>%
  filter(Continent != "Europe") %>%
  group_by(Sector,Type, Category) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x= Type, y = value, fill = Sector)) +
  labs(x = '', y = '') +
  geom_bar(stat = 'identity', position = 'fill') +
  facet_grid(~Category) + 
  coord_flip() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=5)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", size=0.1),
    panel.grid.major.y = element_line(color="gray", size=0.1, linetype="dotted")
  ) + scale_fill_manual(values = colorvalues)


ggsave("../Figures/figure_07.tiff", width=10, height=6)

----------------------------

colues <- c('#E5EBEA', '#DBD9DB', '#B098A4', '#747572')  
   
ds %>%
  filter(Continent != "Europe") %>%
  group_by(Indus_Sector,Type, Category) %>%
  summarise(value = sum(value)) %>% 
  ggplot(aes(x= Type, y = value, fill = Indus_Sector)) +
  labs(x = '', y = '') +
  geom_bar(stat = 'identity', position = 'fill') +
  facet_grid(~Category) + 
  coord_flip() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=5)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", size=0.1),
    panel.grid.major.y = element_line(color="gray", size=0.1, linetype="dotted")
  ) + scale_fill_manual(values = colues)

  
ggsave("../Figures/figure_08.tiff", width=10, height=6)

-------------------------------------------
  
ds %>%
  filter(Continent != 'Europe') %>%
  group_by(Sector,Type, Category, Continent) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(x= Type, y = value, fill = Sector)) +
  labs(x = '', y = '') +
  geom_bar(stat = 'identity', position = 'fill') +
  facet_grid(~Continent) + 
  coord_flip() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=5)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", size=0.1),
    panel.grid.major.y = element_line(color="gray", size=0.1, linetype="dotted")
  ) + scale_fill_manual(values = colorvalues)


ggsave("../Figures/figure_09.tiff", width=10, height=6)


-----------------------------------
  
ds %>%
  filter(Continent != 'Europe') %>%
  group_by(Sector, Type) %>%
  summarise(value = mean(value)) %>%
  ggplot(aes(x = Type, y  = value, fill = Sector)) +
  geom_bar(stat = 'identity', position = 'fill') +
  coord_flip() + 
  labs(x=NULL, y=NULL,
       title="Global Southern cities virtual water decomposition", subtitle = 'Average (%) - Liters per capita/year') + 

  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face="bold", margin= margin(b=5)),
    plot.caption = element_markdown(hjust=0, color="darkgray"),
    plot.caption.position = "plot",
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(color="darkgray"),
    panel.grid.major.x = element_line(color="gray", size=0.1),
    panel.grid.major.y = element_line(color="gray", size=0.1, linetype="dotted")
  ) + scale_fill_manual(values = colorvalues)
  
  
ggsave("../Figures/A_Graphical_abstract.tiff", width=10, height=6)
  
  
