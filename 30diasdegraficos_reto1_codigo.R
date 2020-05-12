#30díasdegráficos con R - Día 1 Barras/Columnas

library(tidyverse) #install.packages("tidyverse")
library(ggplot2) #install.packages("ggplot2")

reto1 <- read.csv("https://query.data.world/s/5g255oegex7d4mz42va66l47g2kycv", 
                   header=TRUE, stringsAsFactors=FALSE)

reto1 <- reto1 %>% mutate(
  "Music Genre" = case_when(startsWith(Genre, "Rock") ~ "Rock",
                    startsWith(Genre, "Pop") ~ "Pop",
                    startsWith(Genre, "Funk") ~ "Funk",
                    startsWith(Genre, "Hip Hop") ~ "Hip Hop",
                    startsWith(Genre, "Electronic") ~ "Electronic",
                    startsWith(Genre, "Soul") ~ "Soul",
                    startsWith(Genre, "Jazz") ~ "Jazz",
                    startsWith(Genre, "Blues") ~ "Blues",
                    startsWith(Genre, "Folk") ~ "Folk",
                    startsWith(Genre, "Classical") ~ "Classical",
                    startsWith(Genre, "Reggae") ~ "Reggae",
                    startsWith(Genre, "Latin") ~ "Latin"))
                           
ggplot(reto1, aes(Year, 
                        fill = reto1$`Music Genre`)) + 
  geom_histogram(binwidth = 1) + 
  guides(fill=guide_legend(ncol=2,
                           title ="Genre")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
  labs(title="Rolling Stone's 500 Greatest Albums of All Time",
       subtitle = "#30díasdegráficos con R - Día 1: Barras/Columnas",
       caption = "@AGraciaRomero - 12/05/2020",
       y = "Number of albums") +
  theme(plot.caption = element_text(hjust = 1)) +
  theme(legend.position = c(0.8, 0.8)) + 
  theme(plot.title = element_text(size = 16, 
                                  face = "bold")) +
  theme(panel.background = element_rect(fill = NA), 
        axis.line = element_line(size = 1, 
                                 colour = "black")) 
ggsave("30diasdegraficos_reto1.jpg", width = 30, height = 15, units = "cm")

