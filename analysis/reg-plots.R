#-------------------------------------------
# This script sets out to graph the data
# using linear models
#
# NOTE: This script requires setup.R to
# have been run first
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 17 September 2020
#-------------------------------------------

# Define palette for plotting

the_palette <- c("International Artist" = "#189AB4",
                 "Australian Artist" = "#FD62AD")

#----------------------- PLOTTING ----------------------------------

# Ungrouped

p <- the_data %>%
  mutate(australian = case_when(
    australian == "0" ~ "International Artist",
    TRUE              ~ "Australian Artist")) %>%
  mutate(fb_likes = log(fb_likes),
         plays = log(plays)) %>%
  ggplot(aes(x = fb_likes, y = plays)) +
  geom_smooth(formula = y ~ s(x), method = "gam", colour = "#05445E", fill = "steelblue2",
              alpha = 0.4) +
  geom_point(aes(colour = australian), size = 2.5) +
  labs(title = "Facebook fanbase size and Spotify plays for 2019 Triple J Hottest 100 songs",
       x = "Log-scaled Facebook Likes",
       y = "Log-scaled Spotify Plays",
       caption = "Analysis: Orbisant Analytics.",
       colour = NULL) +
  scale_colour_manual(values = the_palette) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
print(p)

# Grouped

p1 <- the_data %>%
  mutate(australian = case_when(
    australian == "0" ~ "International Artist",
    TRUE              ~ "Australian Artist")) %>%
  mutate(fb_likes = log(fb_likes),
         plays = log(plays)) %>%
  ggplot(aes(x = fb_likes, y = plays)) +
  geom_smooth(aes(group = australian, fill = australian, colour = australian), formula = y ~ s(x), method = "gam", 
              alpha = 0.4) +
  geom_point(size = 2.5, aes(colour = australian)) +
  labs(title = "Facebook fanbase size and Spotify plays for 2019 Triple J Hottest 100 songs",
       x = "Log-scaled Facebook Likes",
       y = "Log-scaled Spotify Plays",
       caption = "Analysis: Orbisant Analytics.",
       colour = NULL,
       fill = NULL) +
  scale_colour_manual(values = the_palette) +
  scale_fill_manual(values = the_palette, guide = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
print(p1)

#----------------------- EXPORTS --------------------------

CairoPNG("output/hottest-100-both-plots.png", 800, 600)
ggarrange(p, p1, nrow = 2, ncol = 1)
dev.off()
