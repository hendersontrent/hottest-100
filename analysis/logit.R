#-------------------------------------------
# This script sets out to produce some
# initial logit plots
#
# NOTE: This script requires setup.R to
# have been run first
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 18 September 2020
#-------------------------------------------

# Prep data

df <- the_data %>%
  mutate(plays_c = (log(plays) - mean(log(plays)))/sd(log(plays)),
         likes_c = (log(fb_likes) - mean(log(fb_likes)))/sd(log(fb_likes)),
         days_c = (days_released - mean(days_released))/sd(days_released),
         rank_c = (the_rank - mean(the_rank))/sd(the_rank)) %>%
  mutate(australian = as.numeric(australian)) %>%
  mutate(australian = case_when(
    australian == 1 ~ 0,
    TRUE            ~ 1))

#--------------------- DATA VISUALISATION --------------------------

p <- df %>%
  gather(key = metric, value = value, 10:13) %>%
  mutate(metric = case_when(
         metric == "plays_c" ~ "Spotify Plays",
         metric == "likes_c" ~ "Facebook Likes",
         metric == "days_c"  ~ "Days Since Release",
         metric == "rank_c"  ~ "Hottest 100 Rank")) %>%
  ggplot(aes(x = value, y = australian)) +
  geom_point(colour = "#189AB4", size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              fill = "steelblue2", colour = "steelblue2",
              alpha = 0.4) +
  labs(title = "Logit plots of various metrics predicting artist nationality",
       x = "Value (Centred & Standardised)",
       y = "Artist Nationality (0 = International, 1 = Australian)",
       caption = "Analysis: Orbisant Analytics.") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#05445E"),
        strip.text = element_text(colour = "white", face = "bold")) +
  facet_wrap(~metric)
print(p)

# Export

CairoPNG("output/logit.png", 800, 600)
print(p)
dev.off()
