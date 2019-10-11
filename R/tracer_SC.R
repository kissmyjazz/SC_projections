library(tidyverse)
library(magrittr)
library(dplyr)
library(ggthemes)
library(grid)
library(here)
theme_set(theme_economist(base_size = 15)) 

fp_i6310 <- here("raw_data", "Injections", "marmoset_I6310_injection_measurements_tidy.csv")
fp_i6313 <- here("raw_data", "Injections", "marmoset_I6313_injection_measurements_tidy.csv")
fp_adam <- here("raw_data", "Adam_data", "injection_site_data_adam.csv")
fp_inj_manual_plot <- here("figures", "spread_manual_injections.pdf")
fp_inj_automatic_plot <- here("figures", "spread_automatic_injections.pdf")

i6310 <- readr::read_csv(fp_i6310)
i6313 <- readr::read_csv(fp_i6313)
df_adam <- readr::read_csv(fp_adam)

i6310$y <- i6310$y + 2
both_cases <- bind_rows(i6310, i6313)

i6310 <- i6310 %>% dplyr::mutate(y_raised = ifelse(tracer == "red",
                                                           y, y + 0.5))

group_colors <- c(red = "#ED1C24", green = "#39B54A", purple = "#93278F")

ggplot() + 
  geom_segment(data=i6310, mapping=aes(x=left_edge, xend=right_edge,
                                       y=y_raised, yend=y_raised, color=tracer,
                                       size = 0.8)) + 
  geom_vline(xintercept=0) +
  scale_color_manual(values = group_colors) +
  scale_y_continuous(breaks = seq(9, 2, by = -1)) + 
  labs(title="Tracer locations in SC of marmoset I6310", 
       x="distance from midline (mm)", y="section number") + 
  theme(legend.position="none") 

ggplot() + 
  geom_segment(data=i6313, mapping=aes(x=left_edge, xend=right_edge,
                                       y=y, yend=y, color=tracer,
                                       size = 0.8)) + 
  geom_vline(xintercept=0) +
  scale_color_manual(values = group_colors) +
  scale_y_continuous(breaks = seq(9, 1, by = -1)) + 
  scale_x_continuous(breaks = seq(0, 2.5, by = 0.5)) + 
  labs(title="Tracer locations in SC of marmoset I6313", 
       x="distance from midline (mm)", y="section number") + 
  theme(legend.position="none") 

both_cases_interleaved <- both_cases %>% dplyr::mutate(y_raised = ifelse(tracer == "red",
                                                   y, ifelse(tracer == "green", 
                                                             y + 0.2, y + 0.6)))

ggplot() + 
  geom_segment(data=both_cases_interleaved, mapping=aes(x=left_edge, xend=right_edge,
                                       y=y_raised, yend=y_raised, color=tracer,
                                       size = 0.8)) + 
  geom_vline(xintercept=0) +
  scale_color_manual(values = group_colors) +
  scale_y_continuous(breaks = seq(9, 0, by = -1)) + 
  scale_x_continuous(breaks = seq(0, 2.5, by = 0.5)) + 
  labs(title = "Superimposed tracer locations in SC\nof cases with manual cell counting", 
       x = "distance from midline (mm)", y = "section number") + 
  geom_segment(aes(x = 0.2, y = 9, xend = 0.2, yend = 5),
                 arrow = arrow(length = unit(0.5, "cm"), ends = "both"), size = 1.2) +
  annotate("text", x = 0.2, y = 9.7, label = "rostral", size = 5.5) +
  annotate("text", x = 0.2, y = 4.4, label = "caudal", size = 5.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, size = 16)) 

ggsave(fp_inj_manual_plot, device = "pdf")

# Adam's data
adam_colors <- c(m6328F = "#FF7F00", m6344F = "#FFF88F")
ggplot() + 
  geom_segment(data=df_adam, mapping=aes(x=beginning, xend=end,
                                                        y=ap, yend=ap, color=animal,
                                                        size = 0.8)) + 
  geom_vline(xintercept=0) +
  scale_color_manual(values = adam_colors) +
  scale_x_continuous(breaks = seq(0, 2.5, by = 0.5)) + 
  labs(title = "Superimposed tracer locations in SC\nof cases from high-throughput pipeline", 
       x = "distance from midline (mm)", y = "distance from interaural line (mm)") + 
  geom_segment(aes(x = 0.2, y = 2.2, xend = 0.2, yend = 0.9),
               arrow = arrow(length = unit(0.5, "cm"), ends = "both"), size = 1.2) +
  annotate("text", x = 0.2, y = 2.4, label = "rostral", size = 5.5) +
  annotate("text", x = 0.2, y = 0.7, label = "caudal", size = 5.5) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, size = 16)) 
ggsave(fp_inj_automatic_plot, device = "pdf")
