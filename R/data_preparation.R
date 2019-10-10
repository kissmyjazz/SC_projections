library(tidyverse)
library(here)
library(data.table)
library(openxlsx)
library(papaja)
theme_set(theme_apa(base_size = 15)) 

fp_6310_488 <- here("raw_data", "Neurolucida", "I6310_488_counts")
fp_6310_555 <- here("raw_data", "Neurolucida", "I6310_555_counts")
fp_6313_555 <- here("raw_data", "Neurolucida", "I6313_555_counts")
fp_summary_counts <- here("derived_data", "summary_counts.csv")
fp_density_ipsi_spread <- here("derived_data", "summary_density_ipsi_spread.csv")
fp_density_contra_spread <- here("derived_data", "summary_density_contra_spread.csv")
fp_counts_ipsi_spread <- here("derived_data", "summary_counts_ipsi_spread.csv")
fp_counts_contra_spread <- here("derived_data", "summary_counts_contra_spread.csv")
fp_label_ipsi_spread <- here("derived_data", "summary_label_ipsi_spread.csv")
fp_label_contra_spread <- here("derived_data", "summary_label_contra_spread.csv")
fp_heatmap <- here("figures", "manual_count_heatmap.pdf")

i6310_488_flist <- list.files(fp_6310_488, pattern = "*.xlsx", full.names = TRUE)
i6310_555_flist <- list.files(fp_6310_555, pattern = "*.xlsx", full.names = TRUE)
i6313_555_flist <- list.files(fp_6313_555, pattern = "*.xlsx", full.names = TRUE)

animal_ptn <- "I[0-9]{4}"
section_ptn <- "(?<=_)[A-Za-z0-9_]+(?=_A)"
probe_ptn <- "(?<=a\\s)\\d{3}(?=_a)"
animal_id <- str_extract(i6310_488_flist[[1]], animal_ptn)
section_id <- str_extract(i6310_488_flist[[1]], section_ptn)
probe_id <- str_extract(i6310_488_flist[[1]], probe_ptn)

sections_to_df <- function(flist) {
  fname <- map(flist, basename)
  df <- flist %>% map(read.xlsx) %>% set_names(fname) %>% 
  bind_rows(.id = "fname") %>% 
    mutate(animal_id = str_extract(fname, animal_ptn), 
         section_id = str_extract(fname, section_ptn),
         probe_id = str_extract(fname, probe_ptn)) %>% 
    select(-fname)
}

df_6310_488 <- sections_to_df(i6310_488_flist)
df_6310_555 <- sections_to_df(i6310_555_flist)
df_6313_555 <- sections_to_df(i6313_555_flist)

df_6310_488_summary <- df_6310_488 %>% 
  mutate(area = `488_area` / 1e6, side = ifelse(str_detect(name, "_c"), "contra", "ipsi")) %>% 
  group_by(name, side) %>% 
  summarise(counts = sum(`488`), n = n(), density = (sum(`488`))/(sum(area))) %>% 
  group_by(side) %>% mutate(label = 100 * counts/max(counts)) %>% ungroup() 

df_6310_555_summary <- df_6310_555 %>% group_by(name) %>% 
  mutate(area = `555_area` / 1e6, side = ifelse(str_detect(name, "_c"), "contra", "ipsi")) %>% 
  group_by(name, side) %>% 
  summarise(counts = sum(`555`), n = n(), density = (sum(`555`))/(sum(area))) %>% 
  group_by(side) %>% mutate(label = 100 * counts/max(counts)) %>% ungroup()

df_6313_555_summary <- df_6313_555 %>% group_by(name) %>% 
  mutate(area = `555_area` / 1e6, side = ifelse(str_detect(name, "_c"), "contra", "ipsi")) %>% 
  group_by(name, side) %>% 
  summarise(counts = sum(`555`), n = n(), density = (sum(`555`))/(sum(area))) %>% 
  group_by(side) %>% mutate(label = 100 * counts/max(counts)) %>% ungroup()

summary_list <- list(I6310_488 = df_6310_488_summary, I6310_555 = df_6310_555_summary,
                     I6313_555 = df_6313_555_summary)
df_summary_counts <- bind_rows(summary_list, .id = "case")
write_csv(df_summary_counts, fp_summary_counts)

df_summary_density_spread_ipsi <- dcast.data.table(name ~ case, value.var = "density", 
                                            subset = .(side == "ipsi"),
                                            data = as.data.table(df_summary_counts)) %>% 
  arrange(desc(I6310_488, I6310_555))
write_csv(df_summary_density_spread_ipsi, fp_density_ipsi_spread)

df_summary_density_spread_contra <- dcast.data.table(name ~ case, value.var = "density", 
                                                  subset = .(side == "contra"),
                                                  data = as.data.table(df_summary_counts)) %>% 
  arrange(desc(I6310_488, I6310_555))
write_csv(df_summary_density_spread_contra, fp_density_contra_spread)

df_summary_counts_spread_ipsi <- dcast.data.table(name ~ case, value.var = "counts", 
                                                   subset = .(side == "ipsi"),
                                                   data = as.data.table(df_summary_counts)) %>% 
  arrange(desc(I6310_488, I6310_555))
write_csv(df_summary_counts_spread_ipsi, fp_counts_ipsi_spread)

df_summary_counts_spread_contra <- dcast.data.table(name ~ case, value.var = "counts", 
                                                     subset = .(side == "contra"),
                                                     data = as.data.table(df_summary_counts)) %>% 
  arrange(desc(I6310_488, I6310_555))
write_csv(df_summary_counts_spread_contra, fp_counts_contra_spread)

df_summary_label_spread_ipsi <- dcast.data.table(name ~ case, value.var = "label", 
                                                  subset = .(side == "ipsi"),
                                                  data = as.data.table(df_summary_counts)) %>% 
  arrange(desc(I6310_488, I6310_555))
write_csv(df_summary_label_spread_ipsi, fp_label_ipsi_spread)

df_summary_label_spread_contra <- dcast.data.table(name ~ case, value.var = "label", 
                                                    subset = .(side == "contra"),
                                                    data = as.data.table(df_summary_counts)) %>% 
  arrange(desc(I6310_488, I6310_555))
write_csv(df_summary_label_spread_contra, fp_label_contra_spread)

df_summary_counts_all <- df_summary_counts %>% mutate(name = str_replace(name, "_c", ""))

df_summary_label_spread <- dcast.data.table(name ~ side + case, value.var = "label", 
                                                   data = as.data.table(df_summary_counts_all)) %>% 
  select(name, starts_with("ipsi"), everything()) %>% na.omit() %>% 
  mutate_if(is.numeric, ~round(.x, 1)) %>% 
  arrange(desc(ipsi_I6310_488), desc(ipsi_I6310_555))

df_summary_label_tall <- df_summary_label_spread %>% 
  gather(key = "hemisphere", value = "label", -name) %>% 
  mutate(hemisphere = factor(hemisphere, levels = names(df_summary_label_spread)[2:7]))



g_raster <- ggplot(df_summary_label_tall, 
                   aes(y = fct_reorder(name, label, .fun = mean, na.rm = TRUE), 
                       x = hemisphere, fill = label, 
                                              label = label)) +
  geom_raster() + geom_text(color="red2") + 
  scale_fill_viridis_c() + labs(y = NULL, fill = "cell count %", 
                                x = "injection case & hemisphere", 
                                title = "Manual cell counts in prefrontal cortex") + 
  scale_x_discrete(labels = c("green ipsi", "red ipsi", "purple ipsi", 
                              "green contra", "red contra", "purple contra")) +
  theme(axis.text.x = element_text(size = 15, 
                                   colour = rep(c("limegreen", "firebrick2", "darkorchid3"), 2)),
        plot.title = element_text(size = 16, hjust = 0.5))

pdf(fp_heatmap, width = 12, height = 12, useDingbats = FALSE)
g_raster
dev.off()