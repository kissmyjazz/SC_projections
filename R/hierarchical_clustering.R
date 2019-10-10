library(tidyverse)
library(here)
library(data.table)
library(openxlsx)
library(papaja)
library(dendextend)
theme_set(theme_apa(base_size = 15)) 

normalize <- function(x) {
  return((x- min(x)) /(max(x)-min(x)))
}

fp_label_ipsi_spread <- here("derived_data", "summary_counts_ipsi_spread.csv")
fp_m6344 <- here("raw_data", "Adam_data", "m6344_contra_ipsi.csv")
fp_m6328 <- here("raw_data", "Adam_data", "m6328_contra_ipsi.csv")
fp_dendro <- here("figures", "dendro_plot.pdf")

df_label_ipsi <- read_csv(fp_label_ipsi_spread) %>% rename("green" = I6310_488, "red" = I6310_555, 
                                                           "purple" = I6313_555) %>% na.omit()
df_m6344 <- read_csv(fp_m6344) %>% select(-contra) %>% rename("yellow" = ipsi)
df_m6328 <- read_csv(fp_m6328) %>% select(-contra) %>% rename("orange" = ipsi)

df_ipsi <- merge(df_m6344, df_m6328) %>% merge(df_label_ipsi) %>% 
  mutate_if(is.numeric, normalize) %>% as.data.table()
mat_ipsi <- as.matrix(df_ipsi, rownames = "name") %>% t()

par(mar=c(3, 2, 3, 4))
ipsi_clust_wardD2 <- dist(mat_ipsi) %>% hclust(method = "ward.D2") %>% as.dendrogram() %>% 
  color_branches(k = 3) %>% set("branches_lwd", 2) %>% hang.dendrogram(hang_height = 0.8) %>% 
  set("labels_cex", 1.5)

pdf(fp_dendro, width = 8, height = 8, useDingbats = FALSE)
plot(ipsi_clust_wardD2, horiz = TRUE, 
     main = "Hierarchical clustering of injection sites\nby the pattern of prefrontal projections") 
dev.off()