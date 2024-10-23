library(ggplot2)
library(readxl)

# set and change working directory
DIR <- "E:\\projects\\small_animal_mri\\05_results\\experiment1_histo"
setwd(DIR)

# load in dataframe containing histological data
df <- read_excel("results.xlsx", sheet="dataframe")

# typesetting
group_names <- c("shm","sci")
region_names <- c("wm_dor", "wm_lat")
df$group  <- factor(df$group, levels = group_names)
df$region <- factor(df$region)

# Note: the statistical analyses were done in Jan Schwab's lab. The results were sent to us.

# ==============
# plot

ylim <- c(0,100)
xlabel <- "White Matter Region"
ylabel <- "Number of SMI-32-labeled Axon Profiles"
title <- "SMI-32 Immunohistochemistry"

# create box plot
ggplot(df, aes(x=region, lower=p25, middle=p50, upper=p75, ymin=p00, ymax=p100, fill=group, color=group)) +
  geom_errorbar(stat="identity", width=0.4, position=position_dodge(width=0.8), aes(ymin=p00, ymax=p100), color="black", linewidth=0.5) +
  geom_boxplot(stat="identity", width=0.8, position=position_dodge()) +
  scale_fill_manual(values = c("grey30","grey100"), labels = group_names) +
  scale_color_manual(values = c("black","black"), labels = group_names) +
  labs(x=xlabel, y=ylabel) +
  scale_x_discrete(labels=region_names) +
  coord_cartesian(ylim=ylim) +
  ggtitle(title) +
  theme_classic() +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(axis.title = element_text(size=18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(plot.title = element_text(size=20, hjust = 0.5))