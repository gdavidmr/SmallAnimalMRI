library(emmeans)
library(ggplot2)
library(readxl)


# set and change working directory
DIR <- "E:\\projects\\small_animal_mri\\05_results"
setwd(DIR)

# load in dataframe containing histological data
df <- read_excel("histo_extracted_values.xlsx", sheet="dataframe")

# typesetting
group_names <- c("SHM","MLD","MOD","SEV")
day_names <- c("day 2", "day 30", "day 90")
df$group <- factor(df$group, levels = group_names)
df$day <- factor(df$day)

# two-way ANOVA:
#   dependent variable: smi32
#   independent variables: group (cat.), day (cat.), group*day
anova_smi32 <- aov(smi32 ~ group*day, data=df)
summary(anova_smi32)

# post-hoc analyses
# per day
emm_day02 <- emmeans(anova_smi32, ~group, at=list(day="2"))
emm_day30 <- emmeans(anova_smi32, ~group, at=list(day="30"))
emm_day90 <- emmeans(anova_smi32, ~group, at=list(day="90"))

pairs(emm_day02, adjust="no")
pairs(emm_day30, adjust="no")
pairs(emm_day90, adjust="no")

# per group
emm_shm <- emmeans(anova_smi32, ~day, at=list(group="SHM"))
emm_mld <- emmeans(anova_smi32, ~day, at=list(group="MLD"))
emm_mod <- emmeans(anova_smi32, ~day, at=list(group="MOD"))
emm_sev <- emmeans(anova_smi32, ~day, at=list(group="SEV"))

pairs(emm_shm, adjust="no")
pairs(emm_mld, adjust="no")
pairs(emm_mod, adjust="no")
pairs(emm_sev, adjust="no")



# ==============
# plot

ylim <- c(0,3800)
xlabel <- "Days after injury"
ylabel <- "Number of SMI-32-stained Clusters"
title <- "SMI-32 immunohistochemistry"

# OPTION 1
# create box plot
# THIS IS THE ONE USED IN THE PAPER
ggplot(df, aes(x=day, y=smi32, fill=group, color=group)) +
  stat_boxplot(geom = 'errorbar', position=position_dodge(width=0.8), width=0.4) +
  geom_boxplot(width=0.8, position=position_dodge()) +
  scale_fill_manual(values = c("grey30","grey53","grey76","grey100"), labels = group_names) +
  scale_color_manual(values = c("black","black","black","black"), labels = group_names) +
  labs(x=xlabel, y=ylabel) +
  scale_x_discrete(labels=day_names) +
  coord_cartesian(ylim=ylim) + 
  ggtitle(title) +
  theme_classic() +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(axis.title = element_text(size=18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(plot.title = element_text(size=20, hjust = 0.5))

# OPTION 2
# create bar plot
ggplot(df, aes(x=day, y=smi32, fill=group, color=group)) +
  stat_summary(fun="mean", geom="bar", na.rm=TRUE, width=0.8, position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean, ymax=Mean+Sd), position=position_dodge(width=0.8), width=0.4) +
  scale_fill_manual(values = c("grey30","grey53","grey76","grey100"), labels = group_names) +
  scale_color_manual(values = c("black","black","black","black"), labels = group_names) +
  labs(x="", y=ylabel) +
  scale_x_discrete(labels=day_names) +
  coord_cartesian(ylim=ylim) + 
  ggtitle(title) +
  theme_classic() +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(axis.title = element_text(size=18)) +
  theme(legend.text = element_text(size = 16)) +
  theme(plot.title = element_text(size=20, hjust = 0.5))




# ==============
# correlation analysis between SMI-32 and BBB scores

# compute Pearson's correlation
cor.test(df[df$day==2,] $BBB_score, df[df$day==2,] $smi32, method = "pearson", exact=FALSE)
cor.test(df[df$day==30,]$BBB_score, df[df$day==30,]$smi32, method = "pearson", exact=FALSE)
cor.test(df[df$day==90,]$BBB_score, df[df$day==90,]$smi32, method = "pearson", exact=FALSE)

# compute Spearman's correlation
cor.test(df[df$day==2,] $BBB_score, df[df$day==2,] $smi32, method = "spearman", exact=FALSE)
cor.test(df[df$day==30,]$BBB_score, df[df$day==30,]$smi32, method = "spearman", exact=FALSE)
cor.test(df[df$day==90,]$BBB_score, df[df$day==90,]$smi32, method = "spearman", exact=FALSE)



# ==============
# scatter plot

# define function
plot_scatterplot <- function(M, m, figname) {
  
  xlabel <- "Number of SMI-32-stained Clusters"
  ylabel <- "BBB Score"
  ylim <- c(0,21)
  
  ggplot(M, aes(x=x, y=y)) +
    geom_point(aes(shape=group, fill=group), size=5, color="black") +
    scale_shape_manual(values=c(22,21,24,23)) +
    scale_fill_manual(values=c("grey30","grey53","grey76","grey100")) +
    geom_abline(slope=m$coefficients[2], intercept=m$coefficients[1], color='black') +
    ylim(ylim) +
    labs(x=xlabel, y=ylabel) +
    theme_classic() +
    theme(axis.text.x = element_text(size=22)) +
    theme(axis.text.y = element_text(size=22)) +
    theme(axis.title = element_text(size=24))
  ggsave(figname, height=6.7, width=8)
}

# compute linear fit for the plots
m_dpi02 <- lm(df[df$day==2,]$BBB_score ~ df[df$day==2,]$smi32)
m_dpi30 <- lm(df[df$day==30,]$BBB_score ~ df[df$day==30,]$smi32)
m_dpi90 <- lm(df[df$day==90,]$BBB_score ~ df[df$day==90,]$smi32)

# SMI-32 vs BBB at 2 dpi 
x <- df[df$day==2,]$smi32
y <- df[df$day==2,]$BBB_score
group <- df[df$day==2,]$group
M <- data.frame(cbind(x,y))
figname <- "fig_smi32_vs_bbb_dpi02.png"
plot_scatterplot(M, m_dpi02, figname)

# SMI-32 vs BBB at 30 dpi 
x <- df[df$day==30,]$smi32
y <- df[df$day==30,]$BBB_score
group <- df[df$day==30,]$group
M <- data.frame(cbind(x,y))
figname <- "fig_smi32_vs_bbb_dpi30.png"
plot_scatterplot(M, m_dpi30, figname)

# SMI-32 vs BBB at 84 dpi 
x <- df[df$day==90,]$smi32
y <- df[df$day==90,]$BBB_score
group <- df[df$day==90,]$group
M <- data.frame(cbind(x,y))
figname <- "fig_smi32_vs_bbb_dpi84.png"
plot_scatterplot(M, m_dpi90, figname)



