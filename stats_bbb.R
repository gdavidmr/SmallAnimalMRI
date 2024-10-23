library(dplyr)
library(emmeans)
library(ggplot2)
library(readxl)
library(sciplot)

# set and change working directory
DIR <- 'E:\\projects\\small_animal_mri\\05_results'
setwd(DIR)

# ==========================================================================
# load in dataframe containing BBB scores for ex vivo MRI

dfl1 <- read.csv('relaxometry_groupstats_avg.csv', header = TRUE, sep = ',')

# change column names
colnames(dfl1)[6]  <- "level"
colnames(dfl1)[9]  <- "area"
colnames(dfl1)[12] <- "area_sc"

# typesetting
group_names <- c("SHM","MLD","MOD","SEV")
day_names   <- unique(dfl1$BBB_day)
days        <- as.numeric(levels(day_names))
  
dfl1$group    <- as.factor(dfl1$group)
dfl1$group    <- factor(dfl1$group, levels=group_names)

dfl1$contrast <- as.factor(dfl1$contrast)
dfl1$tissue   <- as.factor(dfl1$tissue)
dfl1$level    <- as.factor(dfl1$level)

# In this dataframe of longitudinal format, the BBB scores are repetitions for each contrast, tissue, and segment.
# Therefore, to extract BBB scores for each timepoint, we can slice the dataframe for an arbitrary contrast, tissue, and segment.
# select T1 contrast, gm tissue, and T3 segment only
dfl1 <- dfl1[(dfl1$contrast=="T1" & dfl1$tissue=="gm" & dfl1$level=="T3"),]

# convert to wide format for plotting
dfw1 <- reshape(dfl1, idvar = "id", timevar = "BBB_day", direction = "wide")

# get group identifier
group <- dfw1$group.1

# extract the needed columns
dfw1 <- select_(dfw1, "BBB_score.1", "BBB_score.7", "BBB_score.14", "BBB_score.21", "BBB_score.28", "BBB_score.35", "BBB_score.42", "BBB_score.49", "BBB_score.56",
                  "BBB_score.63", "BBB_score.70", "BBB_score.77", "BBB_score.84")


# ==========================================================================
# load in dataframe containing BBB scores for ex vivo MRI
# not needed here, the correlation between SMI-32 and BBB scores is done in script "stats_histology.R"



# ==========================================================================
# repeated measures ANOVA to investigate group differences in BBB scores over time
# IMPORTANT: the longitudinal BBB dataset (of the ex vivo MRI group) is used here!!!
#   - BBB score: dependent variable
#   - fixed effects: group (between-subject factor), time  (within-subject factor), and their interaction (factor)
#   - random effect: subject
dfl1$BBB_day <- as.factor(dfl1$BBB_day)
lm1 <- aov(BBB_score ~ group*BBB_day + Error(id/(BBB_day)), data = dfl1)
summary(lm1)


# post-hoc pair-wise group comparisons with Fisher's LSD correcion
emm1_group <- emmeans(lm1, specs = ~ group); emm1_group;

shm <- c(1,0,0,0)
mld <- c(0,1,0,0)
mod <- c(0,0,1,0)
sev <- c(0,0,0,1)

list <- list(
  "sev-shm" = sev-shm,
  "mod-shm" = mod-shm,
  "mld-shm" = mld-shm,
  "sev-mld" = sev-mld,
  "mod-mld" = mod-mld,
  "sev-mod" = sev-mod)

c1_group  <- contrast(emm1_group, method = list); c1_group;




# post-hoc pair-wise time comparison with Fisher's LSD correcion
emm1_day <- emmeans(lm1, specs = ~ BBB_day); emm1_day;

day_01 <- c(1,0,0,0,0,0,0,0,0,0,0,0,0)
day_07 <- c(0,1,0,0,0,0,0,0,0,0,0,0,0)
day_14 <- c(0,0,1,0,0,0,0,0,0,0,0,0,0)
day_21 <- c(0,0,0,1,0,0,0,0,0,0,0,0,0)
day_28 <- c(0,0,0,0,1,0,0,0,0,0,0,0,0)
day_35 <- c(0,0,0,0,0,1,0,0,0,0,0,0,0)
day_42 <- c(0,0,0,0,0,0,1,0,0,0,0,0,0)
day_49 <- c(0,0,0,0,0,0,0,1,0,0,0,0,0)
day_56 <- c(0,0,0,0,0,0,0,0,1,0,0,0,0)
day_63 <- c(0,0,0,0,0,0,0,0,0,1,0,0,0)
day_70 <- c(0,0,0,0,0,0,0,0,0,0,1,0,0)
day_77 <- c(0,0,0,0,0,0,0,0,0,0,0,1,0)
day_84 <- c(0,0,0,0,0,0,0,0,0,0,0,0,1)


list <- list(
  "day_84-day_01" = day_84-day_01,
  "day_84-day_07" = day_84-day_07,
  "day_84-day_14" = day_84-day_14,
  "day_84-day_21" = day_84-day_21,
  "day_84-day_28" = day_84-day_28,
  "day_84-day_35" = day_84-day_35,
  "day_84-day_42" = day_84-day_42,
  "day_84-day_49" = day_84-day_49,
  "day_84-day_56" = day_84-day_56,
  "day_84-day_63" = day_84-day_63,
  "day_84-day_70" = day_84-day_70,
  "day_84-day_77" = day_84-day_77,
  "day_84-day_84" = day_84-day_84)

c1_day  <- contrast(emm1_day, method = list); c1_day;


# ==========================================================================
# plot BBB scores over time

# compute mean and std across subjects, by group and time point
Mean_SHM <- apply(dfw1[group=='SHM',], 2, mean, na.rm=TRUE)
Mean_MLD <- apply(dfw1[group=='MLD',], 2, mean, na.rm=TRUE)
Mean_MOD <- apply(dfw1[group=='MOD',], 2, mean, na.rm=TRUE)
Mean_SEV <- apply(dfw1[group=='SEV',], 2, mean, na.rm=TRUE)

Std_SHM <- apply(dfw1[group=='SHM',], 2, sd, na.rm=TRUE)
Std_MLD <- apply(dfw1[group=='MLD',], 2, sd, na.rm=TRUE)
Std_MOD <- apply(dfw1[group=='MOD',], 2, sd, na.rm=TRUE)
Std_SEV <- apply(dfw1[group=='SEV',], 2, sd, na.rm=TRUE)

# plot
plot.new()
dev.new(height=15, width=20)
par(mar = c(6,6,2,2))
xlab <- "Days Post-Injury"
ylab <- "BBB Score"
ylim <- c(0,25)

plot(  days, Mean_SHM, pch=15, cex=2, col="black",  xlab=xlab, ylab=ylab, ylim=ylim, xaxt="n", yaxt="n", cex.lab=2, bty='l')
points(days, Mean_MLD, pch=16, cex=2, col="blue")
points(days, Mean_MOD, pch=17, cex=2, col="orange")
points(days, Mean_SEV, pch=18, cex=2, col="red")

lines(days, Mean_SHM, lwd=2, col="black")
lines(days, Mean_MLD, lwd=2, col="blue")
lines(days, Mean_MOD, lwd=2, col="orange")
lines(days, Mean_SEV, lwd=2, col="red")

arrows(days, Mean_SHM - sd(Mean_SHM, na.rm = FALSE), days, Mean_SHM + sd(Mean_SHM, na.rm = FALSE), length=0.05, angle=90, code=3, col="black")
arrows(days, Mean_MLD - sd(Mean_MLD, na.rm = FALSE), days, Mean_MLD + sd(Mean_MLD, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
arrows(days, Mean_MOD - sd(Mean_MOD, na.rm = FALSE), days, Mean_MOD + sd(Mean_MOD, na.rm = FALSE), length=0.05, angle=90, code=3, col="orange")
arrows(days, Mean_SEV - sd(Mean_SEV, na.rm = FALSE), days, Mean_SEV + sd(Mean_SEV, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")

axis(1, at=days, cex.axis=2, cex.lab=3)
axis(2, cex.axis=2, cex.lab=3)

legend()
