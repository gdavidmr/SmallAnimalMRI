# ======================================================================================
# ==================== PART II. ANALYSIS OF CROSS-SECTIONAL AREAS ======================
# ======================================================================================

# ==================== SET UP LINEAR MIXED EFFECT MODEL ========================

# FIT MIXED EFFECT MODEL
# mixed effect model to investigate group differences over segments
#   - group: fixed effect (4 levels)
#   - segment: fixed effect (12 levels)
#   - group-segment interaction: fixed effect (48 levels)
#   - subject over segments: random effect
#   - area: dependent variable
#
# Model1: area ~ group + segment + segment*subject(group) + segment|subject

ctrl <- lmeControl(maxIter = 1000, msMaxIter = 1000, optimMethod = "optim", msMaxEval = 1000, niterEM = 1000)

summary(lme_sca     <- lme(area ~ group*level, random = ~ 1|id, data = dfl_sca,     na.action = na.omit, control = ctrl))
summary(lme_gma     <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma,     na.action = na.omit, control = ctrl))
summary(lme_gma_dor <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_dor, na.action = na.omit, control = ctrl))
summary(lme_gma_ven <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_ven, na.action = na.omit, control = ctrl))
summary(lme_wma     <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma,     na.action = na.omit, control = ctrl))
summary(lme_wma_dor <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_dor, na.action = na.omit, control = ctrl))
summary(lme_wma_lat <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_lat, na.action = na.omit, control = ctrl))
summary(lme_wma_ven <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_ven, na.action = na.omit, control = ctrl))

# To reduce the number of levels, we combine the 16 vertebral levels into 4 regions of 3 vertebral level each:
#   upper cervical cord (uc): C3, C4, C5
#   lower cervical cord (lc): C6, C7, C8
#   upper thoracic cord (ut): T1, T2, T3
#   mid   thoracic cord (mt): T4, T5, T6

dfl_sca_uc <- dfl_sca[dfl_sca$level=="C3" | dfl_sca$level=="C4" | dfl_sca$level=="C5",];
dfl_sca_lc <- dfl_sca[dfl_sca$level=="C6" | dfl_sca$level=="C7" | dfl_sca$level=="C8",];
dfl_sca_ut <- dfl_sca[dfl_sca$level=="T1" | dfl_sca$level=="T2" | dfl_sca$level=="T3",];
dfl_sca_mt <- dfl_sca[dfl_sca$level=="T4" | dfl_sca$level=="T5" | dfl_sca$level=="T6",];

dfl_gma_uc <- dfl_gma[dfl_gma$level=="C3" | dfl_gma$level=="C4" | dfl_gma$level=="C5",];
dfl_gma_lc <- dfl_gma[dfl_gma$level=="C6" | dfl_gma$level=="C7" | dfl_gma$level=="C8",];
dfl_gma_ut <- dfl_gma[dfl_gma$level=="T1" | dfl_gma$level=="T2" | dfl_gma$level=="T3",];
dfl_gma_mt <- dfl_gma[dfl_gma$level=="T4" | dfl_gma$level=="T5" | dfl_gma$level=="T6",];

dfl_gma_dor_uc <- dfl_gma_dor[dfl_gma_dor$level=="C3" | dfl_gma_dor$level=="C4" | dfl_gma_dor$level=="C5",];
dfl_gma_dor_lc <- dfl_gma_dor[dfl_gma_dor$level=="C6" | dfl_gma_dor$level=="C7" | dfl_gma_dor$level=="C8",];
dfl_gma_dor_ut <- dfl_gma_dor[dfl_gma_dor$level=="T1" | dfl_gma_dor$level=="T2" | dfl_gma_dor$level=="T3",];
dfl_gma_dor_mt <- dfl_gma_dor[dfl_gma_dor$level=="T4" | dfl_gma_dor$level=="T5" | dfl_gma_dor$level=="T6",];

dfl_gma_ven_uc <- dfl_gma_ven[dfl_gma_ven$level=="C3" | dfl_gma_ven$level=="C4" | dfl_gma_ven$level=="C5",];
dfl_gma_ven_lc <- dfl_gma_ven[dfl_gma_ven$level=="C6" | dfl_gma_ven$level=="C7" | dfl_gma_ven$level=="C8",];
dfl_gma_ven_ut <- dfl_gma_ven[dfl_gma_ven$level=="T1" | dfl_gma_ven$level=="T2" | dfl_gma_ven$level=="T3",];
dfl_gma_ven_mt <- dfl_gma_ven[dfl_gma_ven$level=="T4" | dfl_gma_ven$level=="T5" | dfl_gma_ven$level=="T6",];

dfl_wma_uc <- dfl_wma[dfl_wma$level=="C3" | dfl_wma$level=="C4" | dfl_wma$level=="C5",];
dfl_wma_lc <- dfl_wma[dfl_wma$level=="C6" | dfl_wma$level=="C7" | dfl_wma$level=="C8",];
dfl_wma_ut <- dfl_wma[dfl_wma$level=="T1" | dfl_wma$level=="T2" | dfl_wma$level=="T3",];
dfl_wma_mt <- dfl_wma[dfl_wma$level=="T4" | dfl_wma$level=="T5" | dfl_wma$level=="T6",];

dfl_wma_dor_uc <- dfl_wma_dor[dfl_wma_dor$level=="C3" | dfl_wma_dor$level=="C4" | dfl_wma_dor$level=="C5",];
dfl_wma_dor_lc <- dfl_wma_dor[dfl_wma_dor$level=="C6" | dfl_wma_dor$level=="C7" | dfl_wma_dor$level=="C8",];
dfl_wma_dor_ut <- dfl_wma_dor[dfl_wma_dor$level=="T1" | dfl_wma_dor$level=="T2" | dfl_wma_dor$level=="T3",];
dfl_wma_dor_mt <- dfl_wma_dor[dfl_wma_dor$level=="T4" | dfl_wma_dor$level=="T5" | dfl_wma_dor$level=="T6",];

dfl_wma_lat_uc <- dfl_wma_lat[dfl_wma_lat$level=="C3" | dfl_wma_lat$level=="C4" | dfl_wma_lat$level=="C5",];
dfl_wma_lat_lc <- dfl_wma_lat[dfl_wma_lat$level=="C6" | dfl_wma_lat$level=="C7" | dfl_wma_lat$level=="C8",];
dfl_wma_lat_ut <- dfl_wma_lat[dfl_wma_lat$level=="T1" | dfl_wma_lat$level=="T2" | dfl_wma_lat$level=="T3",];
dfl_wma_lat_mt <- dfl_wma_lat[dfl_wma_lat$level=="T4" | dfl_wma_lat$level=="T5" | dfl_wma_lat$level=="T6",];

dfl_wma_ven_uc <- dfl_wma_ven[dfl_wma_ven$level=="C3" | dfl_wma_ven$level=="C4" | dfl_wma_ven$level=="C5",];
dfl_wma_ven_lc <- dfl_wma_ven[dfl_wma_ven$level=="C6" | dfl_wma_ven$level=="C7" | dfl_wma_ven$level=="C8",];
dfl_wma_ven_ut <- dfl_wma_ven[dfl_wma_ven$level=="T1" | dfl_wma_ven$level=="T2" | dfl_wma_ven$level=="T3",];
dfl_wma_ven_mt <- dfl_wma_ven[dfl_wma_ven$level=="T4" | dfl_wma_ven$level=="T5" | dfl_wma_ven$level=="T6",];

# Linear mixed effect models on the segment-specific MRI metrics
summary(lme_sca_uc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_sca_uc, na.action = na.omit, control = ctrl))
summary(lme_sca_lc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_sca_lc, na.action = na.omit, control = ctrl))
summary(lme_sca_ut <- lme(area ~ group*level, random = ~ 1|id, data = dfl_sca_ut, na.action = na.omit, control = ctrl))
summary(lme_sca_mt <- lme(area ~ group*level, random = ~ 1|id, data = dfl_sca_mt, na.action = na.omit, control = ctrl))

summary(lme_gma_uc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_uc, na.action = na.omit, control = ctrl))
summary(lme_gma_lc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_lc, na.action = na.omit, control = ctrl))
summary(lme_gma_ut <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_ut, na.action = na.omit, control = ctrl))
summary(lme_gma_mt <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_mt, na.action = na.omit, control = ctrl))

summary(lme_gma_dor_uc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_dor_uc, na.action = na.omit, control = ctrl))
summary(lme_gma_dor_lc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_dor_lc, na.action = na.omit, control = ctrl))
summary(lme_gma_dor_ut <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_dor_ut, na.action = na.omit, control = ctrl))
summary(lme_gma_dor_mt <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_dor_mt, na.action = na.omit, control = ctrl))

summary(lme_gma_ven_uc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_ven_uc, na.action = na.omit, control = ctrl))
summary(lme_gma_ven_lc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_ven_lc, na.action = na.omit, control = ctrl))
summary(lme_gma_ven_ut <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_ven_ut, na.action = na.omit, control = ctrl))
summary(lme_gma_ven_mt <- lme(area ~ group*level, random = ~ 1|id, data = dfl_gma_ven_mt, na.action = na.omit, control = ctrl))

summary(lme_wma_uc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_uc, na.action = na.omit, control = ctrl))
summary(lme_wma_lc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_lc, na.action = na.omit, control = ctrl))
summary(lme_wma_ut <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_ut, na.action = na.omit, control = ctrl))
summary(lme_wma_mt <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_mt, na.action = na.omit, control = ctrl))

summary(lme_wma_dor_uc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_dor_uc, na.action = na.omit, control = ctrl))
summary(lme_wma_dor_lc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_dor_lc, na.action = na.omit, control = ctrl))
summary(lme_wma_dor_ut <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_dor_ut, na.action = na.omit, control = ctrl))
summary(lme_wma_dor_mt <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_dor_mt, na.action = na.omit, control = ctrl))

summary(lme_wma_lat_uc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_lat_uc, na.action = na.omit, control = ctrl))
summary(lme_wma_lat_lc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_lat_lc, na.action = na.omit, control = ctrl))
summary(lme_wma_lat_ut <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_lat_ut, na.action = na.omit, control = ctrl))
summary(lme_wma_lat_mt <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_lat_mt, na.action = na.omit, control = ctrl))

summary(lme_wma_ven_uc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_ven_uc, na.action = na.omit, control = ctrl))
summary(lme_wma_ven_lc <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_ven_lc, na.action = na.omit, control = ctrl))
summary(lme_wma_ven_ut <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_ven_ut, na.action = na.omit, control = ctrl))
summary(lme_wma_ven_mt <- lme(area ~ group*level, random = ~ 1|id, data = dfl_wma_ven_mt, na.action = na.omit, control = ctrl))







# ==================== ANOVA (effect of severity )==============================
# Here we are only interested in the effect of severity (group), NOT the effect of level

anova(lme_sca);
anova(lme_gma);
anova(lme_gma_dor);
anova(lme_gma_ven);
anova(lme_wma);
anova(lme_wma_dor);
anova(lme_wma_lat);
anova(lme_wma_ven);

# Results for group effect:
# no significant group effect.
# significant level effect: p<0.0001 for all.
  
  
  
  
  
  
# ========== post-hoc test: differences to sham ==========
# In the previous step (ANOVA), we have observed no significant effect of severity (group)
# Now, we ask the question: Does any of the SCI group differ from the sham group?
# For that, we perform 3 comparisons: (mild SCI vs. sham), (moderate SCI vs. sham), and (severe SCI vs. sham)
# We apply Bonferroni correction (n=3)

# estimate marginal means
emm_sca     <- emmeans(lme_sca,     specs = ~ group*level)
emm_gma     <- emmeans(lme_gma,     specs = ~ group*level)
emm_gma_dor <- emmeans(lme_gma_dor, specs = ~ group*level)
emm_gma_ven <- emmeans(lme_gma_ven, specs = ~ group*level)
emm_wma     <- emmeans(lme_wma,     specs = ~ group*level)
emm_wma_dor <- emmeans(lme_wma_dor, specs = ~ group*level)
emm_wma_lat <- emmeans(lme_wma_lat, specs = ~ group*level)
emm_wma_ven <- emmeans(lme_wma_ven, specs = ~ group*level)

# define contrasts
shm_uc <- c(1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
mld_uc <- c(0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
mod_uc <- c(0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
sev_uc <- c(0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
shm_lc <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
mld_lc <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
mod_lc <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
sev_lc <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
shm_ut <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
mld_ut <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
mod_ut <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)/3
sev_ut <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)/3
shm_mt <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0)/3
mld_mt <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0)/3
mod_mt <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0)/3
sev_mt <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1)/3

list_uc <- list(
  "sev_uc-shm_uc" = sev_uc - shm_uc,
  "mod_uc-shm_uc" = mod_uc - shm_uc,
  "mld_uc-shm_uc" = mld_uc - shm_uc)

list_lc <- list(
  "sev_lc-shm_lc" = sev_lc - shm_lc,
  "mod_lc-shm_lc" = mod_lc - shm_lc,
  "mld_lc-shm_lc" = mld_lc - shm_lc)

list_ut <- list(
  "sev_ut-shm_ut" = sev_ut - shm_ut,
  "mod_ut-shm_ut" = mod_ut - shm_ut,
  "mld_ut-shm_ut" = mld_ut - shm_ut)

list_mt <- list(
  "sev_mt-shm_mt" = sev_mt - shm_mt,
  "mod_mt-shm_mt" = mod_mt - shm_mt,
  "mld_mt-shm_mt" = mld_mt - shm_mt)

# estimate contrasts
c_sca_uc <- contrast(emm_sca, method = list_uc, adjust = "Bonferroni"); c_sca_uc;
c_sca_lc <- contrast(emm_sca, method = list_lc, adjust = "Bonferroni"); c_sca_lc;
c_sca_ut <- contrast(emm_sca, method = list_ut, adjust = "Bonferroni"); c_sca_ut;
c_sca_mt <- contrast(emm_sca, method = list_mt, adjust = "Bonferroni"); c_sca_mt;

c_gma_uc <- contrast(emm_gma, method = list_uc, adjust = "Bonferroni"); c_gma_uc;
c_gma_lc <- contrast(emm_gma, method = list_lc, adjust = "Bonferroni"); c_gma_lc;
c_gma_ut <- contrast(emm_gma, method = list_ut, adjust = "Bonferroni"); c_gma_ut;
c_gma_mt <- contrast(emm_gma, method = list_mt, adjust = "Bonferroni"); c_gma_mt;

c_gma_dor_uc <- contrast(emm_gma_dor, method = list_uc, adjust = "Bonferroni"); c_gma_dor_uc;
c_gma_dor_lc <- contrast(emm_gma_dor, method = list_lc, adjust = "Bonferroni"); c_gma_dor_lc;
c_gma_dor_ut <- contrast(emm_gma_dor, method = list_ut, adjust = "Bonferroni"); c_gma_dor_ut;
c_gma_dor_mt <- contrast(emm_gma_dor, method = list_mt, adjust = "Bonferroni"); c_gma_dor_mt;

c_gma_ven_uc <- contrast(emm_gma_ven, method = list_uc, adjust = "Bonferroni"); c_gma_ven_uc;
c_gma_ven_lc <- contrast(emm_gma_ven, method = list_lc, adjust = "Bonferroni"); c_gma_ven_lc;
c_gma_ven_ut <- contrast(emm_gma_ven, method = list_ut, adjust = "Bonferroni"); c_gma_ven_ut;
c_gma_ven_mt <- contrast(emm_gma_ven, method = list_mt, adjust = "Bonferroni"); c_gma_ven_mt;

c_wma_uc <- contrast(emm_wma, method = list_uc, adjust = "Bonferroni"); c_wma_uc;
c_wma_lc <- contrast(emm_wma, method = list_lc, adjust = "Bonferroni"); c_wma_lc;
c_wma_ut <- contrast(emm_wma, method = list_ut, adjust = "Bonferroni"); c_wma_ut;
c_wma_mt <- contrast(emm_wma, method = list_mt, adjust = "Bonferroni"); c_wma_mt;

c_wma_dor_uc <- contrast(emm_wma_dor, method = list_uc, adjust = "Bonferroni"); c_wma_dor_uc;
c_wma_dor_lc <- contrast(emm_wma_dor, method = list_lc, adjust = "Bonferroni"); c_wma_dor_lc;
c_wma_dor_ut <- contrast(emm_wma_dor, method = list_ut, adjust = "Bonferroni"); c_wma_dor_ut;
c_wma_dor_mt <- contrast(emm_wma_dor, method = list_mt, adjust = "Bonferroni"); c_wma_dor_mt;

c_wma_lat_uc <- contrast(emm_wma_lat, method = list_uc, adjust = "Bonferroni"); c_wma_lat_uc;
c_wma_lat_lc <- contrast(emm_wma_lat, method = list_lc, adjust = "Bonferroni"); c_wma_lat_lc;
c_wma_lat_ut <- contrast(emm_wma_lat, method = list_ut, adjust = "Bonferroni"); c_wma_lat_ut;
c_wma_lat_mt <- contrast(emm_wma_lat, method = list_mt, adjust = "Bonferroni"); c_wma_lat_mt;

c_wma_ven_uc <- contrast(emm_wma_ven, method = list_uc, adjust = "Bonferroni"); c_wma_ven_uc;
c_wma_ven_lc <- contrast(emm_wma_ven, method = list_lc, adjust = "Bonferroni"); c_wma_ven_lc;
c_wma_ven_ut <- contrast(emm_wma_ven, method = list_ut, adjust = "Bonferroni"); c_wma_ven_ut;
c_wma_ven_mt <- contrast(emm_wma_ven, method = list_mt, adjust = "Bonferroni"); c_wma_ven_mt;


# RESULTS (Bonferroni correction): Atlas-based segmentation
# gma         is different between SHM and MOD at upper-th (p=0.0392)
# gma_ven     is different between SHM and MOD at upper-th (p=0.0282)













# ========== post-hoc test 2: group-wise analysis per each spinal level  ==========
# NOT SHOWN IN THE PAPER
# We've previously detected significant group differences between the mild SCI and SHAM groups.
# Now, we want to test at which levels these differences exist.

# get marginal means
emm2_sca       <- emmeans(lme_sca,     specs = ~ group*level)
emm2_gma       <- emmeans(lme_gma,     specs = ~ group*level)
emm2_gma_dor   <- emmeans(lme_gma_dor, specs = ~ group*level)
emm2_gma_ven   <- emmeans(lme_gma_ven, specs = ~ group*level)
emm2_wma       <- emmeans(lme_wma,     specs = ~ group*level)
emm2_wma_dor   <- emmeans(lme_wma_dor, specs = ~ group*level)
emm2_wma_lat   <- emmeans(lme_wma_lat, specs = ~ group*level)
emm2_wma_ven   <- emmeans(lme_wma_ven, specs = ~ group*level)


# define contrasts
shm_C3  <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mld_C3  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod_C3  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sev_C3  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C4  <- c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mld_C4  <- c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod_C4  <- c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sev_C4  <- c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C5  <- c(1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mld_C5  <- c(0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod_C5  <- c(0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sev_C5  <- c(0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C6  <- c(1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mld_C6  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod_C6  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sev_C6  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C7  <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mld_C7  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod_C7  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sev_C7  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_C8  <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mld_C8  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod_C8  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sev_C8  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_T1  <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mld_T1  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod_T1  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sev_T1  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_T2  <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mld_T2  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod_T2  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
sev_T2  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
shm_T3  <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mld_T3  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod_T3  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
sev_T3  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
shm_T4  <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
mld_T4  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
mod_T4  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
sev_T4  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
shm_T5  <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
mld_T5  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
mod_T5  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
sev_T5  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
shm_T6  <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
mld_T6  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
mod_T6  <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
sev_T6  <- c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)


list <- list(
  "sev_C3-shm_C3" = sev_C3 - shm_C3,
  "mod_C3-shm_C3" = mod_C3 - shm_C3,
  "mld_C3-shm_C3" = mld_C3 - shm_C3,
  
  "sev_C4-shm_C4" = sev_C4 - shm_C4,
  "mod_C4-shm_C4" = mod_C4 - shm_C4,
  "mld_C4-shm_C4" = mld_C4 - shm_C4,
  
  "sev_C5-shm_C5" = sev_C5 - shm_C5,
  "mod_C5-shm_C5" = mod_C5 - shm_C5,
  "mld_C5-shm_C5" = mld_C5 - shm_C5,
  
  "sev_C6-shm_C6" = sev_C6 - shm_C6,
  "mod_C6-shm_C6" = mod_C6 - shm_C6,
  "mld_C6-shm_C6" = mld_C6 - shm_C6,
  
  "sev_C7-shm_C7" = sev_C7 - shm_C7,
  "mod_C7-shm_C7" = mod_C7 - shm_C7,
  "mld_C7-shm_C7" = mld_C7 - shm_C7,
  
  "sev_C8-shm_C8" = sev_C8 - shm_C8,
  "mod_C8-shm_C8" = mod_C8 - shm_C8,
  "mld_C8-shm_C8" = mld_C8 - shm_C8,
  
  "sev_T1-shm_T1" = sev_T1 - shm_T1,
  "mod_T1-shm_T1" = mod_T1 - shm_T1,
  "mld_T1-shm_T1" = mld_T1 - shm_T1,
  
  "sev_T2-shm_T2" = sev_T2 - shm_T2,
  "mod_T2-shm_T2" = mod_T2 - shm_T2,
  "mld_T2-shm_T2" = mld_T2 - shm_T2,
  
  "sev_T3-shm_T3" = sev_T3 - shm_T3,
  "mod_T3-shm_T3" = mod_T3 - shm_T3,
  "mld_T3-shm_T3" = mld_T3 - shm_T3,
  
  "sev_T4-shm_T4" = sev_T4 - shm_T4,
  "mod_T4-shm_T4" = mod_T4 - shm_T4,
  "mld_T4-shm_T4" = mld_T4 - shm_T4,
  
  "sev_T5-shm_T5" = sev_T5 - shm_T5,
  "mod_T5-shm_T5" = mod_T5 - shm_T5,
  "mld_T5-shm_T5" = mld_T5 - shm_T5,
  
  "sev_T6-shm_T6" = sev_T6 - shm_T6,
  "mod_T6-shm_T6" = mod_T6 - shm_T6,
  "mld_T6-shm_T6" = mld_T6 - shm_T6
)

# estimate contrasts
c2_sca       <- contrast(emm2_sca,     method = list); c2_sca;
c2_gma       <- contrast(emm2_gma,     method = list); c2_gma;
c2_gma_dor   <- contrast(emm2_gma_dor, method = list); c2_gma_dor;
c2_gma_ven   <- contrast(emm2_gma_ven, method = list); c2_gma_ven;
c2_wma       <- contrast(emm2_wma,     method = list); c2_wma;
c2_wma_dor   <- contrast(emm2_wma_dor, method = list); c2_wma_dor;
c2_wma_lat   <- contrast(emm2_wma_lat, method = list); c2_wma_lat;
c2_wma_ven   <- contrast(emm2_wma_ven, method = list); c2_wma_ven;

# RESULTS (no correction): Atlas-based CSA analysis: Post-hoc tests revealed that
# sca     in different between SHM and MOD at T1 (p=0.0478)

# gma     is different between SHM and SEV at T1 (p=0.0477), C5 (p=0.0366)
# gma     is different between SHM and MOD at T2 (p=0.0462), T1 (p=0.0074), C8 (p=0.0138), 
# gma     is different between SHM and MLD

# gma_dor is different between SHM and MOD at T5 (p=0.0322), T4 (p=0.0303), T3 (p=0.0336), T2 (p=0.0351), T1 (p=0.0114), C8 (p=0.0265), C5 (p=0.0379), C3 (p=0.0278)
# gma_dor is different between SHM and MLD at T3 (p=0.0498), T2 (p=0.0490), C5 (p=0.0226), C4 (p=0.0145), C3 (p=0.0281)

# gma_ven is different between SHM and MOD at T1 (p=0.0072), C8 (p=0.0114)
# gma_ven is different between SHM and SEV at T1 (p=0.0243), C5 (p=0.0265)

# wma_dor is different between SHM and MOD at T6 (p=0.0375), T4 (p=0.0327), T3 (p=0.0451), T2 (p=0.0445), T1 (p=0.0176), C8 (p=0.386), C5 (p=0.0316)











