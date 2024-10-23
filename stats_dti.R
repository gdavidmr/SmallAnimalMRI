# ======================================================================================
# ==================== PART III. ANALYSIS OF DTI METRICS ===============================
# ======================================================================================

# GENERATE PLOTS HERE; use the script
# stats_plot_figs.R

# ==================== SET UP LINEAR MIXED EFFECT MODEL ========================

# FIT MIXED EFFECT MODEL (segment as categorical variable)
# mixed effect model to investigate group differences over segments
#   - group: fixed effect (4 levels)
#   - segment: fixed effect (12 levels)
#   - group-segment interaction: fixed effect (48 levels)
#   - subject over segments: random effect
#   - area: dependent variable
#
# Model: dti_metric ~ group + segment + segment*subject(group_bin) + segment|subject

ctrl <- lmeControl(maxIter = 1000, msMaxIter = 1000, optimMethod = "optim", msMaxEval = 1000, niterEM = 1000)

summary(lme_fa_gm     <- lme(data = dfl_fa_gm,     value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_dor <- lme(data = dfl_fa_gm_dor, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_ven <- lme(data = dfl_fa_gm_ven, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm     <- lme(data = dfl_fa_wm,     value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_dor <- lme(data = dfl_fa_wm_dor, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_lat <- lme(data = dfl_fa_wm_lat, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_ven <- lme(data = dfl_fa_wm_ven, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm     <- lme(data = dfl_md_gm,     value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_dor <- lme(data = dfl_md_gm_dor, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_ven <- lme(data = dfl_md_gm_ven, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm     <- lme(data = dfl_md_wm,     value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_dor <- lme(data = dfl_md_wm_dor, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_lat <- lme(data = dfl_md_wm_lat, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_ven <- lme(data = dfl_md_wm_ven, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm     <- lme(data = dfl_ad_gm,     value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_dor <- lme(data = dfl_ad_gm_dor, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_ven <- lme(data = dfl_ad_gm_ven, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm     <- lme(data = dfl_ad_wm,     value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_dor <- lme(data = dfl_ad_wm_dor, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_lat <- lme(data = dfl_ad_wm_lat, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_ven <- lme(data = dfl_ad_wm_ven, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm     <- lme(data = dfl_rd_gm,     value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_dor <- lme(data = dfl_rd_gm_dor, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_ven <- lme(data = dfl_rd_gm_ven, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm     <- lme(data = dfl_rd_wm,     value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_dor <- lme(data = dfl_rd_wm_dor, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_lat <- lme(data = dfl_rd_wm_lat, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_ven <- lme(data = dfl_rd_wm_ven, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))

# To reduce the number of levels, we combine the 16 vertebral levels into 4 regions of 3 vertebral level each:
#   upper cervical cord (uc): C3, C4, C5
#   lower cervical cord (lc): C6, C7, C8
#   upper thoracic cord (ut): T1, T2, T3
#   mid   thoracic cord (mt): T4, T5, T6

dfl_fa_wm_uc     <- dfl_fa_wm[dfl_fa_wm$level=="C3" | dfl_fa_wm$level=="C4" | dfl_fa_wm$level=="C5",];
dfl_fa_wm_lc     <- dfl_fa_wm[dfl_fa_wm$level=="C6" | dfl_fa_wm$level=="C7" | dfl_fa_wm$level=="C8",];
dfl_fa_wm_ut     <- dfl_fa_wm[dfl_fa_wm$level=="T1" | dfl_fa_wm$level=="T2" | dfl_fa_wm$level=="T3",];
dfl_fa_wm_mt     <- dfl_fa_wm[dfl_fa_wm$level=="T4" | dfl_fa_wm$level=="T5" | dfl_fa_wm$level=="T6",];
dfl_fa_wm_dor_uc <- dfl_fa_wm_dor[dfl_fa_wm_dor$level=="C3" | dfl_fa_wm_dor$level=="C4" | dfl_fa_wm_dor$level=="C5",];
dfl_fa_wm_dor_lc <- dfl_fa_wm_dor[dfl_fa_wm_dor$level=="C6" | dfl_fa_wm_dor$level=="C7" | dfl_fa_wm_dor$level=="C8",];
dfl_fa_wm_dor_ut <- dfl_fa_wm_dor[dfl_fa_wm_dor$level=="T1" | dfl_fa_wm_dor$level=="T2" | dfl_fa_wm_dor$level=="T3",];
dfl_fa_wm_dor_mt <- dfl_fa_wm_dor[dfl_fa_wm_dor$level=="T4" | dfl_fa_wm_dor$level=="T5" | dfl_fa_wm_dor$level=="T6",];
dfl_fa_wm_lat_uc <- dfl_fa_wm_lat[dfl_fa_wm_lat$level=="C3" | dfl_fa_wm_lat$level=="C4" | dfl_fa_wm_lat$level=="C5",];
dfl_fa_wm_lat_lc <- dfl_fa_wm_lat[dfl_fa_wm_lat$level=="C6" | dfl_fa_wm_lat$level=="C7" | dfl_fa_wm_lat$level=="C8",];
dfl_fa_wm_lat_ut <- dfl_fa_wm_lat[dfl_fa_wm_lat$level=="T1" | dfl_fa_wm_lat$level=="T2" | dfl_fa_wm_lat$level=="T3",];
dfl_fa_wm_lat_mt <- dfl_fa_wm_lat[dfl_fa_wm_lat$level=="T4" | dfl_fa_wm_lat$level=="T5" | dfl_fa_wm_lat$level=="T6",];
dfl_fa_wm_ven_uc <- dfl_fa_wm_ven[dfl_fa_wm_ven$level=="C3" | dfl_fa_wm_ven$level=="C4" | dfl_fa_wm_ven$level=="C5",];
dfl_fa_wm_ven_lc <- dfl_fa_wm_ven[dfl_fa_wm_ven$level=="C6" | dfl_fa_wm_ven$level=="C7" | dfl_fa_wm_ven$level=="C8",];
dfl_fa_wm_ven_ut <- dfl_fa_wm_ven[dfl_fa_wm_ven$level=="T1" | dfl_fa_wm_ven$level=="T2" | dfl_fa_wm_ven$level=="T3",];
dfl_fa_wm_ven_mt <- dfl_fa_wm_ven[dfl_fa_wm_ven$level=="T4" | dfl_fa_wm_ven$level=="T5" | dfl_fa_wm_ven$level=="T6",];
dfl_fa_gm_uc     <- dfl_fa_gm[dfl_fa_gm$level=="C3" | dfl_fa_gm$level=="C4" | dfl_fa_gm$level=="C5",];
dfl_fa_gm_lc     <- dfl_fa_gm[dfl_fa_gm$level=="C6" | dfl_fa_gm$level=="C7" | dfl_fa_gm$level=="C8",];
dfl_fa_gm_ut     <- dfl_fa_gm[dfl_fa_gm$level=="T1" | dfl_fa_gm$level=="T2" | dfl_fa_gm$level=="T3",];
dfl_fa_gm_mt     <- dfl_fa_gm[dfl_fa_gm$level=="T4" | dfl_fa_gm$level=="T5" | dfl_fa_gm$level=="T6",];
dfl_fa_gm_dor_uc <- dfl_fa_gm_dor[dfl_fa_gm_dor$level=="C3" | dfl_fa_gm_dor$level=="C4" | dfl_fa_gm_dor$level=="C5",];
dfl_fa_gm_dor_lc <- dfl_fa_gm_dor[dfl_fa_gm_dor$level=="C6" | dfl_fa_gm_dor$level=="C7" | dfl_fa_gm_dor$level=="C8",];
dfl_fa_gm_dor_ut <- dfl_fa_gm_dor[dfl_fa_gm_dor$level=="T1" | dfl_fa_gm_dor$level=="T2" | dfl_fa_gm_dor$level=="T3",];
dfl_fa_gm_dor_mt <- dfl_fa_gm_dor[dfl_fa_gm_dor$level=="T4" | dfl_fa_gm_dor$level=="T5" | dfl_fa_gm_dor$level=="T6",];
dfl_fa_gm_ven_uc <- dfl_fa_gm_ven[dfl_fa_gm_ven$level=="C3" | dfl_fa_gm_ven$level=="C4" | dfl_fa_gm_ven$level=="C5",];
dfl_fa_gm_ven_lc <- dfl_fa_gm_ven[dfl_fa_gm_ven$level=="C6" | dfl_fa_gm_ven$level=="C7" | dfl_fa_gm_ven$level=="C8",];
dfl_fa_gm_ven_ut <- dfl_fa_gm_ven[dfl_fa_gm_ven$level=="T1" | dfl_fa_gm_ven$level=="T2" | dfl_fa_gm_ven$level=="T3",];
dfl_fa_gm_ven_mt <- dfl_fa_gm_ven[dfl_fa_gm_ven$level=="T4" | dfl_fa_gm_ven$level=="T5" | dfl_fa_gm_ven$level=="T6",];

dfl_md_wm_uc     <- dfl_md_wm[dfl_md_wm$level=="C3" | dfl_md_wm$level=="C4" | dfl_md_wm$level=="C5",];
dfl_md_wm_lc     <- dfl_md_wm[dfl_md_wm$level=="C6" | dfl_md_wm$level=="C7" | dfl_md_wm$level=="C8",];
dfl_md_wm_ut     <- dfl_md_wm[dfl_md_wm$level=="T1" | dfl_md_wm$level=="T2" | dfl_md_wm$level=="T3",];
dfl_md_wm_mt     <- dfl_md_wm[dfl_md_wm$level=="T4" | dfl_md_wm$level=="T5" | dfl_md_wm$level=="T6",];
dfl_md_wm_dor_uc <- dfl_md_wm_dor[dfl_md_wm_dor$level=="C3" | dfl_md_wm_dor$level=="C4" | dfl_md_wm_dor$level=="C5",];
dfl_md_wm_dor_lc <- dfl_md_wm_dor[dfl_md_wm_dor$level=="C6" | dfl_md_wm_dor$level=="C7" | dfl_md_wm_dor$level=="C8",];
dfl_md_wm_dor_ut <- dfl_md_wm_dor[dfl_md_wm_dor$level=="T1" | dfl_md_wm_dor$level=="T2" | dfl_md_wm_dor$level=="T3",];
dfl_md_wm_dor_mt <- dfl_md_wm_dor[dfl_md_wm_dor$level=="T4" | dfl_md_wm_dor$level=="T5" | dfl_md_wm_dor$level=="T6",];
dfl_md_wm_lat_uc <- dfl_md_wm_lat[dfl_md_wm_lat$level=="C3" | dfl_md_wm_lat$level=="C4" | dfl_md_wm_lat$level=="C5",];
dfl_md_wm_lat_lc <- dfl_md_wm_lat[dfl_md_wm_lat$level=="C6" | dfl_md_wm_lat$level=="C7" | dfl_md_wm_lat$level=="C8",];
dfl_md_wm_lat_ut <- dfl_md_wm_lat[dfl_md_wm_lat$level=="T1" | dfl_md_wm_lat$level=="T2" | dfl_md_wm_lat$level=="T3",];
dfl_md_wm_lat_mt <- dfl_md_wm_lat[dfl_md_wm_lat$level=="T4" | dfl_md_wm_lat$level=="T5" | dfl_md_wm_lat$level=="T6",];
dfl_md_wm_ven_uc <- dfl_md_wm_ven[dfl_md_wm_ven$level=="C3" | dfl_md_wm_ven$level=="C4" | dfl_md_wm_ven$level=="C5",];
dfl_md_wm_ven_lc <- dfl_md_wm_ven[dfl_md_wm_ven$level=="C6" | dfl_md_wm_ven$level=="C7" | dfl_md_wm_ven$level=="C8",];
dfl_md_wm_ven_ut <- dfl_md_wm_ven[dfl_md_wm_ven$level=="T1" | dfl_md_wm_ven$level=="T2" | dfl_md_wm_ven$level=="T3",];
dfl_md_wm_ven_mt <- dfl_md_wm_ven[dfl_md_wm_ven$level=="T4" | dfl_md_wm_ven$level=="T5" | dfl_md_wm_ven$level=="T6",];
dfl_md_gm_uc     <- dfl_md_gm[dfl_md_gm$level=="C3" | dfl_md_gm$level=="C4" | dfl_md_gm$level=="C5",];
dfl_md_gm_lc     <- dfl_md_gm[dfl_md_gm$level=="C6" | dfl_md_gm$level=="C7" | dfl_md_gm$level=="C8",];
dfl_md_gm_ut     <- dfl_md_gm[dfl_md_gm$level=="T1" | dfl_md_gm$level=="T2" | dfl_md_gm$level=="T3",];
dfl_md_gm_mt     <- dfl_md_gm[dfl_md_gm$level=="T4" | dfl_md_gm$level=="T5" | dfl_md_gm$level=="T6",];
dfl_md_gm_dor_uc <- dfl_md_gm_dor[dfl_md_gm_dor$level=="C3" | dfl_md_gm_dor$level=="C4" | dfl_md_gm_dor$level=="C5",];
dfl_md_gm_dor_lc <- dfl_md_gm_dor[dfl_md_gm_dor$level=="C6" | dfl_md_gm_dor$level=="C7" | dfl_md_gm_dor$level=="C8",];
dfl_md_gm_dor_ut <- dfl_md_gm_dor[dfl_md_gm_dor$level=="T1" | dfl_md_gm_dor$level=="T2" | dfl_md_gm_dor$level=="T3",];
dfl_md_gm_dor_mt <- dfl_md_gm_dor[dfl_md_gm_dor$level=="T4" | dfl_md_gm_dor$level=="T5" | dfl_md_gm_dor$level=="T6",];
dfl_md_gm_ven_uc <- dfl_md_gm_ven[dfl_md_gm_ven$level=="C3" | dfl_md_gm_ven$level=="C4" | dfl_md_gm_ven$level=="C5",];
dfl_md_gm_ven_lc <- dfl_md_gm_ven[dfl_md_gm_ven$level=="C6" | dfl_md_gm_ven$level=="C7" | dfl_md_gm_ven$level=="C8",];
dfl_md_gm_ven_ut <- dfl_md_gm_ven[dfl_md_gm_ven$level=="T1" | dfl_md_gm_ven$level=="T2" | dfl_md_gm_ven$level=="T3",];
dfl_md_gm_ven_mt <- dfl_md_gm_ven[dfl_md_gm_ven$level=="T4" | dfl_md_gm_ven$level=="T5" | dfl_md_gm_ven$level=="T6",];


dfl_ad_wm_uc     <- dfl_ad_wm[dfl_ad_wm$level=="C3" | dfl_ad_wm$level=="C4" | dfl_ad_wm$level=="C5",];
dfl_ad_wm_lc     <- dfl_ad_wm[dfl_ad_wm$level=="C6" | dfl_ad_wm$level=="C7" | dfl_ad_wm$level=="C8",];
dfl_ad_wm_ut     <- dfl_ad_wm[dfl_ad_wm$level=="T1" | dfl_ad_wm$level=="T2" | dfl_ad_wm$level=="T3",];
dfl_ad_wm_mt     <- dfl_ad_wm[dfl_ad_wm$level=="T4" | dfl_ad_wm$level=="T5" | dfl_ad_wm$level=="T6",];
dfl_ad_wm_dor_uc <- dfl_ad_wm_dor[dfl_ad_wm_dor$level=="C3" | dfl_ad_wm_dor$level=="C4" | dfl_ad_wm_dor$level=="C5",];
dfl_ad_wm_dor_lc <- dfl_ad_wm_dor[dfl_ad_wm_dor$level=="C6" | dfl_ad_wm_dor$level=="C7" | dfl_ad_wm_dor$level=="C8",];
dfl_ad_wm_dor_ut <- dfl_ad_wm_dor[dfl_ad_wm_dor$level=="T1" | dfl_ad_wm_dor$level=="T2" | dfl_ad_wm_dor$level=="T3",];
dfl_ad_wm_dor_mt <- dfl_ad_wm_dor[dfl_ad_wm_dor$level=="T4" | dfl_ad_wm_dor$level=="T5" | dfl_ad_wm_dor$level=="T6",];
dfl_ad_wm_lat_uc <- dfl_ad_wm_lat[dfl_ad_wm_lat$level=="C3" | dfl_ad_wm_lat$level=="C4" | dfl_ad_wm_lat$level=="C5",];
dfl_ad_wm_lat_lc <- dfl_ad_wm_lat[dfl_ad_wm_lat$level=="C6" | dfl_ad_wm_lat$level=="C7" | dfl_ad_wm_lat$level=="C8",];
dfl_ad_wm_lat_ut <- dfl_ad_wm_lat[dfl_ad_wm_lat$level=="T1" | dfl_ad_wm_lat$level=="T2" | dfl_ad_wm_lat$level=="T3",];
dfl_ad_wm_lat_mt <- dfl_ad_wm_lat[dfl_ad_wm_lat$level=="T4" | dfl_ad_wm_lat$level=="T5" | dfl_ad_wm_lat$level=="T6",];
dfl_ad_wm_ven_uc <- dfl_ad_wm_ven[dfl_ad_wm_ven$level=="C3" | dfl_ad_wm_ven$level=="C4" | dfl_ad_wm_ven$level=="C5",];
dfl_ad_wm_ven_lc <- dfl_ad_wm_ven[dfl_ad_wm_ven$level=="C6" | dfl_ad_wm_ven$level=="C7" | dfl_ad_wm_ven$level=="C8",];
dfl_ad_wm_ven_ut <- dfl_ad_wm_ven[dfl_ad_wm_ven$level=="T1" | dfl_ad_wm_ven$level=="T2" | dfl_ad_wm_ven$level=="T3",];
dfl_ad_wm_ven_mt <- dfl_ad_wm_ven[dfl_ad_wm_ven$level=="T4" | dfl_ad_wm_ven$level=="T5" | dfl_ad_wm_ven$level=="T6",];
dfl_ad_gm_uc     <- dfl_ad_gm[dfl_ad_gm$level=="C3" | dfl_ad_gm$level=="C4" | dfl_ad_gm$level=="C5",];
dfl_ad_gm_lc     <- dfl_ad_gm[dfl_ad_gm$level=="C6" | dfl_ad_gm$level=="C7" | dfl_ad_gm$level=="C8",];
dfl_ad_gm_ut     <- dfl_ad_gm[dfl_ad_gm$level=="T1" | dfl_ad_gm$level=="T2" | dfl_ad_gm$level=="T3",];
dfl_ad_gm_mt     <- dfl_ad_gm[dfl_ad_gm$level=="T4" | dfl_ad_gm$level=="T5" | dfl_ad_gm$level=="T6",];
dfl_ad_gm_dor_uc <- dfl_ad_gm_dor[dfl_ad_gm_dor$level=="C3" | dfl_ad_gm_dor$level=="C4" | dfl_ad_gm_dor$level=="C5",];
dfl_ad_gm_dor_lc <- dfl_ad_gm_dor[dfl_ad_gm_dor$level=="C6" | dfl_ad_gm_dor$level=="C7" | dfl_ad_gm_dor$level=="C8",];
dfl_ad_gm_dor_ut <- dfl_ad_gm_dor[dfl_ad_gm_dor$level=="T1" | dfl_ad_gm_dor$level=="T2" | dfl_ad_gm_dor$level=="T3",];
dfl_ad_gm_dor_mt <- dfl_ad_gm_dor[dfl_ad_gm_dor$level=="T4" | dfl_ad_gm_dor$level=="T5" | dfl_ad_gm_dor$level=="T6",];
dfl_ad_gm_ven_uc <- dfl_ad_gm_ven[dfl_ad_gm_ven$level=="C3" | dfl_ad_gm_ven$level=="C4" | dfl_ad_gm_ven$level=="C5",];
dfl_ad_gm_ven_lc <- dfl_ad_gm_ven[dfl_ad_gm_ven$level=="C6" | dfl_ad_gm_ven$level=="C7" | dfl_ad_gm_ven$level=="C8",];
dfl_ad_gm_ven_ut <- dfl_ad_gm_ven[dfl_ad_gm_ven$level=="T1" | dfl_ad_gm_ven$level=="T2" | dfl_ad_gm_ven$level=="T3",];
dfl_ad_gm_ven_mt <- dfl_ad_gm_ven[dfl_ad_gm_ven$level=="T4" | dfl_ad_gm_ven$level=="T5" | dfl_ad_gm_ven$level=="T6",];


dfl_rd_wm_uc     <- dfl_rd_wm[dfl_rd_wm$level=="C3" | dfl_rd_wm$level=="C4" | dfl_rd_wm$level=="C5",];
dfl_rd_wm_lc     <- dfl_rd_wm[dfl_rd_wm$level=="C6" | dfl_rd_wm$level=="C7" | dfl_rd_wm$level=="C8",];
dfl_rd_wm_ut     <- dfl_rd_wm[dfl_rd_wm$level=="T1" | dfl_rd_wm$level=="T2" | dfl_rd_wm$level=="T3",];
dfl_rd_wm_mt     <- dfl_rd_wm[dfl_rd_wm$level=="T4" | dfl_rd_wm$level=="T5" | dfl_rd_wm$level=="T6",];
dfl_rd_wm_dor_uc <- dfl_rd_wm_dor[dfl_rd_wm_dor$level=="C3" | dfl_rd_wm_dor$level=="C4" | dfl_rd_wm_dor$level=="C5",];
dfl_rd_wm_dor_lc <- dfl_rd_wm_dor[dfl_rd_wm_dor$level=="C6" | dfl_rd_wm_dor$level=="C7" | dfl_rd_wm_dor$level=="C8",];
dfl_rd_wm_dor_ut <- dfl_rd_wm_dor[dfl_rd_wm_dor$level=="T1" | dfl_rd_wm_dor$level=="T2" | dfl_rd_wm_dor$level=="T3",];
dfl_rd_wm_dor_mt <- dfl_rd_wm_dor[dfl_rd_wm_dor$level=="T4" | dfl_rd_wm_dor$level=="T5" | dfl_rd_wm_dor$level=="T6",];
dfl_rd_wm_lat_uc <- dfl_rd_wm_lat[dfl_rd_wm_lat$level=="C3" | dfl_rd_wm_lat$level=="C4" | dfl_rd_wm_lat$level=="C5",];
dfl_rd_wm_lat_lc <- dfl_rd_wm_lat[dfl_rd_wm_lat$level=="C6" | dfl_rd_wm_lat$level=="C7" | dfl_rd_wm_lat$level=="C8",];
dfl_rd_wm_lat_ut <- dfl_rd_wm_lat[dfl_rd_wm_lat$level=="T1" | dfl_rd_wm_lat$level=="T2" | dfl_rd_wm_lat$level=="T3",];
dfl_rd_wm_lat_mt <- dfl_rd_wm_lat[dfl_rd_wm_lat$level=="T4" | dfl_rd_wm_lat$level=="T5" | dfl_rd_wm_lat$level=="T6",];
dfl_rd_wm_ven_uc <- dfl_rd_wm_ven[dfl_rd_wm_ven$level=="C3" | dfl_rd_wm_ven$level=="C4" | dfl_rd_wm_ven$level=="C5",];
dfl_rd_wm_ven_lc <- dfl_rd_wm_ven[dfl_rd_wm_ven$level=="C6" | dfl_rd_wm_ven$level=="C7" | dfl_rd_wm_ven$level=="C8",];
dfl_rd_wm_ven_ut <- dfl_rd_wm_ven[dfl_rd_wm_ven$level=="T1" | dfl_rd_wm_ven$level=="T2" | dfl_rd_wm_ven$level=="T3",];
dfl_rd_wm_ven_mt <- dfl_rd_wm_ven[dfl_rd_wm_ven$level=="T4" | dfl_rd_wm_ven$level=="T5" | dfl_rd_wm_ven$level=="T6",];
dfl_rd_gm_uc     <- dfl_rd_gm[dfl_rd_gm$level=="C3" | dfl_rd_gm$level=="C4" | dfl_rd_gm$level=="C5",];
dfl_rd_gm_lc     <- dfl_rd_gm[dfl_rd_gm$level=="C6" | dfl_rd_gm$level=="C7" | dfl_rd_gm$level=="C8",];
dfl_rd_gm_ut     <- dfl_rd_gm[dfl_rd_gm$level=="T1" | dfl_rd_gm$level=="T2" | dfl_rd_gm$level=="T3",];
dfl_rd_gm_mt     <- dfl_rd_gm[dfl_rd_gm$level=="T4" | dfl_rd_gm$level=="T5" | dfl_rd_gm$level=="T6",];
dfl_rd_gm_dor_uc <- dfl_rd_gm_dor[dfl_rd_gm_dor$level=="C3" | dfl_rd_gm_dor$level=="C4" | dfl_rd_gm_dor$level=="C5",];
dfl_rd_gm_dor_lc <- dfl_rd_gm_dor[dfl_rd_gm_dor$level=="C6" | dfl_rd_gm_dor$level=="C7" | dfl_rd_gm_dor$level=="C8",];
dfl_rd_gm_dor_ut <- dfl_rd_gm_dor[dfl_rd_gm_dor$level=="T1" | dfl_rd_gm_dor$level=="T2" | dfl_rd_gm_dor$level=="T3",];
dfl_rd_gm_dor_mt <- dfl_rd_gm_dor[dfl_rd_gm_dor$level=="T4" | dfl_rd_gm_dor$level=="T5" | dfl_rd_gm_dor$level=="T6",];
dfl_rd_gm_ven_uc <- dfl_rd_gm_ven[dfl_rd_gm_ven$level=="C3" | dfl_rd_gm_ven$level=="C4" | dfl_rd_gm_ven$level=="C5",];
dfl_rd_gm_ven_lc <- dfl_rd_gm_ven[dfl_rd_gm_ven$level=="C6" | dfl_rd_gm_ven$level=="C7" | dfl_rd_gm_ven$level=="C8",];
dfl_rd_gm_ven_ut <- dfl_rd_gm_ven[dfl_rd_gm_ven$level=="T1" | dfl_rd_gm_ven$level=="T2" | dfl_rd_gm_ven$level=="T3",];
dfl_rd_gm_ven_mt <- dfl_rd_gm_ven[dfl_rd_gm_ven$level=="T4" | dfl_rd_gm_ven$level=="T5" | dfl_rd_gm_ven$level=="T6",];

# Linear mixed effect models on the segment-specific MRI metrics
summary(lme_fa_wm_uc <- lme(data = dfl_fa_wm_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_lc <- lme(data = dfl_fa_wm_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_ut <- lme(data = dfl_fa_wm_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_mt <- lme(data = dfl_fa_wm_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_dor_uc <- lme(data = dfl_fa_wm_dor_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_dor_lc <- lme(data = dfl_fa_wm_dor_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_dor_ut <- lme(data = dfl_fa_wm_dor_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_dor_mt <- lme(data = dfl_fa_wm_dor_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_lat_uc <- lme(data = dfl_fa_wm_lat_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_lat_lc <- lme(data = dfl_fa_wm_lat_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_lat_ut <- lme(data = dfl_fa_wm_lat_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_lat_mt <- lme(data = dfl_fa_wm_lat_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_ven_uc <- lme(data = dfl_fa_wm_ven_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_ven_lc <- lme(data = dfl_fa_wm_ven_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_ven_ut <- lme(data = dfl_fa_wm_ven_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_wm_ven_mt <- lme(data = dfl_fa_wm_ven_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_uc <- lme(data = dfl_fa_gm_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_lc <- lme(data = dfl_fa_gm_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_ut <- lme(data = dfl_fa_gm_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_mt <- lme(data = dfl_fa_gm_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_dor_uc <- lme(data = dfl_fa_gm_dor_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_dor_lc <- lme(data = dfl_fa_gm_dor_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_dor_ut <- lme(data = dfl_fa_gm_dor_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_dor_mt <- lme(data = dfl_fa_gm_dor_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_ven_uc <- lme(data = dfl_fa_gm_ven_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_ven_lc <- lme(data = dfl_fa_gm_ven_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_ven_ut <- lme(data = dfl_fa_gm_ven_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_fa_gm_ven_mt <- lme(data = dfl_fa_gm_ven_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))

summary(lme_md_wm_uc <- lme(data = dfl_md_wm_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_lc <- lme(data = dfl_md_wm_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_ut <- lme(data = dfl_md_wm_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_mt <- lme(data = dfl_md_wm_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_dor_uc <- lme(data = dfl_md_wm_dor_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_dor_lc <- lme(data = dfl_md_wm_dor_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_dor_ut <- lme(data = dfl_md_wm_dor_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_dor_mt <- lme(data = dfl_md_wm_dor_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_lat_uc <- lme(data = dfl_md_wm_lat_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_lat_lc <- lme(data = dfl_md_wm_lat_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_lat_ut <- lme(data = dfl_md_wm_lat_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_lat_mt <- lme(data = dfl_md_wm_lat_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_ven_uc <- lme(data = dfl_md_wm_ven_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_ven_lc <- lme(data = dfl_md_wm_ven_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_ven_ut <- lme(data = dfl_md_wm_ven_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_wm_ven_mt <- lme(data = dfl_md_wm_ven_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_uc <- lme(data = dfl_md_gm_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_lc <- lme(data = dfl_md_gm_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_ut <- lme(data = dfl_md_gm_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_mt <- lme(data = dfl_md_gm_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_dor_uc <- lme(data = dfl_md_gm_dor_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_dor_lc <- lme(data = dfl_md_gm_dor_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_dor_ut <- lme(data = dfl_md_gm_dor_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_dor_mt <- lme(data = dfl_md_gm_dor_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_ven_uc <- lme(data = dfl_md_gm_ven_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_ven_lc <- lme(data = dfl_md_gm_ven_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_ven_ut <- lme(data = dfl_md_gm_ven_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_md_gm_ven_mt <- lme(data = dfl_md_gm_ven_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))

summary(lme_ad_wm_uc <- lme(data = dfl_ad_wm_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_lc <- lme(data = dfl_ad_wm_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_ut <- lme(data = dfl_ad_wm_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_mt <- lme(data = dfl_ad_wm_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_dor_uc <- lme(data = dfl_ad_wm_dor_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_dor_lc <- lme(data = dfl_ad_wm_dor_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_dor_ut <- lme(data = dfl_ad_wm_dor_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_dor_mt <- lme(data = dfl_ad_wm_dor_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_lat_uc <- lme(data = dfl_ad_wm_lat_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_lat_lc <- lme(data = dfl_ad_wm_lat_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_lat_ut <- lme(data = dfl_ad_wm_lat_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_lat_mt <- lme(data = dfl_ad_wm_lat_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_ven_uc <- lme(data = dfl_ad_wm_ven_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_ven_lc <- lme(data = dfl_ad_wm_ven_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_ven_ut <- lme(data = dfl_ad_wm_ven_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_wm_ven_mt <- lme(data = dfl_ad_wm_ven_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_uc <- lme(data = dfl_ad_gm_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_lc <- lme(data = dfl_ad_gm_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_ut <- lme(data = dfl_ad_gm_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_mt <- lme(data = dfl_ad_gm_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_dor_uc <- lme(data = dfl_ad_gm_dor_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_dor_lc <- lme(data = dfl_ad_gm_dor_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_dor_ut <- lme(data = dfl_ad_gm_dor_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_dor_mt <- lme(data = dfl_ad_gm_dor_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_ven_uc <- lme(data = dfl_ad_gm_ven_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_ven_lc <- lme(data = dfl_ad_gm_ven_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_ven_ut <- lme(data = dfl_ad_gm_ven_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_ad_gm_ven_mt <- lme(data = dfl_ad_gm_ven_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))

summary(lme_rd_wm_uc <- lme(data = dfl_rd_wm_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_lc <- lme(data = dfl_rd_wm_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_ut <- lme(data = dfl_rd_wm_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_mt <- lme(data = dfl_rd_wm_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_dor_uc <- lme(data = dfl_rd_wm_dor_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_dor_lc <- lme(data = dfl_rd_wm_dor_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_dor_ut <- lme(data = dfl_rd_wm_dor_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_dor_mt <- lme(data = dfl_rd_wm_dor_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_lat_uc <- lme(data = dfl_rd_wm_lat_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_lat_lc <- lme(data = dfl_rd_wm_lat_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_lat_ut <- lme(data = dfl_rd_wm_lat_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_lat_mt <- lme(data = dfl_rd_wm_lat_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_ven_uc <- lme(data = dfl_rd_wm_ven_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_ven_lc <- lme(data = dfl_rd_wm_ven_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_ven_ut <- lme(data = dfl_rd_wm_ven_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_wm_ven_mt <- lme(data = dfl_rd_wm_ven_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_uc <- lme(data = dfl_rd_gm_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_lc <- lme(data = dfl_rd_gm_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_ut <- lme(data = dfl_rd_gm_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_mt <- lme(data = dfl_rd_gm_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_dor_uc <- lme(data = dfl_rd_gm_dor_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_dor_lc <- lme(data = dfl_rd_gm_dor_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_dor_ut <- lme(data = dfl_rd_gm_dor_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_dor_mt <- lme(data = dfl_rd_gm_dor_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_ven_uc <- lme(data = dfl_rd_gm_ven_uc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_ven_lc <- lme(data = dfl_rd_gm_ven_lc, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_ven_ut <- lme(data = dfl_rd_gm_ven_ut, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))
summary(lme_rd_gm_ven_mt <- lme(data = dfl_rd_gm_ven_mt, value ~ group*level, random = ~ 1|id, na.action = na.omit, control = ctrl))







# ==================== ANOVA (effect of severity )==============================
# Here we are only interested in the effect of severity (group), NOT the effect of level
anova(lme_fa_gm); anova(lme_fa_gm_dor); anova(lme_fa_gm_ven);
anova(lme_fa_wm); anova(lme_fa_wm_dor); anova(lme_fa_wm_lat); anova(lme_fa_wm_ven);
anova(lme_md_gm); anova(lme_md_gm_dor); anova(lme_md_gm_ven);
anova(lme_md_wm); anova(lme_md_wm_dor); anova(lme_md_wm_lat); anova(lme_md_wm_ven);
anova(lme_ad_gm); anova(lme_ad_gm_dor); anova(lme_ad_gm_ven);
anova(lme_ad_wm); anova(lme_ad_wm_dor); anova(lme_ad_wm_lat); anova(lme_ad_wm_ven);
anova(lme_rd_gm); anova(lme_rd_gm_dor); anova(lme_rd_gm_ven);
anova(lme_rd_wm); anova(lme_rd_wm_dor); anova(lme_rd_wm_lat); anova(lme_rd_wm_ven);

# Results for group effect:
# fa_wm_lat: p=0.0185
  
# ========== post-hoc test: differences to sham ==========
# Now, we ask the question: Does any of the SCI group differ from the sham group?
# For that, we perform 3 pairwise comparisons: (mild SCI vs. sham), (moderate SCI vs. sham), and (severe SCI vs. sham)
# We apply Bonferroni correction (n=3)  
  
# estimate marginal means
emm_fa_wm     <- emmeans(lme_fa_wm,     specs = ~ group*level)
emm_fa_wm_dor <- emmeans(lme_fa_wm_dor, specs = ~ group*level)
emm_fa_wm_lat <- emmeans(lme_fa_wm_lat, specs = ~ group*level)
emm_fa_wm_ven <- emmeans(lme_fa_wm_ven, specs = ~ group*level)
emm_fa_gm     <- emmeans(lme_fa_gm,     specs = ~ group*level)
emm_fa_gm_dor <- emmeans(lme_fa_gm_dor, specs = ~ group*level)
emm_fa_gm_ven <- emmeans(lme_fa_gm_ven, specs = ~ group*level)

emm_md_wm     <- emmeans(lme_md_wm,     specs = ~ group*level)
emm_md_wm_dor <- emmeans(lme_md_wm_dor, specs = ~ group*level)
emm_md_wm_lat <- emmeans(lme_md_wm_lat, specs = ~ group*level)
emm_md_wm_ven <- emmeans(lme_md_wm_ven, specs = ~ group*level)
emm_md_gm     <- emmeans(lme_md_gm,     specs = ~ group*level)
emm_md_gm_dor <- emmeans(lme_md_gm_dor, specs = ~ group*level)
emm_md_gm_ven <- emmeans(lme_md_gm_ven, specs = ~ group*level)

emm_ad_wm     <- emmeans(lme_ad_wm,     specs = ~ group*level)
emm_ad_wm_dor <- emmeans(lme_ad_wm_dor, specs = ~ group*level)
emm_ad_wm_lat <- emmeans(lme_ad_wm_lat, specs = ~ group*level)
emm_ad_wm_ven <- emmeans(lme_ad_wm_ven, specs = ~ group*level)
emm_ad_gm     <- emmeans(lme_ad_gm,     specs = ~ group*level)
emm_ad_gm_dor <- emmeans(lme_ad_gm_dor, specs = ~ group*level)
emm_ad_gm_ven <- emmeans(lme_ad_gm_ven, specs = ~ group*level)

emm_rd_wm     <- emmeans(lme_rd_wm,     specs = ~ group*level)
emm_rd_wm_dor <- emmeans(lme_rd_wm_dor, specs = ~ group*level)
emm_rd_wm_lat <- emmeans(lme_rd_wm_lat, specs = ~ group*level)
emm_rd_wm_ven <- emmeans(lme_rd_wm_ven, specs = ~ group*level)
emm_rd_gm     <- emmeans(lme_rd_gm,     specs = ~ group*level)
emm_rd_gm_dor <- emmeans(lme_rd_gm_dor, specs = ~ group*level)
emm_rd_gm_ven <- emmeans(lme_rd_gm_ven, specs = ~ group*level)

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
c_fa_gm_uc     <- contrast(emm_fa_gm,     method = list_uc, adjust = "Bonferroni"); c_fa_gm_uc;
c_fa_gm_lc     <- contrast(emm_fa_gm,     method = list_lc, adjust = "Bonferroni"); c_fa_gm_lc;
c_fa_gm_ut     <- contrast(emm_fa_gm,     method = list_ut, adjust = "Bonferroni"); c_fa_gm_ut;
c_fa_gm_mt     <- contrast(emm_fa_gm,     method = list_mt, adjust = "Bonferroni"); c_fa_gm_mt;
c_fa_gm_dor_uc <- contrast(emm_fa_gm_dor, method = list_uc, adjust = "Bonferroni"); c_fa_gm_dor_uc;
c_fa_gm_dor_lc <- contrast(emm_fa_gm_dor, method = list_lc, adjust = "Bonferroni"); c_fa_gm_dor_lc;
c_fa_gm_dor_ut <- contrast(emm_fa_gm_dor, method = list_ut, adjust = "Bonferroni"); c_fa_gm_dor_ut;
c_fa_gm_dor_mt <- contrast(emm_fa_gm_dor, method = list_mt, adjust = "Bonferroni"); c_fa_gm_dor_mt;
c_fa_gm_ven_uc <- contrast(emm_fa_gm_ven, method = list_uc, adjust = "Bonferroni"); c_fa_gm_ven_uc;
c_fa_gm_ven_lc <- contrast(emm_fa_gm_ven, method = list_lc, adjust = "Bonferroni"); c_fa_gm_ven_lc;
c_fa_gm_ven_ut <- contrast(emm_fa_gm_ven, method = list_ut, adjust = "Bonferroni"); c_fa_gm_ven_ut;
c_fa_gm_ven_mt <- contrast(emm_fa_gm_ven, method = list_mt, adjust = "Bonferroni"); c_fa_gm_ven_mt;
c_fa_wm_uc     <- contrast(emm_fa_wm,     method = list_uc, adjust = "Bonferroni"); c_fa_wm_uc;
c_fa_wm_lc     <- contrast(emm_fa_wm,     method = list_lc, adjust = "Bonferroni"); c_fa_wm_lc;
c_fa_wm_ut     <- contrast(emm_fa_wm,     method = list_ut, adjust = "Bonferroni"); c_fa_wm_ut;
c_fa_wm_mt     <- contrast(emm_fa_wm,     method = list_mt, adjust = "Bonferroni"); c_fa_wm_mt;
c_fa_wm_dor_uc <- contrast(emm_fa_wm_dor, method = list_uc, adjust = "Bonferroni"); c_fa_wm_dor_uc;
c_fa_wm_dor_lc <- contrast(emm_fa_wm_dor, method = list_lc, adjust = "Bonferroni"); c_fa_wm_dor_lc;
c_fa_wm_dor_ut <- contrast(emm_fa_wm_dor, method = list_ut, adjust = "Bonferroni"); c_fa_wm_dor_ut;
c_fa_wm_dor_mt <- contrast(emm_fa_wm_dor, method = list_mt, adjust = "Bonferroni"); c_fa_wm_dor_mt;
c_fa_wm_lat_uc <- contrast(emm_fa_wm_lat, method = list_uc, adjust = "Bonferroni"); c_fa_wm_lat_uc;
c_fa_wm_lat_lc <- contrast(emm_fa_wm_lat, method = list_lc, adjust = "Bonferroni"); c_fa_wm_lat_lc;
c_fa_wm_lat_ut <- contrast(emm_fa_wm_lat, method = list_ut, adjust = "Bonferroni"); c_fa_wm_lat_ut;
c_fa_wm_lat_mt <- contrast(emm_fa_wm_lat, method = list_mt, adjust = "Bonferroni"); c_fa_wm_lat_mt;
c_fa_wm_ven_uc <- contrast(emm_fa_wm_ven, method = list_uc, adjust = "Bonferroni"); c_fa_wm_ven_uc;
c_fa_wm_ven_lc <- contrast(emm_fa_wm_ven, method = list_lc, adjust = "Bonferroni"); c_fa_wm_ven_lc;
c_fa_wm_ven_ut <- contrast(emm_fa_wm_ven, method = list_ut, adjust = "Bonferroni"); c_fa_wm_ven_ut;
c_fa_wm_ven_mt <- contrast(emm_fa_wm_ven, method = list_mt, adjust = "Bonferroni"); c_fa_wm_ven_mt;

c_md_gm_uc     <- contrast(emm_md_gm,     method = list_uc, adjust = "Bonferroni"); c_md_gm_uc;
c_md_gm_lc     <- contrast(emm_md_gm,     method = list_lc, adjust = "Bonferroni"); c_md_gm_lc;
c_md_gm_ut     <- contrast(emm_md_gm,     method = list_ut, adjust = "Bonferroni"); c_md_gm_ut;
c_md_gm_mt     <- contrast(emm_md_gm,     method = list_mt, adjust = "Bonferroni"); c_md_gm_mt;
c_md_gm_dor_uc <- contrast(emm_md_gm_dor, method = list_uc, adjust = "Bonferroni"); c_md_gm_dor_uc;
c_md_gm_dor_lc <- contrast(emm_md_gm_dor, method = list_lc, adjust = "Bonferroni"); c_md_gm_dor_lc;
c_md_gm_dor_ut <- contrast(emm_md_gm_dor, method = list_ut, adjust = "Bonferroni"); c_md_gm_dor_ut;
c_md_gm_dor_mt <- contrast(emm_md_gm_dor, method = list_mt, adjust = "Bonferroni"); c_md_gm_dor_mt;
c_md_gm_ven_uc <- contrast(emm_md_gm_ven, method = list_uc, adjust = "Bonferroni"); c_md_gm_ven_uc;
c_md_gm_ven_lc <- contrast(emm_md_gm_ven, method = list_lc, adjust = "Bonferroni"); c_md_gm_ven_lc;
c_md_gm_ven_ut <- contrast(emm_md_gm_ven, method = list_ut, adjust = "Bonferroni"); c_md_gm_ven_ut;
c_md_gm_ven_mt <- contrast(emm_md_gm_ven, method = list_mt, adjust = "Bonferroni"); c_md_gm_ven_mt;
c_md_wm_uc     <- contrast(emm_md_wm,     method = list_uc, adjust = "Bonferroni"); c_md_wm_uc;
c_md_wm_lc     <- contrast(emm_md_wm,     method = list_lc, adjust = "Bonferroni"); c_md_wm_lc;
c_md_wm_ut     <- contrast(emm_md_wm,     method = list_ut, adjust = "Bonferroni"); c_md_wm_ut;
c_md_wm_mt     <- contrast(emm_md_wm,     method = list_mt, adjust = "Bonferroni"); c_md_wm_mt;
c_md_wm_dor_uc <- contrast(emm_md_wm_dor, method = list_uc, adjust = "Bonferroni"); c_md_wm_dor_uc;
c_md_wm_dor_lc <- contrast(emm_md_wm_dor, method = list_lc, adjust = "Bonferroni"); c_md_wm_dor_lc;
c_md_wm_dor_ut <- contrast(emm_md_wm_dor, method = list_ut, adjust = "Bonferroni"); c_md_wm_dor_ut;
c_md_wm_dor_mt <- contrast(emm_md_wm_dor, method = list_mt, adjust = "Bonferroni"); c_md_wm_dor_mt;
c_md_wm_lat_uc <- contrast(emm_md_wm_lat, method = list_uc, adjust = "Bonferroni"); c_md_wm_lat_uc;
c_md_wm_lat_lc <- contrast(emm_md_wm_lat, method = list_lc, adjust = "Bonferroni"); c_md_wm_lat_lc;
c_md_wm_lat_ut <- contrast(emm_md_wm_lat, method = list_ut, adjust = "Bonferroni"); c_md_wm_lat_ut;
c_md_wm_lat_mt <- contrast(emm_md_wm_lat, method = list_mt, adjust = "Bonferroni"); c_md_wm_lat_mt;
c_md_wm_ven_uc <- contrast(emm_md_wm_ven, method = list_uc, adjust = "Bonferroni"); c_md_wm_ven_uc;
c_md_wm_ven_lc <- contrast(emm_md_wm_ven, method = list_lc, adjust = "Bonferroni"); c_md_wm_ven_lc;
c_md_wm_ven_ut <- contrast(emm_md_wm_ven, method = list_ut, adjust = "Bonferroni"); c_md_wm_ven_ut;
c_md_wm_ven_mt <- contrast(emm_md_wm_ven, method = list_mt, adjust = "Bonferroni"); c_md_wm_ven_mt;

c_ad_gm_uc     <- contrast(emm_ad_gm,     method = list_uc, adjust = "Bonferroni"); c_ad_gm_uc;
c_ad_gm_lc     <- contrast(emm_ad_gm,     method = list_lc, adjust = "Bonferroni"); c_ad_gm_lc;
c_ad_gm_ut     <- contrast(emm_ad_gm,     method = list_ut, adjust = "Bonferroni"); c_ad_gm_ut;
c_ad_gm_mt     <- contrast(emm_ad_gm,     method = list_mt, adjust = "Bonferroni"); c_ad_gm_mt;
c_ad_gm_dor_uc <- contrast(emm_ad_gm_dor, method = list_uc, adjust = "Bonferroni"); c_ad_gm_dor_uc;
c_ad_gm_dor_lc <- contrast(emm_ad_gm_dor, method = list_lc, adjust = "Bonferroni"); c_ad_gm_dor_lc;
c_ad_gm_dor_ut <- contrast(emm_ad_gm_dor, method = list_ut, adjust = "Bonferroni"); c_ad_gm_dor_ut;
c_ad_gm_dor_mt <- contrast(emm_ad_gm_dor, method = list_mt, adjust = "Bonferroni"); c_ad_gm_dor_mt;
c_ad_gm_ven_uc <- contrast(emm_ad_gm_ven, method = list_uc, adjust = "Bonferroni"); c_ad_gm_ven_uc;
c_ad_gm_ven_lc <- contrast(emm_ad_gm_ven, method = list_lc, adjust = "Bonferroni"); c_ad_gm_ven_lc;
c_ad_gm_ven_ut <- contrast(emm_ad_gm_ven, method = list_ut, adjust = "Bonferroni"); c_ad_gm_ven_ut;
c_ad_gm_ven_mt <- contrast(emm_ad_gm_ven, method = list_mt, adjust = "Bonferroni"); c_ad_gm_ven_mt;
c_ad_wm_uc     <- contrast(emm_ad_wm,     method = list_uc, adjust = "Bonferroni"); c_ad_wm_uc;
c_ad_wm_lc     <- contrast(emm_ad_wm,     method = list_lc, adjust = "Bonferroni"); c_ad_wm_lc;
c_ad_wm_ut     <- contrast(emm_ad_wm,     method = list_ut, adjust = "Bonferroni"); c_ad_wm_ut;
c_ad_wm_mt     <- contrast(emm_ad_wm,     method = list_mt, adjust = "Bonferroni"); c_ad_wm_mt;
c_ad_wm_dor_uc <- contrast(emm_ad_wm_dor, method = list_uc, adjust = "Bonferroni"); c_ad_wm_dor_uc;
c_ad_wm_dor_lc <- contrast(emm_ad_wm_dor, method = list_lc, adjust = "Bonferroni"); c_ad_wm_dor_lc;
c_ad_wm_dor_ut <- contrast(emm_ad_wm_dor, method = list_ut, adjust = "Bonferroni"); c_ad_wm_dor_ut;
c_ad_wm_dor_mt <- contrast(emm_ad_wm_dor, method = list_mt, adjust = "Bonferroni"); c_ad_wm_dor_mt;
c_ad_wm_lat_uc <- contrast(emm_ad_wm_lat, method = list_uc, adjust = "Bonferroni"); c_ad_wm_lat_uc;
c_ad_wm_lat_lc <- contrast(emm_ad_wm_lat, method = list_lc, adjust = "Bonferroni"); c_ad_wm_lat_lc;
c_ad_wm_lat_ut <- contrast(emm_ad_wm_lat, method = list_ut, adjust = "Bonferroni"); c_ad_wm_lat_ut;
c_ad_wm_lat_mt <- contrast(emm_ad_wm_lat, method = list_mt, adjust = "Bonferroni"); c_ad_wm_lat_mt;
c_ad_wm_ven_uc <- contrast(emm_ad_wm_ven, method = list_uc, adjust = "Bonferroni"); c_ad_wm_ven_uc;
c_ad_wm_ven_lc <- contrast(emm_ad_wm_ven, method = list_lc, adjust = "Bonferroni"); c_ad_wm_ven_lc;
c_ad_wm_ven_ut <- contrast(emm_ad_wm_ven, method = list_ut, adjust = "Bonferroni"); c_ad_wm_ven_ut;
c_ad_wm_ven_mt <- contrast(emm_ad_wm_ven, method = list_mt, adjust = "Bonferroni"); c_ad_wm_ven_mt;

c_rd_gm_uc     <- contrast(emm_rd_gm,     method = list_uc, adjust = "Bonferroni"); c_rd_gm_uc;
c_rd_gm_lc     <- contrast(emm_rd_gm,     method = list_lc, adjust = "Bonferroni"); c_rd_gm_lc;
c_rd_gm_ut     <- contrast(emm_rd_gm,     method = list_ut, adjust = "Bonferroni"); c_rd_gm_ut;
c_rd_gm_mt     <- contrast(emm_rd_gm,     method = list_mt, adjust = "Bonferroni"); c_rd_gm_mt;
c_rd_gm_dor_uc <- contrast(emm_rd_gm_dor, method = list_uc, adjust = "Bonferroni"); c_rd_gm_dor_uc;
c_rd_gm_dor_lc <- contrast(emm_rd_gm_dor, method = list_lc, adjust = "Bonferroni"); c_rd_gm_dor_lc;
c_rd_gm_dor_ut <- contrast(emm_rd_gm_dor, method = list_ut, adjust = "Bonferroni"); c_rd_gm_dor_ut;
c_rd_gm_dor_mt <- contrast(emm_rd_gm_dor, method = list_mt, adjust = "Bonferroni"); c_rd_gm_dor_mt;
c_rd_gm_ven_uc <- contrast(emm_rd_gm_ven, method = list_uc, adjust = "Bonferroni"); c_rd_gm_ven_uc;
c_rd_gm_ven_lc <- contrast(emm_rd_gm_ven, method = list_lc, adjust = "Bonferroni"); c_rd_gm_ven_lc;
c_rd_gm_ven_ut <- contrast(emm_rd_gm_ven, method = list_ut, adjust = "Bonferroni"); c_rd_gm_ven_ut;
c_rd_gm_ven_mt <- contrast(emm_rd_gm_ven, method = list_mt, adjust = "Bonferroni"); c_rd_gm_ven_mt;
c_rd_wm_uc     <- contrast(emm_rd_wm,     method = list_uc, adjust = "Bonferroni"); c_rd_wm_uc;
c_rd_wm_lc     <- contrast(emm_rd_wm,     method = list_lc, adjust = "Bonferroni"); c_rd_wm_lc;
c_rd_wm_ut     <- contrast(emm_rd_wm,     method = list_ut, adjust = "Bonferroni"); c_rd_wm_ut;
c_rd_wm_mt     <- contrast(emm_rd_wm,     method = list_mt, adjust = "Bonferroni"); c_rd_wm_mt;
c_rd_wm_dor_uc <- contrast(emm_rd_wm_dor, method = list_uc, adjust = "Bonferroni"); c_rd_wm_dor_uc;
c_rd_wm_dor_lc <- contrast(emm_rd_wm_dor, method = list_lc, adjust = "Bonferroni"); c_rd_wm_dor_lc;
c_rd_wm_dor_ut <- contrast(emm_rd_wm_dor, method = list_ut, adjust = "Bonferroni"); c_rd_wm_dor_ut;
c_rd_wm_dor_mt <- contrast(emm_rd_wm_dor, method = list_mt, adjust = "Bonferroni"); c_rd_wm_dor_mt;
c_rd_wm_lat_uc <- contrast(emm_rd_wm_lat, method = list_uc, adjust = "Bonferroni"); c_rd_wm_lat_uc;
c_rd_wm_lat_lc <- contrast(emm_rd_wm_lat, method = list_lc, adjust = "Bonferroni"); c_rd_wm_lat_lc;
c_rd_wm_lat_ut <- contrast(emm_rd_wm_lat, method = list_ut, adjust = "Bonferroni"); c_rd_wm_lat_ut;
c_rd_wm_lat_mt <- contrast(emm_rd_wm_lat, method = list_mt, adjust = "Bonferroni"); c_rd_wm_lat_mt;
c_rd_wm_ven_uc <- contrast(emm_rd_wm_ven, method = list_uc, adjust = "Bonferroni"); c_rd_wm_ven_uc;
c_rd_wm_ven_lc <- contrast(emm_rd_wm_ven, method = list_lc, adjust = "Bonferroni"); c_rd_wm_ven_lc;
c_rd_wm_ven_ut <- contrast(emm_rd_wm_ven, method = list_ut, adjust = "Bonferroni"); c_rd_wm_ven_ut;
c_rd_wm_ven_mt <- contrast(emm_rd_wm_ven, method = list_mt, adjust = "Bonferroni"); c_rd_wm_ven_mt;



# RESULTS (Bonferroni correction):
# fa_wm     is different between SEV and SHM at mid-thoracic (p=0.0175), upper thoracic (p=0.0154), and lower cervical (p=0.0786)
# fa_wm_dor is different between SEV and SHM at mid-thoracic (p=0.0358)
# fa_wm_lat is different between SEV and SHM at mid-thoracic (p=0.0080), upper thoracic (p=0.0068), and lower cervical (p=0.0215)
# ad_wm     is different between SEV and SHM at upper cervical (p=0.0473)
# ad_wm     is different between MOD and SHM at upper cervical (p=0.0462)
# ad_wm_dor is different between SEV and SHM at mid-thoracic (p=0.0489)
  

  
  
  
# ========== post-hoc test 4: pair-wise group differences ==========
# We start with the simplest question possible: do the three groups differ from each other? In other words: are there pairwise group differences?
# For this, we combine the mild, moderate, and severe groups into a single SCI group.

# get marginal means
emm4_rd_gm     <- emmeans(lme_fa_gm,     specs = ~ group)
emm4_fa_gm_dor <- emmeans(lme_fa_gm_dor, specs = ~ group)
emm4_fa_gm_ven <- emmeans(lme_fa_gm_ven, specs = ~ group)
emm4_fa_wm     <- emmeans(lme_fa_wm,     specs = ~ group)
emm4_fa_wm_dor <- emmeans(lme_fa_wm_dor, specs = ~ group)
emm4_fa_wm_lat <- emmeans(lme_fa_wm_lat, specs = ~ group)
emm4_fa_wm_ven <- emmeans(lme_fa_wm_ven, specs = ~ group)
emm4_md_gm     <- emmeans(lme_md_gm,     specs = ~ group)
emm4_md_gm_dor <- emmeans(lme_md_gm_dor, specs = ~ group)
emm4_md_gm_ven <- emmeans(lme_md_gm_ven, specs = ~ group)
emm4_md_wm     <- emmeans(lme_md_wm,     specs = ~ group)
emm4_md_wm_dor <- emmeans(lme_md_wm_dor, specs = ~ group)
emm4_md_wm_lat <- emmeans(lme_md_wm_lat, specs = ~ group)
emm4_md_wm_ven <- emmeans(lme_md_wm_ven, specs = ~ group)
emm4_ad_gm     <- emmeans(lme_ad_gm,     specs = ~ group)
emm4_ad_gm_dor <- emmeans(lme_ad_gm_dor, specs = ~ group)
emm4_ad_gm_ven <- emmeans(lme_ad_gm_ven, specs = ~ group)
emm4_ad_wm     <- emmeans(lme_ad_wm,     specs = ~ group)
emm4_ad_wm_dor <- emmeans(lme_ad_wm_dor, specs = ~ group)
emm4_ad_wm_lat <- emmeans(lme_ad_wm_lat, specs = ~ group)
emm4_ad_wm_ven <- emmeans(lme_ad_wm_ven, specs = ~ group)
emm4_rd_gm     <- emmeans(lme_rd_gm,     specs = ~ group)
emm4_rd_gm_dor <- emmeans(lme_rd_gm_dor, specs = ~ group)
emm4_rd_gm_ven <- emmeans(lme_rd_gm_ven, specs = ~ group)
emm4_rd_wm     <- emmeans(lme_rd_wm,     specs = ~ group)
emm4_rd_wm_dor <- emmeans(lme_rd_wm_dor, specs = ~ group)
emm4_rd_wm_lat <- emmeans(lme_rd_wm_lat, specs = ~ group)
emm4_rd_wm_ven <- emmeans(lme_rd_wm_ven, specs = ~ group)

# define contrasts
shm <- c(1,0,0,0)
mld <- c(0,1,0,0)
mod <- c(0,0,1,0)
sev <- c(0,0,0,1)

list <- list(
  "sev-shm" = sev - shm,
  "mod-shm" = mod - shm,
  "mld-shm" = mld - shm,
  "sev-mld" = sev - mld,
  "mod-mld" = mod - mld,
  "sev-mod" = sev - mod)

# estimate contrasts
c4_fa_gm     <- contrast(emm4_fa_gm,     method = list); c4_fa_gm;
c4_fa_gm_dor <- contrast(emm4_fa_gm_dor, method = list); c4_fa_gm_dor;
c4_fa_gm_ven <- contrast(emm4_fa_gm_ven, method = list); c4_fa_gm_ven;
c4_fa_wm     <- contrast(emm4_fa_wm,     method = list); c4_fa_wm;
c4_fa_wm_dor <- contrast(emm4_fa_wm_dor, method = list); c4_fa_wm_dor;
c4_fa_wm_lat <- contrast(emm4_fa_wm_lat, method = list); c4_fa_wm_lat;
c4_fa_wm_ven <- contrast(emm4_fa_wm_ven, method = list); c4_fa_wm_ven;
c4_md_gm     <- contrast(emm4_md_gm,     method = list); c4_md_gm;
c4_md_gm_dor <- contrast(emm4_md_gm_dor, method = list); c4_md_gm_dor;
c4_md_gm_ven <- contrast(emm4_md_gm_ven, method = list); c4_md_gm_ven;
c4_md_wm     <- contrast(emm4_md_wm,     method = list); c4_md_wm;
c4_md_wm_dor <- contrast(emm4_md_wm_dor, method = list); c4_md_wm_dor;
c4_md_wm_lat <- contrast(emm4_md_wm_lat, method = list); c4_md_wm_lat;
c4_md_wm_ven <- contrast(emm4_md_wm_ven, method = list); c4_md_wm_ven;
c4_ad_gm     <- contrast(emm4_ad_gm,     method = list); c4_ad_gm;
c4_ad_gm_dor <- contrast(emm4_ad_gm_dor, method = list); c4_ad_gm_dor;
c4_ad_gm_ven <- contrast(emm4_ad_gm_ven, method = list); c4_ad_gm_ven;
c4_ad_wm     <- contrast(emm4_ad_wm,     method = list); c4_ad_wm;
c4_ad_wm_dor <- contrast(emm4_ad_wm_dor, method = list); c4_ad_wm_dor;
c4_ad_wm_lat <- contrast(emm4_ad_wm_lat, method = list); c4_ad_wm_lat;
c4_ad_wm_ven <- contrast(emm4_ad_wm_ven, method = list); c4_ad_wm_ven;
c4_rd_gm     <- contrast(emm4_rd_gm,     method = list); c4_rd_gm;
c4_rd_gm_dor <- contrast(emm4_rd_gm_dor, method = list); c4_rd_gm_dor;
c4_rd_gm_ven <- contrast(emm4_rd_gm_ven, method = list); c4_rd_gm_ven;
c4_rd_wm     <- contrast(emm4_rd_wm,     method = list); c4_rd_wm;
c4_rd_wm_dor <- contrast(emm4_rd_wm_dor, method = list); c4_rd_wm_dor;
c4_rd_wm_lat <- contrast(emm4_rd_wm_lat, method = list); c4_rd_wm_lat;
c4_rd_wm_ven <- contrast(emm4_rd_wm_ven, method = list); c4_rd_wm_ven;

# RESULTS: Post-hoc tests revealed significant differences

# fa_wm     between SEV-SHM (p=0.0096)
# fa_wm     between SEV-MOD (p=0.0317)

# fa_wm_dor between SEV-SHM (p=0.0314)

# fa_wm_lat between SEV-MOD (p=0.0110)
# fa_wm_lat between SEV-MLD (p=0.0273)
# fa_wm_lat between SEV-SHM (p=0.0030)






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
  "sev_C3-mld_C3" = sev_C3 - mld_C3,
  "mod_C3-mld_C3" = mod_C3 - mld_C3,
  "sev_C3-mod_C3" = sev_C3 - mod_C3,
  
  "sev_C4-shm_C4" = sev_C4 - shm_C4,
  "mod_C4-shm_C4" = mod_C4 - shm_C4,
  "mld_C4-shm_C4" = mld_C4 - shm_C4,
  "sev_C4-mld_C4" = sev_C4 - mld_C4,
  "mod_C4-mld_C4" = mod_C4 - mld_C4,
  "sev_C4-mod_C4" = sev_C4 - mod_C4,
  
  "sev_C5-shm_C5" = sev_C5 - shm_C5,
  "mod_C5-shm_C5" = mod_C5 - shm_C5,
  "mld_C5-shm_C5" = mld_C5 - shm_C5,
  "sev_C5-mld_C5" = sev_C5 - mld_C5,
  "mod_C5-mld_C5" = mod_C5 - mld_C5,
  "sev_C5-mod_C5" = sev_C5 - mod_C5,
  
  "sev_C6-shm_C6" = sev_C6 - shm_C6,
  "mod_C6-shm_C6" = mod_C6 - shm_C6,
  "mld_C6-shm_C6" = mld_C6 - shm_C6,
  "sev_C6-mld_C6" = sev_C6 - mld_C6,
  "mod_C6-mld_C6" = mod_C6 - mld_C6,
  "sev_C6-mod_C6" = sev_C6 - mod_C6,
  
  "sev_C7-shm_C7" = sev_C7 - shm_C7,
  "mod_C7-shm_C7" = mod_C7 - shm_C7,
  "mld_C7-shm_C7" = mld_C7 - shm_C7,
  "sev_C7-mld_C7" = sev_C7 - mld_C7,
  "mod_C7-mld_C7" = mod_C7 - mld_C7,
  "sev_C7-mod_C7" = sev_C7 - mod_C7,
  
  "sev_C8-shm_C8" = sev_C8 - shm_C8,
  "mod_C8-shm_C8" = mod_C8 - shm_C8,
  "mld_C8-shm_C8" = mld_C8 - shm_C8,
  "sev_C8-mld_C8" = sev_C8 - mld_C8,
  "mod_C8-mld_C8" = mod_C8 - mld_C8,
  "sev_C8-mod_C8" = sev_C8 - mod_C8,
  
  "sev_T1-shm_T1" = sev_T1 - shm_T1,
  "mod_T1-shm_T1" = mod_T1 - shm_T1,
  "mld_T1-shm_T1" = mld_T1 - shm_T1,
  "sev_T1-mld_T1" = sev_T1 - mld_T1,
  "mod_T1-mld_T1" = mod_T1 - mld_T1,
  "sev_T1-mod_T1" = sev_T1 - mod_T1,
  
  "sev_T2-shm_T2" = sev_T2 - shm_T2,
  "mod_T2-shm_T2" = mod_T2 - shm_T2,
  "mld_T2-shm_T2" = mld_T2 - shm_T2,
  "sev_T2-mld_T2" = sev_T2 - mld_T2,
  "mod_T2-mld_T2" = mod_T2 - mld_T2,
  "sev_T2-mod_T2" = sev_T2 - mod_T2,
  
  "sev_T3-shm_T3" = sev_T3 - shm_T3,
  "mod_T3-shm_T3" = mod_T3 - shm_T3,
  "mld_T3-shm_T3" = mld_T3 - shm_T3,
  "sev_T3-mld_T3" = sev_T3 - mld_T3,
  "mod_T3-mld_T3" = mod_T3 - mld_T3,
  "sev_T3-mod_T3" = sev_T3 - mod_T3,
  
  "sev_T4-shm_T4" = sev_T4 - shm_T4,
  "mod_T4-shm_T4" = mod_T4 - shm_T4,
  "mld_T4-shm_T4" = mld_T4 - shm_T4,
  "sev_T4-mld_T4" = sev_T4 - mld_T4,
  "mod_T4-mld_T4" = mod_T4 - mld_T4,
  "sev_T4-mod_T4" = sev_T4 - mod_T4,
  
  "sev_T5-shm_T5" = sev_T5 - shm_T5,
  "mod_T5-shm_T5" = mod_T5 - shm_T5,
  "mld_T5-shm_T5" = mld_T5 - shm_T5,
  "sev_T5-mld_T5" = sev_T5 - mld_T5,
  "mod_T5-mld_T5" = mod_T5 - mld_T5,
  "sev_T5-mod_T5" = sev_T5 - mod_T5,
  
  "sev_T6-shm_T6" = sev_T6 - shm_T6,
  "mod_T6-shm_T6" = mod_T6 - shm_T6,
  "mld_T6-shm_T6" = mld_T6 - shm_T6,
  "sev_T6-mld_T6" = sev_T6 - mld_T6,
  "mod_T6-mld_T6" = mod_T6 - mld_T6,
  "sev_T6-mod_T6" = sev_T6 - mod_T6
)

# estimate contrasts
c5_fa_gm     <- contrast(emm5_fa_gm,     method = list); c5_fa_gm;
c5_fa_gm_dor <- contrast(emm5_fa_gm_dor, method = list); c5_fa_gm_dor;
c5_fa_gm_ven <- contrast(emm5_fa_gm_ven, method = list); c5_fa_gm_ven;
c5_fa_wm     <- contrast(emm5_fa_wm,     method = list); c5_fa_wm;
c5_fa_wm_dor <- contrast(emm5_fa_wm_dor, method = list); c5_fa_wm_dor;
c5_fa_wm_lat <- contrast(emm5_fa_wm_lat, method = list); c5_fa_wm_lat;
c5_fa_wm_ven <- contrast(emm5_fa_wm_ven, method = list); c5_fa_wm_ven;
c5_md_gm     <- contrast(emm5_md_gm,     method = list); c5_md_gm;
c5_md_gm_dor <- contrast(emm5_md_gm_dor, method = list); c5_md_gm_dor;
c5_md_gm_ven <- contrast(emm5_md_gm_ven, method = list); c5_md_gm_ven;
c5_md_wm     <- contrast(emm5_md_wm,     method = list); c5_md_wm;
c5_md_wm_dor <- contrast(emm5_md_wm_dor, method = list); c5_md_wm_dor;
c5_md_wm_lat <- contrast(emm5_md_wm_lat, method = list); c5_md_wm_lat;
c5_md_wm_ven <- contrast(emm5_md_wm_ven, method = list); c5_md_wm_ven;
c5_ad_gm     <- contrast(emm5_ad_gm,     method = list); c5_ad_gm;
c5_ad_gm_dor <- contrast(emm5_ad_gm_dor, method = list); c5_ad_gm_dor;
c5_ad_gm_ven <- contrast(emm5_ad_gm_ven, method = list); c5_ad_gm_ven;
c5_ad_wm     <- contrast(emm5_ad_wm,     method = list); c5_ad_wm;
c5_ad_wm_dor <- contrast(emm5_ad_wm_dor, method = list); c5_ad_wm_dor;
c5_ad_wm_lat <- contrast(emm5_ad_wm_lat, method = list); c5_ad_wm_lat;
c5_ad_wm_ven <- contrast(emm5_ad_wm_ven, method = list); c5_ad_wm_ven;
c5_rd_gm     <- contrast(emm5_rd_gm,     method = list); c5_rd_gm;
c5_rd_gm_dor <- contrast(emm5_rd_gm_dor, method = list); c5_rd_gm_dor;
c5_rd_gm_ven <- contrast(emm5_rd_gm_ven, method = list); c5_rd_gm_ven;
c5_rd_wm     <- contrast(emm5_rd_wm,     method = list); c5_rd_wm;
c5_rd_wm_dor <- contrast(emm5_rd_wm_dor, method = list); c5_rd_wm_dor;
c5_rd_wm_lat <- contrast(emm5_rd_wm_lat, method = list); c5_rd_wm_lat;
c5_rd_wm_ven <- contrast(emm5_rd_wm_ven, method = list); c5_rd_wm_ven;







