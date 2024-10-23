# ===================================================================
# ======================= PLOT PER LEVEL ============================
# ===================================================================

# plot all groups (4 groups)
plot_over_levels_4groups <- function(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos,type) {
  plot.new()
  dev.new(width = 20,height = 15)
  par(mar = c(6,6,2,2))
  
  plot(  x, y1, col="black", xlab=xlab, ylab=ylab, ylim=ylim, xaxt="n", yaxt="n", cex.lab = 3)
  points(x, y2, col="blue")
  points(x, y3, col="orange")
  points(x, y4, col="red")
  
  lines(x, y1, col="black")
  lines(x, y2, col="blue")
  lines(x, y3, col="orange")
  lines(x, y4, col="red")
  
  if (type==1) {
    arrows(x, y1 - sd(y1, na.rm = FALSE), x, y1 + sd(y1, na.rm = FALSE), length=0.05, angle=90, code=3, col="black")
    arrows(x, y2 - sd(y2, na.rm = FALSE), x, y2 + sd(y2, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y3 - sd(y3, na.rm = FALSE), x, y3 + sd(y3, na.rm = FALSE), length=0.05, angle=90, code=3, col="orange")
    arrows(x, y4 - sd(y4, na.rm = FALSE), x, y4 + sd(y4, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")}
  if (type==2) {
    arrows(x, y1 - se(y1, na.rm = FALSE), x, y1 + se(y1, na.rm = FALSE), length=0.05, angle=90, code=3, col="black")
    arrows(x, y2 - se(y2, na.rm = FALSE), x, y2 + se(y2, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y3 - se(y3, na.rm = FALSE), x, y3 + se(y3, na.rm = FALSE), length=0.05, angle=90, code=3, col="orange")
    arrows(x, y4 - se(y4, na.rm = FALSE), x, y4 + se(y4, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")}
  axis(1, at=x, labels=labels, cex.axis=2)
  axis(2, cex.axis=2)
  legend(legendpos[1], legendpos[2], legend=c("SHAM","MILD","MODERATE","SEVERE"), ol=c("black","blue","orange","red"), lty=1:4, cex=2)
}

# plot metrics
group <- dfw_sca$group

# sca
plot.new()
dfw_sca2 <- subset(dfw_sca, select=level_names) 
x <- 1:length(level_names)
y1 <- unlist(colMeans(dfw_sca2[group=='SHM',],na.rm = TRUE))
y2 <- unlist(colMeans(dfw_sca2[group=='MLD',],na.rm = TRUE))
y3 <- unlist(colMeans(dfw_sca2[group=='MOD',],na.rm = TRUE))
y4 <- unlist(colMeans(dfw_sca2[group=='SEV',],na.rm = TRUE))
xlab <- "Spinal level"
ylab <- "Cross-sectional area (mm2)"
ylim <- c(4,12)
legendpos <- c(1,4)
plot_over_levels_4groups(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos,2)

# wma
plot.new()
dfw_wma2 <- subset(dfw_wma, select=level_names) 
x <- 1:length(level_names)
y1 <- unlist(colMeans(dfw_wma2[group=='SHM',],na.rm = TRUE))
y2 <- unlist(colMeans(dfw_wma2[group=='MLD',],na.rm = TRUE))
y3 <- unlist(colMeans(dfw_wma2[group=='MOD',],na.rm = TRUE))
y4 <- unlist(colMeans(dfw_wma2[group=='SEV',],na.rm = TRUE))
xlab <- "Spinal level"
ylab <- "Cross-sectional area (mm2)"
ylim <- c(3,9)
legendpos <- c(1,4)
plot_over_levels_4groups(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos,2)


# gma
plot.new()
dfw_gma2 <- subset(dfw_gma, select=level_names) 
x <- 1:length(level_names)
y1 <- unlist(colMeans(dfw_gma2[group=='SHM',],na.rm = TRUE))
y2 <- unlist(colMeans(dfw_gma2[group=='MLD',],na.rm = TRUE))
y3 <- unlist(colMeans(dfw_gma2[group=='MOD',],na.rm = TRUE))
y4 <- unlist(colMeans(dfw_gma2[group=='SEV',],na.rm = TRUE))
xlab <- "Spinal level"
ylab <- "Cross-sectional area (mm2)"
ylim <- c(0,5)
legendpos <- c(1,4)
plot_over_levels_4groups(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos,2)



# =====
# plot 2 groups (SCI vs. SHAM)
plot_over_levels_2groups <- function(x,y1,y2,xlab,xaxt,ylab,ylim,labels,legendpos,type) {
  plot.new()
  dev.new(width = 20,height = 15)
  par(mar = c(6,6,2,2))
  plot(x, y1, lwd=1, col="blue", xlab=xlab, ylab=ylab, ylim=ylim, xaxt="n", yaxt="n", cex.lab = 3)
  points(x, y2, lwd=1, col="red")
  lines(x, y1, lwd=1, col="blue")
  lines(x, y2, lwd=1, col="red")
  if (type==1) {
    arrows(x, y1 - sd(y1, na.rm = FALSE), x, y1 + sd(y1, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y2 - sd(y2, na.rm = FALSE), x, y2 + sd(y2, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")}
  if (type==2) {
    arrows(x, y1 - se(y1, na.rm = FALSE), x, y1 + se(y1, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y2 - se(y2, na.rm = FALSE), x, y2 + se(y2, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")}
  axis(1, at=x, labels=labels, cex.axis=2)
  axis(2, cex.axis=2)
  legend(legendpos[1], legendpos[2], legend=c("SHAM","SCI"), col=c("blue","red"), lty=1:2, cex=2)
}




# =====
# composite plot:
# plot 4 groups, SCA, WMA, GMA in the same figure
plot_over_levels_4groups_composite <- function(x,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,xlab,xaxt,ylab,ylim,labels,legendpos,type) {
  plot.new()
  dev.new(width = 20,height = 15)
  par(mar = c(6,6,2,2))
  
  plot  (x, y1,  lwd=1, col="black", xlab=xlab, ylab=ylab, ylim=ylim, xaxt="n", yaxt="n", cex.lab = 3)
  points(x, y2,  lwd=1, col="blue")
  points(x, y3,  lwd=1, col="orange")
  points(x, y4,  lwd=1, col="red")
  points(x, y5,  lwd=1, col="black")
  points(x, y6,  lwd=1, col="blue")
  points(x, y7,  lwd=1, col="orange")
  points(x, y8,  lwd=1, col="red")
  points(x, y9,  lwd=1, col="black")
  points(x, y10, lwd=1, col="blue")
  points(x, y11, lwd=1, col="orange")
  points(x, y12, lwd=1, col="red")
  
  lines(x, y1,  lwd=1, col="black")
  lines(x, y2,  lwd=1, col="blue")
  lines(x, y3,  lwd=1, col="orange")
  lines(x, y4,  lwd=1, col="red")
  lines(x, y5,  lwd=1, col="black")
  lines(x, y6,  lwd=1, col="blue")
  lines(x, y7,  lwd=1, col="orange")
  lines(x, y8,  lwd=1, col="red")
  lines(x, y9,  lwd=1, col="black")
  lines(x, y10, lwd=1, col="blue")
  lines(x, y11, lwd=1, col="orange")
  lines(x, y12, lwd=1, col="red")
  
  if (type==1) {
    arrows(x, y1  - sd(y1,  na.rm = FALSE), x, y1  + sd(y1,  na.rm = FALSE), length=0.05, angle=90, code=3, col="black")
    arrows(x, y2  - sd(y2,  na.rm = FALSE), x, y2  + sd(y2,  na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y3  - sd(y3,  na.rm = FALSE), x, y3  + sd(y3,  na.rm = FALSE), length=0.05, angle=90, code=3, col="orange")
    arrows(x, y4  - sd(y4,  na.rm = FALSE), x, y4  + sd(y4,  na.rm = FALSE), length=0.05, angle=90, code=3, col="red")
    arrows(x, y5  - sd(y5,  na.rm = FALSE), x, y5  + sd(y5,  na.rm = FALSE), length=0.05, angle=90, code=3, col="black")
    arrows(x, y6  - sd(y6,  na.rm = FALSE), x, y6  + sd(y6,  na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y7  - sd(y7,  na.rm = FALSE), x, y7  + sd(y7,  na.rm = FALSE), length=0.05, angle=90, code=3, col="orange")
    arrows(x, y8  - sd(y8,  na.rm = FALSE), x, y8  + sd(y8,  na.rm = FALSE), length=0.05, angle=90, code=3, col="red")
    arrows(x, y9  - sd(y9,  na.rm = FALSE), x, y9  + sd(y9,  na.rm = FALSE), length=0.05, angle=90, code=3, col="black")
    arrows(x, y10 - sd(y10, na.rm = FALSE), x, y10 + sd(y10, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y11 - sd(y11, na.rm = FALSE), x, y11 + sd(y11, na.rm = FALSE), length=0.05, angle=90, code=3, col="orange")
    arrows(x, y12 - sd(y12, na.rm = FALSE), x, y12 + sd(y12, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")}
  
  if (type==2) {
    arrows(x, y1  - se(y1,  na.rm = FALSE), x, y1  + se(y1,  na.rm = FALSE), length=0.05, angle=90, code=3, col="black")
    arrows(x, y2  - se(y2,  na.rm = FALSE), x, y2  + se(y2,  na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y3  - se(y3,  na.rm = FALSE), x, y3  + se(y3,  na.rm = FALSE), length=0.05, angle=90, code=3, col="orange")
    arrows(x, y4  - se(y4,  na.rm = FALSE), x, y4  + se(y4,  na.rm = FALSE), length=0.05, angle=90, code=3, col="red")
    arrows(x, y5  - se(y5,  na.rm = FALSE), x, y5  + se(y5,  na.rm = FALSE), length=0.05, angle=90, code=3, col="black")
    arrows(x, y6  - se(y6,  na.rm = FALSE), x, y6  + se(y6,  na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y7  - se(y7,  na.rm = FALSE), x, y7  + se(y7,  na.rm = FALSE), length=0.05, angle=90, code=3, col="orange")
    arrows(x, y8  - se(y8,  na.rm = FALSE), x, y8  + se(y8,  na.rm = FALSE), length=0.05, angle=90, code=3, col="red")
    arrows(x, y9  - se(y9,  na.rm = FALSE), x, y9  + se(y9,  na.rm = FALSE), length=0.05, angle=90, code=3, col="black")
    arrows(x, y10 - se(y10, na.rm = FALSE), x, y10 + se(y10, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y11 - se(y11, na.rm = FALSE), x, y11 + se(y11, na.rm = FALSE), length=0.05, angle=90, code=3, col="orange")
    arrows(x, y12 - se(y12, na.rm = FALSE), x, y12 + se(y12, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")}
  
  axis(1, at=x, labels=labels, cex.axis=2)
  axis(2, cex.axis=2)
  legend(legendpos[1], legendpos[2], legend=c("SHAM","MILD","MODERATE","SEVERE"), col=c("black","blue","orange","red"), lty=1:4, cex=2)
}

# composite plot: sca, gma, wma
plot.new()

dfw_sca2 <- subset(dfw_sca, select=level_names)
dfw_wma2 <- subset(dfw_wma, select=level_names)
dfw_gma2 <- subset(dfw_gma, select=level_names)

x   <- 1:length(level_names)
y1  <- unlist(colMeans(dfw_sca2[group=='SHM',],na.rm = TRUE))
y2  <- unlist(colMeans(dfw_sca2[group=='MLD',],na.rm = TRUE))
y3  <- unlist(colMeans(dfw_sca2[group=='MOD',],na.rm = TRUE))
y4  <- unlist(colMeans(dfw_sca2[group=='SEV',],na.rm = TRUE))
y5  <- unlist(colMeans(dfw_wma2[group=='SHM',],na.rm = TRUE))
y6  <- unlist(colMeans(dfw_wma2[group=='MLD',],na.rm = TRUE))
y7  <- unlist(colMeans(dfw_wma2[group=='MOD',],na.rm = TRUE))
y8  <- unlist(colMeans(dfw_wma2[group=='SEV',],na.rm = TRUE))
y9  <- unlist(colMeans(dfw_gma2[group=='SHM',],na.rm = TRUE))
y10 <- unlist(colMeans(dfw_gma2[group=='MLD',],na.rm = TRUE))
y11 <- unlist(colMeans(dfw_gma2[group=='MOD',],na.rm = TRUE))
y12 <- unlist(colMeans(dfw_gma2[group=='SEV',],na.rm = TRUE))

xlab <- "Spinal level"
ylab <- "Cross-sectional area (mm2)"
ylim <- c(0,12)
legendpos <- c(8.6,11.6)
plot_over_levels_4groups_composite(x,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,xlab,xaxt,ylab,ylim,labels,legendpos,2)







# =====
# composite plot:
# plot 2 groups (SCI vs. SHAM), SCA, WMA, GMA in the same figure
plot_over_levels_2groups_composite <- function(x,y1,y2,y3,y4,y5,y6,xlab,xaxt,ylab,ylim,labels,legendpos,type) {
  plot.new()
  dev.new(width = 20,height = 15)
  par(mar = c(6,6,2,2))
  plot(x, y1, lwd=1, col="blue", xlab=xlab, ylab=ylab, ylim=ylim, xaxt="n", yaxt="n", cex.lab = 3)
  points(x, y2, lwd=1, col="red")
  points(x, y3, lwd=1, col="blue")
  points(x, y4, lwd=1, col="red")
  points(x, y5, lwd=1, col="blue")
  points(x, y6, lwd=1, col="red")
  lines(x, y1, lwd=1, col="blue")
  lines(x, y2, lwd=1, col="red")
  lines(x, y3, lwd=1, col="blue")
  lines(x, y4, lwd=1, col="red")
  lines(x, y5, lwd=1, col="blue")
  lines(x, y6, lwd=1, col="red")
  if (type==1) {
    arrows(x, y1 - sd(y1, na.rm = FALSE), x, y1 + sd(y1, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y2 - sd(y2, na.rm = FALSE), x, y2 + sd(y2, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")
    arrows(x, y3 - sd(y3, na.rm = FALSE), x, y3 + sd(y3, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y4 - sd(y4, na.rm = FALSE), x, y4 + sd(y4, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")
    arrows(x, y5 - sd(y5, na.rm = FALSE), x, y5 + sd(y5, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y6 - sd(y6, na.rm = FALSE), x, y6 + sd(y6, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")}
  if (type==2) {
    arrows(x, y1 - se(y1, na.rm = FALSE), x, y1 + se(y1, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y2 - se(y2, na.rm = FALSE), x, y2 + se(y2, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")
    arrows(x, y3 - se(y3, na.rm = FALSE), x, y3 + se(y3, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y4 - se(y4, na.rm = FALSE), x, y4 + se(y4, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")
    arrows(x, y5 - se(y5, na.rm = FALSE), x, y5 + se(y5, na.rm = FALSE), length=0.05, angle=90, code=3, col="blue")
    arrows(x, y6 - se(y6, na.rm = FALSE), x, y6 + se(y6, na.rm = FALSE), length=0.05, angle=90, code=3, col="red")}
  axis(1, at=x, labels=labels, cex.axis=2)
  axis(2, cex.axis=2)
  legend(legendpos[1], legendpos[2], legend=c("SHAM","SCI"), col=c("blue","red"), lty=1:2, cex=2)
}

# composite plot: sca, gma, wma
plot.new()

dfw_sca2 <- subset(dfw_sca, select=labels) 

tmp1 <- subset(dfw_sca, select = -c(id,group,group_bin))
tmp2 <- subset(dfw_gma, select = -c(id,group,group_bin))
tmp3 <- subset(dfw_wma, select = -c(id,group,group_bin))
x <- 1:length(level_names)
y1 <- unlist(colMeans(tmp1[cond0,],na.rm = TRUE))
y2 <- unlist(colMeans(tmp1[cond1,],na.rm = TRUE))
y3 <- unlist(colMeans(tmp2[cond0,],na.rm = TRUE))
y4 <- unlist(colMeans(tmp2[cond1,],na.rm = TRUE))
y5 <- unlist(colMeans(tmp3[cond0,],na.rm = TRUE))
y6 <- unlist(colMeans(tmp3[cond1,],na.rm = TRUE))
xlab <- "Spinal level"
ylab <- "Cross-sectional area (mm2)"
ylim <- c(0,14)
legendpos <- c(9,13)
labels <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
plot_over_levels_2groups_composite(x,y1,y2,y3,y4,y5,y6,xlab,xaxt,ylab,ylim,labels,legendpos)

# composite plot: wma_dor, wma_lat, wma_ven
plot.new()
tmp1 <- subset(dfw_wma_dor, select = -c(id,group,group_bin))
tmp2 <- subset(dfw_wma_lat, select = -c(id,group,group_bin))
tmp3 <- subset(dfw_wma_ven, select = -c(id,group,group_bin))
x <- 1:length(level_names)
y1 <- unlist(colMeans(tmp1[cond0,],na.rm = TRUE))
y2 <- unlist(colMeans(tmp1[cond1,],na.rm = TRUE))
y3 <- unlist(colMeans(tmp2[cond0,],na.rm = TRUE))
y4 <- unlist(colMeans(tmp2[cond1,],na.rm = TRUE))
y5 <- unlist(colMeans(tmp3[cond0,],na.rm = TRUE))
y6 <- unlist(colMeans(tmp3[cond1,],na.rm = TRUE))
xlab <- "Spinal level"
ylab <- "Cross-sectional area (mm2)"
ylim <- c(0,5)
legendpos <- c(9,13)
labels <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
plot_over_levels_2groups_composite(x,y1,y2,y3,y4,y5,y6,xlab,xaxt,ylab,ylim,labels,legendpos)

















# ===================================================================
# =============== PLOT PER COMBINED LEVEL (SEGMENT) =================
# ===================================================================

# create a plot of a metric (csa or dti metric) across the four combined spinal cord levels
# Inputs:
#   df     - dataframe that contains the metrics to plot
#   ylabel - label of the y axis
#   ylim   - range of the y axis
#   dummy_scale_bythousand - scale metrics by 1e3s


# =====
# plot 4 groups
plot_over_segments_4groups <- function(type_plot="boxplot", df, ylabel, ylim, title, output, dummy_scale_bythousand=FALSE) {
  tmp1 <- rowMeans(subset(df, select = c("C5","C4","C3")), na.rm=TRUE)
  tmp2 <- rowMeans(subset(df, select = c("C8","C7","C6")), na.rm=TRUE)
  tmp3 <- rowMeans(subset(df, select = c("T3","T2","T1")), na.rm=TRUE)
  tmp4 <- rowMeans(subset(df, select = c("T6","T5","T4")), na.rm=TRUE)
  tmp  <- data.frame(df$id, df$group, df$group_bin, tmp1, tmp2, tmp3, tmp4)
  
  colnames(tmp) <- c("id","group","group_bin","1","2","3","4")
  
  tmp <- reshape(tmp,
                 direction = "long",
                 varying = colnames(tmp)[4:ncol(tmp)],
                 v.names = "area",
                 timevar = "segment",
                 times = c("1", "2", "3", "4"),
                 idvar = "id")
  
  # scale by thousand (optional)
  if (dummy_scale_bythousand) {tmp$area <- 1000*tmp$area}
  
  tmp$Mean <- ave(tmp$area, tmp$segment, tmp$group_bin, FUN=function(x) mean(x,na.rm=TRUE))
  tmp$Sd  <- ave(tmp$area, tmp$segment, tmp$group_bin, FUN=function(x) sd(x,na.rm=TRUE))
  
  plot.new()
  
  # create barplot
  if (type_plot=="barplot") {
    ggplot(tmp, aes(x=segment, y=area, fill=group, color=group)) +
      stat_summary(fun="mean", geom="bar", na.rm=TRUE, width=0.8, position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean, ymax=Mean+Sd), position=position_dodge(width=0.8), width=0.4) +
      scale_fill_manual(values = c("snow4","snow3"), labels = c("SHAM", "SCI")) +
      scale_color_manual(values = c("black","black"), labels = c("SHAM", "SCI")) +
      labs(x="", y=ylabel) +
      scale_x_discrete(labels=c("1"="C3-C5", "2"="C6-C8", "3"="T1-T3", "4"="T4-T6")) +
      coord_cartesian(ylim=ylim) + 
      ggtitle(title) +
      theme_classic() +
      theme(axis.text.x = element_text(size=16, angle = 45, hjust=1)) +
      theme(axis.text.y = element_text(size=16)) +
      theme(axis.title = element_text(size=18)) +
      theme(legend.text = element_text(size = 16)) +
      theme(plot.title = element_text(size=20, hjust = 0.5))
  }

  # create boxplot
  if (type_plot=="boxplot") {
    ggplot(tmp, aes(x=segment, y=area, fill=group, color=group)) +
      stat_boxplot(geom = 'errorbar', position=position_dodge(width=0.8), width=0.4) +
      geom_boxplot(width=0.8, position=position_dodge()) +
      scale_fill_manual(values = c("grey30","grey53","grey76","grey100"), labels = c("SHM", "MOD", "MLD", "SEV")) +
      scale_color_manual(values = c("black","black","black","black"), labels = c("SHM", "MOD", "MLD", "SEV")) +
      labs(x="", y=ylabel) +
      scale_x_discrete(labels=c("1"="C3-C5", "2"="C6-C8", "3"="T1-T3", "4"="T4-T6")) +
      coord_cartesian(ylim=ylim) + 
      ggtitle(title) +
      theme_classic() +
      theme(axis.text.x = element_text(size=16, angle = 45, hjust=1)) +
      theme(axis.text.y = element_text(size=16)) +
      theme(axis.title = element_text(size=18)) +
      theme(legend.text = element_text(size = 16)) +
      theme(plot.title = element_text(size=20, hjust = 0.5))
  }
  
  # create boxplot with points
  if (type_plot=="boxplot_with_points") {
    ggplot(tmp, aes(x=segment, y=area, fill=group, color=group)) +
      stat_boxplot(geom = 'errorbar', position=position_dodge(width=0.8), width=0.4) +
      geom_boxplot(width=0.8, position=position_dodge(), outlier.shape = NA) +
      geom_point(aes(fill = group_bin, color = group), size = 1.6, shape = 21, position = position_jitterdodge()) +
      #   scale_fill_manual(values = c("snow4","snow3"), labels = c("SHAM", "SCI")) +
      #    scale_fill_manual(values = c("black","red","blue","green","yellow","orange","black"), labels = c("SHAM", "SCI", "MLD", "MOD", "SEV", "SHM")) +
      scale_color_manual(values = c("black","red","blue","green","yellow","orange","black"), labels = c("SHAM", "SCI", "MLD", "MOD", "SEV", "SHM")) +
      labs(x="", y=ylabel) +
      scale_x_discrete(labels=c("1"="C3-C5", "2"="C6-C8", "3"="T1-T3", "4"="T4-T6")) +
      coord_cartesian(ylim=ylim) + 
      ggtitle(title) +
      theme_classic() +
      theme(axis.text.x = element_text(size=16, angle = 45, hjust=1)) +
      theme(axis.text.y = element_text(size=16)) +
      theme(axis.title = element_text(size=18)) +
      theme(legend.text = element_text(size = 16)) +
      theme(plot.title = element_text(size=20, hjust = 0.5))
  }
  
  ggsave(paste(DIR, 'figures', output, sep='\\'))
  
}

plot_over_segments_4groups("boxplot", dfw_sca, "SCA (mm2)", c(4,12), "spinal cord area",  "fig_csa_sc.png")
plot_over_segments_4groups("boxplot", dfw_wma, "WMA (mm2)", c(3,9), "white matter area",  "fig_csa_wm.png")
plot_over_segments_4groups("boxplot", dfw_gma, "GMA (mm2)", c(0,5), "gray matter area",  "fig_csa_gm.png")

plot_over_segments_4groups("boxplot", dfw_gma_dor, "Cross-sectional Area (mm2)", c(0.25,2.05), "dorsal gray matter horns",  "fig_csa_gm_dor.png")
plot_over_segments_4groups("boxplot", dfw_gma_ven, "Cross-sectional Area (mm2)", c(0.25,3.15), "ventral gray matter horns", "fig_csa_gm_ven.png")
plot_over_segments_4groups("boxplot", dfw_wma_dor, "Cross-sectional Area (mm2)", c(0.5,2.6), "dorsal white matter columns",  "fig_csa_wm_dor.png")
plot_over_segments_4groups("boxplot", dfw_wma_lat, "Cross-sectional Area (mm2)", c(2,6.2), "lateral white matter columns", "fig_csa_wm_lat.png")
plot_over_segments_4groups("boxplot", dfw_wma_ven, "Csross-sectional Area (mm2)", c(0.5,2.6), "ventral white matter columns", "fig_csa_wm_ven.png")
plot_over_segments_4groups("boxplot", dfw_fa_wm_dor, "Fractional Anisotropy", c(0.55,0.95), "dorsal white matter columns", "fig_fa_wm_dor.png");
plot_over_segments_4groups("boxplot", dfw_fa_wm_lat, "Fractional Anisotropy", c(0.55,0.95), "lateral white matter columns","fig_fa_wm_lat.png");
plot_over_segments_4groups("boxplot", dfw_fa_wm_ven, "Fractional Anisotropy", c(0.55,0.95), "ventral white matter columns","fig_fa_wm_ven.png");
plot_over_segments_4groups("boxplot", dfw_md_wm_dor, "Mean Diffusivity (microm2/ms)", c(0.2,0.7), "dorsal white matter columns",   "fig_md_wm_dor.png", TRUE);
plot_over_segments_4groups("boxplot", dfw_md_wm_lat, "Mean Diffusivity (microm2/ms)", c(0.2,0.7), "lateral white matter columns",  "fig_md_wm_lat.png", TRUE);
plot_over_segments_4groups("boxplot", dfw_md_wm_ven, "Mean Diffusivity (microm2/ms)", c(0.2,0.7), "ventral white matter columns",  "fig_md_wm_ven.png", TRUE);
plot_over_segments_4groups("boxplot", dfw_ad_wm_dor, "Axial Diffusivity (microm2/ms)", c(0.4,1.3), "dorsal white matter columns",  "fig_ad_wm_dor.png", TRUE);
plot_over_segments_4groups("boxplot", dfw_ad_wm_lat, "Axial Diffusivity (microm2/ms)", c(0.4,1.3), "lateral white matter columns", "fig_ad_wm_lat.png", TRUE);
plot_over_segments_4groups("boxplot", dfw_ad_wm_ven, "Axial Diffusivity (microm2/ms)", c(0.4,1.3), "ventral white matter columns", "fig_ad_wm_ven.png", TRUE);
plot_over_segments_4groups("boxplot", dfw_rd_wm_dor, "Radial Diffusivity (microm2/ms)", c(0.05,0.4), "dorsal white matter columns",   "fig_rd_wm_dor.png", TRUE);
plot_over_segments_4groups("boxplot", dfw_rd_wm_lat, "Radial Diffusivity (microm2/ms)", c(0.05,0.4), "lateral white matter columns",  "fig_rd_wm_lat.png", TRUE);
plot_over_segments_4groups("boxplot", dfw_rd_wm_ven, "Radial Diffusivity (microm2/ms)", c(0.05,0.4), "ventral white matter columns",  "fig_rd_wm_ven.png", TRUE);
plot_over_segments_4groups("boxplot", dfw_T1_wm_dor, "T1 Relaxation time (ms)", c(1200,2000), "dorsal white matter columns", "fig_T1_wm_dor.png");
plot_over_segments_4groups("boxplot", dfw_T1_wm_lat, "T1 Relaxation time (ms)", c(1200,2000), "lateral white matter columns","fig_T1_wm_lat.png");
plot_over_segments_4groups("boxplot", dfw_T1_wm_ven, "T1 Relaxation time (ms)", c(1200,2000), "ventral white matter columns","fig_T1_wm_ven.png");




# =====
# plot 2 groups (SCI vs. SHAM)
plot_over_segments_2groups <- function(type_plot="boxplot", df, ylabel, ylim, title, output, dummy_scale_bythousand=FALSE) {
  tmp1 <- rowMeans(subset(df, select = c("T6","T5","T4")), na.rm=TRUE)
  tmp2 <- rowMeans(subset(df, select = c("T3","T2","T1")), na.rm=TRUE)
  tmp3 <- rowMeans(subset(df, select = c("C8","C7","C6")), na.rm=TRUE)
  tmp4 <- rowMeans(subset(df, select = c("C5","C4","C3")), na.rm=TRUE)
  tmp  <- data.frame(df$id, df$group, df$group_bin, tmp1, tmp2, tmp3, tmp4)
  
  colnames(tmp) <- c("id","group","group_bin","1","2","3","4")
  
  tmp <- reshape(tmp,
                 direction = "long",
                 varying = colnames(tmp)[4:ncol(tmp)],
                 v.names = "area",
                 timevar = "segment",
                 times = c("1", "2", "3", "4"),
                 idvar = "id")
  
  # scale by thousand (optional)
  if (dummy_scale_bythousand) {tmp$area <- 1000*tmp$area}
  
  tmp$Mean <- ave(tmp$area, tmp$segment, tmp$group_bin, FUN=function(x) mean(x,na.rm=TRUE))
  tmp$Sd  <- ave(tmp$area, tmp$segment, tmp$group_bin, FUN=function(x) sd(x,na.rm=TRUE))
  
  plot.new()
  
  # create barplot
  if (type_plot=="barplot") {
    ggplot(tmp, aes(x=segment, y=area, fill=group_bin, color=group_bin)) +
      stat_summary(fun="mean", geom="bar", na.rm=TRUE, width=0.8, position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean, ymax=Mean+Sd), position=position_dodge(width=0.8), width=0.4) +
      scale_fill_manual(values = c("snow4","snow3"), labels = c("SHAM", "SCI")) +
      scale_color_manual(values = c("black","black"), labels = c("SHAM", "SCI")) +
      labs(x="", y=ylabel) +
      scale_x_discrete(labels=c("1"="mid thoracic", "2"="upper thoracic", "3"="lower cervical", "4"="upper cervical")) +
      coord_cartesian(ylim=ylim) + 
      ggtitle(title) +
      theme_classic() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      theme(axis.text.x = element_text(size=16, angle = 45, hjust=1)) +
      theme(axis.text.y = element_text(size=16)) +
      theme(axis.title = element_text(size=18)) +
      theme(legend.text = element_text(size = 16)) +
      theme(plot.title = element_text(size=20, hjust = 0.5))
  }
  
  # create boxplot
  if (type_plot=="boxplot") {
    ggplot(tmp, aes(x=segment, y=area, fill=group_bin, color=group_bin)) +
      stat_boxplot(geom = 'errorbar', position=position_dodge(width=0.8), width=0.4) +
      geom_boxplot(width=0.8, position=position_dodge()) +
      scale_fill_manual(values = c("snow4","snow3"), labels = c("SHAM", "SCI")) +
      scale_color_manual(values = c("black","black"), labels = c("SHAM", "SCI")) +
      labs(x="", y=ylabel) +
      scale_x_discrete(labels=c("1"="mid thoracic", "2"="upper thoracic", "3"="lower cervical", "4"="upper cervical")) +
      coord_cartesian(ylim=ylim) + 
      ggtitle(title) +
      theme_classic() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      theme(axis.text.x = element_text(size=16, angle = 45, hjust=1)) +
      theme(axis.text.y = element_text(size=16)) +
      theme(axis.title = element_text(size=18)) +
      theme(legend.text = element_text(size = 16)) +
      theme(plot.title = element_text(size=20, hjust = 0.5))
  }
  
  # create boxplot with points
  if (type_plot=="boxplot_with_points") {
    ggplot(tmp, aes(x=segment, y=area, fill=group_bin, color=group_bin)) +
      stat_boxplot(geom = 'errorbar', position=position_dodge(width=0.8), width=0.4) +
      geom_boxplot(width=0.8, position=position_dodge(), outlier.shape = NA) +
      geom_point(aes(fill = group_bin, color = group), size = 1.6, shape = 21, position = position_jitterdodge()) +
      #   scale_fill_manual(values = c("snow4","snow3"), labels = c("SHAM", "SCI")) +
      #    scale_fill_manual(values = c("black","red","blue","green","yellow","orange","black"), labels = c("SHAM", "SCI", "MLD", "MOD", "SEV", "SHM")) +
      scale_color_manual(values = c("black","red","blue","green","yellow","orange","black"), labels = c("SHAM", "SCI", "MLD", "MOD", "SEV", "SHM")) +
      labs(x="", y=ylabel) +
      scale_x_discrete(labels=c("1"="mid thoracic", "2"="upper thoracic", "3"="lower cervical", "4"="upper cervical")) +
      coord_cartesian(ylim=ylim) + 
      ggtitle(title) +
      theme_classic() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      theme(axis.text.x = element_text(size=16, angle = 45, hjust=1)) +
      theme(axis.text.y = element_text(size=16)) +
      theme(axis.title = element_text(size=18)) +
      theme(legend.text = element_text(size = 16)) +
      theme(plot.title = element_text(size=20, hjust = 0.5))
  }
  
  ggsave(paste(DIR, 'figures', output, sep='\\'))
  
}

plot_over_segments_2groups("boxplot", dfw_gma_dor, "cross-sectional area (mm2)", c(0.25,2.05), "dorsal gray matter horns",  "fig_csa_gm_dor.png")
plot_over_segments_2groups("boxplot", dfw_gma_ven, "cross-sectional area (mm2)", c(0.25,3.15), "ventral gray matter horns", "fig_csa_gm_ven.png")
plot_over_segments_2groups("boxplot", dfw_wma_dor, "cross-sectional area (mm2)", c(0.5,2.6), "dorsal white matter columns",  "fig_csa_wm_dor.png")
plot_over_segments_2groups("boxplot", dfw_wma_lat, "cross-sectional area (mm2)", c(2,6.2), "lateral white matter columns", "fig_csa_wm_lat.png")
plot_over_segments_2groups("boxplot", dfw_wma_ven, "cross-sectional area (mm2)", c(0.5,2.6), "ventral white matter columns", "fig_csa_wm_ven.png")
plot_over_segments_2groups("boxplot", dfw_fa_wm_dor, "fractional anisotropy", c(0.55,0.95), "dorsal white matter columns", "fig_fa_wm_dor.png");
plot_over_segments_2groups("boxplot", dfw_fa_wm_lat, "fractional anisotropy", c(0.55,0.95), "lateral white matter columns","fig_fa_wm_lat.png");
plot_over_segments_2groups("boxplot", dfw_fa_wm_ven, "fractional anisotropy", c(0.55,0.95), "ventral white matter columns","fig_fa_wm_ven.png");
plot_over_segments_2groups("boxplot", dfw_md_wm_dor, "mean diffusivity (microm2/ms)", c(0.2,0.7), "dorsal white matter columns",   "fig_md_wm_dor.png", TRUE);
plot_over_segments_2groups("boxplot", dfw_md_wm_lat, "mean diffusivity (microm2/ms)", c(0.2,0.7), "lateral white matter columns",  "fig_md_wm_lat.png", TRUE);
plot_over_segments_2groups("boxplot", dfw_md_wm_ven, "mean diffusivity (microm2/ms)", c(0.2,0.7), "ventral white matter columns",  "fig_md_wm_ven.png", TRUE);
plot_over_segments_2groups("boxplot", dfw_ad_wm_dor, "axial diffusivity (microm2/ms)", c(0.4,1.3), "dorsal white matter columns",  "fig_ad_wm_dor.png", TRUE);
plot_over_segments_2groups("boxplot", dfw_ad_wm_lat, "axial diffusivity (microm2/ms)", c(0.4,1.3), "lateral white matter columns", "fig_ad_wm_lat.png", TRUE);
plot_over_segments_2groups("boxplot", dfw_ad_wm_ven, "axial diffusivity (microm2/ms)", c(0.4,1.3), "ventral white matter columns", "fig_ad_wm_ven.png", TRUE);
plot_over_segments_2groups("boxplot", dfw_rd_wm_dor, "radial diffusivity (microm2/ms)", c(0.05,0.4), "dorsal white matter columns",   "fig_rd_wm_dor.png", TRUE);
plot_over_segments_2groups("boxplot", dfw_rd_wm_lat, "radial diffusivity (microm2/ms)", c(0.05,0.4), "lateral white matter columns",  "fig_rd_wm_lat.png", TRUE);
plot_over_segments_2groups("boxplot", dfw_rd_wm_ven, "radial diffusivity (microm2/ms)", c(0.05,0.4), "ventral white matter columns",  "fig_rd_wm_ven.png", TRUE);
plot_over_segments_2groups("boxplot", dfw_T1_wm_dor, "T1 relaxation time (ms)", c(1200,2000), "dorsal white matter columns", "fig_T1_wm_dor.png");
plot_over_segments_2groups("boxplot", dfw_T1_wm_lat, "T1 relaxation time (ms)", c(1200,2000), "lateral white matter columns","fig_T1_wm_lat.png");
plot_over_segments_2groups("boxplot", dfw_T1_wm_ven, "T1 relaxation time (ms)", c(1200,2000), "ventral white matter columns","fig_T1_wm_ven.png");




plot_over_levels_4groups("boxplot", dfw_gma_dor, "cross-sectional area (mm2)", c(0.25,2.05), "dorsal gray matter horns",  "fig_csa_gm_dor.png")





# for Patrick's grant:
x <- 1:length(level_names)
y1 <- unlist(colMeans(dfw_wma_dor[groups_2==0,],na.rm = TRUE))
y2 <- unlist(colMeans(dfw_wma_dor[groups_2==1,],na.rm = TRUE))
y3 <- unlist(colMeans(dfw_wma_lat[groups_2==0,],na.rm = TRUE))
y4 <- unlist(colMeans(dfw_wma_lat[groups_2==1,],na.rm = TRUE))
xlab <- "Spinal level"
ylab <- "Cross-sectional area (mm2)"
ylim <- c(0,5.5)
legendpos <- c(9,2.5)
labels <- c("C3","C4","C5","C6","C7","C8","T1","T2","T3","T4","T5","T6")
plot_over_levels_4lines_se(x,y1,y2,y3,y4,xlab,xaxt,ylab,ylim,labels,legendpos)

DATAw2_gm_tmp <- DATAw2_gm[!is.na(DATAw2_gm$area.C6), ]
DATAw2_gm_tmp <- subset(DATAw2_gm, group.C6=="SHAM" | group.C6=="SEVERE")
DATAw2_gm_tmp$group.C6 <- factor(DATAw2_gm_tmp$group.C6,
                                 levels = c("SHAM","SEVERE"),ordered = TRUE)
ggplot(data = DATAw2_gm_tmp, mapping = aes(x=group.C6, y=area.C6, col=group.C6)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) +
  scale_color_manual(values=c("SHAM"="blue", "SEVERE"="red")) +
  xlab("Group") +
  ylab("Gray matter area at C6 (mm2)") +
  ylim(c(2.5,4.0)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=18))