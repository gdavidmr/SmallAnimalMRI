compute_mean_values_persegment <- function(df) {

  group <- df$group
    
  tmp1 <- rowMeans(subset(df, select = c("C5","C4","C3")), na.rm=TRUE)
  tmp2 <- rowMeans(subset(df, select = c("C8","C7","C6")), na.rm=TRUE)
  tmp3 <- rowMeans(subset(df, select = c("T3","T2","T1")), na.rm=TRUE)
  tmp4 <- rowMeans(subset(df, select = c("T6","T5","T4")), na.rm=TRUE)
  
  print("C3-C5:")
  print("")
  
  print("SHM")
  print(paste("mean:", round(mean(tmp1[group=="SHM"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp1[group=="SHM"],na.rm=TRUE),2)))
  print("")
  
  print("MLD")
  print(paste("mean:", round(mean(tmp1[group=="MLD"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp1[group=="MLD"],na.rm=TRUE),2)))
  print("")
  
  print("MOD")
  print(paste("mean:", round(mean(tmp1[group=="MOD"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp1[group=="MOD"],na.rm=TRUE),2)))
  print("")
  
  print("SEV")
  print(paste("mean:", round(mean(tmp1[group=="SEV"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp1[group=="SEV"],na.rm=TRUE),2)))
  print("")
  
  print("difference: MLD - SHM")
  print(paste("diff:", round(    (mean(tmp1[group=="MLD"],na.rm=TRUE) - mean(tmp1[group=="SHM"],na.rm=TRUE)) / mean(tmp1[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  print("difference: MOD - SHM")
  print(paste("diff:", round(    (mean(tmp1[group=="MOD"],na.rm=TRUE) - mean(tmp1[group=="SHM"],na.rm=TRUE)) / mean(tmp1[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  print("difference: SEV - SHM")
  print(paste("diff:", round(    (mean(tmp1[group=="SEV"],na.rm=TRUE) - mean(tmp1[group=="SHM"],na.rm=TRUE)) / mean(tmp1[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  
  
  print("C6-C8:")
  print("")
  
  print("SHM")
  print(paste("mean:", round(mean(tmp2[group=="SHM"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp2[group=="SHM"],na.rm=TRUE),2)))
  print("")
  
  print("MLD")
  print(paste("mean:", round(mean(tmp2[group=="MLD"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp2[group=="MLD"],na.rm=TRUE),2)))
  print("")
  
  print("MOD")
  print(paste("mean:", round(mean(tmp2[group=="MOD"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp2[group=="MOD"],na.rm=TRUE),2)))
  print("")
  
  print("SEV")
  print(paste("mean:", round(mean(tmp2[group=="SEV"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp2[group=="SEV"],na.rm=TRUE),2)))
  print("")
  
  print("difference: MLD - SHM")
  print(paste("diff:", round(    (mean(tmp2[group=="MLD"],na.rm=TRUE) - mean(tmp2[group=="SHM"],na.rm=TRUE)) / mean(tmp2[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  print("difference: MOD - SHM")
  print(paste("diff:", round(    (mean(tmp2[group=="MOD"],na.rm=TRUE) - mean(tmp2[group=="SHM"],na.rm=TRUE)) / mean(tmp2[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  print("difference: SEV - SHM")
  print(paste("diff:", round(    (mean(tmp2[group=="SEV"],na.rm=TRUE) - mean(tmp2[group=="SHM"],na.rm=TRUE)) / mean(tmp2[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  
  
  print("T1-T3:")
  print("")
  
  print("SHM")
  print(paste("mean:", round(mean(tmp3[group=="SHM"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp3[group=="SHM"],na.rm=TRUE),2)))
  print("")
  
  print("MLD")
  print(paste("mean:", round(mean(tmp3[group=="MLD"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp3[group=="MLD"],na.rm=TRUE),2)))
  print("")
  
  print("MOD")
  print(paste("mean:", round(mean(tmp3[group=="MOD"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp3[group=="MOD"],na.rm=TRUE),2)))
  print("")
  
  print("SEV")
  print(paste("mean:", round(mean(tmp3[group=="SEV"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp3[group=="SEV"],na.rm=TRUE),2)))
  print("")
  
  print("difference: MLD - SHM")
  print(paste("diff:", round(    (mean(tmp3[group=="MLD"],na.rm=TRUE) - mean(tmp3[group=="SHM"],na.rm=TRUE)) / mean(tmp3[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  print("difference: MOD - SHM")
  print(paste("diff:", round(    (mean(tmp3[group=="MOD"],na.rm=TRUE) - mean(tmp3[group=="SHM"],na.rm=TRUE)) / mean(tmp3[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  print("difference: SEV - SHM")
  print(paste("diff:", round(    (mean(tmp3[group=="SEV"],na.rm=TRUE) - mean(tmp3[group=="SHM"],na.rm=TRUE)) / mean(tmp3[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  
  
  print("T4-T6:")
  print("")
  
  print("SHM")
  print(paste("mean:", round(mean(tmp4[group=="SHM"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp4[group=="SHM"],na.rm=TRUE),2)))
  print("")
  
  print("MLD")
  print(paste("mean:", round(mean(tmp4[group=="MLD"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp4[group=="MLD"],na.rm=TRUE),2)))
  print("")
  
  print("MOD")
  print(paste("mean:", round(mean(tmp4[group=="MOD"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp4[group=="MOD"],na.rm=TRUE),2)))
  print("")
  
  print("SEV")
  print(paste("mean:", round(mean(tmp4[group=="SEV"],na.rm=TRUE),2)))
  print(paste("std:",  round(sd(tmp4[group=="SEV"],na.rm=TRUE),2)))
  print("")
  
  print("difference: MLD - SHM")
  print(paste("diff:", round(    (mean(tmp4[group=="MLD"],na.rm=TRUE) - mean(tmp4[group=="SHM"],na.rm=TRUE)) / mean(tmp4[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  print("difference: MOD - SHM")
  print(paste("diff:", round(    (mean(tmp4[group=="MOD"],na.rm=TRUE) - mean(tmp4[group=="SHM"],na.rm=TRUE)) / mean(tmp4[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  print("difference: SEV - SHM")
  print(paste("diff:", round(    (mean(tmp4[group=="SEV"],na.rm=TRUE) - mean(tmp4[group=="SHM"],na.rm=TRUE)) / mean(tmp4[group=="SHM"],na.rm=TRUE)*100, 1)))
  print("")
  
  
  
  


}



compute_mean_values_persegment(dfw_wma_dor)