# 
# 
# 
# 

library(vegan)
library(tidyverse)
library (twinspanR)
library(tictoc)
library(DNCImper)

source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\PerSIMPER_v3.R")
source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\DNCI_ses_v3.R")
#source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\DNCI_ses_overall_v1.R")
#source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\PerSIMPER_Overall_v1.R")

DNCIndex <- function(spe, Nperm = 1000, dataType = "prab", time_sparse = 180){
  # Ward cluster
  ward <- hclust (dist (spe), method = 'ward.D2')
  grouping <- cutree (ward, 2)
  # CCA <- cca(spe)
  # ordiplot (CCA, display = 'si', type = 'n')
  # points (CCA, pch = grouping, col = grouping)
  # 
  if (length(unique(grouping)) != 2) return("only 1 group")
  # DCNI
  res_DNCI <- DNCI.ses(spe, grouping, Nperm = Nperm, dataTYPE = dataType, count = F, plotSIMPER = F, time_sparse = time_sparse)
  return(list(res = res_DNCI)) 
}

# date()
# DNCI(dat_spe, Nperm = 99, dataType = "count") -> res_1
# date()
# DNCI(dat_spe, Nperm = 99, dataType = "prab") -> res_2
# date()            
#             
