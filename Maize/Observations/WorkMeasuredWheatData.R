library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# get dir and file

dataDir <- "C:\\GitHubRepos\\Arbitrator\\Maize\\Observations\\"

dataFile <- "WheatData.txt"

info <- read.table(paste0(dataDir,dataFile), header = TRUE)

head(info)
summary(info)

# sort out formats
info$Clock.Today <- dmy(info$Clock.Today)
info$Plot <- as.factor(info$Plot)
info$Block <- as.factor(info$Block)
info$Nit <- as.factor(info$Nit)
info$DM_kgHa <- info$DM_kgHa/10
info$N_Perc <- info$N_Perc/100
str(info)

# check data
info %>%
 ggplot(aes(x=Nit, y=DM_kgHa)) +
  geom_boxplot(alpha=0.1) +
  geom_jitter(aes(colour=factor(Nit))) +
  facet_grid(Irr+Organ~Clock.Today, scales = "free")

info %>%
  filter(Organ != "Rachis") %>%
  ggplot(aes(x=Nit, y=N_Perc)) +
  geom_boxplot(alpha=0.1) +
  geom_jitter(aes(colour=factor(Nit))) +
  facet_grid(Irr+Organ~Clock.Today, scales = "free")


baseSimName <- "Lincoln2015"

# change data arrangement (FIXME: check units needed in APSIM)

 
# per plot  
df_PerPlot <-  info %>%
  dplyr::select(-Index_Date_Plot_Organ, -Index_Date_Plot, -HarvestNo, -Growth_Stage, -TreatNo, -Organ) %>%
  tidyr::gather("VariableType","Value",6:7) %>%
  mutate(VariableType = ifelse(VariableType == "DM_kgHa", as.character(Apsim_Var_name_Bio), as.character(Apsim_Var_name_N))) %>%
  dplyr::select(-Apsim_Var_name_Bio, -Apsim_Var_name_N) %>%
  tidyr::spread(VariableType,Value) %>%
  group_by(Clock.Today, Nit, Irr) 



# averages per treat
df_PerTreat <- df_PerPlot %>%
  summarise_each(funs(mean)) %>%
  dplyr::select(-Plot, -Block) %>%
  mutate(Irr = ifelse(Irr == "Dryland", "Nil", "Full")) %>%
  mutate(SimulationName = paste0(baseSimName, "Nit", Nit, "Irr", Irr))

head(df_PerTreat)
summary(df_PerTreat)

# check data
df_PerTreat %>%
  gather("Variable", "Value", 4:13) %>%
  mutate(Variable = as.factor(Variable), Value = as.numeric(Value)) %>%
#  summary()
#  filter(Variable == "Stem.Live.Wt") %>%
  filter(Variable != "Wheat.Rachis.Live.Nconc") %>%
  ggplot(aes(x=Clock.Today, y=Value, 
             shape= factor(Nit), 
             colour= factor(Irr))) +
  geom_point() +
  geom_line() +
  facet_wrap(~Variable,ncol=3, scales = "free")

# give * to NA for APSIM file  
df <- df_PerTreat
df[is.na(df)] <- "*"
str(df)
head(df)

# FIXME: Add the "()" in second line of text file printed (units)


write.table(df, file=paste0(dataDir,"WheatDataApsimX.txt"), quote=FALSE, row.names = FALSE)
