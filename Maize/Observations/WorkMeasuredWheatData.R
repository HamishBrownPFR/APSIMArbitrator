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
str(info)


baseSimName <- "Lincoln2015"

# change data arrangement (FIXME: check units needed in APSIM)

df_apsim <- info %>%
  dplyr::select(-Index_Date_Plot_Organ, -Index_Date_Plot, -HarvestNo, -Growth_Stage, -TreatNo, -Organ) %>%
  tidyr::gather("VariableType","Value",6:7) %>%
  mutate(VariableType = ifelse(VariableType == "DM_kgHa", as.character(Apsim_Var_name_Bio), as.character(Apsim_Var_name_N))) %>%
  dplyr::select(-Apsim_Var_name_Bio, -Apsim_Var_name_N) %>%
  tidyr::spread(VariableType,Value) %>%
  group_by(Clock.Today, Nit, Irr) %>%
  summarise_each(funs(mean)) %>%
  dplyr::select(-Plot, -Block) %>%
  mutate(Irr = ifelse(Irr == "Dryland", "Nil", "Full")) %>%
  mutate(SimulationName = paste0(baseSimName, "Nit", Nit, "Irr", Irr))

head(df_apsim)
summary(df_apsim)

# check data
df_apsim %>%
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
df <- df_apsim
df[is.na(df)] <- "*"
str(df)
  
write.table(df, file=paste0(dataDir,"WheatDataApsimX.txt"), quote=FALSE, row.names = FALSE)
