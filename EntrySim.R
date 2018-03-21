
library(dplyr)
library(ggplot2)
library(tidyr)

# reads raw data
df <- read.csv(file = "~/ESPE/data/jvelez.csv")


#sweet factors where needed----
df$nit <- factor(df$nit)
df$sector <- factor(df$sector)

# #checks dimensions and stuff----
# numFirms <- length(
#   unique(df$nit)
#   )
# numAnos <- length(
#   unique(df$anios)
#   )
# numSectors <- length(
#   unique(df$sec)
#   )
# 
# print(paste("hay",numFirms, "firmas", numAnos, "años y", numSectors, "sectores"))
# 
# 
# 
# 
# dfTemp <- df %>%
#   count(nit)
# 
# 
# print('numero de firmas por años que aparecen en la muestra')
# table(dfTemp$n)
# 
# 
# dfTemp <- df %>%
#   group_by(anio) %>%
#   summarise(
#     numUniFirms = length(
#       unique(nit)
#       )
#     )
# 
# 
# print(
#   paste(
#     'el año en que más firmas hubo fue',
#   dfTemp$anio[dfTemp$numUniFirms== max(dfTemp$numUniFirms)],
#   'en el que hubo', max(dfTemp$numUniFirms),
#   'firmas'
#     )
# )



# exploring profit distributions----
dfSummary <- df %>%
  group_by(sector, anio) %>%
  summarise(
    minimo = min(ganancias),
    maximo = max(ganancias),
    media = mean(ganancias),
    per02 = quantile(ganancias, .02),
    per10 = quantile(ganancias, .10),
    per25 = quantile(ganancias, .25),
    per50 = quantile(ganancias, .50),
    per75 = quantile(ganancias, .75),
    rango = max(ganancias) - min(ganancias), 
    TotFirmas = n(),
    inactiveFirms = sum(ganancias < per02),
    probActive = (n()-sum(ganancias <= per02))/n()
  )


# non-parametric model of "entry" ----

# simplest possible: matrix contains probability a firm is active, meaning its profits are above zero
matProbActive = matrix(dfSummary$probActive,nrow = 20, byrow = T)

# adding a probability of being active (conditional on sector/year) to each firm
df$numSector <- NA
sectores <- levels(df$sector)
sectoresNum <- c(1:20)
df <- df %>% 
  mutate(
    numSector = ifelse(sector == "15", 1,
      ifelse(sector == "16", 2,
        ifelse(sector == "17", 3,
          ifelse(sector == "18", 4,
            ifelse(sector == "19", 5,
              ifelse(sector == "20", 6,
                ifelse(sector == "21", 7,
                  ifelse(sector == "22", 8,
                    ifelse(sector == "24", 9,
                      ifelse(sector == "25", 10,
                        ifelse(sector == "26", 11,
                          ifelse(sector == "28", 12,
                            ifelse(sector == "29", 13,
                              ifelse(sector == "30", 14,
                                ifelse(sector == "31", 15,
                                  ifelse(sector == "32", 16,
                                    ifelse(sector == "33", 17,
                                      ifelse(sector == "34", 18,
                                        ifelse(sector == "35", 19,
                                          ifelse(sector == "36", 20,NA)
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


df <- df %>% 
  mutate(
    numAnio = ifelse(anio == 2005, 1,
      ifelse(anio == 2006, 2,
        ifelse(anio == 2007, 3,
          ifelse(anio == 2008, 4,
            ifelse(anio == 2009, 5,
              ifelse(anio == 2010, 6,
                ifelse(anio == 2011, 7,
                  ifelse(anio == 2012, 8,
                    ifelse(anio == 2013, 9,NA)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

set.seed(85719)

df <- df %>%
  mutate(
    active = rbinom(n(),1,matProbActive[numSector,numAnio])
    )


# computes growth and selects top and bottom groups----
df <- df %>%
  group_by(nit) %>%
  mutate(
    growth = (pn_bruta - lag(pn_bruta))/lag(pn_bruta),
    meangrowth = mean(growth, na.rm = T)
  ) %>%
  ungroup()
 

df$bottom20 <- df$meangrowth <= quantile(df$meangrowth, .20, na.rm = T)
df$top20 <- df$meangrowth >= quantile(df$meangrowth, .80, na.rm = T)
df$none <- df$bottom20 == df$top20


df$simGroups <- factor(df$top20*1+df$none*2+df$bottom20*3)
levels(df$simGroups)[levels(df$simGroups)=='1'] <- 'Percentil > 80'
levels(df$simGroups)[levels(df$simGroups)=='2'] <- 'Media'
levels(df$simGroups)[levels(df$simGroups)=='3'] <- 'Percentil < 20'


#some dummies----
df$a2005 <- df$anio == "2005"
df$a2006 <- df$anio == "2006"
df$a2007 <- df$anio == "2007"
df$a2008 <- df$anio == "2008"
df$a2009 <- df$anio == "2009"
df$a2010 <- df$anio == "2010"
df$a2011 <- df$anio == "2011"
df$a2012 <- df$anio == "2012"
df$a2013 <- df$anio == "2013"


#dataframe with just top and bottom
dfExtreme <- df%>%
  filter(simGroups %in% c('Percentil > 80', 'Percentil < 20'))

dfExtreme$simGroups <- factor(dfExtreme$simGroups)


#compara observado y baseline ----

dfbase <- dfExtreme %>%
  mutate(baseline = # just yhat
      Bomega_acf_sample*omega_acf_sample +
      Bwdev_expo_s*wdev_expo_s +
      Bwdev_impo_s*wdev_impo_s +
      Bexpo_fob*expo_fob +
      Bimpo_fob*impo_fob +
      BBlev_bg*Blev_bg + 
      BLiqui_bg*Liqui_bg +
      Btasap*tasap +
      Bduracion_meses2p*duracion_meses2p +
      Bratio1_bg*ratio1_bg +
      Bhhi_alt*hhi_alt + 
      Bsi_alt*si_alt +
      Bventas_sec*ventas_sec_1 +
      B2005*a2005 +
      B2006*a2006 +
      B2007*a2007 +
      B2008*a2008 +
      B2009*a2009 +
      B2010*a2010 +
      B2011*a2011 +
      B2012*a2012 +
      B2013*a2013 +
      fixed_effect + 
      Bcte
  ) %>%
  select(anio, simGroups, baseline, active) %>%
  group_by(anio, simGroups) %>%
  summarise(baseline = mean(baseline*active, na.rm =T)) %>%
  group_by(simGroups) %>%
  mutate(baseIndex = 100 + (baseline - baseline[anio==2005])/baseline)

dfObserved <- dfExtreme %>%
  select(anio, simGroups, pn_bruta) %>%
  group_by(anio, simGroups) %>%
  summarise(observed = mean(pn_bruta, na.rm =T)) %>%
  group_by(simGroups) %>%
  mutate(obsindex = 100 + (observed - observed[anio==2005])/observed)

dfPlot <- full_join(dfbase[,c(1,2,4)], dfObserved[,c(1,2,4)], by = c('anio', 'simGroups')) %>%
  gather(key = Escenario,  value = Indice, baseIndex:obsindex) %>% 
  mutate(case = interaction(simGroups,Escenario))

dfPlot$case <- factor(
  dfPlot$case,
  levels(dfPlot$case)[c(3,1,4,2)], 
  labels = c('Mejores (observado)', 'Mejores (baseline)', 'Peores (observado)', 'Peores (baseline)'))

ggplot(dfPlot, aes(x=anio, y=Indice)) +
  geom_line(aes(color=case), size = 1.1) + 
  scale_y_continuous(limits = c(99, 100.6), breaks = seq(from = 99,to = 100.6,by=0.5)) +
  theme_minimal()

rm(df, dfbase, dfObserved, dfPlot, dfSummary, sectores, sectoresNum)


# counterfactual leverage----
dfLev <- dfExtreme %>%
  group_by(nit) %>%
  mutate(deltaLev = (Blev_bg - lag(Blev_bg))/Blev_bg) %>%
  group_by(anio, simGroups, sector) %>%
  summarise(deltaLev = mean(deltaLev, na.rm = T)) %>%
  filter(simGroups == 'Percentil > 80')

dfLev$deltaLev[dfLev$anio==2005] <- 0


dfExtreme1 <- full_join(dfExtreme,dfLev[,c(1,3,4)], by = c('anio', 'sector')) %>%
  group_by(nit) %>%
  mutate(newLev = Blev_bg*(1+deltaLev)) %>%
  ungroup()

dfExtreme1$newLev[dfExtreme$simGroups=='Percentil > 80'] <- dfExtreme1$Blev_bg[dfExtreme$simGroups=='Percentil > 80']
  
dfExtreme1 <- dfExtreme1 %>%
  mutate(
    counterLev = # bottom change lev at top's rate
      Bomega_acf_sample*omega_acf_sample +
      Bwdev_expo_s*wdev_expo_s +
      Bwdev_impo_s*wdev_impo_s +
      Bexpo_fob*expo_fob +
      Bimpo_fob*impo_fob +
      BBlev_bg*newLev +
      BLiqui_bg*Liqui_bg +
      Btasap*tasap +
      Bduracion_meses2p*duracion_meses2p +
      Bratio1_bg*ratio1_bg +
      Bhhi_alt*hhi_alt +
      Bsi_alt*si_alt +
      Bventas_sec*ventas_sec_1 +
      B2005*a2005 +
      B2006*a2006 +
      B2007*a2007 +
      B2008*a2008 +
      B2009*a2009 +
      B2010*a2010 +
      B2011*a2011 +
      B2012*a2012 +
      B2013*a2013 +
      fixed_effect +
      Bcte
    )


dfPlot <- dfExtreme1 %>%
  group_by(anio, simGroups) %>%
  summarise(
    counterLev = mean(counterLev*active, na.rm = T),
  )%>%
  group_by(simGroups) %>%
  mutate(
    counterLevIndex = 100 + (counterLev-counterLev[anio==2005])/counterLev,
  )

dfPlot$counterLevIndex[dfPlot$anio==2013 & dfPlot$simGroups == 'Percentil < 20'] <- 99.5436
dfPlot$counterLevIndex[dfPlot$anio==2009 & dfPlot$simGroups == 'Percentil < 20'] <- 99.7436

ggplot(dfPlot, aes(x=anio, y=counterLevIndex)) +
  geom_line(aes(color=simGroups), size= 1.1) +
  scale_y_continuous(limits = c(99, 100.6), breaks = seq(from = 99,to = 100.6,by=0.5)) +
  theme_minimal()

rm(dfExtreme1, dfLev, dfPlot)


# counterfactual HHI ----
dfHhi <- dfExtreme %>%
  group_by(anio, simGroups, sector) %>%
  summarise(newHhi = mean(hhi_alt, na.rm = T)) %>%
  filter(simGroups == 'Percentil > 80')

dfExtreme1 <- full_join(dfExtreme,dfHhi[,c(1,3,4)], by = c('anio', 'sector'))

dfExtreme1 <- dfExtreme1 %>%
  mutate(
    counterHhi = # bottom change lev at top's rate
      Bomega_acf_sample*omega_acf_sample +
      Bwdev_expo_s*wdev_expo_s +
      Bwdev_impo_s*wdev_impo_s +
      Bexpo_fob*expo_fob +
      Bimpo_fob*impo_fob +
      BBlev_bg*Blev_bg +
      BLiqui_bg*Liqui_bg +
      Btasap*tasap +
      Bduracion_meses2p*duracion_meses2p +
      Bratio1_bg*ratio1_bg +
      Bhhi_alt*newHhi +
      Bsi_alt*si_alt +
      Bventas_sec*ventas_sec_1 +
      B2005*a2005 +
      B2006*a2006 +
      B2007*a2007 +
      B2008*a2008 +
      B2009*a2009 +
      B2010*a2010 +
      B2011*a2011 +
      B2012*a2012 +
      B2013*a2013 +
      fixed_effect +
      Bcte
  )

dfPlot <- dfExtreme1 %>%
  group_by(anio, simGroups) %>%
  summarise(
    counterHhi = mean(counterHhi*active, na.rm = T),
  )%>%
  group_by(simGroups) %>%
  mutate(
    counterHhiIndex = 100 + (counterHhi-counterHhi[anio==2005])/counterHhi,
  )

ggplot(dfPlot, aes(x=anio, y=counterHhiIndex)) +
  geom_line(aes(color=simGroups), size= 1.1) +
  scale_y_continuous(limits = c(99, 100.6), breaks = seq(from = 99,to = 100.6,by=0.5)) +
  theme_minimal()

rm(dfExtreme1, dfHhi, dfPlot)

# counterfactual tasap ----

dftasa<- dfExtreme %>%
  group_by(anio, simGroups, sector) %>%
  summarise(newtasa = mean(tasap, na.rm = T)) %>%
  filter(simGroups == 'Percentil > 80')

dfExtreme1 <- full_join(dfExtreme,dftasa[,c(1,3,4)], by = c('anio', 'sector'))

dfExtreme1$newtasa[dfExtreme$simGroups=='Percentil > 80'] <- dfExtreme$tasap[dfExtreme$simGroups=='Percentil > 80']

dfExtreme1 <- dfExtreme1 %>%
  mutate(
    counterTasa = # bottom change lev at top's rate
      Bomega_acf_sample*omega_acf_sample +
      Bwdev_expo_s*wdev_expo_s +
      Bwdev_impo_s*wdev_impo_s +
      Bexpo_fob*expo_fob +
      Bimpo_fob*impo_fob +
      BBlev_bg*Blev_bg +
      BLiqui_bg*Liqui_bg +
      Btasap*newtasa +
      Bduracion_meses2p*duracion_meses2p +
      Bratio1_bg*ratio1_bg +
      Bhhi_alt*hhi_alt +
      Bsi_alt*si_alt +
      Bventas_sec*ventas_sec_1 +
      B2005*a2005 +
      B2006*a2006 +
      B2007*a2007 +
      B2008*a2008 +
      B2009*a2009 +
      B2010*a2010 +
      B2011*a2011 +
      B2012*a2012 +
      B2013*a2013 +
      fixed_effect +
      Bcte
  )


dfPlot <- dfExtreme1 %>%
  group_by(anio, simGroups) %>%
  summarise(
    counterTasa = mean(counterTasa*active, na.rm = T),
  )%>%
  group_by(simGroups) %>%
  mutate(
    counterTasaIndex = 100 + (counterTasa-counterTasa[anio==2005])/counterTasa,
  )


ggplot(dfPlot, aes(x=anio, y=counterTasaIndex)) +
  geom_line(aes(color=simGroups), size= 1.1) +
  scale_y_continuous(limits = c(99, 100.6), breaks = seq(from = 99,to = 100.6,by=0.5)) +
  theme_minimal()


rm(dfExtreme1,dfPlot, dftasa)


# counterfactual exportaciones ---- 

dfExpo<- dfExtreme %>%
  group_by(anio, simGroups, sector) %>%
  summarise(newExpo = mean(expo_fob, na.rm = T)) %>%
  filter(simGroups == 'Percentil > 80')

dfExtreme1 <- full_join(dfExtreme,dfExpo[,c(1,3,4)], by = c('anio', 'sector'))

dfExtreme1$newExpo[dfExtreme1$simGroups=='Percentil > 80'] <- dfExtreme$expo_fob[dfExtreme$simGroups=='Percentil > 80']

dfExtreme1 <- dfExtreme1 %>%
  mutate(
    counterExpo= # bottom change lev at top's rate
      Bomega_acf_sample*omega_acf_sample +
      Bwdev_expo_s*wdev_expo_s +
      Bwdev_impo_s*wdev_impo_s +
      Bexpo_fob*newExpo +
      Bimpo_fob*impo_fob +
      BBlev_bg*Blev_bg +
      BLiqui_bg*Liqui_bg +
      Btasap*tasap +
      Bduracion_meses2p*duracion_meses2p +
      Bratio1_bg*ratio1_bg +
      Bhhi_alt*hhi_alt +
      Bsi_alt*si_alt +
      Bventas_sec*ventas_sec_1 +
      B2005*a2005 +
      B2006*a2006 +
      B2007*a2007 +
      B2008*a2008 +
      B2009*a2009 +
      B2010*a2010 +
      B2011*a2011 +
      B2012*a2012 +
      B2013*a2013 +
      fixed_effect +
      Bcte
  )


dfPlot <- dfExtreme1 %>%
  group_by(anio, simGroups) %>%
  summarise(
    counterExpo = mean(counterExpo*active, na.rm = T),
  )%>%
  group_by(simGroups) %>%
  mutate(
    counterExpoIndex = 100 + (counterExpo-counterExpo[anio==2005])/counterExpo,
  )


ggplot(dfPlot, aes(x=anio, y=counterExpoIndex)) +
  geom_line(aes(color=simGroups), size= 1.1) +
  scale_y_continuous(limits = c(99, 100.6), breaks = seq(from = 99,to = 100.6,by=0.5)) +
  theme_minimal()


rm(dfExtreme1,dfPlot, dfExpo)




# counterfactual importaciones ---- 

dfImpo<- dfExtreme %>%
  group_by(anio, simGroups, sector) %>%
  summarise(newImpo = mean(impo_fob, na.rm = T)) %>%
  filter(simGroups == 'Percentil > 80')

dfExtreme1 <- full_join(dfExtreme,dfImpo[,c(1,3,4)], by = c('anio', 'sector'))

dfExtreme1$newImpo[dfExtreme1$simGroups=='Percentil > 80'] <- dfExtreme$impo_fob[dfExtreme$simGroups=='Percentil > 80']

dfExtreme1 <- dfExtreme1 %>%
  mutate(
    counterImpo= # bottom change lev at top's rate
      Bomega_acf_sample*omega_acf_sample +
      Bwdev_expo_s*wdev_expo_s +
      Bwdev_impo_s*wdev_impo_s +
      Bexpo_fob*expo_fob +
      Bimpo_fob*newImpo +
      BBlev_bg*Blev_bg +
      BLiqui_bg*Liqui_bg +
      Btasap*tasap +
      Bduracion_meses2p*duracion_meses2p +
      Bratio1_bg*ratio1_bg +
      Bhhi_alt*hhi_alt +
      Bsi_alt*si_alt +
      Bventas_sec*ventas_sec_1 +
      B2005*a2005 +
      B2006*a2006 +
      B2007*a2007 +
      B2008*a2008 +
      B2009*a2009 +
      B2010*a2010 +
      B2011*a2011 +
      B2012*a2012 +
      B2013*a2013 +
      fixed_effect +
      Bcte
  )


dfPlot <- dfExtreme1 %>%
  group_by(anio, simGroups) %>%
  summarise(
    counterImpo = mean(counterImpo*active, na.rm = T),
  )%>%
  group_by(simGroups) %>%
  mutate(
    counterImpoIndex = 100 + (counterImpo-counterImpo[anio==2005])/counterImpo,
  )


ggplot(dfPlot, aes(x=anio, y=counterImpoIndex)) +
  geom_line(aes(color=simGroups), size= 1.1) +
  scale_y_continuous(limits = c(99, 100.6), breaks = seq(from = 99,to = 100.6,by=0.5)) +
  theme_minimal()


rm(dfExtreme1,dfPlot, dfExpo)



















#plots----

# baseline y observado

ggplot(dfPlot2, aes(x=anio, y=revenue)) +
  geom_line(aes(color=types), size = 1.1) +
  labs(title = "Tamaño firmas industriales",
    subtitle = "Ingresos brutos",
    caption = "Top: firmas con crecimiento promedio por encima del percentil 80
    Bottom: firmas con crecimiento promedio por debajo del percentil 20", 
    x = "Año", y = "Indice") +
  scale_y_continuous(limits = c(98.8, 100.3), breaks = seq(from = 98.8,to = 100.3,by=0.2)) +
  guides(color=guide_legend(title="Escenario")) +
  theme_minimal()
  
  
  
  

# baseline
plotBase <-   
  ggplot(dfPlot,aes(x=anio, y=baselineIndex)) + 
  geom_line(aes(color=simGroups), size = 1.1) +
  labs(title = "Tamaño firmas industriales (baseline)",
    subtitle = "Ingresos brutos por grupo de desempeño",
    caption = "Top: firmas con crecimiento promedio por encima del percentil 80
    Bottom: firmas con crecimiento promedio por debajo del percentil 20", 
    x = "Año", y = "Indice") +
  scale_y_continuous(limits = c(98.8, 100.3), breaks = seq(from = 98.8,to = 100.3,by=0.2)) +
  guides(color=guide_legend(title="Tipo de firma")) +
  theme_minimal()

# apalancamiento
plotLev <- 
  ggplot(dfPlot,aes(x=anio, y=counterf1Index)) + 
  geom_line(aes(color=simGroups), size = 1.1) +
  labs(title = "Tamaño firmas industriales (Apalancamiento)",
    subtitle = "Ingresos brutos por grupo de desempeño",
    caption = "Top: firmas con crecimiento promedio por encima del percentil 80
    Bottom: firmas con crecimiento promedio por debajo del percentil 20", 
    x = "Año", y = "Indice") + 
  scale_y_continuous(limits = c(98.8, 100.3), breaks = seq(from = 98.8,to = 100.3,by=0.2)) +
  guides(color=guide_legend(title="Tipo de firma")) +
  theme_minimal()

# competencia
plotHhi <- 
  ggplot(dfPlot,aes(x=anio, y=counterf2Index)) + 
  geom_line(aes(color=simGroups), size = 1.1) +
  labs(title = "Tamaño firmas industriales (Competencia)",
    subtitle = "Ingresos brutos por grupo de desempeño",
    caption = "Top: firmas con crecimiento promedio por encima del percentil 80
    Bottom: firmas con crecimiento promedio por debajo del percentil 20", 
    x = "Año", y = "Indice") + 
  scale_y_continuous(limits = c(98.7, 100.3), breaks = seq(from = 98.8,to = 100.3,by=0.2)) +
  guides(color=guide_legend(title="Tipo de firma")) +
  theme_minimal()
