
library(dplyr)
library(ggplot2)
library(tidyr)

# reads raw data
df <- read.csv(file = "~/ESPE/data/jvelez.csv")


#sweet factors where needed----
df$nit <- factor(df$nit)
df$sector <- factor(df$sector)

 str(df, list.len=ncol(df))


#checks dimensions and stuff
numFirms <- length(
  unique(df$nit)
  )
numAnos <- length(
  unique(df$anios)
  )
numSectors <- length(
  unique(df$sec)
  )

print(paste("hay",numFirms, "firmas", numAnos, "años y", numSectors, "sectores"))




dfTemp <- df %>%
  count(nit)


print('numero de firmas por años que aparecen en la muestra')
table(dfTemp$n)


dfTemp <- df %>%
  group_by(anio) %>%
  summarise(
    numUniFirms = length(
      unique(nit)
      )
    )


print(
  paste(
    'el año en que más firmas hubo fue',
  dfTemp$anio[dfTemp$numUniFirms== max(dfTemp$numUniFirms)],
  'en el que hubo', max(dfTemp$numUniFirms),
  'firmas'
    )
)



# exploring profit distributions

dfSummary <- df %>%
  group_by(sector, anio) %>%
  summarise(
    minimo = min(ganancias),
    maximo = max(ganancias),
    media = mean(ganancias),
    per10 = quantile(ganancias, .10),
    per25 = quantile(ganancias, .25),
    per50 = quantile(ganancias, .50),
    per75 = quantile(ganancias, .75),
    rango = max(ganancias) - min(ganancias), 
    TotFirmas = n(),
    inactiveFirms = sum(ganancias < 0),
    probActive = (n()-sum(ganancias <= 0))/n()
  )



# non-parametric model of "entry" ----

# simplest possible: matrix contains probability a firm is active, meaning its profits are above zero
matProbActive = matrix(dfSummary$probActive,nrow = 20, byrow = T)

# adding a probability of being active (conditional on sector/year) to each firm
df$numSector <- NA
sectores <- levels(df$sector)
sectoresNum <- c(1:20)
df <- df %>% 
  mutate(numSector = ifelse(sector == "15", 1,
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
                                          ifelse(sector == "36", 20,NA)))))))))))))))))))))


df <- df %>% 
  mutate(numAnio = ifelse(anio == 2005, 1,
    ifelse(anio == 2006, 2,
      ifelse(anio == 2007, 3,
        ifelse(anio == 2008, 4,
          ifelse(anio == 2009, 5,
            ifelse(anio == 2010, 6,
              ifelse(anio == 2011, 7,
                ifelse(anio == 2012, 8,
                  ifelse(anio == 2013, 9,NA))))))))))




# baseline revenues and counterfactual active

# computes growth and selects top and bottom groups----

df <- df %>%
  group_by(nit) %>%
  mutate(
    growth = (pn_bruta - lag(pn_bruta))/lag(pn_bruta),
    meangrowth = mean(growth, na.rm = T)
  ) %>%
  filter(!is.na(meangrowth)) %>%
  ungroup()
 

df$bottom20 <- df$meangrowth <= quantile(df$meangrowth, .20, na.rm = T)
df$top20 <- df$meangrowth >= quantile(df$meangrowth, .80, na.rm = T)
df$none <- df$bottom20 == df$top20


df$simGroups <- factor(df$top20*1+df$none*2+df$bottom20*3)
levels(df$simGroups)[levels(df$simGroups)=='1'] <- 'Percentil 80+'
levels(df$simGroups)[levels(df$simGroups)=='2'] <- 'Media'
levels(df$simGroups)[levels(df$simGroups)=='3'] <- 'Percentil 20-'


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

# counterfactual leverage----

dfLev <- df %>%
  mutate(deltaLev = (Blev_bg - lag(Blev_bg))/Blev_bg, na.rm = T) %>%
  group_by(anio,simGroups) %>%
  summarise(deltaLev = mean(deltaLev, na.rm = T)) %>%
  filter(simGroups == 'Percentil 80+') 


df <- full_join(df,dfLev[,c(1,3)], by ='anio') 

df$deltaLev[df$simGroups == 'Percentil 80+'] <- df$Blev_bg[df$simGroups == 'Percentil 80+']




# yhat----
df <- df %>%
  mutate(
    active = rbinom(n(),1,matProbActive[numSector,numAnio]),
    baseline = 
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
      Bcte, 
    counterf1= Bomega_acf_sample*omega_acf_sample +
      Bwdev_expo_s*wdev_expo_s +
      Bwdev_impo_s*wdev_impo_s +
      Bexpo_fob*expo_fob +
      Bimpo_fob*impo_fob +
      BBlev_bg*deltaLev +
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


dfPlot <- df %>%
  group_by(anio, simGroups) %>%
  summarise(
    baseline = mean(baseline, na.rm = T),
    counterf1 = mean(counterf1*active, na.rm = T)
  ) %>%
  group_by(simGroups) %>%
  mutate(
    baselineIndex = 100 + (baseline - baseline[anio==2005])/baseline,
    counterf1Index = 100 + (counterf1-counterf1[anio==2005])/counterf1
  )


# replaces mean of firms not in top nor bottom
for(i in c(2005:2013)) {
dfPlot$baseline[dfPlot$simGroups=='Media'& dfPlot$anio == i] <- mean(df$pn_bruta[df$anio==i], na.rm = T)
dfPlot$counterf1[dfPlot$simGroups=='Media'& dfPlot$anio == i] <- mean(df$counterf1[df$anio==i], na.rm = T)
}


#plots----

# baseline
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

