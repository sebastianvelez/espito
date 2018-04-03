
library(dplyr)
library(ggplot2)
library(tidyr)
library(plm)
library(cowplot)

# reads raw data
df <- read.csv(file = "~/ESPE/data/jvelez.csv")

# housekeeping> getting the data ready to rock----

#sweet factors where needed
df$nit <- factor(df$nit)
df$sector <- factor(df$sector)


#labels for sectorsManufacture of food products and beverages
levels(df$sector) <- c(
  'Food products and beverages'
,'Tobacco products'
,'Textiles'
,'Wearing apparel; dressing and dyeing of fur'
,'Tanning and dressing of leather'
,'Wood and of products of wood and cork'
,'Paper and paper products'
,'Publishing, printing and reproduction of recorded media'
,'Chemicals and chemical products'
,'Rubber and plastics products'
,'Other non-metallic mineral products'
,'Fabricated metal products, except machinery and equipment'
,'Machinery and equipment'
,'Office, accounting and computing machinery'
,'Electrical machinery and apparatus n.e.c.'
,'Radio, television and communication equipment and apparatus'
,'Medical, precision and optical instruments, watches and clocks'
,'Motor vehicles, trailers and semi-trailers'
,'Other transport equipment'
,'Furniture')



# computes growth and selects top and bottom groups
df <- df %>%
  group_by(nit) %>%
  mutate(
    growth = (pn_bruta - lag(pn_bruta))/lag(pn_bruta),
    meangrowth = mean(growth, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    Grupo = factor(
      case_when(
        meangrowth <= quantile(meangrowth, .20, na.rm = T) ~ 'Peores',
        meangrowth >= quantile(meangrowth, .80, na.rm = T) ~ 'Mejores',
        TRUE ~ 'Resto'
      )
    )
  )

df$Grupo <- factor(df$Grupo, levels(df$Grupo)[c(1,3,2)])

#some dummies
df$a2005 <- df$anio == "2005"
df$a2006 <- df$anio == "2006"
df$a2007 <- df$anio == "2007"
df$a2008 <- df$anio == "2008"
df$a2009 <- df$anio == "2009"
df$a2010 <- df$anio == "2010"
df$a2011 <- df$anio == "2011"
df$a2012 <- df$anio == "2012"
df$a2013 <- df$anio == "2013"


# crea baseline

df <- df%>%
  mutate(
    baseline = # just yhat
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
  )

# crea contrafactual exportaciones
    # cada firma se queda con el nivel incial (propio) de exportaciones
df <- df%>%
  group_by(nit) %>%
  mutate(
    newExpo = first(expo_fob)) %>% 
  ungroup() %>%
  mutate(
    cfExpo= # revenue if exports are stuck in initial year
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


# # crea contrafactual importaciones
# cada firma se queda con el nivel incial (propio) de importaciones
df <- df%>%
  group_by(nit) %>%
  mutate(
    newImpo = first(impo_fob)) %>% 
  ungroup() %>%
  mutate(
    cfImpo= # revenue if imports are stuck in initial year
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

# crea contrafactual leverage
# cada firma se queda con el nivel incial (propio) de apalancamiento
df <- df%>%
  group_by(nit) %>%
  mutate(
    newLev = max(Blev_bg)) %>% 
  ungroup() %>%
  mutate(
    cfLev= # revenue if leverage is stuck in initial year
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

# counterfactual expo ----

# Participación de cada sector en el total de exportaciones
dfStack <- df %>%
  group_by(anio, sector) %>%
  summarise(expo = sum(expo_fob))

ggplot(dfStack, aes(x=anio, y=expo, fill=sector)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal() +  
  annotate("text", x = 2010.7,  y = 1.68e+10, 
    label = "Exportaciones caen desde 2007. Entre 2008-2011 algunos \nsectores se expanden (21:papeles) y otros se contraen \n(34 y 32: manufactura de carros y de aparatos electrónicos)") +
  labs(y='Valor Exportaciones')+ theme(axis.title.x=element_blank())


# Participación de cada grupo en el total de exportaciones
dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(expo = sum(expo_fob))

ggplot(dfStack, aes(x=anio, y=expo, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal() +
  annotate("text", x = 2010.5,  y = 1.65e+10, 
    label = "Las peores nunca se recuperaron. \nLas mejores ganan terreno levemente") +
  labs(y='Valor Exportaciones')+ theme(axis.title.x=element_blank())


# plot contrafactual

  # lines

df <- df %>%
  group_by(nit) %>%
  mutate(
    cfExpoIndex2 = 100 + (cfExpo - first(cfExpo))/cfExpo,
    baseIndex = 100 + (baseline - first(baseline))/baseline
  )

p1 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,103), expand = T) + labs(y = 'Ingreso base')

p2 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  # theme(legend.position = 'none') +
  coord_cartesian(ylim = c(97,103), expand = T) 

leyenda <- get_legend(p2)

p2 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  theme(legend.position = 'none', axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T) 


p4 <- ggplot(df, aes(x=anio, y=cfExpoIndex2)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,103), expand = T)  + labs(y = 'Ingreso contrafactual')

p3 <- ggplot(df, aes(x=anio, y=cfExpoIndex2)) + 
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T)


p5 <- plot_grid(p1, p2, p4, p3)
p6 <- plot_grid( p5, leyenda, rel_widths = c(3, .3))
p7 <- add_sub(p6,label = 'Contrafactual: las exportaciones se fijan en el nivel incial')

ggdraw(p7)



    # areas


dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(
    cfExpo = sum(cfExpo),
    baseline = sum(baseline),
    diff = baseline - cfExpo
    )

p0 <- ggplot(dfStack, aes(x=anio, y=baseline, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal()

leyenda2 <- get_legend(p0)

p1 <- ggplot(dfStack, aes(x=anio, y=baseline, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal() + 
  theme(legend.position = 'none')

p2 <- ggplot(dfStack, aes(x=anio, y=cfExpo, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal() + 
  theme(legend.position = 'none', axis.title.y=element_blank())

p3 <- plot_grid(p1,p2)

p4 <- plot_grid(p3,leyenda2, rel_widths = c(3,.3))



## TEST diverging bars graph by sector (winners and loser of counterfactual)

dfDiv <- df %>%
  group_by(sector) %>%
  summarise(
    cfExpo = sum(cfExpo),
    baseline = sum(baseline),
  ) %>%
  mutate(
    diverge = cfExpo - baseline,
    divergestd = (diverge - mean(diverge, na.rm = T))/sd(diverge, na.rm = T),
    desemp = case_when(
        divergestd >=0 ~ 'Ganan',
        divergestd < 0 ~ 'Pierden',
        TRUE ~ NA_character_
      )
  )

ggplot(dfDiv, aes(x=sector, y=diverge)) + 
  geom_bar(stat='identity', aes(fill=desemp), width=.5) +
  coord_flip()


dfDiv <- df %>%
  group_by(Grupo) %>%
  summarise(
    cfExpo = sum(cfExpo),
    baseline = sum(baseline),
  ) %>% ungroup() %>%
  mutate(
    diverge = cfExpo - baseline,
    divergestd = (diverge - mean(diverge, na.rm = T))/sd(diverge, na.rm = T),
    desemp = case_when(
      divergestd >= 0 ~ 'Above mean',
      divergestd < 0 ~ 'Below mean',
      TRUE ~ NA_character_
    )
  ) 

ggplot(dfDiv, aes(x=Grupo, y=diverge)) + 
  geom_bar(stat='identity', aes(fill=desemp), width=.5) +
  coord_flip()



# counterfactual impo ----

# Participación de cada sector en el total de importaciones
dfStack <- df %>%
  group_by(anio, sector) %>%
  summarise(im = sum(impo_fob))

ggplot(dfStack, aes(x=anio, y=im, fill=sector)) + 
  geom_area(color='black', size=.2, alpha=.3) + 
  annotate("text", x = 2008,  y = 2.3e+10, 
    label = "Importaciones aumentan desde 2009. Entre 2008-2011 algunos sectores \nse contraen (34 y 30: manufactura de carros y de maquinaria )") +
  labs(y='Valor Importaciones')+ theme(axis.title.x=element_blank())

# Participación de cada Grupo en el total de importaciones
dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(im = sum(impo_fob))

ggplot(dfStack, aes(x=anio, y=im, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) +
  labs(y='Valor Importaciones')+ theme(axis.title.x=element_blank())


# plot contrafactual

df <- df %>%
  group_by(nit) %>%
  mutate(
    cfImpoIndex = 100 + (cfImpo - first(cfImpo))/cfImpo,
  ) %>%
  filter(cfImpoIndex > -100)

p1 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,103), expand = T) + labs(y = 'Ingreso base')

p2 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  # theme(legend.position = 'none') +
  coord_cartesian(ylim = c(97,103), expand = T) 

leyenda <- get_legend(p2)

p2 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  theme(legend.position = 'none', axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T) 


p4 <- ggplot(df, aes(x=anio, y=cfImpoIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,103), expand = T)  + labs(y = 'Ingreso contrafactual')

p3 <- ggplot(df, aes(x=anio, y=cfImpoIndex)) + 
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T)

p5 <- plot_grid(p1, p2, p4, p3)
p6 <- plot_grid( p5, leyenda, rel_widths = c(3, .3))
p7 <- add_sub(p6,label = 'Contrafactual: las importaciones se fijan en el nivel incial')

ggdraw(p7)




# counterfactual lev ----
# plot contrafactual

df <- df %>%
  group_by(nit) %>%
  mutate(
    cfLevIndex = 100 + (cfLev - first(cfLev))/cfLev,
  ) %>%
  filter(
    cfLevIndex > -100
  )

p1 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,103), expand = T) + labs(y = 'Ingreso base')

p2 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  # theme(legend.position = 'none') +
  coord_cartesian(ylim = c(97,103), expand = T) 

leyenda <- get_legend(p2)

p2 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  theme(legend.position = 'none', axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T) 


p4 <- ggplot(df, aes(x=anio, y=cfLevIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,103), expand = T)  + labs(y = 'Ingreso contrafactual')

p3 <- ggplot(df, aes(x=anio, y=cfLevIndex)) + 
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T)


p5 <- plot_grid(p1, p2, p4, p3)
p6 <- plot_grid( p5, leyenda, rel_widths = c(3, .3))
p7 <- add_sub(p6,label = 'Contrafactual: el apalancamiento se fija en el nivel incial')

ggdraw(p7)





#checks dimensions and stuff ----
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


# creando variable de sector y año numericas
df <- df %>%
  mutate(
    numSector = case_when(
      sector == '15' ~ 1,
      sector == '16' ~ 2,
      sector == '17' ~ 3,
      sector == '18' ~ 4,
      sector == '19' ~ 5,
      sector == '20' ~ 6,
      sector == '21' ~ 7,
      sector == '22' ~ 8,
      sector == '24' ~ 9,
      sector == '25' ~ 10,
      sector == '26' ~ 11,
      sector == '28' ~ 12,
      sector == '29' ~ 13,
      sector == '30' ~ 14,
      sector == '31' ~ 15,
      sector == '32' ~ 16,
      sector == '33' ~ 17,
      sector == '34' ~ 18,
      sector == '35' ~ 19,
      sector == '36' ~ 20,
      TRUE ~ NA_real_
    )
  )

df <- df %>% 
  mutate(
    numAnio = case_when(
      anio == 2005 ~ 1,
      anio == 2006 ~ 2,
      anio == 2007 ~ 3,
      anio == 2008 ~ 4,
      anio == 2009 ~ 5,
      anio == 2010 ~ 6,
      anio == 2011 ~ 7,
      anio == 2012 ~ 8,
      anio == 2013 ~ 9,
      TRUE ~ NA_real_
    )
  )


# Heterogeneity by sector ----

# Participación de cada sector en el total de la producción bruta
dfStack <- df %>%
  group_by(anio, sector) %>%
  summarise(pn = sum(pn_bruta))

ggplot(dfStack, aes(x=anio, y=pn, fill=sector)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal()

# Participación de cada sector en el total de exportaciones
dfStack <- df %>%
  group_by(anio, sector) %>%
  summarise(expo = sum(expo_fob))

ggplot(dfStack, aes(x=anio, y=expo, fill=sector)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal() +  
  annotate("text", x = 2010.5,  y = 1.65e+10, 
    label = "La suma de exportaciones cae desde 2007. \n Entre 2008-2011  algunos sectores se expanden (21:papeles) \ny otros se contraen (34:manufactura de carros)") +
  labs(y='Valor Exportaciones')+ theme(axis.title.x=element_blank())

# Participación de cada sector en el total de importaciones
dfStack <- df %>%
  group_by(anio, sector) %>%
  summarise(im = sum(impo_fob))

ggplot(dfStack, aes(x=anio, y=im, fill=sector)) + 
  geom_area(color='black', size=.2, alpha=.3)


# Heterogeneity by group----

# Participación de cada grupo en el total de la producción bruta
dfStack <- df%>%
  group_by(anio, Grupo) %>%
  summarise(pn=sum(pn_bruta))

p1 <- ggplot(dfStack, aes(x=anio, y=pn, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) +
  theme_minimal() 

pnStack <- p1 + theme(legend.position = 'none')

# Participación de cada sector en el total de exportaciones
dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(expo = sum(expo_fob))

ggplot(dfStack, aes(x=anio, y=expo, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal()

# Participación de cada Grupo en el total de importaciones
dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(im = sum(impo_fob))

ggplot(dfStack, aes(x=anio, y=im, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3)


# replicando las regresiones de Alejandra X ----
modelo <- plm(
  pn_bruta ~ omega_acf_sample + wdev_expo_s + wdev_impo_s + expo_fob + impo_fob + Blev_bg + Liqui_bg + tasap + duracion_meses2p + ratio1_bg + hhi_alt + si_alt + ventas_sec_1,
  data = df,
  index = c('nit', 'anio'),
  model = 'within',
  effect = 'twoways'
  )
coeficientes <- data.frame(modelo$coefficients)



# condicional en la estructura, contrafactuales del tipo "tal var" queda constante ----

#compara observado y baseline

  # line
dfbase <- df %>%
  select(anio, Grupo, baseline, pn_bruta) %>%
  filter(!is.na(baseline)) %>%
  group_by(anio) %>%
  summarise(
    baseMean = mean(baseline, na.rm =T),
    pnMean = mean(pn_bruta, na.rm = T),
    ) %>%
  gather( key=Escenario, value= Ingresos, baseMean, pnMean
  ) %>%
  group_by(Escenario) %>%
  mutate(
    ingresoIndex = 100 + (Ingresos-Ingresos[anio==2005])/Ingresos,
  )

ggplot(dfbase, aes(x=anio, y=ingresoIndex)) + 
  geom_line(aes(color=Escenario), position = position_dodge(width = 0.05), size =1)
            # ggplot(dfbase, aes(x=anio, y=baseline)) + geom_smooth(method='loess')


  # stack
dfStack2 <- df%>%
  group_by(anio, Grupo) %>%
  summarise(baseline=sum(baseline, na.rm =  T))

baseStack <- ggplot(dfStack2, aes(x=anio, y=baseline, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) +
  theme_minimal() + 
  theme(legend.position = 'none') +
  scale_y_continuous(limits = c(0,9e+10))

leyenda <- get_legend(p1)
 
 
prow <- plot_grid(pnStack, baseStack)

p <- plot_grid( prow, leyenda, rel_widths = c(3, .3))
  


# counterfactual leverage----


dfStack3 <- df %>%
  group_by(anio, Grupo) %>%
  summarise(cfLev=sum(cfLev, na.rm =  T))


levStack <- ggplot(dfStack3, aes(x=anio, y=cfLev, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) +
  theme_minimal() + 
  theme(legend.position = 'none')

leyenda <- get_legend(p1)


prow <- plot_grid(levStack, baseStack)

p <- plot_grid( prow, leyenda, rel_widths = c(3, .3))



    #means

dfStack4 <- df %>%
  group_by(anio, Grupo) %>%
  summarise(
    cfLev = mean(cfLev, na.rm =  T),
    baseline = mean(baseline, na.rm = T)) %>%
  group_by(Grupo) %>%
  mutate(
    cfLevIndex = 100 + (cfLev - cfLev[anio==2005])/cfLev,
    baseindex = 100 + (baseline - baseline[anio==2005])/baseline)

pmeans <- ggplot(dfStack4, aes(x=anio, y=cfLevIndex)) + 
  geom_line(aes(color=Grupo)) +
  theme_minimal() +
  theme(legend.position = 'none') +
  scale_y_continuous(limits = c(99.21,100.25))

pbase <- ggplot(dfStack4, aes(x=anio, y=baseindex)) + 
  geom_line(aes(color=Grupo)) +
  theme_minimal()+ theme(legend.position = 'none') +
  scale_y_continuous(limits = c(99.21,100.25))


p <- plot_grid( pmeans, pbase)

p <- plot_grid( p, leyenda, rel_widths = c(3, .3))





# determinantes salida de la muestra ---- 

    # stayers and leavers

df1 <- df %>%
  mutate(
    stayer = case_when(
      anios == 9 ~ 1,
      anios < 9 ~ 0,
      TRUE ~ NA_real_
      )
    ) %>%
  group_by(nit) %>%
  summarise(
    stayer = max(stayer),
    anios = max(anios)
    )

table(df1$stayer)
table(df1$anios)


df <- df %>%
  mutate(
    stayer = factor(
      case_when(
        anios == 9 ~ 'Stayer',
        anios < 9 ~ 'Leaver',
        TRUE ~ NA_character_
      )
    )
  )
  

p0 <- ggplot(df, aes(x=anio, y=pn_bruta)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank()) + labs(title= 'Producción')
leyenda3 <- get_legend(p0)

p1 <- ggplot(df, aes(x=anio, y=pn_bruta)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Producción')


p2 <- ggplot(df, aes(x=anio, y=omega_acf_sample)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer))  + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Productividad')


p3 <- ggplot(df, aes(x=anio, y=impo_fob)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Importaciones')


p4 <- ggplot(df, aes(x=anio, y=expo_fob)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_noraxis.title.x=element_blank(),mal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(), legend.position = 'none') + labs(title= 'Exportaciones')


p5 <- ggplot(df, aes(x=anio, y=Blev_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Apalancamiento')


p6 <- ggplot(df, aes(x=anio, y=Liqui_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Liquidez')

pa <- plot_grid(p1,p2,p3,p4,p5,p6)
paa <- plot_grid(pa, leyenda3,rel_widths = c(3, .3))



p7 <- ggplot(df, aes(x=anio, y=ratio1_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa Impuestos')


p8 <- ggplot(df, aes(x=anio, y=tasap)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa crédito')


p9 <- ggplot(df, aes(x=anio, y=wdev_expo_s)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa de Cambio(X)')


p10 <- ggplot(df, aes(x=anio, y=wdev_impo_s)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa de Cambio(M)')


p11 <- ggplot(df, aes(x=anio, y=ventas_sec_1)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Ventas Sector')


p12 <- ggplot(df, aes(x=anio, y=si_alt)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Participación')


pb <- plot_grid(p7,p8,p9,p10,p11,p12)
pbb <- plot_grid(pb, leyenda3,rel_widths = c(3, .3))


    #logit (probability of being a stayer)
library(broom)
library(stargazer)

miformula <- formula('stayer ~  pn_bruta + omega_acf_sample +   
    impo_fob + expo_fob + Blev_bg + Liqui_bg + ratio1_bg +          
    tasap + duracion_meses2p + wdev_impo_s + wdev_expo_s + ventas_sec_1 +        
    si_alt + hhi_alt + utilidad_bruta'
  )

miformulaFE <- formula('stayer ~ pn_bruta + omega_acf_sample +   
    impo_fob + expo_fob + Blev_bg + Liqui_bg + ratio1_bg +          
    tasap + duracion_meses2p + wdev_impo_s + wdev_expo_s + ventas_sec_1 +        
    si_alt + hhi_alt + utilidad_bruta +  sector'
  )

dfLogit <- df %>%
  mutate(sector=factor(sector)) %>%
  group_by(nit, stayer,sector) %>%
  summarise(
    pn_bruta = log(mean(pn_bruta, na.rm = T)),
    omega_acf_sample = log(mean(omega_acf_sample, na.rm = T)),
    impo_fob = mean(impo_fob, na.rm = T),
    expo_fob = mean(expo_fob, na.rm = T),
    Blev_bg = log(mean(Blev_bg, na.rm = T)),
    Liqui_bg = mean(Liqui_bg, na.rm = T),
    ratio1_bg = mean(ratio1_bg, na.rm = T),
    tasap = mean(tasap, na.rm = T),
    duracion_meses2p = mean(duracion_meses2p, na.rm = T),
    wdev_impo_s = mean(wdev_impo_s, na.rm = T),
    wdev_expo_s = mean(wdev_expo_s, na.rm = T),
    ventas_sec_1 = log(mean(ventas_sec_1, na.rm = T)),
    si_alt = log(mean(si_alt, na.rm = T)),
    hhi_alt = log(mean(hhi_alt, na.rm = T)),
    utilidad_antesT = mean(utilidad_antesT, na.rm = T),
    utilidad_operacional = mean(utilidad_operacional, na.rm = T),
    utilidad_bruta = mean(utilidad_bruta, na.rm = T)
  )

dfLogit$stayer <- relevel(dfLogit$stayer, ref = 'Stayer')

modelo <- glm(miformula,data = dfLogit,family = binomial(link = 'logit'))
modeloFE <- glm(miformulaFE,data = dfLogit,family = binomial(link = 'logit'))


stargazer(modelo, modeloFE, type = "text")
