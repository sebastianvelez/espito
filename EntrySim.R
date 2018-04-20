
library(dplyr)
library(ggplot2)
library(tidyr)
library(plm)
library(cowplot)
library(stargazer)
library(reshape2)

# reads raw data
rm(list=ls())
df <- read.csv(file = "~/ESPE/data/jvelez.csv")

# housekeeping and getting the data ready to rock----

#sweet factors where needed
df$nit <- factor(df$nit)
df$sector <- factor(df$sector)


# #labels for sectorsManufacture of food products and beverages
# levels(df$sector) <- c(
#   'Food products and beverages'
# ,'Tobacco products'
# ,'Textiles'
# ,'Wearing apparel; dressing and dyeing of fur'
# ,'Tanning and dressing of leather'
# ,'Wood and of products of wood and cork'
# ,'Paper and paper products'
# ,'Publishing, printing and reproduction of recorded media'
# ,'Chemicals and chemical products'
# ,'Rubber and plastics products'
# ,'Other non-metallic mineral products'
# ,'Fabricated metal products, except machinery and equipment'
# ,'Machinery and equipment'
# ,'Office, accounting and computing machinery'
# ,'Electrical machinery and apparatus n.e.c.'
# ,'Radio, television and communication equipment and apparatus'
# ,'Medical, precision and optical instruments, watches and clocks'
# ,'Motor vehicles, trailers and semi-trailers'
# ,'Other transport equipment'
# ,'Furniture')



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

# # crea contrafactual exportaciones EXTREMO
# # A las firmas peores, se les asignan las exportaciones medias (y medianas) de firmas en el mismo sector
# dfsum <- df%>%
#   group_by(sector, anio, Grupo) %>%
#   summarise(
#     meanexp = max(expo_fob, na.rm = T)
#   ) %>%
#   filter(Grupo=='Mejores') %>%
#   select(-Grupo)
# 
# df <- full_join(df, dfsum, by= c('sector', 'anio'))
# 
# 
#     
# df <- 
#   df %>% 
#   mutate(
#     newExpo2 = case_when(
#       Grupo %in% c('Mejores', 'Resto') ~ expo_fob,
#       Grupo == 'Peores' ~ meanexp,
#       TRUE  ~ NA_real_
#     )
#   ) %>% 
#   ungroup() %>%
#   mutate(
#     cfExpo2= # revenue if exports are stuck in initial year
#       Bomega_acf_sample*omega_acf_sample +
#       Bwdev_expo_s*wdev_expo_s +
#       Bwdev_impo_s*wdev_impo_s +
#       Bexpo_fob*newExpo2 +
#       Bimpo_fob*impo_fob +
#       BBlev_bg*Blev_bg + 
#       BLiqui_bg*Liqui_bg +
#       Btasap*tasap +
#       Bduracion_meses2p*duracion_meses2p +
#       Bratio1_bg*ratio1_bg +
#       Bhhi_alt*hhi_alt + 
#       Bsi_alt*si_alt +
#       Bventas_sec*ventas_sec_1 +
#       B2005*a2005 +
#       B2006*a2006 +
#       B2007*a2007 +
#       B2008*a2008 +
#       B2009*a2009 +
#       B2010*a2010 +
#       B2011*a2011 +
#       B2012*a2012 +
#       B2013*a2013 +
#       fixed_effect + 
#       Bcte
#   )
# 

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
    newLev = first(Blev_bg)) %>% 
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

# crea contrafactual tasap
# cada firma se queda con el nivel incial (propio) de tasa de inter閟
df <- df%>%
  group_by(nit) %>%
  mutate(
    newTasap = first(tasap)) %>% 
  ungroup() %>%
  mutate(
    cfTasap= # revenue if leverage is stuck in initial year
      Bomega_acf_sample*omega_acf_sample +
      Bwdev_expo_s*wdev_expo_s +
      Bwdev_impo_s*wdev_impo_s +
      Bexpo_fob*expo_fob +
      Bimpo_fob*impo_fob +
      BBlev_bg*Blev_bg + 
      BLiqui_bg*Liqui_bg +
      Btasap*newTasap +
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


## CF indices
df <- df %>%
  group_by(nit) %>%
  mutate(
    cfExpoIndex2 = 100 + (cfExpo - first(cfExpo))/cfExpo,
    baseIndex = 100 + (baseline - first(baseline))/baseline,
    cfImpoIndex = 100 + (cfImpo - first(cfImpo))/cfImpo,
    cfLevIndex = 100 + (cfLev - first(cfLev))/cfLev,
    cfTasapIndex = 100 + (cfTasap- first(cfTasap))/cfTasap
    
  )


# counterfactual expo ----

# Participaci贸n de cada sector en el total de exportaciones
# dfStack <- df %>%
#   group_by(anio, sector) %>%
#   summarise(expo = sum(expo_fob))
# 
# ggplot(dfStack, aes(x=anio, y=expo, fill=sector)) + 
#   geom_area(color='black', size=.2, alpha=.3) + theme_minimal() +  
#   annotate("text", x = 2010.7,  y = 1.68e+10, 
#     label = "Exportaciones caen desde 2007. Entre 2008-2011 algunos \nsectores se expanden (21:papeles) y otros se contraen \n(34 y 32: manufactura de carros y de aparatos electr贸nicos)") +
#   labs(y='Valor Exportaciones')+ theme(axis.title.x=element_blank())


# Participacion de cada grupo en el total de exportaciones
# dfStack <- df %>%
#   group_by(anio, Grupo) %>%
#   summarise(expo = sum(expo_fob)/1e+09)
# 
# ggplot(dfStack, aes(x=anio, y=expo, fill=Grupo)) + 
#   geom_area(color='black', size=.2, alpha=.3) +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 9)) +
#   labs(y='Valor Exportaciones (1000 millones)', x = 'A駉') +
#   theme_minimal()+
#   theme(panel.grid.minor.x = element_blank() )


# plot contrafactual

  # lines

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
p7 <- add_sub(p6,label = 'Contrafactual: las exportaciones se fijan en el nivel incial') + theme_minimal()

ggdraw(p7) 

# lines 222222222


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
p7 <- add_sub(p6,label = 'Contrafactual: las exportaciones se fijan en el nivel incial') + theme_minimal()

ggdraw(p7) 



    # Bar plot of difference in baseline revenue (by year and aggregated over years) and export CF

dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(
    cfExpo = sum(cfExpo, na.rm = T),
    baseline = sum(baseline, na.rm = T),
    ) %>%
  mutate(diff= (cfExpo - baseline)/1e+9) %>%
  filter(anio != 2005)


 p1bars <- ggplot(dfStack, aes(x=anio, y=diff, fill=Grupo)) +
   geom_bar(stat='identity', position='dodge', width=.5) + 
   geom_hline(yintercept = 0) +
   scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
   labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉') +
   theme_minimal() + 
   theme(panel.grid.minor.x = element_blank(),
     panel.grid.major.x = element_blank())

 
 
 p1lines <- ggplot(dfStack, aes(x=anio, y=diff)) +
   geom_line(aes(colour=Grupo)) + 
   geom_hline(yintercept = 0) +
   scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
   labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉') +
   theme_minimal() + 
   theme(panel.grid.minor.x = element_blank(),
     panel.grid.major.x = element_blank())

# dfDiv <- df %>%
#   group_by(Grupo) %>%
#   summarise(
#     cfExpo = sum(cfExpo, na.rm = T),
#     baseline = sum(baseline, na.rm = T),
#   ) %>% ungroup() %>%
#   mutate(
#     diverge = (cfExpo - baseline)/1e+9,
#     divergestd = (diverge - mean(diverge, na.rm = T))/sd(diverge, na.rm = T),
#     desemp = case_when(
#       divergestd >= 0 ~ 'Ganancias',
#       divergestd < 0 ~ 'P閞didas',
#       TRUE ~ NA_character_
#     )
#   ) 
# 
# p2 <- ggplot(dfDiv, aes(x=Grupo, y=diverge)) + 
#   geom_bar(stat='identity', aes(fill=desemp), width=.5) + 
#   geom_hline(yintercept = 0) +
#   labs(y = 'Diferencia en ingresos (1000 millones)') + 
#   theme_minimal() +
#   theme(legend.title = element_blank(), 
#     panel.grid.major.x = element_blank()) 
# 
# 
# p3 <- plot_grid(p1,p2)

# counterfactual impo ----

# Participaci贸n de cada sector en el total de importaciones
# dfStack <- df %>%
#   group_by(anio, sector) %>%
#   summarise(im = sum(impo_fob))
# 
# ggplot(dfStack, aes(x=anio, y=im, fill=sector)) + 
#   geom_area(color='black', size=.2, alpha=.3) + 
#   annotate("text", x = 2008,  y = 2.3e+10, 
#     label = "Importaciones aumentan desde 2009. Entre 2008-2011 algunos sectores \nse contraen (34 y 30: manufactura de carros y de maquinaria )") +
#   labs(y='Valor Importaciones')+ theme(axis.title.x=element_blank())

# Participaci贸n de cada Grupo en el total de importaciones
# dfStack <- df %>%
#   group_by(anio, Grupo) %>%
#   summarise(im = sum(impo_fob) / 1e+9)
# 
# someplot<- ggplot(dfStack, aes(x=anio, y=im, fill=Grupo)) + 
#   geom_area(color='black', size=.2, alpha=.3) +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 9)) +
#   labs(y='Valor Exportaciones (1000 millones)', x = 'A帽o') +
#   theme_minimal()+
#   theme(panel.grid.minor.x = element_blank() )

# plot contrafactual

df1 <- df %>%
  filter(cfImpoIndex > -100)

p1 <- ggplot(df1, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,103), expand = T) + labs(y = 'Ingreso base')

p2 <- ggplot(df1, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  # theme(legend.position = 'none') +
  coord_cartesian(ylim = c(97,103), expand = T) 

leyenda <- get_legend(p2)

p2 <- ggplot(df1, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  theme(legend.position = 'none', axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T) 


p4 <- ggplot(df1, aes(x=anio, y=cfImpoIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,103), expand = T)  + labs(y = 'Ingreso contrafactual')

p3 <- ggplot(df1, aes(x=anio, y=cfImpoIndex)) + 
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T)

p5 <- plot_grid(p1, p2, p4, p3)
p6 <- plot_grid( p5, leyenda, rel_widths = c(3, .3))
p7 <- add_sub(p6,label = 'Contrafactual: las importaciones se fijan en el nivel incial')

ggdraw(p7)


# Bar plot of difference in baseline revenue (by year and aggregated over years) and import CF

dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(
    cfImpo = sum(cfImpo, na.rm = T),
    baseline = sum(baseline, na.rm = T),
  ) %>%
  mutate(diff= (cfImpo - baseline)/1e+9) %>%
  filter(anio != 2005)


p1abars <- ggplot(dfStack, aes(x=anio, y=diff, fill=Grupo)) +
  geom_bar(stat='identity', position='dodge', width=.5) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉s') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank())

p1alines <- ggplot(dfStack, aes(x=anio, y=diff)) +
  geom_line(aes(colour=Grupo)) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉s') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank())


dfDiv <- df %>%
  group_by(Grupo) %>%
  summarise(
    cfImpo = sum(cfImpo, na.rm = T),
    baseline = sum(baseline, na.rm = T),
  ) %>% ungroup() %>%
  mutate(
    diverge = (cfImpo - baseline)/1e+9,
    divergestd = (diverge - mean(diverge, na.rm = T))/sd(diverge, na.rm = T),
    desemp = case_when(
      divergestd >= 0 ~ 'Ganancias',
      divergestd < 0 ~ 'P閞didas',
      TRUE ~ NA_character_
    )
  ) 

# p2 <- ggplot(dfDiv, aes(x=Grupo, y=diverge)) + 
#   geom_bar(stat='identity', aes(fill=desemp), width=.5) + 
#   geom_hline(yintercept = 0) +
#   labs(y = 'Diferencia en ingresos (1000 millones)') + 
#   theme_minimal() +
#   theme(legend.title = element_blank(), 
#     panel.grid.major.x = element_blank()) 
# 
# 
# p3 <- plot_grid(p1a,p2, align = 'h')


# 1 limpio: barras contrafactuales IMPO-EXPO lado a lado ----

dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(
    cfExpo = sum(cfExpo, na.rm = T),
    baseline = sum(baseline, na.rm = T),
  ) %>%
  mutate(diff= (cfExpo - baseline)/1e+9) %>%
  filter(anio != 2005)


p0 <- ggplot(dfStack, aes(x=anio, y=diff, fill=Grupo)) +
  geom_bar(stat='identity', position='dodge', width=.5) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 60),
    legend.position="bottom", 
    legend.title = element_blank(),
    legend.margin = margin(c(0,0,0,0)))

leyenda <- get_legend(p0)

p1 <- ggplot(dfStack, aes(x=anio, y=diff, fill=Grupo)) +
  geom_bar(stat='identity', position='dodge', width=.5) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60), 
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  coord_cartesian(ylim = c(-4.1,1), expand = T)

p1a <- ggdraw(
  add_sub(p1, "Exportaciones fijas \nen nivel incial")
  )


dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(
    cfImpo = sum(cfImpo, na.rm = T),
    baseline = sum(baseline, na.rm = T),
  ) %>%
  mutate(diff= (cfImpo - baseline)/1e+9) %>%
  filter(anio != 2005)


p2 <- ggplot(dfStack, aes(x=anio, y=diff, fill=Grupo)) +
  geom_bar(stat='identity', position='dodge', width=.5) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60))+
  coord_cartesian(ylim = c(-4.1,1), expand = T)


p2a <- ggdraw(
  add_sub(p2, 'Importaciones fijas \nen nivel incial')
  )

pp <-plot_grid(p1a, p2a, align = 'v')

pp1 <- plot_grid(pp, leyenda,
  ncol = 1,
  rel_heights = c(1,.1))





# counterfactual lev ----
# plot contrafactual

df <- df %>%
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



# Bar plot of difference in baseline revenue (by year and aggregated over years) and leverage CF

dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(
    cfLev = sum(cfLev, na.rm = T),
    baseline = sum(baseline, na.rm = T),
  ) %>%
  mutate(diff= (cfLev  - baseline)/1e+9) %>%
  filter(anio != 2005)


p1 <- ggplot(dfStack, aes(x=anio, y=diff, fill=Grupo)) +
  geom_bar(stat='identity', position='dodge', width=.5) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A帽os') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45))


dfDiv <- df %>%
  group_by(Grupo) %>%
  summarise(
    cfLev = sum(cfLev, na.rm = T),
    baseline = sum(baseline, na.rm = T),
  ) %>% ungroup() %>%
  mutate(
    diverge = (cfLev - baseline)/1e+9,
    divergestd = (diverge - mean(diverge, na.rm = T))/sd(diverge, na.rm = T),
    desemp = case_when(
      divergestd >= 0 ~ 'Ganancias',
      divergestd < 0 ~ 'Perdidas',
      TRUE ~ NA_character_
    )
  ) 

p2 <- ggplot(dfDiv, aes(x=Grupo, y=diverge)) + 
  geom_bar(stat='identity', aes(fill=desemp), width=.5) + 
  geom_hline(yintercept = 0) +
  labs(y = 'Diferencia en ingresos (1000 millones)') + 
  theme_minimal() +
  theme(legend.title = element_blank(), 
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45)) 


p3 <- plot_grid(p1,p2, align = 'h')



# counterfactual tasa ----
# plot contrafactual



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


p4 <- ggplot(df, aes(x=anio, y=cfTasapIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,103), expand = T)  + labs(y = 'Ingreso contrafactual')

p3 <- ggplot(df, aes(x=anio, y=cfTasapIndex)) + 
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T)


p5 <- plot_grid(p1, p2, p4, p3)
p6 <- plot_grid( p5, leyenda, rel_widths = c(3, .3))
p7 <- add_sub(p6,label = 'Contrafactual: la tasa se fija en el nivel incial')

ggdraw(p7)



# Bar plot of difference in baseline revenue (by year and aggregated over years) and leverage CF

dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(
    cfLiq = sum(cfLiq, na.rm = T),
    baseline = sum(baseline, na.rm = T),
  ) %>%
  mutate(diff= (cfLiq  - baseline)/1e+6) %>%
  filter(anio != 2005)


p1 <- ggplot(dfStack, aes(x=anio, y=diff, fill=Grupo)) +
  geom_bar(stat='identity', position='dodge', width=.5) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (millones)', x = 'A帽os') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45))


dfDiv <- df %>%
  group_by(Grupo) %>%
  summarise(
    cfLiq = sum(cfLiq, na.rm = T),
    baseline = sum(baseline, na.rm = T),
  ) %>% ungroup() %>%
  mutate(
    diverge = (cfLiq - baseline)/1e+6,
    divergestd = (diverge - mean(diverge, na.rm = T))/sd(diverge, na.rm = T),
    desemp = case_when(
      divergestd >= 0 ~ 'Ganancias',
      divergestd < 0 ~ 'P茅rdidas',
      TRUE ~ NA_character_
    )
  ) 

p2 <- ggplot(dfDiv, aes(x=Grupo, y=diverge)) + 
  geom_bar(stat='identity', aes(fill=desemp), width=.5) + 
  geom_hline(yintercept = 0) +
  labs(y = 'Diferencia en ingresos (millones)') + 
  theme_minimal() +
  theme(legend.title = element_blank(), 
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45)) 


p3 <- plot_grid(p1,p2, align = 'h')




# checks dimensions and stuff ----
numFirms <- length(
  unique(df$nit)
  )
numAnos <- length(
  unique(df$anios)
  )
numSectors <- length(
  unique(df$sec)
  )

print(paste("hay",numFirms, "firmas", numAnos, "a帽os y", numSectors, "sectores"))

dfTemp <- df %>%
  count(nit)

print('numero de firmas por a帽os que aparecen en la muestra')
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
    'el a帽o en que m谩s firmas hubo fue',
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


# creando variable de sector y a帽o numericas
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

# Participaci髇 de cada sector en el total de la produccion bruta
dfStack <- df %>%
  group_by(anio, sector) %>%
  summarise(pn = sum(pn_bruta))

ggplot(dfStack, aes(x=anio, y=pn, fill=sector)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal()

# Participacion de cada sector en el total de exportaciones
dfStack <- df %>%
  group_by(anio, sector) %>%
  summarise(expo = sum(expo_fob))

ggplot(dfStack, aes(x=anio, y=expo, fill=sector)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal() +  
  annotate("text", x = 2010.5,  y = 1.65e+10, 
    label = "La suma de exportaciones cae desde 2007. \n Entre 2008-2011  algunos sectores se expanden (21:papeles) \ny otros se contraen (34:manufactura de carros)") +
  labs(y='Valor Exportaciones')+ theme(axis.title.x=element_blank())

# Participaci贸n de cada sector en el total de importaciones
dfStack <- df %>%
  group_by(anio, sector) %>%
  summarise(im = sum(impo_fob))

ggplot(dfStack, aes(x=anio, y=im, fill=sector)) + 
  geom_area(color='black', size=.2, alpha=.3)


# Heterogeneity by group----

# Participaci贸n de cada grupo en el total de la producci贸n bruta
dfStack <- df%>%
  group_by(anio, Grupo) %>%
  summarise(pn=sum(pn_bruta)/1e+9)

ggplot(dfStack, aes(x=anio, y=pn, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9)) +
  labs(y='Valor Produci贸n Bruta (1000 millones)', x = 'A帽o') +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
    legend.title = element_blank())


# Participaci贸n de cada sector en el total de exportaciones
dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(expo = sum(expo_fob))

ggplot(dfStack, aes(x=anio, y=expo, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3) + theme_minimal()

# Participaci贸n de cada Grupo en el total de importaciones
dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(im = sum(impo_fob))

ggplot(dfStack, aes(x=anio, y=im, fill=Grupo)) + 
  geom_area(color='black', size=.2, alpha=.3)


# replicando las regresiones de Alejandra X ----

col1 <- formula('pn_bruta ~ omega_acf_sample + wdev_expo_s + wdev_impo_s + expo_fob + impo_fob + Blev_bg + Liqui_bg + tasap + duracion_meses2p + ratio1_bg + hhi_alt + si_alt + ventas_sec_1')
modelo0 <- lm(formula = col1, data = df)
modelo1 <- plm(
  formula = col1,
  data = df,
  index = c('nit', 'anio'),
  model = 'within',
  effect = 'individual'
)
modelo2 <- plm(
  pn_bruta ~ omega_acf_sample + wdev_expo_s + wdev_impo_s + expo_fob + impo_fob + Blev_bg + Liqui_bg + tasap + duracion_meses2p + ratio1_bg + hhi_alt + si_alt + ventas_sec_1,
  data = df,
  index = c('nit', 'anio'),
  model = 'within',
  effect = 'twoways'
  )

stargazer(modelo0, modelo1, modelo2, type = 'text', align = TRUE)
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





# counterfactual liquidez ----
# plot contrafactual

# lines

df <- df %>%
  group_by(nit) %>%
  mutate(
    cfLiq = 100 + (cfLiq - first(cfLiq))/cfLiq,
    baseIndex = 100 + (baseline - first(baseline))/baseline
  )

p1 <- ggplot(df, aes(x=anio, y=baseIndex)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(97,101), expand = T) + labs(y = 'Ingreso base')

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


p4 <- ggplot(df, aes(x=anio, y=cfLiq)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank()) + 
  coord_cartesian(ylim = c(99,100), expand = T)  + labs(y = 'Ingreso contrafactual')

p3 <- ggplot(df, aes(x=anio, y=cfExpoIndex2)) + 
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=Grupo)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=Grupo)) +
  theme(legend.position = 'none',axis.title.x=element_blank(), axis.title.y=element_blank()) +
  coord_cartesian(ylim = c(97,103), expand = T)


p5 <- plot_grid(p1, p2, p4, p3)
p6 <- plot_grid( p5, leyenda, rel_widths = c(3, .3))
p7 <- add_sub(p6,label = 'Contrafactual: las exportaciones se fijan en el nivel incial') + theme_minimal()

ggdraw(p7) 


# determinantes stay en la muestra ---- 

#AN罫ISIS GR罠ICO
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
  
  # producci髇 y productividad lado a lado
p0 <- ggplot(df, aes(x=anio, y=pn_bruta)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  theme_minimal() +
  theme(
    axis.title.y=element_blank(), 
    legend.position="bottom", 
    legend.title = element_blank(),
    legend.margin = margin(c(0,0,0,0))
    ) 

leyenda3 <- get_legend(p0)

p1 <- ggplot(df, aes(x=anio, y=pn_bruta/1e+6)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
    ) + 
  labs(title= 'Producci髇', y = 'Millones de pesos')


p2 <- ggplot(df, aes(x=anio, y=omega_acf_sample)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
    ) + 
  labs(title= 'Productividad', y = '%')


pa <- plot_grid(p1,p2)
paa <- plot_grid(pa,
  leyenda3,ncol = 1, 
  rel_heights = c(1, .1))


ggsave("~/ESPE/output/Producci髇OmegaSideBySidePlotStayers.pdf", width = 16, height = 8, units = 'in')


# tasa creditos e impuestos lado a lado

p1 <- ggplot(df, aes(x=anio, y=tasap)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Tasa cr閐itos', y = '%')


p2 <- ggplot(df, aes(x=anio, y=ratio1_bg*100)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Tasa impuestos', y = '%')


pa <- plot_grid(p1,p2)
paa <- plot_grid(pa,
  leyenda3,ncol = 1, 
  rel_heights = c(1, .1))


ggsave("~/ESPE/output/TasasSideBySidePlotStayers.pdf", width = 16, height = 8, units = 'in')



# tasas de cambio lado a lado

p1 <- ggplot(df, aes(x=anio, y=wdev_expo_s)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Tasa de cambio (X)', y = '%')


p2 <- ggplot(df, aes(x=anio, y=wdev_impo_s)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=stayer)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=stayer))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Tasa de cambio (M)', y = '%')


pa <- plot_grid(p1,p2)
paa <- plot_grid(pa,
  leyenda3,ncol = 1, 
  rel_heights = c(1, .1))

ggsave("~/ESPE/output/TasasCambioSideBySidePlotStayers.pdf", width = 16, height = 8, units = 'in')




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


 #Salida 

dfSalida <- df %>%
  group_by(nit) %>%
  mutate(sale0 = row_number() == n()) %>% ungroup() %>%
  mutate(
    sale = factor(sale0)
  ) %>%
  filter(
    anio != 2013
  )

dfSalida$sale <- relevel(dfSalida$sale, ref = 'TRUE')




# produccion y apalancamiento

p0 <- ggplot(dfSalida, aes(x=anio, y=pn_bruta)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() +
  theme(
    axis.title.y=element_blank(), 
    legend.position="bottom", 
    legend.title = element_blank(),
    legend.margin = margin(c(0,0,0,0))
  ) 

leyenda3 <- get_legend(p0)

p1 <- ggplot(dfSalida, aes(x=anio, y=pn_bruta/1e+6)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Producci髇', y = 'Millones de pesos')


p2 <- ggplot(dfSalida, aes(x=anio, y=Blev_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Apalancamiento', y = '%')


pa <- plot_grid(p1,p2)
paa <- plot_grid(pa,
  leyenda3,ncol = 1, 
  rel_heights = c(1, .1))


ggsave("~/ESPE/output/Producci髇ApalaSideBySidePlotSalientes.pdf", width = 16, height = 8, units = 'in')



p1 <- ggplot(dfSalida, aes(x=anio, y=expo_fob/1e+6)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Exportaciones', y = 'Millones de pesos')


p2 <- ggplot(dfSalida, aes(x=anio, y=impo_fob/1e+6)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Importaciones', y = 'Millones de pesos')


pa <- plot_grid(p1,p2)
paa <- plot_grid(pa,
  leyenda3,ncol = 1, 
  rel_heights = c(1, .1))

ggsave("~/ESPE/output/ExpoImpoSideBySidePlotSalientes.pdf", width = 16, height = 8, units = 'in')













p0 <- ggplot(dfSalida, aes(x=anio, y=pn_bruta)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank()) + labs(title= 'Producci贸n')

leyenda4 <- get_legend(p0)

p1 <- ggplot(dfSalida, aes(x=anio, y=pn_bruta)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Producci贸n')


p2 <- ggplot(dfSalida, aes(x=anio, y=omega_acf_sample)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale))  + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Productividad')


p3 <- ggplot(dfSalida, aes(x=anio, y=impo_fob)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Importaciones')


p4 <- ggplot(dfSalida, aes(x=anio, y=expo_fob)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) +
  theme_minimal() + theme(axis.title.y=element_blank(), legend.position = 'none') + labs(title= 'Exportaciones')


p5 <- ggplot(dfSalida, aes(x=anio, y=Blev_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Apalancamiento')


p6 <- ggplot(dfSalida, aes(x=anio, y=Liqui_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Liquidez')

pc <- plot_grid(p1,p2,p3,p4,p5,p6)
pcc <- plot_grid(pc, leyenda4,rel_widths = c(3, .3))




p7 <- ggplot(dfSalida, aes(x=anio, y=ratio1_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa Impuestos')


p8 <- ggplot(dfSalida, aes(x=anio, y=tasap)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa cr茅dito')


p9 <- ggplot(dfSalida, aes(x=anio, y=wdev_expo_s)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa de Cambio(X)')


p10 <- ggplot(dfSalida, aes(x=anio, y=wdev_impo_s)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa de Cambio(M)')


p11 <- ggplot(dfSalida, aes(x=anio, y=ventas_sec_1)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Ventas Sector')


p12 <- ggplot(dfSalida, aes(x=anio, y=si_alt)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=sale)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=sale)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Participaci贸n')


pd <- plot_grid(p7,p8,p9,p10,p11,p12)
pdd <- plot_grid(pd, leyenda4,rel_widths = c(3, .3))




miformula <- formula('sale ~  pn_bruta + omega_acf_sample +   
    impo_fob + expo_fob + Blev_bg + Liqui_bg + ratio1_bg +          
    tasap + duracion_meses2p + wdev_impo_s + wdev_expo_s + ventas_sec_1 +        
    si_alt + hhi_alt + utilidad_bruta'
)

miformulaFESector <- formula('sale ~ pn_bruta + omega_acf_sample +   
    impo_fob + expo_fob + Blev_bg + Liqui_bg + ratio1_bg +          
    tasap + duracion_meses2p + wdev_impo_s + wdev_expo_s + ventas_sec_1 +        
    si_alt + hhi_alt + utilidad_bruta + sector'
)

miformulaFEAnio <- formula('sale ~ pn_bruta + omega_acf_sample +   
    impo_fob + expo_fob + Blev_bg + Liqui_bg + ratio1_bg +          
  tasap + duracion_meses2p + wdev_impo_s + wdev_expo_s + ventas_sec_1 +        
  si_alt + hhi_alt + utilidad_bruta + sector + anio'
)


dfSalidaLog <- dfSalida %>%
  mutate(
    pn_bruta = log(pn_bruta),
    anio = factor(anio),
    omega_acf_sample = log(omega_acf_sample),
    Blev_bg = log(Blev_bg),
    ventas_sec_1 = log(ventas_sec_1),
    si_alt = log(si_alt),
    hhi_alt = log(hhi_alt),
  ) 



modelo <- glm(miformula,data = dfSalidaLog,family = binomial(link = 'logit'))
modeloFESector <- glm(miformulaFESector,data = dfSalidaLog,family = binomial(link = 'logit'))
modeloFEAnio <- glm(miformulaFEAnio,data = dfSalidaLog,family = binomial(link = 'logit'))

stargazer(modelo, modeloFESector, modeloFEAnio, type = "text")



# determinantes entrada a la muestra ---- 

# entrada

dfEntrada <- df %>%
  group_by(nit) %>%
  mutate(
    entra = row_number() == 1
  ) %>%
  filter(
    anio != 2005
  ) %>%
  summarize(
    entra = max(entra)
  )

table(dfEntrada$entra)

dfEntrada <- df %>%
  group_by(nit) %>%
  mutate(
    entra = row_number() == 1
  ) %>%
  filter(
    anio != 2005
  )


  # Producci髇 y productividad lado a lado
p0 <- ggplot(dfEntrada, aes(x=anio, y=pn_bruta)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() +
  theme(
    axis.title.y=element_blank(), 
    legend.position="bottom", 
    legend.title = element_blank(),
    legend.margin = margin(c(0,0,0,0))
  ) 

leyenda3 <- get_legend(p0)

p1 <- ggplot(dfEntrada, aes(x=anio, y=pn_bruta/1e+6)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Producci髇', y = 'Millones de pesos')


p2 <- ggplot(dfEntrada, aes(x=anio, y=omega_acf_sample)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Productividad', y = '%')


pa <- plot_grid(p1,p2)
paa <- plot_grid(pa,
  leyenda3,ncol = 1, 
  rel_heights = c(1, .1))


ggsave("~/ESPE/output/Producci髇OmegaSideBySidePlotEntrants.pdf", width = 16, height = 8, units = 'in')




p1 <- ggplot(dfEntrada, aes(x=anio, y=Blev_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Apalancamiento', y = '%')


p2 <- ggplot(dfEntrada, aes(x=anio, y=ventas_sec_1/1e+9)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra))  +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal() + 
  theme(
    axis.title.x=element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60),
    panel.grid.minor.x = element_blank()
  ) + 
  labs(title= 'Ventas sector', y = '1000 millones de pesos')


pa <- plot_grid(p1,p2)
paa <- plot_grid(pa,
  leyenda3,ncol = 1, 
  rel_heights = c(1, .1))


ggsave("~/ESPE/output/AapalancaVentasSideBySidePlotEntrants.pdf", width = 16, height = 8, units = 'in')



p0 <- ggplot(dfEntrada, aes(x=anio, y=pn_bruta)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  scale_color_discrete(breaks=c('FALSE', 'TRUE'), labels =c('Incumbente', 'Entrante'))+
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(), legend.title = element_blank()) + labs(title= 'Producci贸n')

leyenda4 <- get_legend(p0)

p1 <- ggplot(dfEntrada, aes(x=anio, y=pn_bruta)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Producci贸n')


p2 <- ggplot(dfEntrada, aes(x=anio, y=omega_acf_sample)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra))  + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Productividad')


p3 <- ggplot(dfEntrada, aes(x=anio, y=impo_fob)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Importaciones')


p4 <- ggplot(dfEntrada, aes(x=anio, y=expo_fob)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) +
  theme_minimal() + theme(axis.title.y=element_blank(), axis.title.x=element_blank(),legend.position = 'none') + labs(title= 'Exportaciones')


p5 <- ggplot(dfEntrada, aes(x=anio, y=Blev_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Apalancamiento')


p6 <- ggplot(dfEntrada, aes(x=anio, y=Liqui_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Liquidez')

pc <- plot_grid(p1,p2,p3,p4,p5,p6)
pcc <- plot_grid(pc, leyenda4,rel_widths = c(3, .3))



p7 <- ggplot(dfEntrada, aes(x=anio, y=ratio1_bg)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa Impuestos')


p8 <- ggplot(dfEntrada, aes(x=anio, y=tasap)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa cr茅dito')


p9 <- ggplot(dfEntrada, aes(x=anio, y=wdev_expo_s)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa de Cambio(X)')


p10 <- ggplot(dfEntrada, aes(x=anio, y=wdev_impo_s)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Tasa de Cambio(M)')


p11 <- ggplot(dfEntrada, aes(x=anio, y=ventas_sec_1)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Ventas Sector')


p12 <- ggplot(dfEntrada, aes(x=anio, y=si_alt)) + 
  stat_summary(geom="line", fun.y=mean, linetype="solid" ,  aes(color=entra)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_normal,alpha=0.03,linetype="dotted", aes(color=entra)) + 
  theme_minimal() + theme(axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = 'none') + labs(title= 'Participaci贸n')


pd <- plot_grid(p7,p8,p9,p10,p11,p12)
pdd <- plot_grid(pd, leyenda4,rel_widths = c(3, .3))




miformula <- formula('entra ~  pn_bruta + omega_acf_sample +   
  impo_fob + expo_fob + Blev_bg + Liqui_bg + ratio1_bg +          
  tasap + duracion_meses2p + wdev_impo_s + wdev_expo_s + ventas_sec_1 +        
  si_alt + hhi_alt + utilidad_bruta'
)

miformulaFESector <- formula('entra ~ pn_bruta + omega_acf_sample +   
  impo_fob + expo_fob + Blev_bg + Liqui_bg + ratio1_bg +          
  tasap + duracion_meses2p + wdev_impo_s + wdev_expo_s + ventas_sec_1 +        
  si_alt + hhi_alt + utilidad_bruta '
)

miformulaFEAnio <- formula('entra ~ pn_bruta + omega_acf_sample +   
  impo_fob + expo_fob + Blev_bg + Liqui_bg + ratio1_bg +          
  tasap + duracion_meses2p + wdev_impo_s + wdev_expo_s + ventas_sec_1 +        
  si_alt + hhi_alt + utilidad_bruta'
)


dfEntradaLog <- dfEntrada %>%
  mutate(
    pn_bruta = log(pn_bruta),
    anio = factor(anio),
    omega_acf_sample = log(omega_acf_sample),
    Blev_bg = log(Blev_bg),
    ventas_sec_1 = log(ventas_sec_1),
    si_alt = log(si_alt),
    hhi_alt = log(hhi_alt),
  ) 

modelo <- lm(
  formula = miformula,
  data = dfEntradaLog)

modeloFEAnio <- plm(
  formula = miformula,
  data = dfEntradaLog,
  index = c('nit', 'anio'),
  model = 'within',
  effect = 'time'
)

modeloFEtodo <- plm(
  formula = miformula,
  data = dfEntradaLog,
  index = c('nit', 'anio'),
  model = 'within',
  effect = 'twoways'
)


stargazer(modelo, modeloFEAnio, modeloFEtodo, type = "text", align = T)



# limpio: barras contrafactuales tasap-lev lado a lado ----

dfStack <- df %>%
  group_by(anio, Grupo) %>%
  summarise(
    cfLev = sum(cfLev, na.rm = T),
    baseline = sum(baseline, na.rm = T),
    cfTasap = sum(cfTasap, na.rm = T)
  ) %>%
  mutate(
    diffLev= (cfLev - baseline)/1e+9,
    diffTasap=(cfTasap - baseline)/1e+9
    ) %>%
  filter(anio != 2005)


p0 <- ggplot(dfStack, aes(x=anio, y=diffLev, fill=Grupo)) +
  geom_bar(stat='identity', position='dodge', width=.5) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 60),
    legend.position="bottom", 
    legend.title = element_blank(),
    legend.margin = margin(c(0,0,0,0)))

leyenda <- get_legend(p0)

p1 <- ggplot(dfStack, aes(x=anio, y=diffLev, fill=Grupo)) +
  geom_bar(stat='identity', position='dodge', width=.5) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60), 
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +  coord_cartesian(ylim = c(-1.1,0.33), expand = T)

p1a <- ggdraw(
  add_sub(p1, "Apalancamiento fijo \nen nivel incial")
)

p2 <- ggplot(dfStack, aes(x=anio, y=diffTasap, fill=Grupo)) +
  geom_bar(stat='identity', position='dodge', width=.5) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(y = 'Diferencia en ingresos (1000 millones)', x = 'A駉') +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(angle = 60)
    ) +  coord_cartesian(ylim = c(-1.1,0.33), expand = T)


p2a <- ggdraw(
  add_sub(p2, 'Tasa promedio de cr閐itos \nfija en nivel incial')
)

pp <-plot_grid(p1a, p2a, align = 'h')

pp1 <- plot_grid(pp, leyenda,
  ncol = 1,
  rel_heights = c(1,.1))



dfSum <- dfStack%>%
  group_by( Grupo)  %>%
  summarise(
    sumlev = sum(diffLev, na.rm = T), 
    sumTasap =sum(diffTasap, na.rm = T)
    )

ggplot(dfSum, aes(x=Grupo, y=sumlev) )+ 
  geom_bar(aes(color=Grupo))

ggplot(dfSum, aes(x=anio, y=sumTasap, fill =Grupo)) + 
  geom_line(aes(color=Grupo))




# ribbon salientes, entrantes y stayers----
require(tidyr)

dfEntrasalestay <- df %>%
  group_by(nit) %>%
  mutate(
    entra = row_number() == 1,
    sale = row_number() == n(),
    prevalece = anios == 9) %>%
  filter(!(anio %in% c(2005,2013)))%>%
  summarise(
    entra = max(entra), 
    sale = max(sale),
    prevalece = max(prevalece)
  ) %>%
  mutate(
    tipo = case_when(
      entra == 1 ~ 'Entrante',
      sale == 1 ~ 'Saliente',
      prevalece == 1 ~ 'Superviviente', 
      TRUE ~ NA_character_
    )
  )


df1 <- full_join(df, dfEntrasalestay, by= 'nit')



dfStack <- df1 %>%
  group_by(anio, tipo) %>%
  summarise(
    pn_bruta =sum(pn_bruta, na.rm =T)
  ) %>% na.omit() %>% ungroup()


row1 <- data.frame(anio=2005,tipo="Entrante", pn_bruta=0, stringsAsFactors = F)
row2 <- data.frame(anio=2013,tipo="Saliente", pn_bruta=0, stringsAsFactors = F)

dfStack <- rbind(dfStack, row1)
dfStack <- rbind(dfStack, row2)



ggplot(dfStack, aes(x=anio, y=pn_bruta/1e+9, fill=tipo)) + 
  geom_area(color='black', size=.2, alpha=.3) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9)) +
  labs(y='Valor total producci髇 (1000 millones)', x = 'A駉') +
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank() )




