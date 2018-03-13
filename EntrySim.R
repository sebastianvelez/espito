
library(dplyr)

# reads raw data
df <- read.csv(file = "~/ESPE/data/jvelez.csv")


#sweet factors where needed
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



# non-parametric model of "entry"



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

      
      




# getting the estimates from the revenue regressions
estimates <- df[1,20:42]

set.seed(85719)




















dfTemp <- data.frame(
  expand.grid(unique(df$anio), unique(df$nit)
    )
)

colnames(dfTemp) <- c('anio', 'nit')


