library(dplyr)
library(lubridate)

# CARREGANDO OS DADOS
papel1 <- read.csv(file.choose(),sep=",")
papel2 <- read.csv(file.choose(),sep=",") 

# PREPARANDO OS DADOS
papel1 <- papel1 %>% arrange(Date) %>% select(c(Date, Volume, Adj.Close))
papel2 <- papel2 %>% arrange(Date) %>% select(c(Date, Volume, Adj.Close))
pairs <- merge(papel1,papel2,by = "Date")
colnames(pairs) <- c("Date", "VolumeP1","Price1","VolumeP2","Price2")

pairs <- pairs %>% filter(VolumeP1 > 0 | VolumeP2 > 0)

pairs <- pairs %>% mutate(VALEON = log(Price1)) %>%
  mutate(VALEPN = log(Price2)) %>%
  mutate(Date=as.Date(Date))

pairsT <- pairs %>% select(-c(VolumeP1,VolumeP2,Price1,Price2))

pairs_train <- pairsT %>% filter(year(Date) >= 2012 & year(Date) <= 2015)
pairs_test <- pairsT %>% filter(year(Date) == 2016 & month(Date) <= 9)

rm(list = c("papel1" , "papel2"))

# GRAFICO DOS PARES
library(ggplot2)
library(reshape2)

logPrices <- ggplot(data = melt(pairsT,id.var="Date")) +
  geom_line(aes(x=Date,y=value,colour=variable)) +
  scale_colour_manual(values = c("blue", "red")) +
  ylab("Log price") +
  ggtitle("Log prices") +
  theme(legend.position = "bottom")

# TESTANDO RAIZES UNITARIAS DE CADA SERIE COM O TESTE ERS-DF_GLS
library(urca)
library(vars)

unitRootP1 <- ur.ers(pairs_train$VALEON,type="DF-GLS",model="const",lag.max=5)
summary(unitRootP1)
unitRootP2 <- ur.ers(pairs_train$VALEPN,type="DF-GLS",model="const",lag.max=5)
summary(unitRootP2)

# REGREDINDO UMA SERIE NA OUTRA PARA OBTER RESIDUO
m1 <- lm(VALEON ~ VALEPN, data = pairs_train)
wt <- m1$residuals
wtWithDate <- as.data.frame(cbind(seq(1,length(wt),1),wt))
colnames(wtWithDate) <- c("Date","wt")
wtPlot <- ggplot(wtWithDate) +
  geom_line(aes(x=Date,y=wt)) +
  ylab("Spread") +
  xlab("")

# VERIFICANDO ESTACIONARIEDADE DO RESIDUO
unitRootWt <- ur.df(wt,type=c('none'), lags=1, selectlags = c('BIC'))
summary(unitRootWt)

# DEFININDO O NUMERO DE DEFASAGENS
par <- pairs_train[,c('VALEON','VALEPN')]
K <- VARselect(par,type = 'trend')
K$selection["SC(n)"]

# RODANDO O TESTE DO TRACO DE JOHANSEN
traco <- ca.jo(par, type = "trace", ecdet = "const", K = 2, spec = "transitory")
summary(traco)
eigen <- ca.jo(par, type = "eigen", ecdet = "const", K = 2, spec = "transitory")
summary(eigen)

