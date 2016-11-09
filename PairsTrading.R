library(dplyr)
library(lubridate)

papel1 <- read.csv(file.choose(),sep=",")
papel2 <- read.csv(file.choose(),sep=",") 

papel1 <- papel1 %>% filter(Volume > 0) %>% arrange(Date) %>% select(c(Date, Adj.Close))
papel1_treino <- papel1 %>% filter(year(Date) >= 2012 & year(Date) <= 2015)
papel1_test <- papel1 %>% filter(year(Date) == 2016)

papel2 <- papel2 %>% filter(Volume > 0) %>% arrange(Date) %>% select(c(Date, Adj.Close))
papel2_treino <- papel2 %>% filter(year(Date) >= 2012 & year(Date) <= 2015)
papel2_test <- papel2 %>% filter(year(Date) == 2016)

if(nrow(papel1_treino) != nrow(papel2_treino)){
  
  for(i in 1:min(nrow(papel1_treino, papel2_treino))){
    
  }
  
} else {
  
}

