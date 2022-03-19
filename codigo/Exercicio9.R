#Exercicio 2
#install.packages("tidyverse")
#install.packages("ggQC")
#install.packages("bbl")

library("ggQC")
library("tidyverse")
library("bbl")
library(readxl)

TabelaFrequencias  <- read_excel("dados/exercicio9.xls")
TabelaFrequencias[,'Faixa Salarial'] <- NA

TabelaFrequencias$`Faixa Salarial`[TabelaFrequencias$Salários < 10] <- 'baixo'
TabelaFrequencias$`Faixa Salarial`[TabelaFrequencias$Salários > 10 ] <- 'medio'
TabelaFrequencias$`Faixa Salarial`[TabelaFrequencias$Salários > 15] <- 'alto'


d <- freq2raw(data=TabelaFrequencias, freq= TabelaFrequencias$qtde_func)


ggplot(data = TabelaFrequencias) +
geom_histogram(aes(x = `Faixa Salarial`), stat="count") +
  ggtitle(paste("Histograma da Frequência de salários")) +
  theme(plot.title = element_text(hjust = 0.5))
