#Exercicio 2
install.packages("tidyverse")
install.packages("bbl")

library("tidyverse")
library(readxl)

TabelaFrequencias  <- read_excel("dados/exercicio5.xls")

TabelaFrequencias <- TabelaFrequencias %>% rename(marca=1,qtde_pessoas=2)

ggplot(data = TabelaFrequencias) +
  geom_col(aes(x = marca, y = qtde_pessoas)) +
  ggtitle(paste("Preferência de Antitérmicos")) +
  theme(plot.title = element_text(hjust = 0.5))
