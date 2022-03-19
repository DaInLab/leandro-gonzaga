#Exercicio 2
#install.packages("tidyverse")
#install.packages("ggQC")

library("ggQC")
library("tidyverse")
library(readxl)

TabelaFrequencias  <- read_excel("dados/exercicio7.xls")

TabelaFrequencias <- TabelaFrequencias %>% rename(areas=1,atendimento=2)

ggplot(data = TabelaFrequencias) +
  geom_col(aes(y = atendimento, x = areas)) +
  ggtitle(paste("Frequência de Atendimento")) +
  theme(plot.title = element_text(hjust = 0.5))
