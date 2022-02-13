#Exercicio 2
install.packages("tidyverse")
install.packages("ggQC")

library("ggQC")
library("tidyverse")
library(readxl)

TabelaFrequencias  <- read_excel("dados/exercicio6.xls")

TabelaFrequencias <- TabelaFrequencias %>% rename(qualidade=1,qtde_pessoas=2)

ggplot(data = TabelaFrequencias, aes(x = qualidade, y = qtde_pessoas)) +
  stat_pareto(point.color = "red",
              point.size = 3,
              line.color = "black",
              bars.fill = c("blue","orange")) +
  ggtitle(paste("Pesquisa de Satisfação")) +
  theme(plot.title = element_text(hjust = 0.5))
