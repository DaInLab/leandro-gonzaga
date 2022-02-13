#Exercicio 2
install.packages("tidyverse")
install.packages("ggQC")
install.packages("bbl")

library("ggQC")
library("tidyverse")
library("bbl")
library(readxl)

TabelaFrequencias  <- read_excel("dados/exercicio8.xls")
TabelaFrequencias <- as.data.frame(table(TabelaFrequencias))
TabelaFrequencias <- TabelaFrequencias %>% rename(altura=1,qtde_pacientes=2)
TabelaFrequencias <- TabelaFrequencias  %>% mutate(TabelaFrequencias,
                                               observacoes = c(1:n()))


d <- freq2raw(data=TabelaFrequencias, freq= TabelaFrequencias$qtde_pacientes)


ggplot(data = d) +
  geom_histogram(aes(x = altura), stat="count") +
  ggtitle(paste("Histograma da altura dos pacientes")) +
  theme(plot.title = element_text(hjust = 0.5))
