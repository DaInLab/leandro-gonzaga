#Exercicio 2
install.packages("tidyverse")
install.packages("bbl")

library("tidyverse")
library("bbl")
library(readxl)

calcularmoda <- function(v){
  observacao <- unique(v)
  observacao[which.max(tabulate(match(v,observacao)))]
}

TabelaFrequencias  <- read_excel("dados/exercicio3.xls")

dataset_inicial <- freq2raw(data=TabelaFrequencias[,"Num_Filhos"], freq= TabelaFrequencias$Qtde_Familias)



# Calculo das Medidas de Posição e Dispersão
descritivas_dataset <- summarise(dataset_inicial,
                                 observacoes = n(),
                                 mediana=median(Num_Filhos),
                                 moda=calcularmoda(Num_Filhos))

dataset_inicial <- dataset_inicial  %>% mutate(dataset_inicial,
                                               observacoes = c(1:descritivas_dataset$observacoes))

#Plotagem dos gráficos

ggplot(data = TabelaFrequencias) +
  geom_col(aes(x = Num_Filhos, y = Qtde_Familias)) +
  geom_line(aes(y = Qtde_Familias, x = descritivas_dataset$mediana), colour="green")+
  ggtitle(paste("Mediana: ", descritivas_dataset$mediana, " filhos por familia")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = TabelaFrequencias) +
  geom_col(aes(x = Num_Filhos, y = Qtde_Familias)) +
  geom_line(aes(y = Qtde_Familias, x = descritivas_dataset$moda), colour="blue")+
  ggtitle(paste("Moda: ", descritivas_dataset$moda, " filhos por familia")) +
  theme(plot.title = element_text(hjust = 0.5))
