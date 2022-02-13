#Exercicio 2
install.packages("tidyverse")
install.packages("bpgmm")

library("tidyverse")
library("bpgmm")
library(readxl)

calcularmoda <- function(v){
    observacao <- unique(v)
    observacao[which.max(tabulate(match(v,observacao)))]
}


dataset_inicial  <- read_excel("dados/exercicio2.xls")

# Criação da tabela de Frequencias
tabela_de_frequencias <- as.data.frame(table(dataset_inicial))

tabela_de_frequencias <- tabela_de_frequencias %>% rename(Qtde_casas=1,Numero_de_Quarteiroes=2)


# Calculo das Medidas de Posição e Dispersão
descritivas_dataset <- summarise(dataset_inicial,
                                 observacoes = n(),
                                 media=mean(Casas),
                                 mediana=median(Casas),
                                 moda= calcularmoda(Casas),
                                 variancia= var(Casas),
                                 desv_pad=sd(Casas))

dataset_inicial <- dataset_inicial  %>% mutate(dataset_inicial,
                                               observacoes = c(1:descritivas_dataset$observacoes))

#Plotagem dos gráficos

ggplot(data = dataset_inicial) +
  geom_col(aes(y = Casas, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$media), colour="red")+
  ggtitle(paste("Media: ", descritivas_dataset$media)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = Casas, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$mediana), colour="green")+
  ggtitle(paste("Mediana: ", descritivas_dataset$mediana)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = Casas, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$moda), colour="blue")+
  ggtitle(paste("Moda: ", descritivas_dataset$moda)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = Casas, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$desv_pad), colour="pink")+
  ggtitle(paste("Desvio Padrão: ", descritivas_dataset$desv_pad)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = Casas, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$variancia), colour="black")+
  ggtitle(paste("Variância: ", descritivas_dataset$variancia)) +
  theme(plot.title = element_text(hjust = 0.5))


