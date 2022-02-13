#Exercicio 1
install.packages("tidyverse")

library("tidyverse")

library(readxl)



dataset_inicial  <- read_excel("dados/exercicio1.xls")

dataset_inicial  <- dataset_inicial %>% rename( taxa = "Taxas de juros")

descritivas_dataset <- summarise(dataset_inicial,
                                   media=mean(taxa),
                                   mediana=median(taxa),
                                   variancia= var(taxa),
                                   desv_pad=sd(taxa),
                                   minimo=min(taxa),
                                   maximo=max(taxa),
                                   quartil_1=quantile(taxa, type=5, 0.25),
                                   quartil_3=quantile(taxa, type=5, 0.75))

dataset_inicial %>% ggplot( geom_bar(aes(x=taxa)))
                            #geom_line(aes(x= descritivas_dataset$média))))

dataset_inicial <- dataset_inicial  %>% mutate(dataset_inicial,
                      observacoes = c(1:10))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = taxa, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$media), colour="red")+
  ggtitle(paste("Média: ", descritivas_dataset$media)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = taxa, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$mediana), colour="green")+
  ggtitle(paste("Mediana: ", descritivas_dataset$mediana)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = taxa, x = observacoes)) +
  geom_errorbar(aes(x = observacoes, ymin = dataset_inicial$taxa - descritivas_dataset$desv_pad, ymax = dataset_inicial$taxa ), width = .2, colour="red")+
  ggtitle(paste("Desvio Padrão: ", descritivas_dataset$desv_pad)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = taxa, x = observacoes)) +
  geom_errorbar(aes(x = observacoes, ymin = dataset_inicial$taxa - descritivas_dataset$variancia, ymax = dataset_inicial$taxa ), width = .2, colour="gold1")+
  ggtitle(paste("Variância: ", descritivas_dataset$variancia)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = taxa, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$minimo), colour="brown")+
  ggtitle(paste("Mínimo: ", descritivas_dataset$minimo)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = taxa, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$maximo), colour="purple")+
  ggtitle(paste("Máximo: ", descritivas_dataset$maximo)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = taxa, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$quartil_1), colour="yellow")+
  ggtitle(paste("Quartil Q1: ", descritivas_dataset$quartil_1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dataset_inicial) +
  geom_col(aes(y = taxa, x = observacoes)) +
  geom_line(aes(x = observacoes, y = descritivas_dataset$quartil_3), colour="orange")+
  ggtitle(paste("Quartil Q3: ", descritivas_dataset$quartil_3)) +
  theme(plot.title = element_text(hjust = 0.5))

