library(factoextra)
#arquivos <-list.fileslibrary(pixmap)

# ------------- Entendendo o dataset ---------- #

digito0 <- read.csv("C:\\Users\\pasto\\Documents\\Engenharia de Software\\Graduação PUCC\\6o sem\\Tópicos Avançados em BD\\Relatórios\\Relatório4\\DigitosCompleto\\0_001.BMP.inv.pgm", header=FALSE, sep=" ")
#plot(digito0)

# remove a coluna 18, uma vez que só tem NAs
dig0 <- subset(digito0, select = -c(18))

# mostra só as primeiras linhas das colunas (vendo que a coluna 18 foi realmente removida)
head(dig0)

# criando um vetor com os valores 1, 2 e 3 para remoção das 3 primeiras linhas
linhas <- c(1,2,3)

# removendo as 3 primeiras linhas do dataframe, pois elas só tem NAs
dig0_novo <- dig0[-linhas, ]


# ------- Importação dos arquivos, tratamento dos dados e dimensionalidade ---------- #

arquivos <- list.files(path="C:\\Users\\pasto\\Documents\\Engenharia de Software\\Graduação PUCC\\6o sem\\Tópicos Avançados em BD\\Relatórios\\Relatório4\\DigitosCompleto\\")

# criando um vetor com os valores 1, 2 e 3 para remoção das 3 primeiras linhas
linhasRemover <- c(1, 2, 3) 

data <- NULL

for(i in 1:1999){
  numero<-read.csv(paste0("C:\\Users\\pasto\\Documents\\Engenharia de Software\\Graduação PUCC\\6o sem\\Tópicos Avançados em BD\\Relatórios\\Relatório4\\DigitosCompleto\\", arquivos[i]), sep = "", header = FALSE)
  numero<-numero[-linhasRemover,]
  vetor<-as.vector(t(numero))
  data<-rbind(data, vetor)
}

#adicionando a coluna de classe
for(i in 1:200){
  data[i,4097]<-0
}
for(i in 201:400){
  data[i,4097]<-1
}
for(i in 401:600){
  data[i,4097]<-2
}
for(i in 601:800){
  data[i,4097]<-3
}
for(i in 801:1000){
  data[i,4097]<-4
}
for(i in 1001:1200){
  data[i,4097]<-5
}
for(i in 1201:1400){
  data[i,4097]<-6
}
for(i in 1401:1600){
  data[i,4097]<-7
}
for(i in 1601:1800){
  data[i,4097]<-8
}
for(i in 1801:2000){
  data[i,4097]<-9
}

#garantindo que o dataset seja inteiramente numérico
data <- mapply(data, FUN=as.numeric)
data <- matrix(data=data, ncol=4097, nrow=2000)

indToRemove<- NULL

#removendo colunas de variância 0 para o funcionamento do pca
for(i in 1:4097){
  vetor <- data[,i]
  if(all(vetor == 0) || all(vetor == 1)){
    indToRemove <- append(indToRemove, i)
  }
}

data2 <- data[,-indToRemove]

#aplicando o pca
data_pca <- prcomp(data2, center = TRUE, scale. = TRUE)
summary(data_pca)

fviz_eig(data_pca)
fviz_pca_ind(data_pca,col.ind = "black", # Color by the quality of representation
             repel=TRUE # Avoid text overlapping
)

fviz_pca_var(data_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
