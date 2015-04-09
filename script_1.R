library("TTR")
library("class")
library("dplyr")
library(plyr)
library(gmodels)

set.seed(123456)

ttrc <- read.csv2("eter3.csv", stringsAsFactors = FALSE,sep = ",")
ttrc$Open <- as.numeric(ttrc$Open)
ttrc$High <- as.numeric(ttrc$High)
ttrc$Low <- as.numeric(ttrc$Low)
ttrc$Close <- as.numeric(ttrc$Close)
ttrc$Volume <- as.numeric(ttrc$Volume)
ttrc$Adj.Close <- as.numeric(ttrc$Adj.Close)

dados_MACD <- as.data.frame(MACD(ttrc$Close,12,26,9,maType = "EMA"))
dados_Aroon <- as.data.frame(aroon(HL = matrix(cbind(ttrc$Low, ttrc$High), ncol = 2), n = 10))
dados_ATR <- as.data.frame(ATR(HL = matrix(cbind(ttrc$Low, ttrc$High, ttrc$Close), ncol = 3), n = 10))
dados_CLV <- as.data.frame(CLV(matrix(cbind(ttrc$Low, ttrc$High, ttrc$Close), ncol = 3)))
dados_CMF <- as.data.frame(CMF(matrix(cbind(ttrc$Low, ttrc$High, ttrc$Close), ncol = 3), ttrc$Volume, 10))
dados_DVI <- as.data.frame(DVI(ttrc$Close, n = 10))

calcularSinalCompra <- function(dados, indice, n = 10, dif = 0.05){
  valorAlvo <- dados[indice] * (1+dif)
  if(length(dados) < indice + n){
    return(NA)
  }else{
    for(i in (indice + 1):(indice+n)){
      if(dados[i] >= valorAlvo){
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
}

dados_Sinal_compra <- c()

for(i in 1:length(ttrc$Close)){
  dados_Sinal_compra <- rbind(dados_Sinal_compra,calcularSinalCompra(ttrc$Close, i))
}

dados_Sinal_compra <- as.data.frame(dados_Sinal_compra)

dados <- tbl_df(cbind(dados_MACD, dados_Aroon,dados_ATR,dados_CLV,dados_CMF,dados_DVI,dados_Sinal_compra))
dados_uncomplete <- dados
dados <- dados[complete.cases(dados),]
names(dados) <- c("macd","sinal","aroonUp","aroonDn","oscilator","tr","atr","trueHigh","trueLow","CLV",
                  "CMF","dvi.mag","dvi.str","dvi","PRED")

normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

dados_n <- lapply(dados[,1:14], normalize)
dados_n <- cbind(dados_n, dados[,15])
dados_n$PRED <- as.factor(dados_n$PRED)

dados_uncomplete_n <- lapply(dados[,1:14], normalize)
dados_uncomplete_n <- cbind(dados_n, dados[,15])
dados_uncomplete_n$PRED <- as.factor(dados_n$PRED)

indicador_treino_aleatorio <- rbinom(length(dados_n$dvi), size = 1, prob = 0.82)

classificador_treino <- dados_n[indicador_treino_aleatorio == 1,15]
classificador_teste <- dados_n[indicador_treino_aleatorio == 0,15]

data_treino <- dados_n[indicador_treino_aleatorio == 1,-15]
data_teste <- dados_n[indicador_treino_aleatorio == 0,-15]


data_predict <- knn(train = data_treino, test = data_teste, cl = classificador_treino, k = sqrt(length(classificador_treino)))

CrossTable(classificador_teste, data_predict, prop.chisq = FALSE)


predict(data_predict, dado_predicao)