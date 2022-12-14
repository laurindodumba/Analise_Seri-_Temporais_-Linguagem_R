############################################################
####    SELE??O DO MELHOR MODELO E AN?LISE DOS ERROS    ####
############################################################


# CARREGAR PACOTES
library(dplyr)

# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/Luciano/Desktop/Cursos_Udemy/series_temporais_R")

# ABRIR ARQUIVO
chuva_sp <- read.csv('chuva_tratado.csv', sep = ";")
View(chuva_sp)

#Excluir Linhas
chuva_sp2 <- chuva_sp[-36, ]
View(chuva_sp2) 
str(chuva_sp2)

chuva_sp3 <- as.vector(t(chuva_sp2))
print(chuva_sp3)



##### Cria??o da s?rie #####

serie <- ts(chuva_sp3, start = c(1985,1), end = c(2019,12), frequency=12)
print(serie)
plot(serie)



### DECOMPOSI??O
decomposicao <- decompose(serie)
plot(decomposicao)


### SUAVIZA??O
library("forecast")
serie2 <- tsclean(serie)

# Compara??o
plot(serie)
lines(serie2, col="red")


### NORMALIDADE
qqnorm(serie2)
qqline(serie2)


### TRANSFORMA??O
serie3 <- (serie2)^(1/3)
print(serie3)

hist(serie2)
hist(serie3)

qqnorm(serie3)
qqline(serie3)


#### ESTACIONARIDADE
library("urca")
# Teste pp (Philips-Perron)

# Ho = ? estacion?ria: p > 0.05
# Ha = n?o ? estacion?ria: p <= 0.05
estacionaridade <- ur.pp(serie3)
summary(estacionaridade)

ndiffs(serie3)


#### AUTOCORRELA??O
tsdisplay(serie3)

# Teste de Autocorrela??o (Ljung-Box)
# Ho = n?o ? autocorrelacionado: p > 0.05
# Ha = ? autocorrelacionado: p <= 0.05
Box.test(serie3, type = "Ljung-Box")




#### MODELO SARIMA: (p,d,q)(P,D,Q)

modelo_sarima1 <- arima(serie3, order = c(0,0,0), seasonal = c(2,1,0))
summary(modelo_sarima1)

modelo_sarima2 <- arima(serie3, order = c(0,0,0), seasonal = c(4,1,0))
summary(modelo_sarima2)

# RMSE e AIC melhor para o modelo_sarima2

# An?lise dos res?duos (qualidade do modelo)
checkresiduals(modelo_sarima1)
checkresiduals(modelo_sarima2)

# Os dois modelos foram aprovados na an?lise dos res?duos


plot(serie3)
lines(serie3-modelo_sarima1$resid, col= "red")
lines(serie3-modelo_sarima2$resid, col= "blue")





# PREVIS?ES

# MODELO 1
previsao1 <- forecast(modelo_sarima1,h=16)
plot(previsao1)
lines(serie3-modelo_sarima1$resid, col= "red")

print(previsao1)

prev_escala1 <- as.data.frame(previsao1)^3
View(prev_escala1)

prev_escala1 <- prev_escala1[ , 1]
print(prev_escala1)

chuva_real <- chuva_sp[36, ]
chuva_real <- as.vector(t(chuva_real))
print(chuva_real)

chuva_real <- c(chuva_real,373.3,174.1,137.8,55.7)
print(chuva_real)

# Raiz do erro quadr?tico m?dio 
rmse1 <- sqrt(mean((prev_escala1 - chuva_real) ** 2))
print(rmse1)


# MODELO 2
previsao2 <- forecast(modelo_sarima2,h=16)
plot(previsao2)
lines(serie3-modelo_sarima2$resid, col= "blue")

print(previsao2)

prev_escala2 <- as.data.frame(previsao2)^3
View(prev_escala2)

prev_escala2 <- prev_escala2[ , 1]
print(prev_escala2)

# Raiz do erro quadr?tico m?dio
rmse2 <- sqrt(mean((prev_escala2 - chuva_real) ** 2))
print(rmse2)
print(rmse1)

# Desvio padr?o do erro absoluto
mean(abs(prev_escala1 - chuva_real))
sd(abs(prev_escala1 - chuva_real))
mean(abs(prev_escala2 - chuva_real))
sd(abs(prev_escala2 - chuva_real))


# Pela RMSE o modelo 1 est? melhor que o modelo 2
