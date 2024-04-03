library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(stringr)
library(varhandle)
library(rmarkdown)
library(FSA)
library(VIM)
library(MASS)
library(DBI)
library(RPostgres)

#install.packages("remotes")
#remotes::install_github("droglenc/FSAmisc")
#library(FSAmisc)

dbicon <-  DBI::dbConnect(RPostgres::Postgres(),
                          db = Sys.getenv('POSTGRES_DB'), 
                          host = Sys.getenv('POSTGRES_HOST'), 
                          port = Sys.getenv('POSTGRES_PORT'), 
                          user = Sys.getenv('POSTGRES_USER'),
                          password = Sys.getenv('POSTGRES_PASSWORD'))

sql <- 'SELECT * FROM "tblCruzeiro"'
df <- DBI::dbGetQuery(dbicon, sql)

DBI::dbDisconnect(dbicon)


summary(df[c('Total_length','Furcal_length','Peso_total')])

#Existem valores até 0 tanto para o peso quanto para o tamanho total.   
#Seria melhor filtrar esses valores. Parece que temos 35 NAs no Peso_total.  
#Por outro lado, também devemos filtrar a especie GOR que será nosso objetivo de estudio.

df <- df %>% 
        filter(Total_length>0 & Peso_total>0 & Especie=='GOR' & !is.na(Peso_total))

ggplot(df, aes(x=Total_length, y=Peso_total)) + 
  geom_point() +
  stat_smooth(method='lm', formula = y~poly(x,2))

ggsave("figures/Regresion_poly.png")

#A relação entre o tamanho e o peso dos peixes amostrados parece não ser linear.
#Isso se explica pelo fato de que o tamanho é uma medida linear e, no entanto, o 
#peso é uma medida que depende do volume.
#
#A relação tamanho-peso violará as suposições de linearidade e homocedasticidade 
#da regressão linear, pois peixes menores apresentarão menor variabilidade de peso 
#do que o peso de peixes maiores.
#
#Poderemos ajustar os dados através de uma regressão exponencial, fazendo previamente 
#uma transformação logarítmica sobre eles para poder linearizar o modelo.
#Isso também nos ajudará a estabilizar as variâncias.


#Nós nos adaptamos ao modelo:
df$logL <- log(df$Total_length)
df$logW <- log(df$Peso_total)

df <- df %>%
        filter(logW >= -0.5)

lm1 <- lm(logW~logL,data=df)
summary(lm1)

ggplot(df, aes(x=logL, y=logW)) + 
  geom_point()+
  stat_smooth(method='lm', formula = y ~ x)

ggsave("figures/Regresion_log.png")


# Outliers detection
cooksD <- cooks.distance(lm1)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
names_of_influential <- names(influential)
outliers <- df[names_of_influential,]
df_without_outliers <- df %>% anti_join(outliers)
lm2 <- lm(logW ~ logL, data = df_without_outliers)
summary(lm2)
#R squared=0.9568
#Se o tamanho aumentar em 1%, o peso aumentará em 2,68%

ggplot(df_without_outliers, aes(x=logL, y=logW)) + 
  geom_point()+
  stat_smooth(method='lm', formula = y ~ x)

ggsave("figures/Regresion_log_outliers.png")


#Inference
#FSAmisc package
#hoCoef(lm1,2,3)
#confint(lm1)

#Base package
std <- sqrt(diag(vcov(lm2)))[2]
coef <- coef(lm2)[2]
Tscore <- (coef-3)/std
n <- dim(df)[1]
p_value <- 2*pt(q=T, df=n-1, lower.tail=TRUE)

data <- c(3,coef,std,Tscore,p_value)
cols = c('H0','Estimate','Std. error','T-Score','p-value')

data.frame(matrix(data,1,5, dimnames=list(c(), cols)))

#Goraz exhibits allometric growth

#Previsões do valor médio da variável de resposta dado um valor da variável 
#explicativa podem ser feitas com predict().
#O peso médio previsto do toro para todos os 55 cm:

pred_log <- predict(lm2,data.frame(logL=log(55)),interval="c")
bias_pred_orig <- exp(pred_log)
# este é o fator de correção de viés
syx <- summary(lm2)$sigma
cf <- exp((syx^2)/2)

( pred_orig <- cf*bias_pred_orig )

#Aproximadamente 3 kg de peso para um peixe Goraz de 55 cm


#Um dos aspectos desta pesquisa foi determinar se os parâmetros da relação 
#peso-comprimento mudaram significativamente ao longo dos anos.

df$fAno <- factor(df$Ano)

lm3 <- lm(logW~logL*fAno,data=df)
anova(lm3)

#Há bastante evidências para concluir que há diferença nas inclinações na 
#relação comprimento-peso entre os anos.

