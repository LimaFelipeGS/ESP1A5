#### IMPORTAÇÃO E FILTRAGEM DOS DADOS ####
#Dados importados
install.packages('bit64')
install.packages("ggplot2")
install.packages("dplyr")
library(data.table)
library(ggplot2)
library(dplyr)

testing <- fread("/cloud/project/Projeto/Data/covid-testing-all-observations.csv")
total_cases <- fread("/cloud/project/Projeto/Data/total_cases.csv")

#Dados a serem analisados
#Estatística descritiva
top_testing <- testing[testing$`ISO code` %in% c("IND", "IDN", "USA", "PAK") & testing$Date %in% c("2021-03-19", "2021-06-18", "2021-09-18", "2021-12-18", "2022-03-14")]

top1_cases <- data.frame(total_cases$date, total_cases$India, total_cases$Indonesia, total_cases$Pakistan, total_cases$`United States`)
top1_cases <- top1_cases[c(440, 532, 623, 714, 805), ]
colnames(top1_cases) <- c("date", "India", "Indonesia", "Pakistan", "United States")
top_cases <- data.frame(t(top1_cases[-1]))
colnames(top_cases) <- top1_cases[, 1]

#Probabilidade e Inferência
top_testing_inf <- testing[testing$`ISO code` %in% c("IND", "IDN", "USA", "PAK")]
top_cases_inf <- data.frame(total_cases$date, total_cases$India, total_cases$Indonesia, total_cases$Pakistan, total_cases$`United States`)
colnames(top_cases_inf) <- c("date", "India", "Indonesia", "Pakistan", "United States")
brazil_testing <- testing[testing$`ISO code` == "BRA"]
brazil_cases <- data.frame(total_cases$date ,total_cases$Brazil, total_cases$Brazil/212771.4)
colnames(brazil_cases) <- c("date", "total_cases", "total_cases_per_thousand")

#### ESTATÍSTICA DESCRITIVA ####
#Análise de novos testes diários por mil habitantes
summary(top_testing$`Daily change in cumulative total per thousand`)

hist(top_testing$`Daily change in cumulative total per thousand`, main = "Histograma de testes diários por mil habitantes", xlab = "Testes diários")
boxplot(top_testing$`Daily change in cumulative total per thousand`)

#Análise de casos - India e Pakistan
#Calculando cov diretamente
cov(top1_cases$India, top1_cases$Pakistan, method = "pearson")
cov(top1_cases$India, top1_cases$Pakistan, method = "spearman")

#Calculando o coeficiente de correlação diretamente
cor(top1_cases$India, top1_cases$Pakistan, method = "pearson")
cor(top1_cases$India, top1_cases$Pakistan, method = "spearman")

#Gráfico
top1_cases%>%
  ggplot(aes(x = top1_cases$India / 1000000, y = top1_cases$Pakistan / 1000000)) +
  geom_point(colour = "red") + xlab("India") + ylab("Pakistan")
geom_smooth(method = "lm", fill = NA)

#Análise de casos - Indonesia e USA
#Calculando cov diretamente
cov(top1_cases$Indonesia, top1_cases$`United States`, method = "pearson")
cov(top1_cases$Indonesia, top1_cases$`United States`, method = "spearman")

#Calculando o coeficiente de correlação diretamente
cor(top1_cases$Indonesia, top1_cases$`United States`, method = "pearson")
cor(top1_cases$Indonesia, top1_cases$`United States`, method = "spearman")

#Gráfico
top1_cases%>%
  ggplot(aes(x = top1_cases$Indonesia / 1000000, y = top1_cases$`United States` / 1000000)) +
  geom_point(colour = "red") + xlab("Indonesia") + ylab("United States")
geom_smooth(method = "lm", fill = NA)


  #### PROBABILIDADE ####
#Probabilidade de um brasileiro ter sido testado P(A) = 0.329
#Probabilidade de um brasileiro ter tido um caso de Covid-19 P(B) = 0.134
#Probabilidade de um brasileiro que foi testado testado ter um caso de Covid-19 P(B|A) = 0.0728
bayes_theorem <- function(p_a, p_b, p_b_given_a) {
  p_a_given_b = (p_b_given_a * p_a) / p_b
  return(p_a_given_b)
}
p_a = 0.329
p_b = 0.134
p_b_given_a = 0.0728
bayes_theorem(p_a, p_b, p_b_given_a) 
#Probabilidade de um brasileiro que teve um caso de Covid-19 ter sido testado P(A|B) = 0.178

#Distribuição binomial para descobrir a probabilidade de todos os casos de Covid-19
#no Brasil terem sido testados
#X = quantidade de casos; size = quantidade de testes; prob = probabilidade de que um teste seja positivo
dbinom(x = 136, size = 331, prob = .0728)
#Probabilidade obtida = 6.9567e-66

#Gráfico da probabilidade para todos os valores
testes_positivos <- dbinom(x = 0:331, 331, 0.0728)
df_testes <- data.frame(x = 0:331, testes_positivos)
ggplot(df_testes, aes(x = 0:331, y = testes_positivos)) + geom_col() + ylim(0,0.1) + xlim(0,50)

#Maior valor da probabilidade
dbinom(x = 24, size = 331, prob = .0728)

#Distribuição Poisson
#Média de novos casos por mês
testes_mes <- (brazil_testing$`Cumulative total per thousand`[661] - brazil_testing$`Cumulative total per thousand`[20]) / 21
casos_mes <- brazil_cases$total_cases_per_thousand[1579] / 50

#Probabilidade de receber o dobro de testes e casos
dpois(x = round(testes_mes * 2, digits = 0), testes_mes)
dpois(x = round(casos_mes * 2, digits = 0), lambda = casos_mes)

#Probabilidade de receber a metade de testes e casos
dpois(x = round(testes_mes * 0.5, digits = 0), lambda = testes_mes)
dpois(x = round(casos_mes * 0.5, digits = 0), lambda = casos_mes)

#Probabilidade de receber nenhum novo teste e caso
dpois(x = 0, lambda = testes_mes)
dpois(x = 0, lambda = casos_mes)

#Probabilidade do período de jan/2022
testes_jan22 <- brazil_testing$`Cumulative total per thousand`[647] - brazil_testing$`Cumulative total per thousand`[615]
casos_jan22 <- brazil_cases$total_cases_per_thousand[758] - brazil_cases$total_cases_per_thousand[728]

dpois(x = round(testes_jan22, digits = 0), lambda = testes_mes)
dpois(x = round(casos_jan22, digits = 0), lambda = casos_mes)

#Probabilidade de 2021
testes_2021 <- brazil_testing$`Cumulative total per thousand`[615] - brazil_testing$`Cumulative total per thousand`[251]
casos_2021 <- brazil_cases$total_cases_per_thousand[727] - brazil_cases$total_cases_per_thousand[363]

dpois(x = round(testes_2021, digits = 0), lambda = testes_mes * 12)
dpois(x = round(casos_2021, digits = 0), lambda = casos_mes * 12)

#Gráfico
testes_realizados <- dpois(x = 0:round(testes_mes * 2), testes_mes)
df_testes <- data.frame(x = 0:round(testes_mes * 2), testes_realizados)
ggplot(df_testes, aes(x = 0:round(testes_mes * 2), y = testes_realizados)) + geom_col() + ylim(0,0.08)

casos_registrados <- dpois(x = 0:round(casos_mes * 2), casos_mes)
df_casos <- data.frame(x = 0:round(casos_mes * 2), casos_registrados)
ggplot(df_casos, aes(x = 0:round(casos_mes * 2), y = casos_registrados)) + geom_col()

#Distribuição Normal
sd(brazil_testing$`Daily change in cumulative total per thousand`, na.rm=TRUE)
summary(brazil_testing$`Daily change in cumulative total per thousand`)

#Probabilidade de ficar abaixo da mediana
pnorm(q=0.27, mean = 0.65, sd = 1.815, lower.tail = TRUE)

#Probabilidade de ficar acima da mediana
pnorm(q=0.27, mean = 0.65, sd = 1.815, lower.tail = FALSE)

#Probabilidade de ficar entre os quartis
- (pnorm(q=0.14, mean = 0.65, sd = 1.815, lower.tail = TRUE) - pnorm(q=0.45, mean = 0.65, sd = 1.815, lower.tail = TRUE))

#Probabilidade de ser um outlier
pnorm(q=0.45+(1.5*0.31), mean = 0.65, sd = 1.815, lower.tail = FALSE)

#Probabilidade de ser um outlier extremo
pnorm(q=0.45+(3*0.31), mean = 0.65, sd = 1.815, lower.tail = FALSE)

#Gráfico
novos_testes <- seq(from=0, to=15, by=0.5)
prob <- dnorm(novos_testes, mean=0.65, sd=1.815)
plot(novos_testes, prob, type = "l",
     main = "Distribuição normal para X: Média=0.65, sd=1.815)",
     xlab = "x", ylab = "FDP",las=1) +
  abline(v=120)

acumulada <- pnorm(novos_testes, mean=0.65, sd=1.815)
plot(novos_testes, acumulada, type = "l",
     main = "Distribuição normal para X: Média=0.65, sd=1.815)",
     xlab = "x", ylab = "FDA",las=1) +
  abline(v=120)

#### INFERÊNCIA ####
#Intervalo de confiança para novos testes diários por mil habitantes
z_star_95 <- qnorm(0.975)

#Quantidade de amostras de novos testes Brazil = 685 - NAs(420) = 265
menor_brazil = 0.65 - z_star_95 * (1.815 / sqrt(265))
maior_brazil = 0.65 + z_star_95 * (1.815 / sqrt(265))

#Quantidade de amostras de novos testes dos quatro países = 2878 - NAs(271) = 2607
summary(top_testing_inf$`Daily change in cumulative total per thousand`)
sd(top_testing_inf$`Daily change in cumulative total per thousand`, na.rm = TRUE)
menor_top = 1.375 - z_star_95 * (1.729 / sqrt(2878))
maior_top = 1.375 + z_star_95 * (1.729 / sqrt(2878))


#Testes de Hipótese
#Brazil
d_0 = 0
alpha = 0.05

brazil_cases_recorte <- data.frame(brazil_cases$date[113:797], brazil_cases$total_cases_per_thousand[113:797])
colnames(brazil_cases_recorte) <- c("date", "total_cases_per_thousand")

shapiro.test(brazil_testing$`Daily change in cumulative total per thousand`)
shapiro.test(top_testing_inf$`Daily change in cumulative total per thousand`)
#Ambas as amostras são normais

#Teste de Variância
alpha = 0.05
var.test(x = brazil_testing$`Daily change in cumulative total per thousand`, 
         y = top_testing_inf$`Daily change in cumulative total per thousand`, 
         ratio = 1, 
         alternative = "two.sided", 
         conf.level = .95)
#H0 falso para a variância

#Comparando as médias
t.test(brazil_testing$`Daily change in cumulative total per thousand`,top_testing_inf$`Daily change in cumulative total per thousand`, alternative="two.sided", paired=FALSE, conf.level = 0.95)
#Não há diferença estatisticamente significante entre as médias de testes do Brasil e dos quatro países







