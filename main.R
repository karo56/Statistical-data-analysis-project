library(ggplot2)
library(dplyr)
library(ggpubr)
library(Rmisc)
library(GGally)
library(hrbrthemes)
library(viridis)
library(data.table)
library(MASS)
library(ggpubr)
library(regclass)

url <- "https://www.mimuw.edu.pl/~szczurek/SAD1/ZadanieZaliczeniowe/people.tab"

ggpairs(df, columns=c(1,2,3,6))
df <- read.table(url, sep = '\t',header = TRUE)
summary(df)


VIF = round(cor(df[,c(1,2,3)]),2)

pairs(~age+weight+height+number_of_kids,data = df,
      main = "Scatterplot Matrix", color="#00AFBB")


ggplot(df, aes(sample = age)) + stat_qq()+ stat_qq_line()
ggplot(df, aes(sample = height)) + stat_qq()+ stat_qq_line()


wiek = df[[1]]
wzrost = df[[3]]




przedzial_wiek = CI(wiek, ci=0.95)
przedzial_wzrost = CI(wzrost, ci=0.95)

ks.test(wiek, "pnorm", mean=mean(wiek), sd=sd(wiek))
ggplot(df, aes(sample = expenses)) + stat_qq()+ stat_qq_line()

