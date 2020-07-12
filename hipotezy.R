
wzrost_kobiet = df[df$gender == "woman",3]
wzrost_men = df[df$gender == "man",3]

srednia_kobiet = mean(wzrost_kobiet)
srednia_men = mean(wzrost_men)


wiek = df[[3]]

tbl = table(df$pet,df$married)

x = fisher.test(tbl)


ggscatter(df, x = "weight", y = "height", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


t.test(x, y, alternative = "two.sided", var.equal = FALSE)

ggplot(df, aes(age)) +
  geom_histogram()
