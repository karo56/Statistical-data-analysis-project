



ggplot(df, aes(x=gender, y=height, fill=gender)) + 
  geom_boxplot(alpha=1) +
  theme(legend.position="none")


ggplot(df, aes(x=age,fill=gender, color=gender)) +
  geom_histogram(alpha=0.5, position="identity")


ggplot(df, aes(sample = age)) + stat_qq()+ stat_qq_line()
ggplot(df, aes(sample = height)) + stat_qq()+ stat_qq_line()

freq = as.data.frame(table(unlist(df[[1]])))

ggplot(freq, aes(x=Var1, y=Freq,group=1)) +
  geom_line(col = "grey")+
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) + 
  ylab("count")+
  xlab("wiek")


ggparcoord(df,
           columns = c(1,2,3,6,8), groupColumn = 4, order = "anyClass",
           showPoints = TRUE, 
           title = "Wykres dla plci",
           alphaLines = 0.1) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )

pet_freq = as.data.frame(table(unlist(df[[7]])))

names <- c("pet", "count")
colnames(pet_freq) <- names

pet_freq$fraction = pet_freq$count / sum(pet_freq$count)
pet_freq$ymax = cumsum(pet_freq$fraction)
pet_freq$ymin = c(0, head(pet_freq$ymax, n=-1))

ggplot(pet_freq, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=pet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4))



ggplot(df, aes(x=gender, y=expenses, fill=gender)) + 
  geom_violin()

                              