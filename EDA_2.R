library(gridExtra)
library(ggplot2)
library(dplyr)
library(pdp) #pima dataset
library(corrplot)
library(palmerpenguins)
library(plotly)
library(car)
library(dplyr)
library(GGally)
library(MASS)

# Grouped
barplot_grouped <- ggplot(diamonds, aes(color, fill=cut)) + geom_bar(position="dodge")
# Stacked
barplot_stacked <- ggplot(diamonds, aes(color, fill=cut)) + geom_bar(position="stack")
grid.arrange(barplot_grouped, barplot_stacked, ncol=2)

# Por paneles
ggplot(diamonds, aes(color)) + geom_bar() +
  facet_wrap(~ cut)




p <- pima %>%
  ggplot( aes(x=glucose, fill=diabetes)) +
  geom_histogram( color="#e9ecef", alpha=0.4, position = 'identity') +
  scale_fill_manual(values=c("black", "yellow")) +
  theme_bw() +
  labs(fill="")
p
ggplotly(p)






a <-ggplot(penguins, aes(x = body_mass_g, y = bill_depth_mm)) +
  geom_point()
a
ggplotly(a)


ggplot(penguins, aes(x = body_mass_g, y = bill_depth_mm)) +
  geom_point(aes(color = island,shape = island),
                 size=3)


aa <- ggplot(penguins, aes(x = body_mass_g, y = bill_depth_mm)) +
  geom_point(aes(color = island,shape = island),
             size=3) + 
  scale_shape_manual(values=c(3, 16, 17))+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  scale_size_manual(values=c(2,3,4))+
  theme(legend.position="top")
ggplotly(aa)



M = cor(iris[,-5])
corrplot(M)

## Por paneles
pima %>%
  ggplot(aes(x = pressure, y = glucose)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ diabetes)



## Cleveland dotplot (variable cuantitativa frente a categórica)
## Media de activos por sector para todos los países

aa2 <- Ornstein %>%
  filter(sector != 'BNK') %>%
  group_by(sector) %>%
  summarise(avg_assets = mean(assets))

ggplot(aa2,aes(x = avg_assets, y = sector)) +
  geom_point(size = 5)

ggplot(aa2,aes(x = avg_assets, y = reorder(sector, desc(avg_assets)))) +
  geom_point(size = 5)

ggplot(aa2,aes(x = avg_assets, y = reorder(sector, avg_assets))) +
  geom_point(size = 5)







## Por paneles
Ornstein %>%
  filter(sector != 'BNK') %>%
  group_by(nation, sector) %>%
  summarise(avg_assets = mean(assets)) %>%
  ggplot(aes(x = avg_assets, y = reorder(sector, avg_assets))) +
  geom_point(size = 5) +
  facet_wrap(~ nation)


# Diagrama de calor (heatmap)
# The mtcars dataset:
data <- as.matrix(mtcars)

# No dendrogram nor reordering for neither column or row
heatmap(data, Colv = NA, Rowv = NA, scale="column")





ggpairs(iris, columns = 1:4, aes(color = Species, alpha = 0.5),
        upper = list(continuous = wrap("cor", size =4))) 



## Transformación de variables
bank = read.csv('https://raw.githubusercontent.com/rafiag/DTI2020/main/data/bank.csv')

hist_var <- ggplot(data = bank) +
  geom_histogram(mapping = aes(x = age), binwidth = 5)

hist_log <- ggplot(data = bank) +
  geom_histogram(mapping = aes(x = log(age)), binwidth = .1)

grid.arrange(hist_var, hist_log, ncol=2)



#Trying Transformations
bank %>% symbox(~ age, data = .)



# Transformacion

a <- ggplot(Animals, aes(x = body, y = brain)) + geom_point() + ggtitle('Sin transformación')
b <- ggplot(Animals, aes(x = log10(body), y = log10(brain))) + geom_point() + geom_smooth(method = "loess") + ggtitle('Log')


grid.arrange(a, b, ncol=2 ,top = 'Relación peso medio cerebro vs. peso medio corporal animales terrestres')




