library(klaR)
library(MASS)
library(dplyr)
library(caret)
library(pdp)

# Vamos a hacer un primer ejemplo con los datos iris que cumplen las hipótesis
data(iris)
str(iris)

# Cambiamos el nombre de las categorías del target para facilitar un visualización posterior
iris$Species <- recode(iris$Species, setosa = 'S',
                       versicolor  = 'V',
                       virginica = 'G')
levels(iris$Species)

# Hacemos división en train-test
set.seed(123)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- iris[ind==1,]
testing <- iris[ind==2,]

# Aplicamos LDA
linear <- lda(Species~., training)
linear

# Otra manera de escribirlo si no queremos usar ~
# linear2 <- lda(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, training)
# linear2


# Comenzamos a visualizar los resultados
pairs(linear)

p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$Species) # en base a ld1
ldahist(data = p$x[,2], g = training$Species) # en base a ld2

partimat(Species~., data = training, method = "lda",
         image.colors = c("orange", "white", "skyblue"))

# Comprobamos rendimiento en train
cf_train <- confusionMatrix(p$class, training$Species)
print(cf_train)
# Hacemos predicción en test
prediction_test <- predict(linear,newdata=testing)
cf_test <- confusionMatrix(prediction_test$class, testing$Species)
print(cf_test)


## Probamos con un caso binario (¡ojo! habría que comprobar las hipótesis)
# Usaremos los datos pima
pima
pima_df <- na.omit(pima)
pima_df <- pima_df[,-c(1,6,7,8)]

set.seed(123)
ind <- sample(2, nrow(pima_df),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- pima_df[ind==1,]
testing <- pima_df[ind==2,]

# Aplicamos LDA
linear <- lda(diabetes~., training)
linear

p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$diabetes) # en base a ld1

partimat(diabetes~., data = training, method = "lda")

# Evaluar el rendimiento
# Train
cf_train <- confusionMatrix(p$class, training$diabetes)
print(cf_train)
# Test
prediction_test <- predict(linear,newdata=testing)
cf_test <- confusionMatrix(prediction_test$class, testing$diabetes)
print(cf_test)







