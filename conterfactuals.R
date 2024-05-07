library(counterfactuals)
library(iml)
library(randomForest)
data("german", package = "rchallenge")
# Se hace una selección de los datos por razones ilustrativas
credit = german[, c("duration", "amount", "purpose", "age",
                    "employment_duration", "housing", "number_credits",
                    "credit_risk")]
str(credit)
View(credit)

### EJEMPLO CON EL ALGORITMO MOC

# Entrenamos un modelo Random Forest para predecir credit_risk
# Quitamos la observación 998 del conjunto de train puesto que la usaremos para buscar sus contrafácticos
set.seed(20210816)
rf = randomForest(credit_risk ~ ., data = credit[-998,])

# Tenemos que usar iml::Predictor para poner el modelo en el formato adecuado
# type='prob' para tener las probabilidades y no las etiquetas binarizadas
predictor = iml::Predictor$new(rf, type = "prob") 
# Punto de interés para buscar contrafácticos
x_interest = credit[998, ]
# Predicción de la probabilidad de cada clase
predictor$predict(x_interest) # probability of being a good credit risk of 38.2%

# Ahora estudiamos que factores de riesgo se deben cambiar para que la probabilidad de good credit risk sea
# de al menos un 60%

# Se puede penalizar aquellos individuos que estén más lejos del intervalo deseado que un umbral epsilon.
# Si ponemos epsilon=0 entonces estamos penalizando a todos aquellos individuos cuya predicción está fuera del
# intervalo deseado
# Con fixed_features se fijan las variables que no se pueden mover

# Ponemos las opciones para el algoritmo
moc_classif = MOCClassif$new(
  predictor, epsilon = 0, fixed_features = c("age", "employment_duration"))
# Sacamos los contrafácticos
cfactuals = moc_classif$find_counterfactuals(
   x_interest, desired_class = "good", desired_prob = c(0.6, 1))

# Observamos los contrafácticos logrados
print(cfactuals)

head(cfactuals$predict(), 3)

# Filtramos por aquellos que son válidos, es decir, aquellos que tienen la predicción deseada
cfactuals$subset_to_valid()
nrow(cfactuals$data)

head(cfactuals$data,3)

# Pintamos las variables más importantes para lograr el cambio deseado
# Setting subset_zero = TRUE excludes all unchanged features from the plot.
cfactuals$plot_freq_of_feature_changes(subset_zero = TRUE)

# En azul el punto de interés y en gris los contrafácticos
cfactuals$plot_parallel(feature_names = names(
  cfactuals$get_freq_of_feature_changes()), digits_min_max = 2L)
# Todos los contrafácticos proponen un descenso de credit amount 
# mientras que la duraction o no tiene modificación, o requiere un aumento o un descenso (más frecuente)
# En otro contrafáctico, purpose se cambia a coche nuevo, la casa de propia a alquilada y aumenta el número de créditos

