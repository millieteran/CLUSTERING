library(dplyr)
library(factoextra)
library(knitr)
library(psych)
library(cowplot)
library(ggdendro)
library(readxl)
library(janitor)
library(proxy)

install.packages("proxy")


#aaaa

nacimientos <- read_excel("nacimientos.xls")
View(nacimientos)


summary(nacimientos)
# agregar variables
#Debido a que las variables del estudio son dicotómicas, consideramos que este análisis no es relevante. 


str(nacimientos)

# $ id               : chr [1:51] "A001" "C002" "A003" "C004" ...
# $ centro           : chr [1:51] "A" "C" "A" "C" ...
# $ eg_menor_30      : num [1:51] 1 1 0 1 0 0 1 1 0 1 ...
# $ ematerna_mayor_40: num [1:51] 0 1 0 0 0 0 0 0 0 0 ...
# $ hta              : num [1:51] 0 0 0 1 0 0 0 1 0 0 ...
# $ sexo_masculino   : num [1:51] 0 0 0 0 0 0 1 0 1 1 ...
# $ corti            : num [1:51] 1 1 1 0 1 1 1 1 1 1 ...
# $ cesarea          : num [1:51] 1 1 1 1 1 1 1 1 1 0 ...
# $ peso_menor_1000  : num [1:51] 0 1 0 0 0 0 0 0 0 0 ...
# $ cluster          : int [1:51] 1 1 2 1 2 2 3 1 4 3 ...

tabyl(nacimientos$eg_menor_30)
#A partir de esta tabla, podemos ver que en un 43% de los nacimientos, la edad gestacional
#fue menor a 30 meses. En el restante 56% fue mayor. 
tabyl(nacimientos$centro)
# nacimientos$centro  n    percent
# A 30 0.58823529
# B  5 0.09803922
# C 16 0.31372549
tabyl(nacimientos$ematerna_mayor_40)
tabyl(nacimientos$hta)
tabyl(nacimientos$sexo_masculino)
tabyl(nacimientos$cesarea)
tabyl(nacimientos$corti)
tabyl(nacimientos$peso_menor_1000)

# tabyl(nacimientos$ematerna_mayor_40)
# nacimientos$ematerna_mayor_40  n   percent
# 0 42 0.8235294
# 1  9 0.1764706
# > tabyl(nacimientos$hta)
# nacimientos$hta  n   percent
# 0 40 0.7843137
# 1 11 0.2156863
# > tabyl(nacimientos$sexo_masculino)
# nacimientos$sexo_masculino  n   percent
# 0 26 0.5098039
# 1 25 0.4901961
# > tabyl(nacimientos$cesarea)
# nacimientos$cesarea  n    percent
# 0  2 0.03921569
# 1 49 0.96078431
# > tabyl(nacimientos$corti)
# nacimientos$corti  n   percent
# 0  9 0.1764706
# 1 42 0.8235294
# > tabyl(nacimientos$peso_menor_1000)
# nacimientos$peso_menor_1000  n   percent
# 0 45 0.8823529
# 1  6 0.1176471



# Asignar el 'id' de los nacimientos como nombre de las filas
rownames(nacimientos) <- nacimientos$id
names(nacimientos)

# seleccionar solo las variables dicotómicas
vars_to_cluster <- c(
  "eg_menor_30", "ematerna_mayor_40","hta","sexo_masculino","corti","cesarea","peso_menor_1000"  
)

df_to_cluster <- nacimientos %>%
  dplyr::select(all_of(vars_to_cluster))


#No estandarizamos porque son dicotomicas

# Calcular las distancias
d   <- dist(df_to_cluster, method = "binary") #no considera 00
#Otro cálculo de la distancia, es utilizando el método SM, SOCKAL & MICHENER s2

install.packages("ade4")
library(ade4)

d2 <- dist.binary(df_to_cluster, method = 2)
d2

# Realizar el agrupamiento jerárquico. El método de Ward, que busca minimizar la varianza dentro de los grupos.

fit <- hclust(d, method = "ward.D")

fit2 <- hclust(d2, method = "ward.D")

# Graficar el dendograma
plot(fit)
plot(fit2)

# Indice Silhouette  ---
fviz_nbclust(df_to_cluster, kmeans, method = "silhouette") +
  labs(title    = "Número óptimo de clusters a considerar",
       subtitle = "Indice Silhouette")

#DA 10 PERO ELEGIMOS 4 ya que se observa un codo en dicho punto.

# Indentificar los distintos clusters elegidos
rect.hclust(fit, k = 4, border = "red")

rect.hclust(fit2, k = 4, border = "blue")

#Al analizar ambos graficos, no pudimos observar grandes diferencias
#La altura de los cortes comparando ambos dendogramas, es muy similar. 
#si pudimos observar que no distribuyeron de la misma manera todas las observaciones. algunas cambiaron de cluster. 
#evidentemente considerar las frecuencias de coincidencias 0-0 afecta a los clusters.


groups <- cutree(fit, k = 4)
df_to_cluster         <- data.frame(df_to_cluster)
df_to_cluster$cluster <- groups # Guarda los cluster en conjunto de datos estandarizdo que se utilizó para agrupar
nacimientos$cluster            <- groups # Guarda los cluster en conjunto de datos original


groups2 <- cutree(fit2, k = 4)
df_to_cluster$cluster2 <- groups2 # Guarda los cluster en conjunto de datos estandarizdo que se utilizó para agrupar
nacimientos$cluster2            <- groups2

#observando el data frame de nacimientos, con los nombres de los clusters, observamos que
#al utilizar los dos metodos para el calculo de las distancias, algunas observaciones
#del cluster 1 cambiaron al cluster 3. El resto de las observaciones pertenecen a los mismos clusters. 



# calcular medias por grupos para todas las variables

#ESTO NO TENDRIA SENTIDO EN ESTE CASO POR SER BINARIAS, NO???
# df_means <- data.frame(
#   nacimientos %>%
#     group_by(cluster) %>%
#     summarise_at(vars(vars_to_cluster), list(name = mean))
# )
# 
# # Transponer el df de las medias
# df_means_t = setNames(data.frame(t(df_means[, -1])), df_means[, 1] )
# 
# kable(df_means_t, digits = 1)


#Hacer tabla con proporciones

# Tabla de frecuencias absolutas
tabla_frecuencias <- table(groups)

tabla_frecuencias2 <- table(groups2)
# Convertir a proporciones
tabla_proporciones <- prop.table(tabla_frecuencias)

tabla_proporciones2 <- prop.table(tabla_frecuencias2)

# Mostrar como data.frame con porcentajes
tabla_final <- data.frame(
  Cluster = names(tabla_proporciones),
  Frecuencia = as.vector(tabla_frecuencias),
  Proporcion = round(tabla_proporciones, 3),
  Porcentaje = paste0(round(tabla_proporciones * 100, 1), "%")
)

print(tabla_final)

# Cluster Frecuencia Proporcion.groups Proporcion.Freq Porcentaje
# 1       1         19                 1           0.373      37.3%
# 2       2          9                 2           0.176      17.6%
# 3       3         11                 3           0.216      21.6%
# 4       4         12                 4           0.235      23.5%


tabla_final2 <- data.frame(
  Cluster = names(tabla_proporciones2),
  Frecuencia = as.vector(tabla_frecuencias2),
  Proporcion = round(tabla_proporciones2, 3),
  Porcentaje = paste0(round(tabla_proporciones2 * 100, 1), "%")
)

print(tabla_final2)

# Cluster Frecuencia Proporcion.groups2 Proporcion.Freq Porcentaje
# 1       1         13                  1           0.255      25.5%
# 2       2          9                  2           0.176      17.6%
# 3       3         18                  3           0.353      35.3%
# 4       4         11                  4           0.216      21.6%


#CON DISTANCIA BINARY
# Agregar columna de clúster al dataframe
df_con_grupo <- df_to_cluster
df_con_grupo$cluster <- groups

# Calcular proporciones por variable dentro de cada clúster
tabla_porcentajes <- aggregate(. ~ cluster, data = df_con_grupo, FUN = mean)

# Convertir a porcentaje
tabla_porcentajes[,-1] <- round(tabla_porcentajes[,-1] * 100, 1)

# Ver la tabla
print(tabla_porcentajes)

# cluster eg_menor_30 ematerna_mayor_40  hta sexo_masculino corti cesarea peso_menor_1000
# 1       1        94.7              42.1 36.8           21.1  78.9   100.0            15.8
# 2       2         0.0               0.0  0.0            0.0 100.0   100.0             0.0
# 3       3       100.0               0.0  0.0          100.0 100.0    81.8            18.2
# 4       4         0.0               8.3 33.3           83.3  58.3   100.0             8.3

#Cluster 1: la mayoria de los recien nacidos tienen edad gestacionaL menor a 30 meses.
#un poco menos que la mitad de las mujeres eran mayores a 40. Mayoria bebes mujeres. 
#todas cesarea, un 75% peso mas que 1kg. La mayoria de las mujeres recibieron corticoide.  

#Cluster 2: todos bebes mujeres, todas las mujeres recibieron corticoides y fue por cesarea.
#ninguna madre con ha ni mayor a 40 anos. 

#Cluster 3: todos los recien nacidos tuvieron edad gestacional menor a 30 meses.
#todos los bebes fueron hombres, todas las mujeres recibieron corticoides y la mayoria tuvo cesarea. 
#tambien la mayoria de los bebes pesaron mas de 1kg. Ninguna mujer mayor de 40 ni con ha.

#Cluster 4: ningun rn tuvo edad gestacional menor a 30 meses. pocas madres mayores a 40. 
#la mayoria de sexo masculino, todas tuvieron cesarea, y la mayoria fue administrada con corticoides
#mayoria de los bebes pesaron mas de 1kg. 


#CON DISTANCIA SOKAL MICH

# Agregar columna de clúster al dataframe
df_con_grupo <- df_to_cluster
df_con_grupo$cluster2 <- groups2

# Calcular proporciones por variable dentro de cada clúster
tabla_porcentajes2 <- aggregate(. ~ cluster2, data = df_con_grupo, FUN = mean)

# Convertir a porcentaje
tabla_porcentajes2[,-1] <- round(tabla_porcentajes2[,-1] * 100, 1)

# Ver la tabla
print(tabla_porcentajes2)

# cluster2 eg_menor_30 ematerna_mayor_40  hta sexo_masculino corti cesarea
# 1        1       100.0              46.2 30.8            0.0  92.3   100.0
# 2        2         0.0               0.0  0.0            0.0 100.0   100.0
# 3        3        27.8              16.7 38.9           77.8  55.6   100.0
# 4        4       100.0               0.0  0.0          100.0 100.0    81.8
# peso_menor_1000 cluster
# 1            23.1     100
# 2             0.0     200
# 3             5.6     300
# 4            18.2     300

#Cluster 1: Todos los recien nacidos tienen edad gestacional menor a 30 meses.
#un poco menos que la mitad de las mujeres eran mayores a 40. Todos los bebes mujeres. 
#todas cesarea, un 23%  peso mas que 1kg. La mayoria de las mujeres recibieron corticoide.  

#Cluster 2: todos bebes mujeres, todas las mujeres recibieron corticoides y tuvieron parto por cesarea.
#ninguna madre con ha ni mayor a 40 anos. Ningun recien nacido tuvo edad gestacional menor a 
#30 meses.

#Cluster 3: cerca del 30% de los recien nacidos tuvieron edad gestacional menor a 30 meses.
#la mayoria de los bebes fueron hombres, la mitad de las mujeres recibieron corticoides y 
#todas tuvieron cesarea. 
#a su vez, un 5% de los bebes pesaron mas de 1kg. Pocas mujeres mayores de 40, y casi un 40% 
#con presion arterial alta.

#Cluster 4: todos los rn tuvieron edad gestacional menor a 30 meses. ninguna madre mayor a 40. 
#todos los bebes de sexo masculino, la mayoria tuvo cesarea, y todas recibieron  corticoides
#minoria de los bebes pesaron mas de 1kg. ninguna mujer tuvo presion arterial alta. 



#BINARYYY
table(nacimientos$cluster, nacimientos$centro)
#   A B C
# 1 8 2 9
# 2 8 1 0
# 3 5 1 5
# 4 9 1 2

#Para el cluster 1, la mayoria de los nacimientos fueron en centros A y C. 
# El cluster 2, tuvo la mayoria de los nacimientos en el centro A
# Para el 3, hubo nacimientos en igual medida en centros A y C. 
# y en el cluster 4, hubo mayoria de nacimientos en el centro A. 
#El centro B es el que menos nacimientos tuvo. Y el A el que mas.


#SOKAL MICHH
table(nacimientos$cluster2, nacimientos$centro)
#    A  B  C
# 1  6  1  6
# 2  8  1  0
# 3 11  2  5
# 4  5  1  5

#Para el cluster 1, la mayoria de los nacimientos fueron en centros A y C. 
# El cluster 2, tuvo la mayoria de los nacimientos en el centro A, sin nacimientos en el C.
# Para el 3, la mayoria de los nacimientos fueron en el A, seguido del centro C.
# y en el cluster 4, hubo misma cantidad de nacimientos en los centros A y C.  
#El centro B es el que menos nacimientos tuvo. Y el A el que mas.