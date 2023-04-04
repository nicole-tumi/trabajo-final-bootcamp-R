
# Nicole Gabriela Tumi Alarcón

# PREGUNTA 1 - ESTRUCTURA DE DATOS ===================================================================================
# ====================================================================================================================
# 1. Crear vectores con los títulos de la dataset

data <- read.csv("C:\\Users\\tumin\\Environment\\RBootcamp\\games.csv")
head(data)

#install.packages("dplyr")
library(dplyr)

app_id <- pull(data, "app_id")
title <- pull(data, "title")
date_release <- pull(data, "date_release")
win <- pull(data, "win")
mac <- pull(data, "mac")
linux <- pull(data, "linux")
rating <- pull(data, "rating")
positive_ratio <- pull(data, "positive_ratio")
user_reviews <- pull(data, "user_reviews")
price_final <- pull(data, "price_final")
price_original <- pull(data, "price_original")
discount <- pull(data, "discount")
steam_deck <- pull(data, "steam_deck")

# ====================================================================================================================
# 2. Crear vector numérico con el precio final de los videojuegos

precio_final <- data$price_final
precio_final

# ====================================================================================================================
# 3. Crear una condición lógica de precios bajos

precios_bajos <- data$price_final < 5.00 # En este caso se valida si el precio es menor a 5.00
precios_bajos

# ====================================================================================================================
# 4. Sumar 5 al vector creado (price_final)

precio_final <- precio_final + 5
precio_final

# ====================================================================================================================
# 5. Dividir la puntuación (rating) entre 2

division_rating <- split(rating , 1:2)
print(division_rating)

# ====================================================================================================================
# 6. Calcular la media, moda, max, min de los datos de tipo numérico (Verificar con la función Class)

    class(app_id) # "integer"
    
    class(title) # "character"
    
    class(date_release) # "character"
    
    class(win) # "character"
    
    class(mac) # "character"
    
    class(linux) # "character"
    
    class(rating) # "character"
    
    class(positive_ratio) # "integer"
    
    class(user_reviews) # "integer"
    
    class(steam_deck) # "character"
    
    class(price_final) # "numeric"
        # Calculamos la media
        media_precio_final <- mean(price_final)
        print(media_precio_original)
        
        # Calculamos la moda
        frecuencias <- table(data$price_final)
        frecuencias.ordenada <- frecuencias[order(frecuencias, decreasing = TRUE)]
        moda_precio_final <- frecuencias.ordenada[1]
        print(moda_precio_final)
        
        # Calculamos el máximo
        max_precio_final <- max(price_final)
        print(max_precio_final)
        
        # Calculamos el mínimo
        min_precio_final <- min(price_final)
        print(min_precio_final)
    
        
    class(price_original) # "numeric"
        # Calculamos la media
        media_precio_original <- mean(price_original)
        print(media_precio_original)
        
        # Calculamos la moda
        frecuencias2 <- table(data$price_original)
        frecuencias2.ordenada <- frecuencias2[order(frecuencias2, decreasing = TRUE)]
        moda_precio_original <- frecuencias2.ordenada[1]
        print(moda_precio_original)
        
        # Calculamos el máximo
        max_precio_original <- max(price_original)
        print(max_precio_original)
        
        # Calculamos el mínimo
        min_precio_original <- min(price_original)
        print(min_precio_original)
    
         
    class(discount) # "numeric"
        # Calculamos la media
        media_descuento <- mean(discount)
        print(media_descuento)
        
        # Calculamos la moda
        frecuencias3 <- table(data$discount)
        frecuencias3.ordenada <- frecuencias3[order(frecuencias, decreasing = TRUE)]
        moda_descuento <- frecuencias2.ordenada[1]
        print(moda_descuento)
        
        # Calculamos el máximo
        max_descuento <- max(discount)
        print(max_descuento)
        
        # Calculamos el mínimo
        min_descuento <- min(discount)
        print(min_descuento)


# ====================================================================================================================
# 7. Crear un dataFrame de 13 col con la base de datos y guardar en una nueva variable
df <- data.frame(data$app_id, data$title, data$date_release, data$win, data$mac, data$linux, data$rating,
                 data$positive_ratio, data$user_reviews, data$price_final, data$price_original, data$discount,
                 data$steam_deck)

# ====================================================================================================================
# 8. Agregar filas y columnas a la matriz (Sugerencia una columna de "1" y una fila de datos de un juego de tu preferencia)

matriz <- as.matrix(df)

    matriz <- cbind(matriz, 1) # Agregamos la columna de 1
    matriz
    
    nueva_fila <- c(19998, "Turismo 4", 2008-11-18, "true", "true", "true", "Positive", 98, 2001, 31.13, 210.00, 0, "false", 1)
    matriz <- rbind(matriz, nueva_fila) # Agregamos la fila nueva

# ====================================================================================================================
# 9. Agregar columna de "1"

matriz <- cbind(matriz, 1)
matriz

# ====================================================================================================================
# 10. Agregar fila con los datos de un video juego de tu preferencia

nueva_fila2 <- c(23453, "Mario Bros", 2001-10-21, "false", "true", "true", "Very Positive", 9, 1998, 25.03, 198.00, 0, "false", 1, 1)
matriz <- rbind(matriz, nueva_fila2)

# ====================================================================================================================
# 11. Eliminar filas y columnas de la matriz

matriz <- matriz[-c(nrow(matriz)-1, nrow(matriz))] #Eliminamos las dos ultimas filas
print(matriz)

matriz <- matriz[, -c(ncol(matriz)-1, ncol(matriz))] #Eliminamos las dos ultimas columnas
print(matriz)

# ====================================================================================================================
# 12. Seleccionar los elementos de la matriz

    # Seleccionamos los elementos de la tercera fila
    fila3 <- matriz[3,]
    fila3
    # Seleccionamos elementos de la quinta columna
    columna5 <- matriz[,5]
    columna5
    
    # Seleccionamos los elementos de la 8va y 9na fila y la 7ma columna
    seleccion <- matriz[c(8,9),c(7)]
    seleccion

# ====================================================================================================================
# 13. Convertir la matriz en data.frame y asignar nombres a las columnas

    df2 <- as.data.frame(matriz) # Se convierte la matriz en dataframe
    print(df2)
    
    names(df2) <- c("app_id", "title", "date_release", "win", "mac", "linux", "rating", "positive_ratio", 
                    "user_reviews", "price_final", "price_original", "discount", "steam_deck")
    print(head(df2))

# ====================================================================================================================
# 14. Acceder a los datos del dataframe
    
    # Accedemos a la columna date_release
    fecha_lanzamiento <- df2$date_release
    print(fecha_lanzamiento)
    
    # Accedemos al dato de la posición 9,8 - fila 9 y columna 8
    dato <- df2[9,8]
    print(dato)

# ====================================================================================================================
# 15. Cambiar nombre de dataframe
    
    names(df2) <- c("id", "titulo", "fecha_lanzamiento", "win", "mac", "linux", "calificacion", "positive_ratio", 
                     "user_reviews", "precio_final", "precio_original", "descuento", "steam_deck")
    head(df2)

# ====================================================================================================================
# 16. Seleccionar un elemento del dataframe

    # Seleccionamos el dato ubicado en la posición 1003,2 - fila 1003 y columna 2 (título)
    dato2 <- df2[1003,2]
    print(dato2)

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# PREGUNTA 2 - IMPORTAR DATO ========================================================================================

# ====================================================================================================================
# 1. Importar Datos desde Excel y Ordenar los datos con la función order(), de preferencia para la variable Price_final
    
    # install.packages("readxl")
    library(readxl)
    # file.choose() # C:\Users\tumin\Environment\RBootcamp\games.xlsx
    
    path <- "C:\\Users\\tumin\\Environment\\RBootcamp\\games.xlsx"
    excel_sheets(path)
    
    juegos_steam <- read_excel(path) # Asignamos los valores
    
    class(juegos_steam)
    
    juegos_steam <- as.data.frame(juegos_steam) # Convertimos a dataframe
    
    # Ordenamos el dataframe con la función order() considerando la columna Price_final
    
    orden_precio_final <- order(juegos_steam$price_final, decreasing = FALSE)
    juegos_orden <- juegos_steam[orden_precio_final, ]
    juegos_orden
    
# ====================================================================================================================    
# 2. Mostrar el dataframe ordenado de manera ascendente y descendente
    orden_asc <- order(juegos_steam$app_id, decreasing = FALSE)
    juegos_asc <- juegos_steam[orden_asc, ]
    juegos_asc # dataframe ordenado ascendente considerando la columna app_id 
    
    orden_desc <- order(juegos_steam$app_id, decreasing = TRUE)
    juegos_desc <- juegos_steam[orden_desc, ]
    juegos_desc # dataframe ordenado descendente considerando la columna app_id 

# ====================================================================================================================
# 3. Calcular el resumen estadístico de los datos con la función que corresponde
    
    summary(juegos_steam)

# ====================================================================================================================
# 4. Realizar las graficas
    
    juegos_data <- read_excel(path)
    juegos_data <- as.data.frame(juegos_data)
    
    # Histograma
    hist(juegos_data$price_final,
         breaks = 50,
         main = "Distribución de Precios finales",
         xlab = "Precio final", ylab = "Cantidad de juegos",
         xlim = c(0,50),
         ylim = c (0, 20000),
         col = "brown",
         border = "white")
    
    # curve(dnorm(x, mean=mean(juegos_data$price_final), sd=sd(juegos_data$price_final)), add=TRUE, col="black", lwd=3)
    
    
    # Boxplot - precios originales de acuerdo a la calificación del juego
    
    a <- rainbow(9)
    b <- rainbow(6, alpha=0.2)
    c <- rainbow(6, v=0.5)
    
    boxplot(juegos_data$price_original ~ juegos_data$rating,
            main = "Precio original de juegos en STEAM",
            ylab = "Calificación", xlab = "Precio original",
            col = b, 
            boxcol = c, 
            medcol = c,
            whiskcol = a,
            staplecol = c, 
            outcol = c,
            outbg =c,
            pch = 20,
            cex = 1,
            horizontal = TRUE)

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# PREGUNTA 3 - PROGRAMACIÓN ========================================================================================

# ====================================================================================================================
# 1. Implementar una función para la multiplicación de dos vectores(xy) y probar con valores
    
    # Definimos la función
    multiplicar_vectores <- function(x, y) {
      return(x * y)
    }
    # Probamos con valores
    resultado <- multiplicar_vectores(x = c(7, 12, 205), y = c(135, 6, 99))
    print(resultado)

# ====================================================================================================================
# 2. Implementar una función que muestre el resultado de la ecuación de Bhaskara y probar con valores.
    
    # Definimos la función
    bhaskara <- function(a, b, c) {
      
      discriminante <- b^2 - 4*a*c
      
      if (discriminante < 0) {
        mensaje <- "La ecuación no tiene solución real"
        return(mensaje)
      } else {
        x1 <- (-b + sqrt(discriminante)) / (2*a)
        x2 <- (-b - sqrt(discriminante)) / (2*a)
        resultado <- c(x1, x2)
        return(resultado)
      }
    }
    
    # Probamos con valores
    rpta <- bhaskara(a = 5, b = -9, c = -4)
    print(rpta)
    
# ====================================================================================================================
# 3. Se quiere conocer la media muestral de n observaciones obtenidas independientemente de una distribución normal 
#    con media = 0 y varianza =1.
    
    # Generamos 100 observaciones de una distribución normal con media 0 y varianza 1
    x <- rnorm(100, mean = 0, sd = 1)
    
    # Calculamos la media muestral
    mean_x <- mean(x)
    mean_x
    
    # Calcula,os la desviación estándar de la media muestral
    sd_x <- sd(x) / sqrt(length(x))
    sd_x
    
# ====================================================================================================================
      # 3.1. Realizar una simulación, luego calcular las estadísticas descriptivas aplicando la función que corresponde 
      #      y graficar.
  
      summary(x)
      # Min.    1st Qu.  Median   Mean     3rd Qu.  Max. 
      # -2.0543 -0.6120  -0.1080  -0.1010  0.4115   3.0794 
      
      
      #install.packages("ggplot2")
      library(ggplot2)
      
      df <- data.frame(x = x)
      
      # Graficamos la distribución
      ggplot(df, aes(x)) + 
        geom_histogram(aes(y = ..density..), binwidth = 0.5, colour = "black", fill = "purple", alpha=0.2) +
        stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)), colour = "purple", size = 1) +
        labs(x = "Valores de x", y = "Densidad", title = "Distribución normal") +
        theme_bw()
   