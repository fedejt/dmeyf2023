# ORDEN SELECCIONADO (Cz# = CAZATALENTOS #)

# Se ordena en base a las probabilidades obtenidas y la confianza en cada una.
# ~ --> aprox. igual

# Cz6 ~ Cz7 > Cz8 > Cz5 > Cz1 > Cz2 > Cz3 > Cz9 > Cz4








# CAZATALENTOS 1 ----------------------------------------------------------

# En este caso tenemos una única judagora de 80 aciertos y muchas similares
# de 79, 78, 77, etc. La probabilidad de haber seleccionado a la de 80 aciertos
# (suponiendo que exista) es baja debido a la similitud entre las jugadoras.

mejor <- 0.8
peloton <- c(seq(from=79,to=10, by=-1),rep(10,(99-70)))/100
jugadoras <- c(mejor, peloton)


primera_ganadora <- 0

for (i in 1:10000) { # diez mil experimentos
  
  vaciertos <- mapply(rbinom, 1, 100, jugadoras) # 100 tiros libres cada jugador
  
  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}

print("CAZATALENTOS 1")
print(primera_ganadora)


# CAZATALENTOS 2 ----------------------------------------------------------

# Similar al caso de la cazatalentos 1, pero en este caso hay más jugadoras
# parecidas entre sí, por lo que el riesgo de no seleccionar a la verdaderamente
# mejor es mayor (más riesgo de "overfitear").
# En ambos casos (Cz1 y Cz2), si la mejor jugadora tiene un indice de enceste
# 0.8 < p < 0.85 (no puede ser mayor a .85 según el enunciado), la probabilidad
# se seleccionar a la mejor aumenta a medida que aumenta p (menos prob. overfiting).
# Pero siempre la prob. para la Cz2 se mantiene menor que para la Cz1 (por la mayor
# cantidad de jugadoras cercanas a .8).

mejor <- 0.8
peloton <- c(unlist(sapply(c(1,2,3,4,5,6), function(x) { rep(80-x,7-x)})), 
             seq(from=73, to=10, by = -1))
peloton <- c(peloton, rep(10, (199-length(peloton))))/100
jugadoras <- c(mejor, peloton)

primera_ganadora <- 0

for (i in 1:10000) { # diez mil experimentos
  
  vaciertos <- mapply(rbinom, 1, 100, jugadoras) # 100 tiros libres cada jugador
  
  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}

print("CAZATALENTOS 2")
print(primera_ganadora)


# CAZATALENTOS 3 ----------------------------------------------------------

# Este caso es similar a las casatalentos 1 y 2, pero con solo 2 jugadoras.
# Tenemos menos confianza en tener una buena jugadora, dado que probamos a menos.
# Dado que la "mejor" conpite solo con otra ligeramente inferior, tenemos más
# posibilidad de haber seleccionado a la mejor de las dos.

mejor <- 0.8
peloton <- 0.75
jugadoras <- c(mejor, peloton)

primera_ganadora <- 0

for (i in 1:10000) { # diez mil experimentos
  
  vaciertos <- mapply(rbinom, 1, 100, jugadoras) # 100 tiros libres cada jugador
  
  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}

print("CAZATALENTOS 3")
print(primera_ganadora)


# CAZATALENTOS 4 ----------------------------------------------------------

# Solo tira 10 tiros. Mas probabilidad de overfiting.
# Si suponemos una probabilidad de 0.7 de enceste para todas, de 10000 torneos 
# en casi el 40% observaríamos alguna jugadora con un indice de enceste aparente
# del 0.80 o m'as
print("CAZATALENTOS 4")
print(paste0("Si hicieramos 10000 torneos de 10 tiros esperaríamos un índice de enceste >= 0.8 en el ",
             sum(rbinom(10000,10,.7) >= 8) * 100/10000, "% de los casos"))

# CAZATALENTOS 5 ----------------------------------------------------------

# En este caso tenemos tres torneos de 100 tiros, podemos calcular 
# la probabilidad promedio de enceste para cada jugadora.
# La cazatalentos seleccionó la jugadora A, al parecer le iría mejor
# seleccionando a la jugadora B.
# Al tener una probabilidad promedio de 3 torneos tenemos más confianza en esta
# estimación que en otros casos donde solo hubo un torneo.

# La jugadora A tiene un promedio de acieros de 0.75
(85 + 69 + 70) / (3*100)

cz5_tiros <- 100
cz5_aciertos_ronda_1 <- 85
cz5_aciertos_ronda_2 <- 69
cz5_aciertos_ronda_3 <- 70
cz5_errores_ronda_1 <- (cz5_tiros - cz5_aciertos_ronda_1)
cz5_errores_ronda_2 <- (cz5_tiros - cz5_aciertos_ronda_2)
cz5_errores_ronda_3 <- (cz5_tiros - cz5_aciertos_ronda_3)

cz5_probabilidad_aposteriori <- (cz5_aciertos_ronda_1+cz5_aciertos_ronda_2+cz5_aciertos_ronda_3)/(3*cz5_tiros)

cz5_IC_95_aposteriori <- qbeta(c(.025, .975), 
                               (cz5_aciertos_ronda_1+cz5_aciertos_ronda_2+cz5_aciertos_ronda_3), 
                               (cz5_errores_ronda_1+cz5_errores_ronda_2+cz5_errores_ronda_3))

print("CAZATALENTOS 5")
print(paste0("JUGADORA A, prob. aposteriori: ", cz5_probabilidad_aposteriori, " IC95% (", 
             cz5_IC_95_aposteriori[1], " - ", cz5_IC_95_aposteriori[2], ")"))


# La jugadora B parece la mejor de las 5 con un promedio de acieros de 0.78

(84 + 74 + 76) / (3*100)

cz5_aciertos_ronda_1 <- 84
cz5_aciertos_ronda_2 <- 74
cz5_aciertos_ronda_3 <- 76
cz5_errores_ronda_1 <- (cz5_tiros - cz5_aciertos_ronda_1)
cz5_errores_ronda_2 <- (cz5_tiros - cz5_aciertos_ronda_2)
cz5_errores_ronda_3 <- (cz5_tiros - cz5_aciertos_ronda_3)

cz5_probabilidad_aposteriori <- (cz5_aciertos_ronda_1+cz5_aciertos_ronda_2+cz5_aciertos_ronda_3)/(3*cz5_tiros)

cz5_IC_95_aposteriori <- qbeta(c(.025, .975), (cz5_aciertos_ronda_1+cz5_aciertos_ronda_2+cz5_aciertos_ronda_3), 
                               (cz5_errores_ronda_1+cz5_errores_ronda_2+cz5_errores_ronda_3))

print("CAZATALENTOS 5")
print(paste0("JUGADORA A, prob. aposteriori: ", cz5_probabilidad_aposteriori, " IC95% (", 
             cz5_IC_95_aposteriori[1], " - ", cz5_IC_95_aposteriori[2], ")"))


# CAZATALENTOS 6 ----------------------------------------------------------

# Como el umbral es de 80% de aciertos (según la consigna), podemos suponer que 
# si la cazatalentos seleccionó a esta jugadora sin tirar previamente es porque 
# le dá una probabilidad a priori de al menos 0.8. Luego de hacerla tirar 100 
# tiros obtiene 80 aciertos, por lo que se actualiza la creencia del 80% para 
# obtener una probabilidad a posteriori de 0.80 pero con más confianza (IC más
# estrecho).

cz6_tiros_antes <- 100
cz6_aciertos_antes <- 80
cz6_errores_antes <- (cz6_tiros_antes - cz6_aciertos_antes)

cz6_probabilidad_apriori <- cz6_aciertos_antes/cz6_tiros_antes

cz6_IC_95_apriori <- qbeta(c(.025, .975), cz6_aciertos_antes, cz6_errores_antes)

# La probabilidad a priori es 0.8 con un intervalo de confianza del 95% de (0.717, 0.872)
# Si fuera de 85% (máximo de acuerdo a la consigna) sería de 0.85 (0.774 - 0.913)

# Luego de tirar 100 tiros y acertar 80 calculamos la probabilidad a posteriori.

cz6_tiros_despues <- 100
cz6_aciertos_despues <- 80
cz6_errores_despues <- (cz6_tiros_despues - cz6_aciertos_despues)

cz6_probabilidad_aposteriori <- (cz6_aciertos_antes + cz6_aciertos_despues)/
  (cz6_tiros_antes + cz6_tiros_despues)

cz6_IC_95_aposteriori <- qbeta(c(.025, .975), 
                               (cz6_aciertos_despues + cz6_aciertos_despues), 
                               (cz6_errores_despues + cz6_errores_despues))

# La probabilidad a posteriori se mantiene en 0.8 pero ahora tenemos más 
# confianza en este valor (0.742 - 0.852).
# Si la probabiliad a priori hubiera sido de .85 obtendríamos una probabiidad 
# de .825 (.742 -.852), por lo que aun asignando una probabilidad a priori 
# de 0.8 luego de observar el resultado de 80 aciertos confiamos aún más en la 
# selección de la cazatalentos 6.

print("CAZATALENTOS 6")
print(paste0("Prob. aposteriori: ", cz6_probabilidad_aposteriori, " IC95% (", 
             cz6_IC_95_aposteriori[1], " - ", cz6_IC_95_aposteriori[2], ")"))


# CAZATALENTOS 7 ----------------------------------------------------------

# Vamos a suponer que el criterio para entrar en el grupo de las 5 seleccionadas
# es tener al menos 80 aciertos de 100 tiros. Entonces estamos en una situacion 
# similar a la cazatalentos 6, donde la mejor de la primer ronda (80 aciertos) 
# repite el desempeño (80 aciertos).
# Caso contrario, por la "maldicion de la ganadora" debería haber empeorado.

cz7_tiros_antes <- 100
cz7_aciertos_antes <- 80
cz7_errores_antes <- (cz7_tiros_antes - cz7_aciertos_antes)

cz7_probabilidad_apriori <- cz7_aciertos_antes/cz7_tiros_antes

cz7_IC_95_apriori <- qbeta(c(.025, .975), cz7_aciertos_antes, cz7_errores_antes)

# La probabilidad a priori es 0.8 con un intervalo de confianza del 95% de (0.717, 0.872)

# Luego de tirar 100 tiros y acertar 80 calculamos la probabilidad a posteriori.

cz7_tiros_despues <- 100
cz7_aciertos_despues <- 80
cz7_errores_despues <- (cz7_tiros_despues - cz7_aciertos_despues)

cz7_probabilidad_aposteriori <- (cz7_aciertos_antes + cz7_aciertos_despues)/(cz7_tiros_antes + cz7_tiros_despues)

cz7_IC_95_aposteriori <- qbeta(c(.025, .975), 
                               (cz7_aciertos_despues + cz7_aciertos_despues), 
                               (cz7_errores_despues + cz7_errores_despues))

print("CAZATALENTOS 7")
print(paste0("Prob. aposteriori: ", cz7_probabilidad_aposteriori, " IC95% (", 
             cz7_IC_95_aposteriori[1], " - ", cz7_IC_95_aposteriori[2], ")"))

# La probabilidad a posteriori se mantiene en 0.8 pero ahora tenemos más confianza en este valor (0.742 - 0.852)


# CAZATALENTOS 8 ----------------------------------------------------------

# Al igual que con la cazatalentos 6, tenemos una probabilidad a priori que 
# vamos a actualizar luego de que la jugadora tire 100 tiros.
# En este caso, dado que tenemos un historial de 1000 tiros, esperamos
# que la probabilidad este bastante consolidada alrededor de 0.79.

cz8_tiros_antes <- 1000
cz8_aciertos_antes <- 790
cz8_errores_antes <- (cz8_tiros_antes - cz8_aciertos_antes)

cz8_probabilidad_apriori <- cz8_aciertos_antes/cz8_tiros_antes

cz8_IC_95_apriori <- qbeta(c(.025, .975), cz8_aciertos_antes, cz8_errores_antes)

# La probabilidad a priori es de .79 (.764 - .815)
# Luego de tirar 100 tiros la actuaizamos:

cz8_tiros_despues <- 100
cz8_aciertos_despues <- 85
cz8_errores_despues <- (cz8_tiros_despues - cz8_aciertos_despues)

cz8_probabilidad_aposteriori <- (cz8_aciertos_antes + cz8_aciertos_despues)/
  (cz8_tiros_antes + cz8_tiros_despues)

cz8_IC_95_aposteriori <- qbeta(c(.025, .975), 
                               (cz8_aciertos_despues + cz8_aciertos_despues), 
                               (cz8_errores_despues + cz8_errores_despues))

# La probabilidad a posteriori resulta de .795 (0.797 - .815)


print("CAZATALENTOS 8")
print(paste0("Prob. aposteriori: ", cz8_probabilidad_aposteriori, " IC95% (", 
             cz8_IC_95_aposteriori[1], " - ", cz8_IC_95_aposteriori[2], ")"))


# CAZATALENTOS 9 ----------------------------------------------------------

# Como son varios "torneos" tenemos bastante confianza en que es una jugadora de .7

cz9_prob_promedio <- sum(c(68, 74, 78, 70, 68, 63, 80, 68, 67, 65))/(10*100)

print("CAZATALENTOS 9")
print(paste0("Prob. : ", cz9_prob_promedio))

