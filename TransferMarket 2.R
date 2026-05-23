# =========================================================
# PROYECTO FINAL - ANALÍTICA DE NEGOCIOS
# MODELO PREDICTIVO VALOR DE MERCADO
# =========================================================


library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(sandwich)
library(Metrics)
library(stargazer)

# Evitar notación científica
options(scipen = 999)

# =========================================================
# CARGAR BASE
# =========================================================

data <- read_excel(
  "Data_Jugadores_Modelo_Actualizado.xlsx"
)

# =========================================================
# LIMPIAR NOMBRES
# =========================================================

names(data) <- trimws(names(data))

# Revisar columnas
names(data)

# =========================================================
# VARIABLES NUMÉRICAS
# =========================================================

data$`Valor de mercado` <- as.numeric(
  data$`Valor de mercado`
)

data$Edad <- as.numeric(
  data$Edad
)

data$Part_Ofensiva <- as.numeric(
  data$Part_Ofensiva
)

data$Minutos_Aprox <- as.numeric(
  data$Minutos_Aprox
)

data$Puntos_Ranking_FIFA <- as.numeric(
  data$Puntos_Ranking_FIFA
)

# =========================================================
# VARIABLES CATEGÓRICAS
# =========================================================

data$`Perfil Ofensivo` <- as.factor(
  data$`Perfil Ofensivo`
)

data$Liga <- as.factor(
  data$Liga
)

data$Seleccion_Nacional <- as.factor(
  data$Seleccion_Nacional
)

data$Comp_Internacional <- as.factor(
  data$Comp_Internacional
)

data$Popularidad_Alta <- as.factor(
  data$Popularidad_Alta
)

data$ClubTop <- as.factor(
  data$ClubTop
)

# =========================================================
# VARIABLES NUEVAS
# =========================================================

# Logaritmo natural valor mercado
data$ln_ValorMercado <- log(
  data$`Valor de mercado`
)

# Edad al cuadrado
data$Edad2 <- data$Edad^2

# Participación ofensiva al cuadrado
data$Part_Ofensiva2 <- (
  data$Part_Ofensiva^2
)

# =========================================================
# ELIMINAR MISSINGS
# =========================================================

data <- na.omit(data)
