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

