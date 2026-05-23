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


# =========================================================
# REVISIÓN GENERAL
# =========================================================

summary(data)

str(data)

# =========================================================
# FILTRO FINAL
# =========================================================

data_filtrada <- data %>%
  
  filter(
    
    `Valor de mercado` > 32000000,
    
    `Perfil Ofensivo` %in%
      
      c(
        "Delantero centro",
        "Extremo derecho",
        "Extremo izquierdo"
      )
  )

# =========================================================
# CENTRAR EDAD
# =========================================================

data_filtrada$Edad_centrada <- 
  
  data_filtrada$Edad -
  
  mean(data_filtrada$Edad)

# Edad centrada al cuadrado
data_filtrada$Edad_centrada2 <- 
  
  data_filtrada$Edad_centrada^2

# =========================================================
# REVISAR MUESTRA FINAL
# =========================================================

table(
  data_filtrada$`Perfil Ofensivo`
)

nrow(data_filtrada)

# =========================================================
# MODELO PREDICTIVO FINAL
# =========================================================

modelo_final <- lm(
  
  ln_ValorMercado ~
    
    Part_Ofensiva +
    
    Edad_centrada +
    Edad_centrada2 +
    
    `Perfil Ofensivo` +
    
    ClubTop +
    
    Liga +
    
    Puntos_Ranking_FIFA +
    
    Seleccion_Nacional +
    
    Comp_Internacional +
    
    Popularidad_Alta +
    
    Popularidad_Alta:Comp_Internacional,
  
  data = data_filtrada
)

# =========================================================
# RESULTADOS MODELO
# =========================================================

summary(modelo_final)

# =========================================================
# R2 AJUSTADO
# =========================================================

summary(modelo_final)$adj.r.squared

# =========================================================
# MULTICOLINEALIDAD
# =========================================================

vif(modelo_final)

# =========================================================
# HETEROCEDASTICIDAD
# =========================================================

bptest(modelo_final)

# =========================================================
# RESET TEST
# =========================================================

resettest(modelo_final)

# =========================================================
# ERRORES ROBUSTOS
# =========================================================

coeftest(
  
  modelo_final,
  
  vcov = vcovHC(
    modelo_final,
    type = "HC1"
  )
)

# =========================================================
# PREDICCIONES
# =========================================================

pred <- predict(modelo_final)

# =========================================================
# RMSE
# =========================================================

rmse_valor <- rmse(
  
  data_filtrada$ln_ValorMercado,
  pred
)

rmse_valor

# =========================================================
# MAE
# =========================================================

mae_valor <- mae(
  
  data_filtrada$ln_ValorMercado,
  pred
)

mae_valor

# =========================================================
# HISTOGRAMA RESIDUOS
# =========================================================

hist(
  
  residuals(modelo_final),
  
  main = "Histograma de Residuos",
  
  xlab = "Residuos",
  
  col = "lightgray"
)

# =========================================================
# RESIDUOS VS AJUSTADOS
# =========================================================

plot(
  
  modelo_final$fitted.values,
  
  residuals(modelo_final),
  
  xlab = "Valores Ajustados",
  
  ylab = "Residuos",
  
  main = "Residuos vs Ajustados",
  
  pch = 19
)

abline(
  h = 0,
  col = "red"
)

# =========================================================
# VALORES REALES VS PREDICHOS
# =========================================================

plot(
  
  data_filtrada$ln_ValorMercado,
  
  pred,
  
  xlab = "Valores Reales",
  
  ylab = "Valores Predichos",
  
  main = "Valores Reales vs Predichos",
  
  pch = 19
)

abline(
  0,
  1,
  col = "blue"
)

# =========================================================
# NORMALIDAD RESIDUOS
# =========================================================

shapiro.test(
  
  residuals(modelo_final)
)

# =========================================================
# MATRIZ CORRELACIÓN
# =========================================================

cor(
  
  data_filtrada %>%
    
    select(
      
      `Valor de mercado`,
      
      Part_Ofensiva,
      
      Edad_centrada,
      
      Puntos_Ranking_FIFA
      
    ),
  
  use = "complete.obs"
)

# =========================================================
# EXPORTAR RESULTADOS HTML
# =========================================================

stargazer(
  
  modelo_final,
  
  type = "html",
  
  title = "Modelo Predictivo Valor de Mercado",
  
  dep.var.labels = "Log Valor Mercado",
  
  out = "Resultados_Modelo_Final.html"
)

# =========================================================
# EXPORTAR CSV PREDICCIONES
# =========================================================

data_filtrada$Predicciones <- pred

write.csv(
  
  data_filtrada,
  
  "Predicciones_Jugadores.csv",
  
  row.names = FALSE
)

# =========================================================
# FIN
# =========================================================

