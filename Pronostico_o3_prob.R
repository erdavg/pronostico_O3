rm(list = ls())
gc(reset = T)

#Original comentado

library(dplyr)
#library(evola2.1)
library(evola.exp)
library(TSA)
library(forecast)



if (rstudioapi::isAvailable()) { #verificar si la funcion corre en RStudio
  wd_pron <- dirname( rstudioapi::getSourceEditorContext()$path ) # obtener la ruta del script
} else {
  wd_pron <- normalizePath(dirname(commandArgs( # En caso de no estar en RStudio
    trailingOnly = FALSE)[grep("--file=", commandArgs())] %>% 
      sub("--file=", "", .))) 
}

setwd( substr(wd_pron, 1, nchar(wd_pron)-7) ) # Establece directorio
#def_user("jaguilar", servidor = "h165")
evola.exp::def_user(user = "evazquez", servidor = "165")

# Establecer rango de años
year1 <- 2000
year2 <- as.numeric(format(Sys.Date(), "%Y")) - 1 #Obtener anio anterior
years <- c(2001, 2008, 2012, 2018, 2021)

## Datos horarios de O3
o3.h <- evola.exp::redes_con(start = year1, end = year2)

# Datos horarios de O3 temporales
o3.ht <- evola.exp::redes_con(start = year1, end = year2, temporal = T) %>% 
  dplyr::filter(!date %in% o3.h$date) # Esto es solo para comparar si estan las mismas fechas en bases diferentes
# En este caso comparaba las fechas de validados con temporales


# Junta validados con temporales
# Filtra estaciones transporte de 2018 en adelante
# Transporte se quita desde 2017
o3.h <- rbind(o3.h, o3.ht) %>% 
  dplyr::filter(!(as.numeric(format(date, "%Y")) >= 2017 & id_station %in% c("ACO", "AJU", "INN", "MON", "MPA"))) 

# o3.h <- con_rama_con_2("o3", year1, year2)
# o3.ht <- con_rama_con_2("o3", year1, year2, temporal = T) %>% filter(!date %in% o3.h$date)

# o3.h <- rbind(o3.h, o3.ht) %>%
#   filter(!(as.numeric(format(date, "%Y")) >= 2017 & id_station %in% c("ACO", "AJU", "INN", "MON", "MPA")))

## Maximos diarios
o3.d <- o3.h %>%
  dplyr::group_by(date = as.Date(date)) %>%
  dplyr::summarise(valor = max(valor, na.rm = T))


## Conteo de contingencias por mes y año en temporada de ozono - 15 febrero a 15 junio
o3.temp <- o3.d %>%
  dplyr::filter(format(date, "%m") %in% sprintf("%02d", 2:6),
         !format(date, "%m-%d") %in% paste("02", sprintf("%02d", 1:14), sep = "-"),
         !format(date, "%m-%d") %in% paste("06", 16:30, sep = "-")) %>%
  dplyr::group_by(year = format(date, "%Y"), month = factor(format(date, "%B"), c("febrero", "marzo", "abril", "mayo", "junio"))) %>%
  dplyr::summarise(ndias = sum(valor >= 155))

# write.csv(o3.temp, paste0("tablas/contingencias_o3_", year2, ".csv"), row.names = F)
## Convertir a objeto ts (serie de tiempo)


# Conteo Contingencias todo el año
# o3.temp <- o3.d %>%
#   dplyr::group_by(
#     year = as.numeric(format(date, "%Y")),
#     month = as.numeric(format(date, "%m"))
#   ) %>%
#   dplyr::summarise(ndias = sum(valor >= 155, na.rm = TRUE), .groups = "drop") %>%
#   dplyr::arrange(year, month)



# Serie de tiempo

# Serie para temporada de ozono
o3.ts <- ts(o3.temp$ndias, start = c(year1, 1), end = c(year2, 5), frequency = 5)
plot(o3.ts)

# Serie para todo el año
# o3.ts <- ts(o3.temp$ndias, start = c(year1, 1), end = c(year2, 12), frequency = 12)
# plot(o3.ts)


## Modelo auto ARIMA y pronostico

# Ajusta el modelo de ARIMA, busca automaticamente el mejor modelo (autoregresivo "AR", diferenciacion "I", media movil "MA")
# Seasonal T considera estacionalidad
# AR(1) usa el valor anterior
# I (0) no necesita diferenciar (ya es estacionara)
# MA(1) usa el error anterior
# Parte estacional (2,1,0)[5] <- 5 periodos, D=1 diferencia estacional (quita patron anual)
#SAR(2) usa 2 rezagos estacionales (años pasados)
# ar1 ≈ 1 la serie depende mucho del valor anterior
# ma1 negativo -> corrige errores anteriores
# sar1, sar2 negativos -> hay efecto fuerte de temporadas pasadas
# sigma^2 = 9.425 (varianza del error) mas bajo = mejor ajuste. Aqui es moderado
#AIC y BIC mas bajo = mejor, auto.arima() elijio este porque miniiza esto
# ME ≈ 0 no hay sesgo
# Error promedio 2.96 unidades
# MAE 2.17 error absoluto promedio
# ACF1 = -0.045 residuales casi no correlacionados (bueno)
# Resumen el modelo ajusta razonablemnete bien
# el modelo funciona pero no es robusto, intentar observar patrones con todos los meses
o3.mod.ts <- auto.arima(o3.ts, seasonal = T)

summary(o3.mod.ts)
checkresiduals(o3.mod.ts)

#Temp ozono
#pronostico <- forecast(o3.mod.ts, h = 5, level = c(77, 92))

# Se hace el pronostico con el modelo, con intervalo de confianza de 77-92% 
# el argumento "h" es el horizonte de pronostico
# pronostica los proximos 5 periodos (febrero, marzo, abril, mayo, junio)

pronostico <- forecast(o3.mod.ts, h = 5, level = c(77, 92))

## Ajuste del pronostico
if(min(pronostico$mean) < 0){
  aju <- round(min(pronostico$mean))
  pronostico$mean <- round(pronostico$mean) - aju
  pronostico$fitted <- pronostico$fitted - aju
}
plot(pronostico)

## Tabla de pronostico de dias por mes
pron.dias <- data.frame(month = factor(c("febrero","marzo","abril","mayo","junio")),
                        min = pronostico$mean,
                        p_80 = round(pronostico$upper[,1]),
                        p_95 = round(pronostico$upper[,2]))
  


### Calculo de probabilidades con años seleccionados con enos
o3.temp <- merge(o3.temp, pron.dias, by = "month")

probs <- o3.temp %>%
  dplyr::filter(year %in% years) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(prob_80 = sum(ndias <= first(p_80))/n(),
            prob_95 = sum(ndias <= first(p_95))/n())


## Tabla final con el pronostico de dias por mes
tabla.exp <- data.frame(mes = as.character(pron.dias$month),
                        Minimo = as.integer(round(pron.dias$min)),
                        Promedio = as.integer(round(pron.dias$p_80*probs$prob_80)),
                        Maximo = as.integer(round(pron.dias$p_95*probs$prob_95)))

total <- tabla.exp %>% 
  dplyr::summarise(Minimo = sum(Minimo),
                   Promedio = sum(Promedio),
                   Maximo = sum(Maximo)) %>% 
  dplyr::mutate(mes = "Total") %>% 
  dplyr::relocate(mes)

tabla.exp <- rbind(tabla.exp, total)

# selection_meses <- read.csv("contingencias_o3_2025.csv") %>% 
#   dplyr::filter(year == 2025) %>% 
#   dplyr::select(-year) %>% 
#   dplyr::rename(mes = month, obs = ndias)
# 
# tabla.exp <- left_join(tabla.exp, selection_meses)
# tabla.exp[6, 5] <- sum(tabla.exp$obs, na.rm = T)
# 
# write.csv(tabla.exp, paste0("evaluacion_pronost_", year2+1, "_prob.csv"), row.names = F)

write.csv(tabla.exp, paste0("tablas/pronostico_prob_", year2+1, "_prob.csv"), row.names = F)
