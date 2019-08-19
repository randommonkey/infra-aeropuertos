library(tidyverse)
library(hgchmagic)

a0 <- read_csv2("data/aeropuertos_historico_2004_data.csv")
a1 <- read_csv2("data/aeropuertos_historico_2004.csv")
a2 <- a1
names(a2) <- a2[3, ]
a2 <- a2[-c(1:3), ]
write_csv(a2, "data/aeropuertos_historico_2004_data.csv")
# pruebas concordancia
n_distinct(a2$`Nombre Empresa`) == n_distinct(a2$`Sigla Empresa`) 

n_distinct(a2$`Continente Origen`) < n_distinct(a2$`Pais Origen`) < n_distinct(a2$`Ciudad Origen`) < n_distinct(a2$Origen)

n_distinct(a2$Destino) < n_distinct(a2$Origen)

# algunas gráficas
# 3140 trayectos distintos
# 111 empresas distintas
# por trayecto
c0 <- paste(a2$Origen, "-", a2$Destino)
d0 <- data.frame(a = c0, b = a2$Vuelos, stringsAsFactors = FALSE)
d0$b <- as.numeric(d0$b)
hgch_bar_CatNum(d0, sort = "desc", sliceN = 100)

d1 <- data.frame(a = c0, b = a2$`Carga Ofrecida Kg`, stringsAsFactors = FALSE)
d1$b <- as.numeric(d1$b)
hgch_bar_CatNum(d1, sort = "desc", sliceN = 100, agg = "mean")

d2 <- data.frame(a = c0, b = a2$Sillas, stringsAsFactors = FALSE)
d2$b <- as.numeric(d2$b)
hgch_bar_CatNum(d2, sort = "desc", sliceN = 100, agg = "mean")

# por empresa
c1 <- paste(a2$`Sigla Empresa`, "-", a2$`Nombre Empresa`)
d3 <- data.frame(a = c1, b = a2$Vuelos, stringsAsFactors = FALSE)
d3$b <- as.numeric(d3$b)
hgch_bar_CatNum(d3, sort = "desc", orientation = "hor", labelWrap = 34)

d4 <- data.frame(a = c1, b = a2$`Carga Ofrecida Kg`, stringsAsFactors = FALSE)
d4$b <- as.numeric(d8$b)
hgch_bar_CatNum(d4, sort = "desc", orientation = "hor", labelWrap = 34, agg = "mean")

d5 <- data.frame(a = c1, b = a2$Sillas, stringsAsFactors = FALSE)
d5$b <- as.numeric(d5$b)
hgch_bar_CatNum(d5, sort = "desc", sliceN = 100, agg = "mean")

# por tráfico
d6 <- data.frame(a = a2$Trafico, b = a2$Vuelos, stringsAsFactors = FALSE)
d6$b <- as.numeric(d6$b)
hgch_bar_CatNum(d6, sort = "desc", orientation = "hor")

d7 <- data.frame(a = a2$Trafico, b = a2$`Carga Ofrecida Kg`, stringsAsFactors = FALSE)
d7$b <- as.numeric(d7$b)
hgch_bar_CatNum(d7, sort = "desc", agg = "mean")

d8 <- data.frame(a = a2$Trafico, b = a2$Sillas, stringsAsFactors = FALSE)
d8$b <- as.numeric(d8$b)
hgch_bar_CatNum(d8, sort = "desc", agg = "mean")

# por tipo de equipo
d9 <- data.frame(a = a2$`Tipo de Equipo`, b = a2$Vuelos, stringsAsFactors = FALSE)
d9$b <- as.numeric(d9$b)
hgch_bar_CatNum(d9, sort = "desc", sliceN = 100, orientation = TRUE)

d10 <- data.frame(a = a2$`Tipo de Equipo`, b = a2$`Carga Ofrecida Kg`, stringsAsFactors = FALSE)
d10$b <- as.numeric(d10$b)
hgch_bar_CatNum(d10, sort = "desc", sliceN = 100, agg = "mean")

d11 <- data.frame(a = a2$`Tipo de Equipo`, b = a2$Sillas, stringsAsFactors = FALSE)
d11$b <- as.numeric(d11$b)
hgch_bar_CatNum(d11, sort = "desc", sliceN = 100, agg = "mean")

# cuál agreganción cuando
# top 10, 5 trayectos con mayor número de vuelos y carga a lo largo de los años
# top 10, 5 empresas con mayor número de vuelos y carga a lo largo de los años
# Número de vuelos y carga por tráfico< a lo largo de los años


