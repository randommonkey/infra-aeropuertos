library(tidyverse)
library(hgchmagic)
library(htmlwidgets)


b0 <- read_csv2("data/aeropuertos_data.csv")
b0$`gcd_municipal,,,` <- gsub(",,,$", "", b0$`gcd_municipal,,,`)
names(b0)[23] <- "gcd_municipal"

h0 <- read_csv2("data/aeropuertos_historico_data.csv")
h0$`carga_bordo,,` <- gsub(",,$", "", h0$`carga_bordo,,`)
names(h0)[13] <- "carga_bordo"


# variable tipo ACLARAR con cris...

# tipo por categoría o al revés
hgch_bar_CatCat(b0[, c("categoria", "tipo")])
hgch_bar_CatCat(b0[, c("categoria", "tipo")], graphType = "stacked")
hc0 <- hgch_bar_CatCat(b0[, c("tipo", "categoria")], 
                       dropNaV = c(TRUE, TRUE),
                       horLabel = "CATEGORÍA",
                       verLabel = "CANTIDAD",
                       title = "NÚMERO DE AEROPUERTOS POR CATEGORÍA Y USO",
                       tooltip = list(headerFormat = NULL,
                                      pointFormat = "Categoría: <b> {point.category} </b><br/> Uso: <b> {series.name} </b><br/> Cantidad: <b> {point.y} </b>")) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        fontSize = "10px")) %>%
  hc_title(style = list(color = "#FFFFFF", 
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>% 
  hc_yAxis(gridLineColor = "#475e92",
           title = list(style = list(color = "#FFFFFF", 
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"))) %>%
  hc_xAxis(lineColor = "#FFFFFF",
           lineWidth = 0.7,
           title = list(style = list(color = "#FFFFFF", 
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"))) %>% 
  hc_legend(itemStyle = list(color = "#ffffff",
                             fontFamily = "Raleway",
                             fontWeight = "normal",
                             fontSize = "10px")) %>%
  hc_add_theme(tma(custom = list(showText = FALSE,
                                 colors = c("#00b2e2", "#124577", "#ffffff", "#2a7dd1", "#00e0ca"), 
                                 background = "transparent"))) 

# leyenda blanca y sin bold
saveWidget(hc0, "gf0.html", selfcontained = TRUE, background = "transparent")


# coropleta de aeródromos y aeropuertos por departamento (dos)
# número de aeródromos y el resto ¿los llamamos aeropuertos?
b1 <- b0 %>%
  filter(!categoria %in% "Aeródromo") %>%
  group_by(departamento) %>%
  summarise(b = n())
b1$departamento <- gsub("^Bogotá,D\\.C\\.", "Bogota", b1$departamento)
b1 <- rbind(b1, c("Cundinamarca", 0))
b1$b <- as.numeric(b1$b)

hc1 <- hcmap("countries/co/co-all",
             data = b1,
             value = "b", 
             borderColor = "#ffffff",
             joinBy = list("name", "departamento"),
             allowPointSelect = TRUE,
             cursor = "pointer",
             stroke = "#e08674",
             states = list(hover = list(stroke = "#662d91")),
             tooltip = list(headerFormat = "",
                            pointFormat = "{point.name}: <b> {point.b} </b>")) %>%
  hc_colorAxis(labels = list(format = "{value}")) %>%
  hc_legend(align = "left",
            layout = "vertical",
            verticalAlign = "middle",
            title = list(text = "Cantidad")) %>%
  hc_colorAxis(minColor = "#00b2e2", maxColor = "#1f3248") %>%
  hc_credits(enabled = FALSE) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        background = "transparent")) %>%
  hc_title(text = "NÚMERO DE AEROPUERTOS POR DEPARTAMENTO",
           align = "left",
           style = list(fontSize = "16px",
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800"))

saveWidget(hc1, "gf1.html", selfcontained = TRUE, background = "transparent")

# aeródromos por departamento
b2 <- b0 %>%
  filter(categoria %in% "Aeródromo") %>%
  group_by(departamento) %>%
  summarise(b = n())
# ¿dejamos en blanco donde no hay?
b2 <- rbind(b2, c("Bogota", 0))
b2$b <- as.numeric(b2$b)


hc2 <- hcmap("countries/co/co-all",
             data = b2,
             value = "b", 
             borderColor = "#ffffff",
             joinBy = list("name", "departamento"),
             allowPointSelect = TRUE,
             cursor = "pointer",
             stroke = "#e08674",
             states = list(hover = list(stroke = "#662d91")),
             tooltip = list(headerFormat = "",
                            pointFormat = "{point.name}: <b> {point.b} </b>")) %>%
  hc_colorAxis(labels = list(format = "{value}")) %>%
  hc_legend(align = "left",
            layout = "vertical",
            verticalAlign = "middle",
            title = list(text = "Cantidad")) %>%
  hc_colorAxis(minColor = "#00b2e2", maxColor = "#1f3248") %>%
  hc_credits(enabled = FALSE) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        background = "transparent")) %>%
  hc_title(text = "NÚMERO DE AERÓDROMOS POR DEPARTAMENTO",
           align = "left",
           style = list(fontSize = "16px",
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800"))

saveWidget(hc2, "gf2.html", selfcontained = TRUE, background = "transparent")


# por año
# número de vuelos, número de sillas, número de pasajeros, carga ofrecida
h1 <- h0[, c("ano", "vuelos", "sillas", "pasajeros", "carga_ofrecida", "trafico")]
h1$ano <- as.character(h1$ano)
h1 <- h1 %>%
  filter(!trafico %in% "E")
h2 <- h1 %>%
  group_by(ano) %>%
  summarise(vuelos = sum(vuelos, na.rm = TRUE),
            sillas = sum(sillas, na.rm = TRUE),
            pasajeros = sum(pasajeros, na.rm = TRUE),
            carga_ofrecida = sum(carga_ofrecida, na.rm = TRUE))
h3 <- h2 %>% 
  gather("categories", "count", names(h2)[-1])

mean(h2$vuelos)
hgch_line_CatNum(h2[, c(1, 2)])

h1 <- h0 %>%
  filter(trafico %in% c("I", "N"))
h1$trafico <- gsub("I", "Internacional", h1$trafico)
h1$trafico <- gsub("N", "Nacional", h1$trafico)

hc3 <- hgch_line_CatCatNum(h1[, c("trafico", "ano", "vuelos")], 
                           dropNaV = c(TRUE, TRUE),
                           horLabel = "AÑO",
                           verLabel = "VUELOS",
                           title = "NÚMERO DE VUELOS POR TIPO DE TRÁFICO",
                           tooltip = list(headerFormat = NULL,
                                          pointFormat = "Año: <b> {point.category} </b><br/> Tipo de tráfico: <b> {series.name} </b><br/> Número de vuelos: <b> {point.y} </b>")) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        fontSize = "10px")) %>%
  hc_yAxis(gridLineColor = "#475e92",
           title = list(style = list(color = "#FFFFFF",
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"))) %>%
  hc_xAxis(lineColor = "#FFFFFF",
           lineWidth = 0.7,
           title = list(style = list(color = "#FFFFFF",
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"))) %>% 
  hc_title(style = list(color = "#FFFFFF",
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>% 
  hc_legend(itemStyle = list(color = "#FFFFFF",
                             fontSize = "10px",
                             fontFamily = "Raleway",
                             fontWeight = "normal"),
            itemHoverStyle = list(color = "#ffffff")) %>%
  hc_add_theme(tma(custom = list(showText = FALSE,
                                 colors = c("#1f3248", "#2a7dd1"), 
                                 background = "transparent")))

saveWidget(hc3, "gf3.html", selfcontained = TRUE, background = "transparent")

# número de pasajeros a lo largo de los años
mean(h2$pasajeros)
hgch_line_CatNum(h2[, c(1, 4)])

hc4 <- hgch_line_CatNum(h1[, c("ano", "pasajeros")], 
                        dropNaV = c(TRUE, TRUE),
                        horLabel = "AÑO",
                        verLabel = "PASAJEROS",
                        title = "NÚMERO DE PASAJEROS A LO LARGO DE LOS AÑOS",
                        tooltip = list(headerFormat = NULL,
                                       pointFormat = "Año: <b> {point.category} </b><br/> Número de pasajeros: <b> {point.y} </b>")) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        fontSize = "10px")) %>%
  hc_yAxis(gridLineColor = "#475e92",
           title = list(style = list(color = "#FFFFFF",
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"))) %>%
  hc_xAxis(lineColor = "#FFFFFF",
           lineWidth = 0.7,
           title = list(style = list(color = "#FFFFFF",
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"))) %>% 
  hc_title(style = list(color = "#FFFFFF",
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>%
  hc_add_theme(tma(custom = list(showText = FALSE,
                                 colors = "#00b2e2", 
                                 background = "transparent")))

saveWidget(hc4, "gf4.html", selfcontained = TRUE, background = "transparent")


# rutas ¿cúantas distintas? HACERLO
# 14597 trayectos distintos
h20 <- data.frame("ruta" = paste0(h0$origen, " - ", h0$destino),
                  "vuelos" = h0$vuelos,
                  "trafico" = h0$trafico, 
                  stringsAsFactors = FALSE) 
h30 <- h20 %>%
  filter(trafico %in% c("I", "N")) %>%
  group_by(ruta, trafico) %>%
  summarise(vuelos = sum(vuelos, na.rm = TRUE))

# RUTAS DISTINTAS
c0 <- c()

map(1:nrow(h0), function(o) {
  o0 <- paste0(h0$origen[o], " - ", h0$destino[o])
  o1 <- paste0(h0$destino[o], " - ", h0$origen[o])
  if (!o0 %in% c0 & !o1 %in% c0) {
    c0 <<- c(c0, o0)
  }
})

# 9998 rutas distintas

# rutas internacionales con mayor número de vuelos
m1 <- h20 %>%
  filter(trafico %in% c("I"))
m1 <-  data.frame("ruta" = c("BOG - MIA", "BOG - PTY", "BOG - UIO", "BOG - CCS",
                             "MDE - MIA", "BOG - LIM", "BOG - MEX", 
                             "MDE - PTY", "CLO - PTY", "BOG - MAD"),
                  "vuelos" = c(65509, 53502, 34773, 32671,
                               21731, 32787, 28904,
                               22916, 16404, 16134),
                  "label" = c("Aeropuerto Internacional <br/> El Dorado (Bogotá, Colombia) - <br/> Aeropuerto Internacional <br/> de Miami (Miami, Estados Unidos)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Colombia) - <br/> Aeropuerto Internacional <br/> de Tocumen (Ciudad de Panamá, Panamá)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Colombia) - <br/> Aeropuerto Internacional <br/> Mariscal Sucre (Tababela, Ecuador)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Colombia) - <br/> Aeropuerto Internacional <br/> de Maiquetía Simón Bolívar (Maiquetía)",
                              "Aeropuerto Internacional <br/> José María Córdova (Rionegro, Colombia) - <br/> Aeropuerto Internacional <br/> de Miami (Miami, Estados Unidos)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Colombia) - <br/> Aeropuerto Internacional <br/> Jorge Chávez (Lima, Perú)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Colombia) - <br/> Aeropuerto Internacional <br/> de la Ciudad de México (Ciudad de México, México)",
                              "Aeropuerto Internacional <br/> José María Córdova (Rionegro, Colombia) - <br/> Aeropuerto> Internacional <br/> de Tocumen (Ciudad de Panamá, Panamá)",
                              "Aeropuerto Internacional <br/> Alfonso Bonilla Aragón (Palmira, Colombia) - <br/> Aeropuerto> Internacional <br/> de Tocumen (Ciudad de Panamá, Panamá)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Colombia) - <br/> Aeropuerto Adolfo Suárez <br/> Madrid-Barajas (Madrid, España)"),
                  stringsAsFactors = FALSE) %>%
  arrange(desc(vuelos))
hc5 <- f0(m1, sort = "desc",
                       horLabel = "RUTA",
                       verLabel = "VUELOS",
                       title = "RUTAS INTERNACIONALES CON MAYOR NÚMERO DE VUELOS",
                       tooltip = list(headerFormat = NULL,
                                      pointFormat = "{point.label} <br/><b> {point.y} </b>")) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        fontSize = "10px")) %>%
  hc_title(style = list(color = "#FFFFFF", 
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>% 
  hc_yAxis(gridLineColor = "#475e92",
           title = list(text = "VUELOS",
                        style = list(color = "#FFFFFF", 
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"))) %>%
  hc_xAxis(lineColor = "#FFFFFF",
           lineWidth = 0.7,
           title = list(text ="RUTA",
                        style = list(color = "#FFFFFF", 
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"),
                         text = list("A", "Ñ", "d"))) %>% 
  hc_add_theme(tma(custom = list(showText = FALSE,
                                 colors = "#1f3248", 
                                 background = "transparent")))

saveWidget(hc5, "gf5.html", selfcontained = TRUE, background = "transparent")


# rutas nacionales con mayor número de vuelos
m2 <- h20 %>%
  filter(trafico %in% c("N"))

m2 <-  data.frame("ruta" = c("BOG - MDE", "BOG - CLO", "BOG - CTG", "BOG - BAQ",
                             "BOG - BGA", "BOG - PEI", "BOG - NVA", 
                             "BOG - EYP", "UIB - EOH", "BOG - SMR"),
                  "vuelos" = c(177598, 158453, 112194, 90484,
                               74045, 65788, 56238,
                               55242, 54553, 53779),
                  "label" = c("Aeropuerto Internacional <br/> El Dorado (Bogotá, Cundinamarca) - <br/> Aeropuerto Internacional <br/> José María Córdova (Rionegro, Antioquia)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Cundinamarca) - <br/> Aeropuerto Internacional <br/> Alfonso Bonilla Aragón (Palmira, Valle del Cauca)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Cundinamarca) - <br/> Aeropuerto Internacional <br/> Rafael Núñez (Cartagena de Indias, Bolívar)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Cundinamarca) - <br/> Aeropuerto Internacional <br/> Ernesto Cortissoz (Soledad, Atlántico)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Cundinamarca) - <br/> Aeropuerto Internacional <br/> Palonegro (Lebrija, Santander)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Cundinamarca) - <br/> Aeropuerto Internacional <br/> Matecaña (Pereira, Risaralda)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Cundinamarca) - <br/> Aeropuerto Benito <br/> Salas (Neiva, Huila)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Cundinamarca) - <br/> Aeropuerto El Acaraván (Yopal, Casanare)",
                              "Aeropuerto El Caraño (Quibdó, Chocó) - <br/> Aeropuerto Olaya Herrea (Medellín, Antioquia)",
                              "Aeropuerto Internacional <br/> El Dorado (Bogotá, Cundinamarca) - <br/> Aeropuerto Internacional <br/> Simón Bolívar (Santa Marta, Magdalena)"),
                  stringsAsFactors = FALSE) 

hc6 <- f0(m2, sort = "desc",
                       horLabel = "RUTA",
                       verLabel = "VUELOS",
                       title = "RUTAS NACIONALES CON MAYOR NÚMERO DE VUELOS",
                       tooltip = list(headerFormat = NULL,
                                      pointFormat = "{point.label} <br/> <b> {point.y} </b>")) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        fontSize = "10px")) %>%
  hc_title(style = list(color = "#FFFFFF", 
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>% 
  hc_yAxis(gridLineColor = "#475e92",
           title = list(style = list(color = "#FFFFFF", 
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"))) %>%
  hc_xAxis(lineColor = "#FFFFFF",
           lineWidth = 0.7,
           title = list(style = list(color = "#FFFFFF", 
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"))) %>% 
  hc_add_theme(tma(custom = list(showText = FALSE,
                                 colors = "#1f3248", 
                                 background = "transparent")))

saveWidget(hc6, "gf6.html", selfcontained = TRUE, background = "transparent")

m0 <- data.frame("ruta" = paste0(h0$origen, " - ", h0$destino),
                 "vuelos" = h0$vuelos,
                 "trafico" = h0$trafico, 
                 stringsAsFactors = FALSE) 
m1 <- m0 %>%
  filter(trafico %in% c("N"))

m2 <- m1 %>%
  group_by(ruta) %>%
  summarise(vuelos = sum(vuelos, na.rm = TRUE))

hgch_bar_CatNum(m2, sort = "desc", sliceN = 20)

m2 <- m0 %>%
  filter(trafico %in% c("N"))
hgch_bar_CatNum(m2[, 1:2], sort = "desc", sliceN = 20)




# treemap vuelos nacionales por empresa
s0 <- h0[, c("empresa", "vuelos", "trafico")]

s1 <- s0 %>%
  filter(trafico %in% c("N")) %>%
  group_by(empresa) %>%
  summarise(vuelos = sum(vuelos, na.rm = TRUE)) %>%
  arrange(desc(vuelos)) %>%
  slice(1:30)

hc7 <- hgch_treemap_CatNum(s1,
                           title = "AEROLÍNEAS CON MAYOR NÚMERO DE VUELOS NACIONALES",
                           colors = c("#00b2e2", "#1f3248"),
                           tooltip = list(headerFormat = NULL,
                                          pointFormat = "{point.name}: <b> {point.value}</b>")) %>%
  hc_chart(style = list(fontFamily = "Raleway")) %>%
  hc_legend(enabled = FALSE) %>%
  hc_title(style = list(color = "#1f3248", 
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>% 
  hc_labels(text = list(style = list(color = "red"))) %>%
  hc_add_theme(tma(custom = list(background = "transparent"))) %>%
  hc_plotOptions(series = list(dataLabels = list(style = list(fontFamily = "Raleway",
                                                              fontWeight = "normal",
                                                              fontSize = "9px"))))


saveWidget(hc7, "gf7.html", selfcontained = TRUE, background = "transparent")


# treemap vuelos internacionales por empresa
s2 <- s0 %>%
  filter(trafico %in% c("I")) %>%
  group_by(empresa) %>%
  summarise(vuelos = sum(vuelos, na.rm = TRUE)) %>%
  arrange(desc(vuelos)) %>%
  slice(1:30)

hc8 <- hgch_treemap_CatNum(s2,
                           title = "AEROLÍNEAS CON MAYOR NÚMERO DE VUELOS INTERNACIONALES",
                           colors = c("#00b2e2", "#1f3248"),
                           tooltip = list(headerFormat = NULL,
                                          pointFormat = "{point.name}: <b> {point.value} COP</b>")) %>%
  hc_chart(style = list(fontFamily = "Raleway")) %>%
  hc_legend(enabled = FALSE) %>%
  hc_title(style = list(color = "#1f3248", 
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>% 
  hc_add_theme(tma(custom = list(background = "transparent"))) %>%
  hc_plotOptions(series = list(dataLabels = list(style = list(fontFamily = "Raleway",
                                                              fontWeight = "normal",
                                                              fontSize = "9px"))))


saveWidget(hc8, "gf8.html", selfcontained = TRUE, background = "transparent")


h1 <- h0 %>%
  filter(!trafico %in% "E")
write_csv(h1, "data/aeropuertos_historico_colombia_data.csv")




















f0 <- function (data, title = NULL, subtitle = NULL, caption = NULL, 
                horLabel = NULL, verLabel = NULL, horLine = NULL, horLineLabel = " ", 
                verLine = NULL, verLineLabel = " ", labelWrap = 12, colors = NULL, 
                colorScale = "no", agg = "sum", agg_text = NULL, orientation = "ver", 
                marks = c(".", ","), nDigits = NULL, dropNa = FALSE, highlightValueColor = "#F9B233", 
                percentage = FALSE, prefix = NULL, suffix = NULL, highlightValue = NULL, 
                order = NULL, sort = "no", sliceN = NULL, showText = TRUE, 
                tooltip = list(headerFormat = NULL, pointFormat = NULL), 
                export = FALSE, theme = NULL, lang = "es", ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  title <- title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  prefix_agg <- ifelse(is.null(agg_text), agg, agg_text)
  labelsXY <- orientationXY(orientation, x = nms[1], y = ifelse(nrow(d) == 
                                                                  dplyr::n_distinct(d$a), nms[2], paste(prefix_agg, nms[2])), 
                            hor = horLabel, ver = verLabel)
  lineXY <- linesOrientation(orientation, horLine, verLine)
  lineLabelsXY <- linesOrLabel(orientation, horLineLabel, verLineLabel)
  if (colorScale == "discrete") {
    colorDefault <- c("#FECA84", "#3DB26F", "#74D1F7", "#F75E64", 
                      "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    colorDefault <- discreteColorSelect(colorDefault, d)
  }
  else {
    colorDefault <- (leaflet::colorNumeric(c("#53255E", "#ff4097"), 
                                           1:length(unique(d$a))))(1:length(unique(d$a)))
  }
  if (!is.null(colors)) {
    colors <- unname(fillColors(d, "a", colors, colorScale))
  }
  else {
    if (colorScale == "no") {
      colors <- c("#FECA84", "#FECA84")
    }
    else {
      colors <- colorDefault
    }
  }
  if (dropNa) 
    d <- d %>% tidyr::drop_na()
  # d <- d %>% tidyr::replace_na(list(a = ifelse(is.character(d$a), 
  #                                              "NA", NA), b = NA)) %>% dplyr::group_by(a) %>% dplyr::summarise(b = agg(agg, 
                                                                                                                       # b))
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- "NA"
  if (is.null(nDigits)) {
    nDig <- 0
  }
  else {
    nDig <- nDigits
  }
  if (percentage) {
    d$b <- (d[["b"]] * 100)/sum(d[["b"]], na.rm = TRUE)
  }
  d$b <- round(d$b, nDig)
  d <- orderCategory(d, "a", order, labelWrap)
  d <- sortSlice(d, "b", sort, sliceN)
  d <- d %>% plyr::rename(c(b = "y"))
  d$color <- NA
  if (!is.null(highlightValue)) {
    w <- which(d$a %in% highlightValue)
    d$color[w] <- highlightValueColor
  }
  
  data <- list()
  bla <- map(1:nrow(d), function(z) {
    data$data[[z]] <<- list(name = d$a[z], y = d$y[z], color = as.character(d$color[z]), label = as.character(d$c[z]))
  })
  formatLabAxis <- paste0("{value:", marks[1], marks[2], "f}")
  if (!is.null(nDigits)) {
    formatLabAxis <- paste0("{value:", marks[1], marks[2], 
                            nDigits, "f}")
  }
  if (is.null(format)) {
    prefix = ""
    suffix = ""
  }
  aggFormAxis <- "function() {return this.value+\"\";}"
  if (percentage) {
    aggFormAxis <- "function() {return this.value+\"%\";}"
    suffix <- "%"
  }
  aggFormAxis <- paste0("function() { return '", prefix, "' + Highcharts.numberFormat(this.value, ", 
                        nDig, ", '", marks[2], "', '", marks[1], "') + '", suffix, 
                        "'}")
  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <- paste0("<b>{point.name}</b><br/>", 
                                  paste0(agg, " ", nms[2], ": "), prefix, "{point.y}", 
                                  suffix)
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- ""
  }
  global_options(marks[1], marks[2])
  exportLang(language = lang)
  hc <- highchart() %>% hc_chart(type = ifelse(orientation == 
                                                 "hor", "bar", "column")) %>% hc_title(text = title) %>% 
    hc_subtitle(text = subtitle) %>% hc_tooltip(useHTML = TRUE, 
                                                pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>% 
    hc_xAxis(title = list(text = labelsXY[1]), plotLines = list(list(value = lineXY[2], 
                                                                     color = "black", dashStyle = "shortdash", zIndex = 5, 
                                                                     width = 2, label = list(text = lineLabelsXY[1], style = list(color = "black")))), 
             type = "category") %>% hc_yAxis(title = list(text = labelsXY[2]), 
                                             plotLines = list(list(value = lineXY[1], color = "black", 
                                                                   dashStyle = "shortdash", width = 2, zIndex = 5, label = list(text = lineLabelsXY[2], 
                                                                                                                                style = list(color = "black")))), labels = list(format = formatLabAxis, 
                                                                                                                                                                                formatter = JS(aggFormAxis))) %>%
    hc_series(data) %>% 
    hc_credits(enabled = TRUE, text = caption) %>% hc_legend(enabled = FALSE)
  if (export) {
    hc <- hc %>% hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = list("printChart", 
                                                                                                   "downloadJPEG", "downloadPNG", "downloadSVG", "downloadPDF"))))
  }
  if (is.null(theme)) {
    hc <- hc %>% hc_add_theme(tma(custom = list(showText = showText, 
                                                colores = colors)))
  }
  else {
    hc <- hc %>% hc_add_theme(theme)
  }
  if (showText) {
    hc <- hc %>% hc_plotOptions(bar = list(dataLabels = list(format = paste0(prefix, 
                                                                             "{y}", suffix))), column = list(dataLabels = list(format = paste0(prefix, 
                                                                                                                                               "{y}", suffix))))
  }
  hc
}

