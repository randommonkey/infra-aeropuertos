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
                       opts = list(dropNaV = c(TRUE, TRUE),
                                   horLabel = "CATEGORÍA",
                                   verLabel = "CANTIDAD",
                                   title = "NÚMERO DE AEROPUERTOS POR CATEGORÍA Y USO",
                                   tooltip = list(headerFormat = NULL,
                                                  pointFormat = "Categoría: <b> {point.category} </b><br/> Uso: <b> {series.name} </b><br/> Cantidad: <b> {point.y} </b>"))) %>%
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
                           opts = list(
                             marks = c(",", "."),
                             dropNaV = c(TRUE, TRUE),
                             horLabel = "AÑO",
                             verLabel = "VUELOS",
                             title = "NÚMERO DE VUELOS POR TIPO DE TRÁFICO",
                             tooltip = list(headerFormat = NULL,
                                            pointFormat = "Año: <b> {point.category} </b><br/> Tipo de tráfico: <b> {series.name} </b><br/> Número de vuelos: <b> {point.y} </b>"))) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        fontSize = "10px")) %>%
  hc_yAxis(gridLineColor = "#475e92",
           title = list(style = list(color = "#FFFFFF",
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF"),
                         formatter = JS(paste0("function() { return Highcharts.numberFormat(this.value, ",
                                               0, ", '.', ',')}"))
           )) %>%
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
                             fontWeight = "norm3al"),
            itemHoverStyle = list(color = "#ffffff")) %>%
  hc_add_theme(tma(custom = list(showText = FALSE,
                                 colors = c("#1f3248", "#2a7dd1"), 
                                 background = "transparent")))

saveWidget(hc3, "gf3.html", selfcontained = TRUE, background = "transparent")

# número de pasajeros a lo largo de los años
# internacional, nacional, total
h20 <- h1
h20$trafico <- gsub("^I$", "Internacional", h20$trafico)
h20$trafico <- gsub("^N$", "Nacional", h20$trafico)
h20 <- h20 %>%
  group_by(ano, trafico) %>%
  summarise(pasajeros = sum(pasajeros, na.rm = TRUE))
h21 <- h20 %>%
  group_by(ano) %>%
  summarise(pasajeros = sum(pasajeros, na.rm = TRUE))
h21$trafico <- "Total"
h21 <- h21[, c(1, 3, 2)]
h23 <- rbind.data.frame(h20, h21)
h23 <- h23[, c(2, 1, 3)]
h23$pasajeros <- h23$pasajeros / 1000000

mean(h2$pasajeros)
hgch_line_CatNum(h2[, c(1, 4)])

hc40 <- hgch_line_CatCatNum(h1[, c("ano", "pasajeros")], 
                            opts = list(dropNaV = c(TRUE, TRUE),
                                        horLabel = "AÑO",
                                        verLabel = "PASAJEROS",
                                        title = "NÚMERO DE PASAJEROS A LO LARGO DE LOS AÑOS",
                                        tooltip = list(headerFormat = NULL,
                                                       pointFormat = "Año: <b> {point.category} </b><br/> Número de pasajeros: <b> {point.y} </b>"))) %>%
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

hc41 <- f1(h23, 
           opts = list(dropNaV = c(TRUE, TRUE),
                       horLabel = "AÑO",
                       marks = c(",", "."),
                       verLabel = "PASAJEROS (MILLONES)",
                       title = "NÚMERO DE PASAJEROS A LO LARGO DE LOS AÑOS",
                       tooltip = list(headerFormat = NULL,
                                      pointFormat = "<b> {series.name} </b><br/> Año: <b> {point.category} </b><br/> Número de pasajeros (millones): <b> {point.y:.,0f} </b>"))) %>%
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
                                 colors = c("#ffffff", "#00b2e2", "#00e0ca"), 
                                 background = "transparent")))

saveWidget(hc41, "gf40.html", selfcontained = TRUE, background = "transparent")

#########################
# kg de carga a lo largo de los años (¿CARGA OFRECIDA, CARGA BORDO?)
# internacional, nacional, total
h200 <- h1
h200$trafico <- gsub("^I$", "Internacional", h200$trafico)
h200$trafico <- gsub("^N$", "Nacional", h200$trafico)
h200 <- h200 %>%
  group_by(ano, trafico) %>%
  summarise(carga = sum(as.numeric(carga_bordo), na.rm = TRUE))
h210 <- h200 %>%
  group_by(ano) %>%
  summarise(carga = sum(carga, na.rm = TRUE))
h210$trafico <- "Total"
h210 <- h210[, c(1, 3, 2)]
h230 <- rbind.data.frame(h200, h210)
h230 <- h230[, c(2, 1, 3)]
h230$carga <- h230$carga / 1000


hgch_line_CatNum(h230[, c(2, 3)])

hc410 <- f1(h230,
            opts = list(dropNaV = c(TRUE, TRUE),
                        horLabel = "AÑO",
                        marks = c(",", "."),
                        verLabel = "CARGA (TONELADAS)",
                        title = "TONELADAS DE CARGA A BORDO A LO LARGO DE LOS AÑOS",
                        tooltip = list(headerFormat = NULL,
                                       pointFormat = "<b> {series.name} </b><br/> Año: <b> {point.category} </b> <br/> Carga a bordo: <b> {point.y} tn</b>"))) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        fontSize = "10px")) %>%
  hc_yAxis(gridLineColor = "#475e92",
           title = list(style = list(color = "#FFFFFF",
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF",
                                      formatter = JS(paste0("function() { return Highcharts.numberFormat(this.value, ",
                                                            0, ", '.', ',')}"))))) %>%
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
                                 colors = c("#ffffff", "#00b2e2", "#00e0ca"), 
                                 background = "transparent")))

saveWidget(hc410, "gf4111.html", selfcontained = TRUE, background = "transparent")

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

h40 <- h0 %>%
  filter(ano %in% 2018,
         trafico %in% c("I", "N"))
# RUTAS DISTINTAS -- h0 (2004 - 2018; nacionales, internacionales, exterior) -- h40 (2018; nacionales, internacionales) 
c0 <- c()
dt0 <- h40
map(1:nrow(dt0), function(o) {
  o0 <- paste0(dt0$origen[o], " - ", dt0$destino[o])
  o1 <- paste0(dt0$destino[o], " - ", dt0$origen[o])
  if (!o0 %in% c0 & !o1 %in% c0) {
    c0 <<- c(c0, o0)
  }
})
length(c0)
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
          marks = c(",", "."),
          horLabel = "RUTA",
          verLabel = "VUELOS",
          title = "RUTAS INTERNACIONALES CON MAYOR NÚMERO DE VUELOS",
          subtitle = "(2004 - 2018)",
          tooltip = list(headerFormat = NULL,
                         pointFormat = "{point.label} <br/><b> {point.y} </b>")) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        fontSize = "10px")) %>%
  hc_title(style = list(color = "#FFFFFF", 
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>%
  hc_subtitle(style = list(color = "#FFFFFF", 
                           fontFamily = "Raleway",
                           fontFamily = "Raleway:800",
                           fontWeight = "800",
                           fontSize = "14px")) %>% 
  hc_yAxis(gridLineColor = "#475e92",
           title = list(text = "VUELOS",
                        style = list(color = "#FFFFFF", 
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF",
                                      formatter = JS(paste0("function() { return Highcharts.numberFormat(this.value, ",
                                                            0, ", '.', ',')}"))))) %>%
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

saveWidget(hc5, "ef5.html", selfcontained = TRUE, background = "transparent")


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
          marks = c(",", "."),
          horLabel = "RUTA",
          verLabel = "VUELOS",
          title = "RUTAS NACIONALES CON MAYOR NÚMERO DE VUELOS",
          subtitle = "(2004 - 2018)",
          tooltip = list(headerFormat = NULL,
                         pointFormat = "{point.label} <br/> <b> {point.y} </b>")) %>%
  hc_chart(style = list(fontFamily = "Raleway",
                        fontSize = "10px")) %>%
  hc_title(style = list(color = "#FFFFFF", 
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>%
  hc_subtitle(style = list(color = "#FFFFFF", 
                           fontFamily = "Raleway",
                           fontFamily = "Raleway:800",
                           fontWeight = "800",
                           fontSize = "14px")) %>% 
  hc_yAxis(gridLineColor = "#475e92",
           title = list(style = list(color = "#FFFFFF", 
                                     fontSize = "12px",
                                     fontFamily = "Raleway",
                                     fontFamily = "Raleway:800",
                                     fontWeight = "800")),
           labels = list(style = list(color = "#FFFFFF",
                                      formatter = JS(paste0("function() { return Highcharts.numberFormat(this.value, ",
                                                            0, ", '.', ',')}"))))) %>%
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

saveWidget(hc6, "ef6.html", selfcontained = TRUE, background = "transparent")

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

hc7 <- f2(s1,
          opts = list(marks = c(",", "."),
                      title = "AEROLÍNEAS CON MAYOR NÚMERO DE VUELOS NACIONALES",
                      colors = c("#00b2e2", "#1f3248"),
                      color_scale = "continuous",
                      tooltip = list(headerFormat = NULL,
                                     pointFormat = "{point.name}: <b> {point.value}</b>"))) %>%
  hc_chart(style = list(fontFamily = "Raleway")) %>%
  hc_legend(enabled = FALSE) %>%
  hc_title(style = list(color = "#1f3248", 
                        fontFamily = "Raleway",
                        fontFamily = "Raleway:800",
                        fontWeight = "800",
                        fontSize = "16px")) %>% 
  hc_add_theme(tma(custom = list(background = "transparent", colors = c("#00b2e2", "#1f3248")))) %>%
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

hc8 <- f2(s2,
          opts = list(marks = c(",", "."),
                      title = "AEROLÍNEAS CON MAYOR NÚMERO DE VUELOS INTERNACIONALES",
                      colors = c("#00b2e2", "#1f3248"),
                      color_scale = "continuous",
                      tooltip = list(headerFormat = NULL,
                                     pointFormat = "{point.name}: <b> {point.value}</b>"))) %>%
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






# tasa crecimiento pasajeros
t0 <- h1[, c("ano", "pasajeros")] %>% 
  group_by(ano) %>% 
  summarise(pasajeros = sum(pasajeros))

c0 <- map_dbl(1:14, function(s) {
  # t0$pasajeros[s + 1] - t0$pasajeros[s]
  (100 * (t0$pasajeros[s + 1] - t0$pasajeros[s])) / t0$pasajeros[s] 
})

# tasa aproximada de crecimiento 
c1 <- mean(c0)


# tasa crecimiento pasajeros nacionales
t1 <- h1[, c("ano", "pasajeros", "trafico")] %>%
  filter(trafico %in% "N") %>%
  group_by(ano) %>% 
  summarise(pasajeros = sum(pasajeros))

c2 <- map_dbl(1:14, function(s) {
  (100 * (t1$pasajeros[s + 1] - t1$pasajeros[s])) / t1$pasajeros[s] 
})

# tasa aproximada de crecimiento - nacional
c3 <- mean(c2)

# tasa crecimiento pasajeros internacionales
t2 <- h1[, c("ano", "pasajeros", "trafico")] %>%
  filter(trafico %in% "I") %>%
  group_by(ano) %>% 
  summarise(pasajeros = sum(pasajeros))

c4 <- map_dbl(1:14, function(s) {
  (100 * (t2$pasajeros[s + 1] - t2$pasajeros[s])) / t2$pasajeros[s] 
})

# tasa aproximada de crecimiento - internacional
c5 <- mean(c4)

# nacional 2025
b0 <- 27150111 + (27150111 * 0.083) #2019
b1 <- b0 + (b0 * 0.083) #2020
b2 <- b1 + (b1 * 0.083) #2021
b3 <- b2 + (b2 * 0.083) #2022
b4 <- b3 + (b3 * 0.083) #2023
b5 <- b4 + (b4 * 0.083) #2024
b6 <- b5 + (b5 * 0.083) #2025

# internacional 2025
k0 <- 14996346 + (14996346 * 0.103) #2019
k1 <- k0 + (k0 * 0.103) #2020
k2 <- k1 + (k1 * 0.103) #2021
k3 <- k2 + (k2 * 0.103) #2022
k4 <- k3 + (k3 * 0.103) #2023
k5 <- k4 + (k4 * 0.103) #2024
k6 <- k5 + (k5 * 0.103) #2025



# tabla de los 5 aeropuertos con mayor número de pasajeros --- aeropuertos con mayor carga
t0 <- h0[, c("ano", "origen", "destino", "pasajeros", "trafico")]
t1 <- t0 %>%
  filter(!trafico %in% "E")
t2 <- t1 %>%
  group_by(origen,
           destino,
           trafico) %>%
  summarise(pasajeros = sum(pasajeros, na.rm = TRUE))
t3 <- map_df(unique(c(t2$origen, t2$destino)), function(d) {
  data.frame(aeropuerto = d, 
             pasajeros = sum(t2$pasajeros[union(grep(paste0("^", d, "$"), t2$origen), grep(paste0("^", d, "$"), t2$destino))]))
}) %>% 
  arrange(desc(pasajeros))
t20 <- t2 %>%
  filter(trafico %in% "I")
t21 <- t2 %>%
  filter(trafico %in% "N")

# aeropuertos con mayor carga
# ¿la carga a bordo se cuenta para los aeropuertos de origen y destino o para cuál?
p0 <- h0[, c("ano", "origen", "destino", "carga_bordo", "trafico")]
p1 <- p0 %>%
  filter(!trafico %in% "E",
         ano %in% 2018)
p2 <- p1 %>%
  group_by(origen,
           destino,
           trafico) %>%
  summarise(carga = sum(as.numeric(carga_bordo), na.rm = TRUE))

p3 <- map_df(unique(c(p2$origen, p2$destino)), function(d) {
  data.frame(aeropuerto = d, 
             carga = sum(p2$carga[union(grep(paste0("^", d, "$"), p2$origen), grep(paste0("^", d, "$"), p2$destino))]))
}) %>% 
  arrange(desc(carga))
p20 <- p2 %>%
  filter(trafico %in% "I")
p30 <- map_df(unique(c(p20$origen, p20$destino)), function(d) {
  data.frame(aeropuerto = d, 
             carga = sum(p20$carga[union(grep(paste0("^", d, "$"), p20$origen), grep(paste0("^", d, "$"), p20$destino))]))
}) %>% 
  arrange(desc(carga))
p21 <- p2 %>%
  filter(trafico %in% "N")
p31 <- map_df(unique(c(p21$origen, p21$destino)), function(d) {
  data.frame(aeropuerto = d, 
             carga = sum(p21$carga[c(grep(paste0("^", d, "$"), p21$origen), grep(paste0("^", d, "$"), p21$destino))]))
}) %>% 
  arrange(desc(carga))




















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


# para  número de pasajeros a lo largo de los años (internacional, nacional, total)
f1 <- function (data, opts = NULL, ...) {
  if (is.null(data)) {
    stop("Load an available dataset")
  }
  opts <- hgchmagic:::getOptions(opts = opts)
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  title <- opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""
  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
  labelsXY <- hgchmagic::orientationXY(opts$orientation, x = nms[2], y = ifelse(nrow(d) == 
                                                                                  dplyr::n_distinct(d$b), nms[3], paste(prefix_agg, nms[3])), 
                                       hor = opts$horLabel, ver = opts$verLabel)
  lineXY <- hgchmagic::linesOrientation(opts$orientation, opts$horLine, 
                                        opts$verLine)
  lineLabelsXY <- hgchmagic::linesOrLabel(opts$orientation, opts$horLine_label, 
                                          opts$verLine_label)
  if (opts$color_scale == "discrete") {
    colorDefault <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", 
                      "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")
    colorDefault <- hgchmagic::discreteColorSelect(colorDefault, d)
  }
  else if (opts$color_scale == "no") {
    colorDefault <- rep("#3DB26F", length(unique(d$a)))
  }
  else {
    colorDefault <- (leaflet::colorNumeric(c("#53255E", "#ff4097"), 
                                           1:length(unique(d$a))))(1:length(unique(d$a)))
  }
  if (!is.null(opts$colors)) {
    opts$colors <- unname(hgchmagic::fillColors(d, "a", opts$colors, 
                                                opts$color_scale))
  }
  else {
    opts$colors <- colorDefault
  }
  if (opts$dropNaV[1]) 
    d <- d %>% tidyr::drop_na(a)
  if (opts$dropNaV[2]) 
    d <- d %>% tidyr::drop_na(b)
  d <- d %>% tidyr::replace_na(list(a = ifelse(is.character(d$a), 
                                               "NA", NA), b = ifelse(is.character(d$b), "NA", NA), c = NA)) %>% 
    dplyr::group_by(a, b) %>% dplyr::summarise(c = agg(opts$agg, 
                                                       c)) %>% tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)
  d$c[is.na(d$c)] <- NA
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- NA
  d$b <- as.character(d$b)
  d$b[is.na(d$b)] <- NA
  if (is.null(opts$nDigits)) {
    nDig <- 0
  }
  else {
    nDig <- opts$nDigits
  }
  if (opts$percentage) {
    d <- d %>% group_by(b) %>% dplyr::mutate(c = (c/sum(c, 
                                                        na.rm = TRUE)) * 100)
  }
  d <- hgchmagic::orderCategory(d, "a", order = opts$order1, labelWrap = opts$labelWrapV[1])
  d <- hgchmagic::orderCategory(d, "b", order = opts$order2, labelWrap = opts$labelWrapV[2])
  d$c <- round(d$c, 6)
  series <- purrr::map(unique(d[[1]]), function(i) {
    d0 <- d %>% dplyr::filter(a %in% i)
    l0 <- list(name = i, data = d0$c)
  })
  if (opts$percentage & is.null(opts$suffix)) {
    aggFormAxis <- "function() {return this.value+\"%\";}"
    opts$suffix <- "%"
  }
  formatLabAxis <- paste0("{value:", opts$marks[1], opts$marks[2], 
                          "f}")
  if (!is.null(opts$nDigits)) {
    formatLabAxis <- paste0("{value:", opts$marks[1], opts$marks[2], 
                            opts$nDigits, "f}")
  }
  if (is.null(opts$prefix)) 
    opts$prefix <- ""
  if (is.null(opts$suffix)) 
    opts$suffix <- ""
  aggFormAxis <- "function() {return this.value+\"\";}"
  aggFormAxis <- paste0("function() { return '", opts$prefix, 
                        "' + Highcharts.numberFormat(this.value, ", nDig, ", '", 
                        opts$marks[2], "', '", opts$marks[1], "') + '", opts$suffix, 
                        "'}")
  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0("<b>", nms[2], ": </b>{point.category}</br>", 
                                       "<b>", nms[1], ": </b>{series.name}</br>", paste0(prefix_agg, 
                                                                                         " ", nms[3], ": "), opts$prefix, "{point.y}", 
                                       opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- " "
  }
  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  hc <- highchart() %>% hc_chart(type = ifelse(opts$spline, 
                                               "spline", "line"), inverted = ifelse(opts$orientation == 
                                                                                      "ver", FALSE, TRUE)) %>% hc_title(text = title) %>% hc_subtitle(text = subtitle) %>% 
    hc_xAxis(categories = purrr::map(as.character(unique(d$b)), 
                                     function(z) z), title = list(text = labelsXY[1]), 
             plotLines = list(list(value = lineXY[2], color = "black", 
                                   dashStyle = "shortdash", zIndex = 5, width = 2, 
                                   label = list(text = lineLabelsXY[1], style = list(color = "black")))), 
             type = "category") %>% hc_yAxis(minRange = if (opts$startAtZero) 
               0.1, min = if (opts$startAtZero) 
                 0, minPadding = if (opts$startAtZero) 
                   0, title = list(text = labelsXY[2]), plotLines = list(list(value = lineXY[1], 
                                                                              color = "black", dashStyle = "shortdash", width = 2, 
                                                                              zIndex = 5, label = list(text = lineLabelsXY[2], style = list(color = "black")))), 
               labels = list(format = formatLabAxis, formatter = JS(aggFormAxis))) %>% 
    hc_add_series_list(series) %>% hc_plotOptions(series = list(marker = list(states = list(hover = list(fillColor = opts$color_hover), 
                                                                                            select = list(fillColor = opts$color_click))), allowPointSelect = opts$allow_point, 
                                                                cursor = opts$cursor, events = list(click = opts$clickFunction))) %>% 
    hc_tooltip(useHTML = TRUE, pointFormat = opts$tooltip$pointFormat, 
               headerFormat = opts$tooltip$headerFormat) %>% hc_credits(enabled = TRUE, 
                                                                        text = caption) %>% hc_legend(enabled = TRUE, align = opts$legend_position)
  if (opts$export) {
    hc <- hc %>% hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = list("printChart", 
                                                                                                   "downloadJPEG", "downloadPNG", "downloadSVG", "downloadPDF"))))
  }
  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(hgchmagic::tma(custom = list(showText = opts$text_show, 
                                                           colors = opts$colors, background = opts$background)))
  }
  else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }
  if (opts$text_show) {
    hc <- hc %>% hc_plotOptions(series = list(dataLabels = list(format = paste0(opts$prefix, 
                                                                                "{y}", opts$suffix))))
  }
  hc
}

# treemap
f2 <- function (data, opts = NULL, ...) 
{
  if (is.null(data)) {
    stop("Load an available dataset")
  }
  opts <- hgchmagic:::getOptions(opts = opts)
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  title <- opts$title %||% ""
  subtitle <- opts$subtitle %||% ""
  caption <- opts$caption %||% ""
  prefix_agg <- ifelse(is.null(opts$agg_text), opts$agg, opts$agg_text)
  if (opts$dropNa) 
    d <- d %>% tidyr::drop_na()
  if (opts$color_scale == "discrete") {
    colorDefault <- c("#74D1F7", "#2E0F35", "#B70F7F", "#C2C4C4", 
                      "#8097A4", "#A6CEDE", "#801549", "#FECA84", "#ACD9C2")
    colorDefault <- discreteColorSelect(colorDefault, d)
  }
  else {
    colorDefault <- (leaflet::colorNumeric(c("#2E0F35", 
                                             "#A6CEDE"), 1:length(unique(d$a))))(1:length(unique(d$a)))
  }
  if (!is.null(opts$colors)) {
    opts$colors <- unname(hgchmagic:::fillColors(d, "a", opts$colors, 
                                                 opts$color_scale))
  }
  else {
    opts$colors <- colorDefault
  }
  d <- d %>% tidyr::replace_na(list(a = ifelse(is.character(d$a), 
                                               "NA", NA), b = NA)) %>% dplyr::group_by(a) %>% dplyr::summarise(b = agg(opts$agg, 
                                                                                                                       b))
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- "NA"
  if (is.null(opts$nDigits)) {
    nDig <- 0
  }
  else {
    nDig <- opts$nDigits
  }
  if (opts$percentage) {
    d$b <- (d[["b"]] * 100)/sum(d[["b"]], na.rm = TRUE)
  }
  d$b <- round(d$b, nDig)
  d <- orderCategory(d, "a", unique(d$a), labelWrap = opts$labelWrap)
  d <- sortSlice(d, "b", "asc", opts$sliceN)
  d <- d %>% plyr::rename(c(b = "value"))
  d$color <- opts$colors
  if (!is.null(opts$highlight_value)) {
    w <- which(d$a %in% opts$highlight_value)
    d$color[w] <- opts$highlight_valueColor
  }
  data <- purrr::map(1:nrow(d), function(z) {
    list(name = d$a[z], value = d$value[z], color = as.character(d$color[z]), 
         colorValue = d$value[z])
  })
  if (is.null(opts$prefix)) 
    opts$prefix <- ""
  if (is.null(opts$suffix)) 
    opts$suffix <- ""
  if (opts$percentage && opts$suffix == "") {
    opts$suffix <- "%"
  }
  formatText <- JS(paste0("function () {\n                return this.point.name + '<br/>' + '", 
                          opts$prefix, "' + Highcharts.numberFormat(this.point.value, ", 
                          nDig, ", '", opts$marks[2], "','", opts$marks[1], "'", 
                          ") + '", opts$suffix, "';}"))
  if (is.null(opts$tooltip$pointFormat)) {
    opts$tooltip$pointFormat <- paste0("<b>{point.name}</b><br/>", 
                                       paste0(prefix_agg, " ", nms[2], ": "), opts$prefix, 
                                       "{point.value}", opts$suffix)
  }
  if (is.null(opts$tooltip$headerFormat)) {
    opts$tooltip$headerFormat <- ""
  }
  global_options(opts$marks[1], opts$marks[2])
  exportLang(language = opts$lang)
  hc <- highchart() %>% hc_title(text = title) %>% hc_subtitle(text = subtitle) %>% 
    hc_tooltip(useHTML = TRUE, pointFormat = opts$tooltip$pointFormat, 
               headerFormat = opts$tooltip$headerFormat) %>% hc_series(list(type = "treemap", 
                                                                            layoutAlgorithm = "squarified", data = data)) %>% hc_plotOptions(series = list(states = list(hover = list(color = opts$color_hover), 
                                                                                                                                                                         select = list(color = opts$color_click)), allowPointSelect = opts$allow_point, 
                                                                                                                                                           cursor = opts$cursor, events = list(click = opts$clickFunction)))
  if (opts$color_scale == "continuous") {
    hc <- hc %>% hc_colorAxis(minColor = opts$colors[1], maxColor = opts$colors[length(d$a)])
  }
  if (opts$text_show) {
    hc <- hc %>% hc_plotOptions(treemap = list(dataLabels = list(formatter = formatText)))
  }
  hc <- hc %>% hc_credits(enabled = TRUE, text = caption) %>% 
    hc_legend(enabled = opts$legend_show, align = opts$legend_position)
  if (opts$export) {
    hc <- hc %>% hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = list("printChart", 
                                                                                                   "downloadJPEG", "downloadPNG", "downloadSVG", "downloadPDF"))))
  }
  if (is.null(opts$theme)) {
    hc <- hc %>% hc_add_theme(hgchmagic:::tma(colors = opts$colors, 
                                              background = opts$background))
  }
  else {
    hc <- hc %>% hc_add_theme(opts$theme)
  }
  hc
}

