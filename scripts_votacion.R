library(tidyverse)
library(RSelenium)
library(rvest)
library(tidytext)



# ============= GENERAR DATASET DE DIPUTADOS ========================

url <- "https://www.camara.cl/diputados/diputados.aspx#mostrarDiputados"

# sesión por rvest
sesion <- html_session(url)

# links del perfil de cada diputado
dip.links <- sesion %>%
  html_nodes("article.grid-2 > h4 > a") %>%
  html_attr("href") %>%
  paste0("https://www.camara.cl/diputados/", .)


temp.lista <- list()

for (i in 1:length(dip.links)) {
  
  closeAllConnections() 
  
  nombre <- dip.links[i] %>%
    html_session() %>%
    html_node("h2.rotulo") %>%
    html_text() %>%
    str_remove("Diputado")  %>%  
    str_remove("Diputada")  %>%  
    str_trim()
  
  partido <- dip.links[i] %>%
    html_session() %>%
    html_node(xpath = '//*[@id="info-ficha"]/div[1]/div[2]/p/text()[5]') %>%
    html_text() %>%
    str_remove_all("[\r\n]") %>%
    sub('.*: ', '', .)  %>%  
    str_trim()
  
  
  temp.lista[[i]] <- c("nombre" = nombre, "partido" = partido)
}

base_diputados <- do.call("rbind", temp.lista) %>%
  as.data.frame()

# algunos diputados renunciaron o se fueron de ministros, así que tenemos que
# agregarlos a la base
nombre <- c(
  "Jaime Bellolio Avaria" ,  
  "Mario Desbordes Jiménez" ,
  "Marcela Sabat Fernández",
  "Hugo Gutiérrez Gálvez"  
)

partido <- c(
  "Unión Demócrata Independiente",
  "Renovación Nacional",
  "Renovación Nacional",
  "Partido Comunista"
)

base_diputados <- rbind(base_diputados, data.frame(nombre, partido))

write.table(base_diputados, "base_diputados.csv", sep = ";", row.names = F)

rm(i, temp.lista, dip.links, nombre, partido, url, sesion)




# ============ EMPEZANDO EXTRACCIÓN DE VOTACIÓN =======================


url <- "https://www.camara.cl/legislacion/ProyectosDeLey/votaciones.aspx?prmID=13636&prmBOLETIN=13090-25"

# sesión por rvest
sesion <- html_session(url)

# sesión por rSelenium
driver <- rsDriver(browser = c("firefox"))

remote_driver <- driver[["client"]]
# si al correr el "driver" ya está en uso, solo abrir 
# la sesión

# COMIENZO EXTRAYENDO LOS LINKS DE CADA ARTÍCULO VOTADO POR SEPARADO
links1 <- sesion %>%
  html_nodes("table.tabla > tbody > tr > td > a") %>%
  html_attr("href") %>% 
  str_replace("\\.{2}", "https://www.camara.cl/legislacion")


# usando rselenium para la hoja 2
# remote_driver$open()

# navego a la hoja 2
remote_driver$navigate(url)
remote_driver$findElement(using = "id",
                          value = "ContentPlaceHolder1_ContentPlaceHolder1_ContentPlaceHolder1_pager_rptPager_page_1")$clickElement()

# repetiré lo mismo de arriba, pero sacando la tabla por selenium
tabla <- remote_driver$findElement(using = "class",
                          value = "tabla")

links2 <- tabla$getElementAttribute("outerHTML")[[1]] %>%
  read_html() %>%
  html_nodes("table.tabla > tbody > tr > td > a") %>%
  html_attr("href") %>% 
  str_replace("\\.{2}", "https://www.camara.cl/legislacion")


# primer consolidado
links <- c(links1, links2)

remote_driver$close()
remote_driver$closeServer()
remote_driver$closeall()

rm(links1, links2, driver, remote_driver, tabla)


# COMENZANDO A EXTRAER DETALLE DE CADA ARTÍCULO

# una pequeña función pa formatear los nombres pa poder armonizar los datasets
format_names <- function(x){
  
  x <- c(x[, 1], x[,2], x[,3])
  
  x <- x[x!=""]
  
  p.nombre <- x %>% str_extract(',.*') %>%
    str_remove(",") %>% 
    str_trim()
  
  apellidos <- x %>%
    sub(',.*', '', .)
  
  return( paste(p.nombre, apellidos, sep = " ") )
}

# preparando la iteración
temp.lista <- list()
for (i in 1:length(links)) {
  
  closeAllConnections()
  
  afavor <- links[i] %>%
    html_session() %>%
    html_nodes(xpath = '//*[@id="ContentPlaceHolder1_ContentPlaceHolder1_PaginaContent_dtlAFavor"]') %>%
    html_table() %>%
    as.data.frame()
  
  afavor <- format_names(afavor)
  
  dat <- data.frame(nombre = afavor, votacion = rep("A favor", length(afavor)))
  
  encontra <-links[i] %>%
    html_session() %>%
    html_nodes(xpath = '//*[@id="ContentPlaceHolder1_ContentPlaceHolder1_PaginaContent_dtlEnContra"]') %>%
    html_table() %>%
    as.data.frame()
  
  encontra <- format_names(encontra)
  
  dat <- rbind(dat, data.frame(nombre = encontra, votacion = rep("En contra", length(encontra))))
  
  abstencion <- links[i] %>%
    html_session() %>%
    html_nodes(xpath = '//*[@id="ContentPlaceHolder1_ContentPlaceHolder1_PaginaContent_dtlAbstencion"]') %>%
    html_table() %>%
    as.data.frame()
  
  if(length(abstencion) > 0){
    abstencion <- format_names(abstencion)
    
    dat <- rbind(dat, data.frame(nombre = abstencion, votacion = rep("Abstención", length(abstencion))))
  }
  
  dat$link <- links[i]
  
  temp.lista[[i]] <- dat
  
  print(paste("iteración:", i))
}

votacion <- do.call("rbind", temp.lista)

rm(temp.lista, dat, i, afavor, encontra, abstencion, links)

write.table(votacion, "votacion.csv", sep = ";", row.names = F)

# ============= JUNTANDO AMBOS DATASETS ============

consolidado <- votacion %>%
  left_join(base_diputados, by = "nombre") 

# hay que pitearse filas que están demás que marcan el nombre con "NA NA"
# Es un poco largo de explicar, pero el placeholder de las votaciones tiene
# una tabla con 3 columnas por defecto, la wea es que si traigo la tabla, y
# hay un solo voto, las otras dos columna se llenan con NA
consolidado <- consolidado[-c(which(consolidado$nombre=="NA NA")),]


# pasamos las siglas pa que sea más fácil de manejar
partidos <- unique(consolidado$partido)
siglas <- c(
  "UDI", "EVOPOLI", "IND",
  "RN", "PDC", "PPD",
  "PRSD", "PR", "PS",
  "PC", "PCS", "RD", 
  "PEV", "PH", "COMUNES",
  "FRVS", "PL"
)
part_siglas <- data.frame(partidos, siglas)
rm(partidos, siglas)

# juntamos las siglas al consolidado
consolidado <- consolidado %>% 
  left_join(part_siglas, by = c("partido"= "partidos"))


consolidado$coalicion <- case_when(
    consolidado$siglas %in% c("RD", "PCS", "COMUNES", "PL", "PH") ~ "FA",
    consolidado$siglas %in% c("UDI", "RN", "EVOPOLI") ~ "ChV",
    consolidado$siglas %in% c("PS", "PRSD", "PDC", "PPD") ~ "Concerta",
    consolidado$siglas == "PC" ~ "PC",
    T ~ "Otros"
  )

# una última limpieza es que asignamos un código a cada votación extrayendolo
# del link

consolidado$id_votacion <- consolidado$link %>% 
  str_extract("[[:digit:]]+")

# y estamos listos para graficar!

write.table(consolidado, "consolidado partidos.csv", sep = ";", row.names = F)
rm(base_diputados, votacion, format_names, part_siglas)

# =========== ANALIZANDO ================

library(tidyverse)
library(ggfortify)
library(cluster)
library(patchwork)
theme_set(theme_classic())

# codigo de votación de aprobación de legislar 32195

resumen <- consolidado %>%
  group_by(id_votacion, coalicion) %>%
  summarize(total_votaciones = n(), 
            a_favor = sum(votacion == "A favor")) %>%
  mutate(p.favor = a_favor/total_votaciones)

resumen$id_votacion[resumen$id_votacion == 32195] <- "A favor de legislar"

g1 <- ggplot(resumen, aes(reorder_within(coalicion, p.favor, id_votacion), 
                    p.favor, fill = coalicion)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(.~fct_rev(id_votacion), scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(title = "Nivel de apoyo a artículos de ley antibarricadas",
       subtitle ="% de voto \"a favor\" de coalición/partido",
       x="", y="", caption = "Números indican id de artículo \nExtraído de página de la cámara de diputados")



ggsave("gráfico 1.png", g1, device="png", width = 10, height = 5, limitsize = F)


# OTRA GRÁFICA
resumen$coalicion <- as.factor(resumen$coalicion)

resumen$coalicion <- factor(resumen$coalicion,
                            levels(resumen$coalicion)[c(1:3, 5, 4)])


ggplot(resumen, aes(p.favor, fill = coalicion, color = coalicion)) +
  facet_wrap(.~coalicion) +
  geom_histogram() +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis_d() + 
  scale_fill_viridis_d() +
  theme(legend.position = "none") +
  labs(x= "", y = "", title = "Apoyo a artículos de la ley antibarricadas",
       subtitle = "Recuento de porcentajes de apoyo por cada artículo de la ley por partido/coalición")


# ANÁLISIS POR KMEANS
# Recodificando votacion, separando votaciones en
# diferentes variables, renombrandolas y
# cambiando el formato a lo ancho
consolidado <- consolidado %>% 
  mutate(voto = case_when(votacion == 'A favor' ~ 1,
                          votacion == 'En contra' ~ -1,
                          TRUE ~ 0),
         id_votacion = paste('id_', id_votacion, sep = '')) %>% 
  select(-link, -partido, -votacion)

# Esto es para usar en otra idea
consolidado_w <- consolidado %>% 
  pivot_wider(names_from = id_votacion, values_from = voto)

# Promedios de votacion y grafico simple
resumen <- consolidado %>%
  group_by(id_votacion, siglas) %>%
  summarize(total_votaciones = n(), 
            a_favor = sum(voto == 1)) %>%
  mutate(p_favor = a_favor/total_votaciones)

# Cambiando a ancho
resumen_w <- resumen %>% 
  select(-total_votaciones, -a_favor) %>% 
  pivot_wider(names_from = id_votacion, values_from = p_favor)

# Data matrix y Componentes Principales
dat_mat <- as.matrix(resumen_w[, 2:ncol(resumen_w)])
rownames(dat_mat) <- resumen_w$siglas

# obteniendo clusters
kmeans_3 <- kmeans(dat_mat, 3)
kmeans_4 <- kmeans(dat_mat, 4)

# graficando clusters
g2 <- autoplot(kmeans_3,  data = resumen_w, frame= T, label= T, size = -1) + 
  theme(legend.position = "none") +
  labs(subtitle = "3 clusters") +
  autoplot(kmeans_4,  data = resumen_w, frame= T, label= T, size = -1) + 
  theme(legend.position = "none") +
  labs(subtitle = "4 clusters") +
  plot_annotation(title = "Aglomeración por k-means")


ggsave("gráfico 2.png", g2, device="png", width = 10, height = 5, limitsize = F)





  





















