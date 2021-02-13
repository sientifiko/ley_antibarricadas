library(tidyverse)
library(RSelenium)
library(rvest)

closeAllConnections()

url <- "https://www.camara.cl/legislacion/ProyectosDeLey/proyectos_ley.aspx"

# Abriendo el driver
driver <- rsDriver(browser = c("firefox"))

remote_driver <- driver[["client"]]
# si al correr el "driver" ya está en uso, solo abrir 
# la sesión


# ============== FUNCIÓN PARA INICIAR TODO ============

start <- function(remote_driver){
  
  # Abrir el phantom js
  remote_driver$open()
  
  # Navegar a la página deseada
  remote_driver$navigate(url)
  
  # ubicar el buscador
  busqueda <- remote_driver$findElement("name", 
                                        "ctl00$ctl00$ContentPlaceHolder1$ContentPlaceHolder1$txtBuscarPLey")
  
  # chantarle el símbolo "-"
  busqueda$sendKeysToElement(list("-"))
  
  # apretar el botón buscar
  remote_driver$findElements("id", 
                             "ContentPlaceHolder1_ContentPlaceHolder1_lnkBuscar")[[1]]$clickElement()
  
  return(remote_driver)
}


# ======  FUNCIÓN PARA METERSE EN CADA LEY ============

detalle_ley <- function(nro, link){
  
  detalle_ley <- list()
  
  for (i in 1:length(link)) {
    
    # navego a la página de cada link
    sesion <- html_session(link[i])
    
    # Obteniendo autores
    autores <- sesion %>%
      html_nodes("#info-ficha > div.auxi > div:nth-child(8) > div.info") %>%
      html_text() %>%
      str_remove_all("[\n\r]") %>%
      str_squish() %>%
      str_split("[|]") %>% 
      unlist() %>%
      str_trim("both")
    
    # saco los detalles de cada autor
    temp.list <- list()
    for (j in autores) {
      
      #meto su trycatch pa no cagarla
      val <- tryCatch(
        {
          # voy al autor
          temp.ses <- sesion %>% follow_link(j)
          
          # saco región
          region <- temp.ses %>%
            html_nodes(xpath = '//*[@id="info-ficha"]/div[1]/div[2]/p/text()[3]') %>%
            html_text() %>%
            str_remove_all("[\n\r]") %>%
            sub('.*: ', '', .)  %>%  
            str_trim()
          
          # saco el partido al que pertenece
          partido <- temp.ses %>%
            html_nodes(xpath = '//*[@id="info-ficha"]/div[1]/div[2]/p/text()[5]') %>%
            html_text() %>%
            str_remove_all("[\n\r]") %>%
            sub('.*: ', '', .) %>%  
            str_trim() 
          
          T
        },
        error= function(e){
          return(F)
        }
      )
      
      if(val){
        # los colecciono en una lista
        temp.list[[j]] <- cbind("autores"= j, region, partido)
      } else{
        # los colecciono en una lista
        temp.list[[j]] <- cbind("autores"= j, "region"= NA, "partido"=NA)
      }
      
    }
    
    # lo paso a un df
    temp.df <- do.call("rbind", temp.list) %>% as.data.frame()
    
    # le asigno el nro de ley
    temp.df$nro <- nro[i]
    
    # lo colecciono todo en una gran lista
    detalle_ley[[i]] <- temp.df
    
    closeAllConnections()
  }
  
  return(do.call("rbind", detalle_ley) %>% as.data.frame())
}



# =============== FUNCIÓN PARA RECORRER LAS PÁGINAS DE LEYES =====

consolidado <- function(remote_driver, list.law, list.autors, i=1, j=1){
  
  # recuperar el listado de leyes de la página en que está el phantom
  listado <- remote_driver$findElement("class", "grid-9")
  
  #Obteniendo número
  nro <- listado$getElementAttribute("outerHTML")[[1]] %>%
    read_html() %>%
    html_nodes("span.numero") %>%
    html_text() %>%
    str_remove_all("[\n\r]") %>%
    str_trim()
  
  #Obteniendo la fecha
  fecha <-  listado$getElementAttribute("outerHTML")[[1]] %>%
    read_html() %>%
    html_nodes("span.fecha") %>%
    html_text() %>%
    str_remove_all("[\n\r]") %>%
    str_trim()
  
  #Obteniendo tipo
  tipo <- listado$getElementAttribute("outerHTML")[[1]] %>%
    read_html() %>%
    html_nodes("article > ul.etapas-legislativas > li.select") %>%
    html_text() %>%
    str_remove_all("[\n\r]") %>%
    str_trim()
  
  #Obteniendo cuerpo de la ley
  cuerpo <-  listado$getElementAttribute("outerHTML")[[1]] %>%
    read_html() %>%
    html_nodes("h3") %>%
    html_text() %>%
    str_remove_all("[\n\r]") %>%
    str_trim()
  
  #Obteniendo estado
  estado <- listado$getElementAttribute("outerHTML")[[1]] %>%
    read_html() %>% 
    html_nodes("div.aleft > ul.etapas-legislativas > li.select") %>%
    html_text() %>%
    str_remove_all("[\n\r]") %>%
    str_trim()
  
  #Obteniendo links
  link <- listado$getElementAttribute("outerHTML")[[1]] %>%
    read_html() %>% 
    html_nodes("div.aleft > h3 > a") %>%
    html_attr("href") %>%
    str_c("https://www.camara.cl/legislacion/ProyectosDeLey/", .)
  
  # cuando llegue a la página 10, pinchar siguiente página
  if(i==10){
    
    print(paste("El valor de J es:", j))
    
    remote_driver$findElements("xpath", 
                               '//a[text()="..."]')[[j]]$clickElement()
    
    break
    
    
    # # reiniciar el contador
    # i <- i - 9
    # 
    # # me volvi locooooooo
    # consolidado(remote_driver, list.law, list.autors, i, j=2)
    
  } else {
    # esta shit es la que me permite iterar!!
    p <- paste0("ContentPlaceHolder1_ContentPlaceHolder1_pager_rptPager_page_", i)
  }
  
  # controlo errores y esas cosas
  val <- tryCatch({
    # me muevo entre las paginas
    remote_driver$findElements("id", 
                               p)[[1]]$clickElement()
    
    # descanso 3 segundos
    Sys.sleep(3)
    
    # capturo los dfs
    df.leyes <- cbind(nro, fecha, tipo, cuerpo, estado, link) %>% as.data.frame()
    df.detalle <- detalle_ley(nro, link)
    
    # ir acumulando los dfs
    list.law[[i]] <- df.leyes
    list.autors[[i]] <- df.detalle
    
    T
  },
    error = function(e){
      print(e)
      return(F)
    }
  )
  
  # controlo si hubo errores
  if(val){
    print(paste("Entrando a la página", i+1))
    # función recursiva porque pro-gamer move
    consolidado(remote_driver, list.law, list.autors, i+1)
    
  } else {
    
    print("Listo para salir")
    df1 <- do.call("rbind", list.law) %>% as.data.frame()
    df2 <- do.call("rbind", list.autors) %>% as.data.frame()
    
    retorno <- list("leyes" = df1,
                    "detalles"= df2)
    return(retorno)
  }
  
}

# ======= ZONA DE EJECUCIÓN ==========

remote_driver$closeall()

remote_driver <- start(remote_driver)






# esta shit es la que me permite iterar!!

# listas vacías provisorias
list.law <- list()
list.autors <- list()

resultado <- consolidado(remote_driver, list.law, list.autors, i=9)


temp <- resultado$leyes



a <- remote_driver$findElement("class", "paginacion")

pags <- a$getElementAttribute("outerHTML")[[1]] %>%
          read_html() %>%
          html_nodes("span") %>%
          html_text() %>%
          str_remove_all("[\n\r]") %>%
          str_squish() %>%
          str_split(" ")

pags <- as.numeric(pags[[1]])
pags <- pags[!is.na(pags)]

pags <- pags[2:length(pags)]


p <- paste0("//a[text()=", pags[1], "]")

p

remote_driver$findElements("xpath", 
                           p)[[1]]$clickElement()










