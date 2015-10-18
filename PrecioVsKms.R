
# ¿Cómo es la relación entre precio y kms en un coche?
# Extraigo datos parseando resultados de búsquedas en la web coches.net


library('httr')
library('rvest')
library(dplyr)

rm(list=ls())

# Citroen: <option value="11">CITROEN</option>
id_marca=11
# C4 Picasso: <option value="619">C4 Picasso</option>
id_modelo=619


url_consulta='http://www.coches.net/coches-de-ocasion.aspx?or=1&fi=Price&MakeId=11&ModelId=619&PowerHpFrom=0000120&PowerHpTo=0000150'
cabecero='http://www.coches.net/coches-de-ocasion.aspx'
#"http://www.coches.net/coches-de-ocasion.aspx?pg=3&MakeId=11&ModelId=619&OfferTypeGroup=0&PowerHpTo=0000999&or=1&fi=Price"

busqueda <- html(url_consulta)
# paginas de resultados: obtener los enlaces a cada una
peticiones <- busqueda %>% html_node("#more_pages ul") %>% html_nodes("li a") %>% html_attr("href")
peticiones <- paste0(cabecero, peticiones)[-length(peticiones)]

# veo num total de paginas
ntot <- html(peticiones[1]) %>% html_node("#_ctl0_ContentPlaceHolder1_Grid1_info_results .floatleft") %>% html_text()
ntot <- as.numeric(gsub('^.*pág. \\d de (\\d+).*', "\\1", ntot))

#más enlaces
if (ntot>length(peticiones)){
    peticiones <- c(peticiones, sapply((length(peticiones)+2):ntot, function(i) gsub("pg=\\d+", sprintf("pg=%d",i), peticiones[1])))
}

GetDataAdd <- function(busqueda){

    # #gridRows div:nth-child(1) .datacar
    precios <- busqueda %>% html_nodes(".preu") %>% html_text()
    precios <- gsub("\\.", "", precios)
    precios <- sapply(strsplit(precios, " "), function(x) as.numeric(x[1]))
    
    anho <- busqueda %>% html_nodes(".d2") %>% html_text()
    comb <- busqueda %>% html_nodes(".d3") %>% html_text()
    kms <- busqueda %>% html_nodes(".d1") %>% html_text()
    kms <- gsub("\\.", "", kms)
    kms <- sapply(strsplit(kms, " "), function(x) as.numeric(x[1]))
    
    modelo <- busqueda %>% html_nodes("#gridRows h2") %>% html_text()
    
    return(data.frame(pr=precios, yr=as.integer(anho), fuel=comb, kms=kms, mod=modelo))

}

datos <- GetDataAdd(busqueda)

for (pet in peticiones){
    cat(pet, '\n')
    busq <- html(pet)
    datos <- bind_rows(datos, GetDataAdd(busq))
}


ggplot(datos, aes(x=kms, y=pr, colour=fuel)) + geom_point() + stat_smooth(se=F, method="loess", span=0.9)
ggplot(datos, aes(x=yr, y=pr, colour=fuel)) + geom_point() + stat_smooth(se=F, method="loess", span=0.9)
