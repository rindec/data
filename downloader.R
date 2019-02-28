library(tidyverse)
library(purrr)

get_bases_eph0307 <- function(anio = 2017, trimestre = 4,etiqueta = FALSE, save=T){
  
  #controles de los parametros
  assertthat::assert_that(is.numeric(anio))
  assertthat::assert_that(is.numeric(trimestre))
  assertthat::assert_that(assertthat::is.flag(etiqueta), msg = "Por favor ingresa TRUE o FALSE")
  assertthat::assert_that(trimestre %in% 1:4, msg = "Por favor ingresa un numero de trimeste valido: 1,2,3,4")
  
  assertthat::assert_that(anio>=2017                      |
                            (anio==2016 &trimestre %in% 2:4)|
                            (anio %in% 2004:2006)           |
                            (anio == 2007 &trimestre == 1)  |
                            (anio == 2003 &trimestre %in% 3:4)
                          ,msg = "Esta funcion solo es valida para las bases comprendidas entre el
                          3er trimestre de 2003 y el primero del 2007, o a partir del 2do trimestre de 2016 ")
  
  if (anio >= 2016){
    if (anio == 2016 ){
      if (trimestre == 1){
        trimestre <- "1ro"}
      if (trimestre == 2){
        trimestre <- "2do"}
      if (trimestre == 3){
        trimestre <- "3er"}
      if (trimestre == 4){
        trimestre <- "4to"}
      
      base = paste0("EPH_usu_", trimestre, "Trim_", anio, "_txt.zip")
    }
    
    if (anio == 2017 & trimestre ==1) {
      base = 'EPH_usu_1er_Trim_2017_txt.zip'
    }
    
    if (anio >= 2018 | (anio == 2017 & trimestre >=2)){
      base = paste0("EPH_usu_", trimestre, "_Trim_", anio, "_txt.zip")
    }
    
    
    link = paste0('https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/', base)
    
  }
  
  if (anio  %in% 2003:2007){
    link_base = 'https://www.indec.gob.ar/dbindec/'
    
    if ( anio >= 2005                      |
         (anio == 2004 & trimestre %in% 3:4)|
         (anio == 2003 & trimestre==3)
    ) {
      
      link = paste0(link_base,"USU_T",trimestre,substr(anio,3,4),".ZIP")
      
    }
    if (anio == 2004 & trimestre %in% 1:2){
      
      link = paste0(link_base,"EPH_PREL_USU_T",trimestre,substr(anio,3,4),".ZIP")
      
    }
    
    if (anio == 2003 & trimestre == 4){
      
      link = paste0(link_base,"USU_",trimestre,substr(anio,3,4),".ZIP")
      
    }
    
    
  }
  
  temp <- tempfile()
  
  check <- NA
  try(check <- utils::download.file(link,temp,method = 'libcurl'),silent = TRUE)
  assertthat::assert_that(assertthat::noNA(check),msg = "problema con la descarga. Posiblemente un error de la conexion a internet")
  
  nombres <- purrr::as_vector(utils::unzip(temp, list = TRUE)['Name'])
  base_hogar_name <- nombres[stringr::str_detect(nombres, 'hog')|
                               stringr::str_detect(nombres, 'USH')|
                               stringr::str_detect(nombres, 'HOG')]
  base_individual_name <- nombres[stringr::str_detect(nombres, 'ind')|
                                    stringr::str_detect(nombres, 'USP')|
                                    stringr::str_detect(nombres, 'PER')|
                                    stringr::str_detect(nombres, 'Individual')]
  
  
  if(anio >= 2016){
    base_individual <- utils::read.table(unz(temp,base_individual_name), sep=";", dec=",", header = TRUE, fill = TRUE)
  }
  
  if(anio < 2016){
    dir.create(temp,showWarnings = F)
    utils::unzip(zipfile = temp,exdir =  tempdir())
    
    base_individual <- foreign::read.dbf(paste0(tempdir(),'/',base_individual_name))
  }
  
  
  if (etiqueta == TRUE) {
    base_individual <- put_labels_eph(base_individual, base = 'individual')
  }
  
  
  if(anio >= 2016){
    base_hogar <- utils::read.table(unz(temp,base_hogar_name), sep=";", dec=",", header = TRUE, fill = TRUE)
  }
  
  if(anio < 2016){
    base_hogar <- foreign::read.dbf(paste0(tempdir(),'/',base_hogar_name))
  }
  
  
  if (etiqueta == TRUE) {
    base_hogar <- put_labels_eph(base_hogar, base = 'hogar')
  }
  
  unlink(temp)
  #bases <- list("base_hogar"=base_hogar,"base_individual"=base_individual)
  
  if (save) {
   saveRDS(object = base_hogar, file = glue::glue('eph/hogar/base_hogar_{anio}T{trimestre}.RDS'))
   saveRDS(object = base_individual, file = glue::glue('eph/individual/base_individual_{anio}T{trimestre}.RDS'))
    
  }
  
}

trimestres <- 1:4

anios_full <- data.frame(anio=rep(c(2004:2006, 2017),4),trimestre = rep(trimestres,each=4))

anios_resto <- data.frame(anio=c(rep(2003,2),2007,rep(2016, 3),rep(2018, 3)), 
                          trimestre=c(3,4,1,2:4,1:3))

periodos <- bind_rows(anios_full,anios_resto) %>% arrange(anio, trimestre)


map2(.x = periodos$anio, .y = periodos$trimestre, get_bases_eph0307)


