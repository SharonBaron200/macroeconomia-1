##############################################################################
#######                                                                #######
#                         Universidad del Quindio                            #
#                                Economía                                    #
#                 (Introducción con el lenguaje de programación R)           #
#######                                                                 ######
##############################################################################

#By: Sharon Baron Bernier
#sharonb.baronb@uqvirtual.edu.co

print("Trabajo con la base  datos de la GEIH año 2025")
getwd()
options( scipen = 100, digits = 4 )
rm(list= ls())
version




#librerias o paquetes 
library("skimr")
library("readxl")
library("stringr")
library("stringi")
library("plyr")
library("haven")
library("tidyverse")
library("readr")

#Directorio
##
GEIH25 = "C:/Users/pc/OneDrive/Nueva carpeta/OneDrive/CUARTO SEMESTRE/GEIH/"


ENERO = merge(
  x = read_delim( file = paste0(GEIH25 ,
                                "ENERO/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "ENERO/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "ENERO/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "ENERO/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "ENERO/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "ENERO/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

xtabs( FEX_C18 ~ MES , data= ENERO , addNA = TRUE)

# FEBRERO-----
FEBRERO = merge( x =
                   read_delim(
                     file = paste0(GEIH25 , "FEBRERO/Características generales, seguridad social en salud y educación.csv") , delim = ";",
                     locale = locale(decimal_mark = "."),
                     col_names = TRUE ) , 
                 y = read_delim(
                   file = paste0(GEIH25, "FEBRERO/Ocupados.csv" ) , delim = ";",
                   locale = locale(decimal_mark = "."),
                   col_names = TRUE ) , 
                 by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
                 all.x = T ,
                 suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "FEBRERO/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "FEBRERO/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "FEBRERO/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "FEBRERO/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

# MARZO------

MARZO = merge( x =
                 read_delim(
                   file = paste0(GEIH25 , "MARZO/Características generales, seguridad social en salud y educación.csv") , delim = ";",
                   locale = locale(decimal_mark = "."),
                   col_names = TRUE ) , 
               y = read_delim(
                 file = paste0(GEIH25, "MARZO/Ocupados.csv" ) , delim = ";",
                 locale = locale(decimal_mark = "."),
                 col_names = TRUE ) , 
               by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
               all.x = T ,
               suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "MARZO/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE  ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "MARZO/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "MARZO/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "MARZO/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

#ABRIL----

ABRIL = merge( x =
                 read_delim(
                   file = paste0(GEIH25 , "ABRIL/Características generales, seguridad social en salud y educación.csv") ) , 
               y = read_delim(
                 file = paste0(GEIH25, "ABRIL/Ocupados.csv" ) , delim = ";",
                 locale = locale(decimal_mark = "."),
                 col_names = TRUE ) , 
               by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
               all.x = T ,
               suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "ABRIL/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "ABRIL/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "ABRIL/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "ABRIL/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

#MAYO----

MAYO = merge( x =
                read_delim(
                  file = paste0(GEIH25 , "MAYO/Características generales, seguridad social en salud y educación.csv") , delim = ";",
                  locale = locale(decimal_mark = "."),
                  col_names = TRUE ) , 
              y = read_delim(
                file = paste0(GEIH25, "MAYO/Ocupados.csv" ) , delim = ";",
                locale = locale(decimal_mark = "."),
                col_names = TRUE ) , 
              by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
              all.x = T ,
              suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "MAYO/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "MAYO/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "MAYO/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "MAYO/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

#JUNIO----

JUNIO = merge( x =
                 read_delim(
                   file = paste0(GEIH25 , "JUNIO/Características generales, seguridad social en salud y educación.csv") , delim = ";",
                   locale = locale(decimal_mark = "."),
                   col_names = TRUE ) , 
               y = read_delim(
                 file = paste0(GEIH25, "JUNIO/Ocupados.csv" ) , delim = ";",
                 locale = locale(decimal_mark = "."),
                 col_names = TRUE ) , 
               by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
               all.x = T ,
               suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "JUNIO/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "JUNIO/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "JUNIO/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "JUNIO/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

#JULIO----

JULIO = merge( x =
                 read_delim(
                   file = paste0(GEIH25 , "JULIO/Características generales, seguridad social en salud y educación.csv") , delim = ";",
                   locale = locale(decimal_mark = "."),
                   col_names = TRUE ) , 
               y = read_delim(
                 file = paste0(GEIH25, "JULIO/Ocupados.csv" ) , delim = ";",
                 locale = locale(decimal_mark = "."),
                 col_names = TRUE ) , 
               by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
               all.x = T ,
               suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "JULIO/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "JULIO/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "JULIO/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "JULIO/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

#AGOSTO----

AGOSTO = merge( x =
                  read_delim(                       
                    file = paste0(GEIH25 , "AGOSTO/Características generales, seguridad social en salud y  educación.csv") , delim = ";",
                    locale = locale(decimal_mark = "."),
                    col_names = TRUE ) , 
                y = read_delim(
                  file = paste0(GEIH25, "AGOSTO/Ocupados.csv" ) , delim = ";",
                  locale = locale(decimal_mark = "."),
                  col_names = TRUE ) , 
                by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
                all.x = T ,
                suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "AGOSTO/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "AGOSTO/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "AGOSTO/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "AGOSTO/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

#SEPTIEMBRE----

SEPTIEMBRE = merge( x =
                      read_delim(                           
                        file = paste0(GEIH25 , "SEPTIEMBRE/Características generales, seguridad social en salud y educación.csv") , delim = ";",
                        locale = locale(decimal_mark = "."),
                        col_names = TRUE ) , 
                    y = read_delim(
                      file = paste0(GEIH25, "SEPTIEMBRE/Ocupados.csv" ) , delim = ";",
                      locale = locale(decimal_mark = "."),
                      col_names = TRUE ) , 
                    by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
                    all.x = T ,
                    suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "SEPTIEMBRE/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "SEPTIEMBRE/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "SEPTIEMBRE/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "SEPTIEMBRE/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

#OCTUBRE----

OCTUBRE = merge( x =
                   read_delim(                           
                     file = paste0(GEIH25 , "OCTUBRE/Características generales, seguridad social en salud y educación.csv") , delim = ";",
                     locale = locale(decimal_mark = "."),
                     col_names = TRUE ) , 
                 y = read_delim(
                   file = paste0(GEIH25, "OCTUBRE/Ocupados.csv" ) , delim = ";",
                   locale = locale(decimal_mark = "."),
                   col_names = TRUE ) , 
                 by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
                 all.x = T ,
                 suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "OCTUBRE/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "OCTUBRE/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "OCTUBRE/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "OCTUBRE/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

#NOVIEMBRE----

NOVIEMBRE = merge( x =
                     read_delim(                           
                       file = paste0(GEIH25 , "NOVIEMBRE/Características generales, seguridad social en salud y educación.csv") , delim = ";",
                       locale = locale(decimal_mark = "."),
                       col_names = TRUE ) , 
                   y = read_delim(
                     file = paste0(GEIH25, "NOVIEMBRE/Ocupados.csv" ) , delim = ";",
                     locale = locale(decimal_mark = "."),
                     col_names = TRUE ) , 
                   by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
                   all.x = T ,
                   suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "NOVIEMBRE/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "NOVIEMBRE/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "NOVIEMBRE/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "NOVIEMBRE/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()

#DICIEMBRE----
DICIEMBRE = merge( x =
                     read_delim(                           
                       file = paste0(GEIH25 , "DICIEMBRE/Características generales, seguridad social en salud y educación.csv") , delim = ";",
                       locale = locale(decimal_mark = "."),
                       col_names = TRUE ) , 
                   y = read_delim(
                     file = paste0(GEIH25, "DICIEMBRE/Ocupados.csv" ) , delim = ";",
                     locale = locale(decimal_mark = "."),
                     col_names = TRUE ) , 
                   by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
                   all.x = T ,
                   suffixes = c("", ".2")
) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "DICIEMBRE/No ocupados.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".4")
  ) |> 
  merge( x = , 
         y = read_delim(                     
           file = paste0(GEIH25, "DICIEMBRE/Fuerza de trabajo.csv") , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T ,
         suffixes = c("", ".6")
  ) |> 
  merge( x = , 
         y = read_delim(
           file = paste0(GEIH25, "DICIEMBRE/Otros ingresos e impuestos.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".8")
  ) |> 
  merge( x = , 
         y = read_delim(                     
           file = paste0(GEIH25, "DICIEMBRE/Otras formas de trabajo.csv" ) , delim = ";",
           locale = locale(decimal_mark = "."),
           col_names = TRUE ) , 
         by = c("DIRECTORIO" , "SECUENCIA_P" , "ORDEN") , 
         all.x = T , 
         suffixes = c("", ".10")
  ) |> data.frame()


GEIH = plyr::rbind.fill(ENERO, FEBRERO , MARZO , ABRIL , MAYO , JUNIO , JULIO , AGOSTO , SEPTIEMBRE , OCTUBRE , NOVIEMBRE , DICIEMBRE)

rm(GEIH25,ENERO, FEBRERO , MARZO , ABRIL , MAYO , JUNIO , JULIO , AGOSTO , SEPTIEMBRE , OCTUBRE , NOVIEMBRE , DICIEMBRE , GEIH22 , list)

## Borramos variables repetidas----
GEIH = GEIH |> dplyr::select(-ends_with(c( ".2" , ".4", ".6" , ".8" , ".10" , ".12" ,".14" ))) |> 
  mutate( MES = as.character(substr( PERIODO , start = 5, stop = 6)))








