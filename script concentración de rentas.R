## Columna concentración de rentas en Colima
## ¿En qué deciles se acumulan las rentas en Colima?
## Por Zatara

## Librerías de trabajo

## Función para descargar paquetes en automático
foo <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}


## Cargamos librerías de trabajo
foo(c("readr",
      "tidyverse",
      "ggsci",
      "ggthemes",
      "sf",
      "srvyr",
      "kableExtra"))

#Descarga de archivos
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_concentradohogar_csv.zip"

##Creación de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="concentradohogar.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "concentradohogar.csv")
unlink(td)

#Leer el archivo
concentrado_hogar<-read.csv(fpath)

concentrado_hogar <- concentrado_hogar %>% 
  ## Identificamos hogares que tienen ingreso por renta
  mutate(ingxrenta = case_when(rentas > 0 ~ 1,
                               TRUE ~ 0),
         ## Identificamos hogares donde se paga renta
         hogar_rentado = case_when(alquiler > 0 ~1,
                                   TRUE ~ 0))

datos_colima <-  concentrado_hogar %>% 
  ## Filtramos para Colima
  filter(ubica_geo %in% c(6001,
                          6002,
                          6003,
                          6004,
                          6005,
                          6006,
                          6007,
                          6008,
                          6009,
                          6010)) 

## Identificamos deciles de ingreso de los datos
quantile(datos_colima$ing_cor,
         prob = seq(0, 1, length = 11),
         type = 5)

## Agregamos columna con decil de ingreso del hogar
datos_colima <- datos_colima %>% 
  mutate(decil_ingresos = case_when(ing_cor < 13795.07 ~ 1,
                                   ing_cor >=  13795.07 &  ing_cor < 19441.07 ~ 2,
                                   ing_cor >=  19441.07 &  ing_cor < 24399.92 ~ 3,
                                   ing_cor >=  24399.92 &  ing_cor < 29617.14 ~ 4,
                                   ing_cor >=  29617.14 &  ing_cor < 35693.88 ~ 5,
                                   ing_cor >=  35693.88 &  ing_cor < 41755.81 ~ 6,
                                   ing_cor >=  41755.81 &  ing_cor < 51176.15 ~ 7,
                                   ing_cor >=  51176.15 &  ing_cor < 64765.74 ~ 8,
                                   ing_cor >=  64765.74 &  ing_cor < 91440.85 ~ 9,
                                   ing_cor >=  91440.85 ~ 10,))
## Limpiamos área de trabajo
rm(concentrado_hogar)

## Diseño muestral para Colima
my_design <- datos_colima %>% 
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)

## Tamaño del ingreso por rentas en Colima
tam_mkt_rnts <- my_design %>% 
  survey_tally(rentas,
    vartype = c("cv", "ci")) %>% 
    mutate(n_cv = round((n_cv * 100),2)) %>% 
  rename(tm_mkt = n,
         tm_mkt_cv = n_cv,
         tm_mkt_low = n_low,
         tm_mkt_upp = n_upp)

## Ingresos estimados por diferentes impuestos al arrendamiento
f_est_imp_arr <- function(imp, chr){
  ing_est_rnts <- tam_mkt_rnts %>% 
    mutate(ing_rnts_pe = tm_mkt*imp,
           ing_rnts_low = tm_mkt_low*imp,
           ing_rnts_upp = tm_mkt_upp*imp,
           impuesto = chr) %>% 
    rename(cv = tm_mkt_cv) %>% 
    select(impuesto,
           ing_rnts_pe,
           ing_rnts_low,
           ing_rnts_upp)
}

## Creamos estimados para distintos porcentaje de impuestos
imp_2 <- f_est_imp_arr(0.02, "2 por ciento")
imp_4 <- f_est_imp_arr(0.04, "4 por ciento")
imp_6 <- f_est_imp_arr(0.06, "6 por ciento")
imp_8 <- f_est_imp_arr(0.08, "8 por ciento")

## Unimos base de datos
ing_est_rnts <- bind_rows(imp_2,
                          imp_4,
                          imp_6,
                          imp_8)
## Lipiamos área de trabajo
rm(imp_2,
   imp_4,
   imp_6,
   imp_8)

ing_est_rnts %>%
  kable(caption=text_spec("Ingreso estimado por impuesto al arrendamiento",
                          bold=T, color="black",font_size = 30),
        format="html",
        align = "c",
        col.names = c("Impuesto",
                      "Estimado de recaudación",
                      "Límite inferior",
                      "Límite superior"))%>%
  kable_styling(full_width = F, font_size = 20,
                html_font = "Montserrat Medium")%>%
  row_spec(0, bold = F, color = "black", background = "#9ebcda")%>%
  footnote(general = "@jkvisfocri. Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2018",
           general_title = "
Fuente: ")

## Hogares por decil de ingresos
hogs_decil <- my_design %>% 
  group_by(decil_ingresos) %>% 
  summarise(hogares=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(hogares_cv=
           hogares_cv*100
  )

## Ingreso por rentas
## Número de hogares que tienen ingreso por rentas en Colima
hog_rents <- my_design %>%
  filter(ingxrenta==1)%>%
  summarise(ingxrenta=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(ingxrenta_cv=
           ingxrenta_cv*100,
  )

## Hogares con rentas por decil de ingresos en Colima
hog_rents_dec <- my_design %>%
  filter(ingxrenta==1)%>%
  group_by(decil_ingresos) %>% 
  summarise(ingxrenta=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(ingxrenta_cv=
           ingxrenta_cv*100
  )

hog_rentas_dec_pct <- hog_rents_dec %>% 
  mutate(pct_ingxrenta = round(ingxrenta / 27507 * 100, 2),
         pct_ingxrenta_low = round(ingxrenta_low / 27507 * 100, 2),
         pct_ingxrenta_upp = round(ingxrenta_upp / 27507 * 100, 2),
         ingxrenta_cv=round(ingxrenta_cv,2),
         ) %>% 
  select(decil_ingresos,
         pct_ingxrenta,
         ingxrenta_cv,
         pct_ingxrenta_low,
         pct_ingxrenta_upp)

#Tabla por decil de ingresos
hog_rentas_dec_pct %>%
  kable(caption=text_spec("Distribución por deciles del ingreso por rentas, Colima, 2018",
                          bold=T, color="black",font_size = 30),
        format="html",
        align = "c",
        col.names = c("Decil de ingresos",
        "Distribución porcentual de la renta",
                      "Coeficiente de variación",
                      "Límite inferior",
                      "Límite superior"))%>%
  kable_styling(full_width = F, font_size = 20,
                html_font = "Montserrat Medium")%>%
  row_spec(0, bold = F, color = "black", background = "#9ebcda")%>%
  footnote(general = "@jkvisfocri. Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2018
           Nota: cuando el coeficiente de variación es superior al 30%, el estimado no es representativo",
           general_title = "
Fuente: ")


## Gráfico 
hog_rentas_dec_pct %>% 
  ggplot(aes(x = decil_ingresos,
             y = pct_ingxrenta))+
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
        axis.title.x =  element_blank()) + 
  geom_bar(stat = "identity",
           color= 'darkblue',
           fill = "cornflowerblue",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=pct_ingxrenta_low,
                    ymax=pct_ingxrenta_upp),
                width=.2,
                position=position_dodge(.9))+
  geom_line(aes(x=decil_ingresos,
                y = cumsum(pct_ingxrenta))) +
  scale_x_continuous(breaks = c(1:10),
                     labels = c("I",
                                "II",
                                "III",
                                "IV",
                                "V",
                                "VI",
                                "VII",
                                "VIII",
                                "IX",
                                "X"))+
  labs(title = "Concentración del ingreso por arrendamiento en hogares, por decil de ingreso",
       subtitle = "Colima, 2018",
       x = "Decil de ingresos",
       y = "Porcentaje de ingresos",
       caption = "Fuente: ENIGH 2018, INEGI,
       Elaborado por @jkvisfocri")



## Hogares que son rentados
hogs_alquiler <- my_design %>%
  filter(hogar_rentado == 1) %>% 
  summarise(hogar_rentado=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(hogar_rentado_cv=
           hogar_rentado_cv*100,
  )


## Hogares que son rentados por decil de ingresos en Colima
hogs_alquiler_dec <- my_design %>%
  filter(hogar_rentado ==1)%>%
  group_by(decil_ingresos) %>% 
  summarise(hogar_rentado=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(hogar_rentado_cv=
           hogar_rentado_cv*100
  )

## Porcentaje de hogares rentados por decil de ingresos
hogs_alquiler_dec_pct <- merge(hogs_alquiler_dec,
                               hogs_decil,
                               by = "decil_ingresos") %>% 
  mutate(pct_hogar_rentado = round(hogar_rentado / hogares * 100, 2),
         pct_hogar_rentado_low = round(hogar_rentado_low / hogares * 100, 2),
         pct_hogar_rentado_upp = round(hogar_rentado_upp / hogares * 100, 2),
         hogar_rentado_cv=round(hogar_rentado_cv,2),
  ) %>% 
  select(decil_ingresos,
         pct_hogar_rentado,
         hogar_rentado_cv,
         pct_hogar_rentado_low,
         pct_hogar_rentado_upp)


## Gráfico 
hogs_alquiler_dec_pct %>% 
  ggplot(aes(x = decil_ingresos,
             y = pct_hogar_rentado))+
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
        axis.title.x =  element_blank()) + 
  geom_bar(stat = "identity",
           color= 'darkblue',
           fill = "cornflowerblue",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=pct_hogar_rentado_low,
                    ymax=pct_hogar_rentado_upp),
                width=.2,
                position=position_dodge(.9))+
  scale_x_continuous(breaks = c(1:10),
                     labels = c("I",
                                "II",
                                "III",
                                "IV",
                                "V",
                                "VI",
                                "VII",
                                "VIII",
                                "IX",
                                "X"))+
  labs(title = "Porcentaje de hogares que son rentados por decil económico",
       subtitle = "Colima, 2018",
       x = "Decil de ingresos",
       y = "Porcentaje de hogares rentados",
       caption = "Fuente: ENIGH 2018, INEGI,
       Elaborado por @jkvisfocri")
