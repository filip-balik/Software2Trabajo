library(tidyverse)
library(ggplot2)

## Práctica grupal
### Paso a tidy data

```{r}
# tabla datos elecciones
datos_elecciones_tidy <- 
  datos_elecciones |> 
  mutate(across(where(is.logical), as.numeric)) |> 
  select(-c(tipo_eleccion, vuelta, codigo_distrito_electoral)) |> 
  pivot_longer(cols = -("anno": "votos_candidaturas"),
                names_to = "partido",
                values_to = "votos",
                values_drop_na = TRUE ) |> 
  filter(votos > 0)

datos_elecciones_tidy <-
  datos_elecciones_tidy |> 
  mutate(fecha = ymd(glue("{anno}{mes}01")),
         cod_mun = glue("{codigo_ccaa}-{codigo_provincia}-{codigo_municipio}"))

datos_tidy_cod_mun <- |> datos_tidy |> 
  left_join(cod_mun, by = "cod_mun")
```
# Agrupar los partidos # creo que ahora deberia de estar bien(filip) hay que borrar datos de la tabla los cuales no son de estos partidos.
datos_elecciones_tidy <-
  datos_elecciones_tidy |> 
  mutate(siglas = case_when(str_detect( partido, "SOCIALIST") ~ "PSOE",
                            str_detect(partido,"PARTIDO POPULAR")~ "PP",
                            str_detect(partido, "CIUDADANOS |CIUTADANS") ~ "CS",
                            str_detect(partido, "PODEM") ~ "UP",
                            str_detect(partido, "VOX") ~ "VOX",
                            str_detect(partido, "PARTIDO NACIONALISTA VASCO") ~ "PNV",
                            str_detect(partido, "BLOQUE NACIONALISTA GALEGO") ~ "BNG",
                            str_detect(partido, "COMPROMÍS") ~ "Compromís",
                            str_detect(partido, "CONVERGÈNCIA I UNIÓ") ~ "CiU",
                            str_detect(partido, "ESQUERRA REPUBLICANA DE CATALUNYA") ~ "ERC",
                            str_detect(partido, "EH-BILDU") ~ "EH Bildu",
                            str_detect(partido, "MÁS PAÍS") ~ "Más País",
                            TRUE ~ "OTROS" ))
```

```{r}
# Tabla encuestas
encuestas_tidy <-
  encuestas |> 
  pivot_longer(cols = -("type_survey":"turnout"),
               names_to = "partido",
               values_to = "votos",
               values_drop_na = TRUE) |> 
  filter(field_date_to - field_date_from > 1 & year(date_elec)>= 2018 &
           exit_poll == FALSE &  size >= 750) |> 
  drop_na(size) |> 
  select(-c(exit_poll))
```
### Preguntas

Pregunta 1

¿Cómo se reparte el voto de partidos de ámbito nacional (PSOE, PP, VOX, CS, MP, UP - IU) frente a los partidos de corte autonómico o nacionalista?

```{r}

```

Pregunta 2

¿Cuál es el partido ganador en los municipios de más de 100 000 habitantes de censo en cada una de las elecciones?

```{r}
datos_elecciones_tidy |> 
  group_by(fecha, cod_mun) |> 
  filter(censo > 100000) |> 
  slice_max(votos, n = 1) |> 
  select(fecha, cod_mun, votos, siglas, censo)
  
  

```

Pregunta 3

¿Qué partido fue el segundo cuando el primero fue el PSOE? ¿Y cuándo el primero fue el PP?

```{r}
# Priemro creamos una funcion que calcula el partido que quedo en la posición n
n_votado <- function(partidos, votos, n = 1) {
  
  datos <- tibble(partidos, votos) 
  siglas <-
    datos |>
    slice_max(n = n, votos, with_ties = FALSE) |> 
    slice_min(n = 1, votos, with_ties = FALSE) |> 
    pull(partidos)
 # output <- arrange(desc(votos)) |> slice(n, votos) |> pull(siglas)
  # mejor manera
  
  return(siglas)
}

# creamos una tabla nueva que nos guarda los ganadores y segundos de cada ellecion
datos_ganadores <- 
  datos_elecciones_tidy |> 
  filter(fecha == max(fecha)) |> # he puesto solo las ultimas elecciones pq si no tarda mucho
  mutate(ganador = n_votado(siglas, votos),
         segundo = n_votado(siglas, votos, n = 2),
         .by = c(fecha, cod_mun))
# calculamos que partidos fueron segundos cuando gano el PSOE
datos_ganadores |>
  filter(ganador == "PSOE") |> 
  count(segundo, sort = TRUE)
# lo mismo para el PP
datos_ganadores |>
  filter(ganador == "PP") |> 
  count(segundo, sort = TRUE)

# se podria añadir unos graficos de barras
```

Pregunta 4

¿En qué municipios de más 2000 habitantes de censo, provincias o autonomías la diferencia entre el ganador y el segundo es más elevada?

```{r}
datos_elecciones_tidy |> 
  filter(censo > 2000) |> 
  group_by(fecha, cod_mun) |> 
  arrange(desc(votos)) |> 
  
  
  
```

Pregunta 5

¿Cuáles son los municipios con mayor porcentaje de votos nulos de España?

```{r}

```

Pregunta 6

¿En qué sitios hay mayor participación? ¿Quién ganó en los sitios con mayor participación y donde menos? ¿A quién le beneficia la baja participación?

```{r}
# hay un error parue los datos me aparecen repetidos
datos_participacion <-
datos_ganadores |>
  #group_by(cod_mun,fecha) |> 
  mutate(participacion = (votos_blancos + votos_nulos + votos_candidaturas)/censo*100) 
# sitios con mayor participacion y sus ganadores
datos_participacion |> 
  #group_by(fecha) |> # Ahora mismo no es necesario pq solo estan la ultima
  arrange(desc(participacion)) |> 
  select(fecha, cod_mun, participacion, ganador)
  
  

```

Pregunta 7

¿Cómo analizar la relación entre censo y voto? ¿Es cierto que ciertos partidos ganan lo rural?

```{r}
# analizar la relacion entre censo y voto
# crear un gráfico de barras apiladas en el que la x sea el censo agrupado por intervalos y la y el count de los ganadores
datos_elecciones_tidy |> 
  ggplot()+
  geom_col(aes())
```

pregunta 8

¿Cómo calibrar el error de las encuestas? ¿Cómo visualizarlo? (recuerda que las encuestas son intención de voto a nivel nacional)

```{r}

```

Pregunta 9

¿En qué elección se equivocaron más las encuestas?

```{r}

```

Pregunta 10

¿Cómo se equivocaron las encuestas en partidos de ámbito nacional (PSOE, PP, VOX, CS, MP, UP - IU)

```{r}

```

Pregunta 11

¿Qué casas encuestadores acertaron más y cuales se desviaron más de los resultados?

```{r}

```
