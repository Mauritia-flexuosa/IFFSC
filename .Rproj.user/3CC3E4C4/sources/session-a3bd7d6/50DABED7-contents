library(tidyverse)
library(sp)
setwd("/home/mcure/Documents/IFFSC/")

# Script para download da serie temporal de EVI2 do Sentinel-2: https://code.earthengine.google.com/4055e6187ec02f33845b54a4b032e44c

# Script para download da série temporal de precipitação do CHIRPS: https://code.earthengine.google.com/bd2c602a7c7f3b46b12992351f64fe60

# Script para download da série temporal de temperatura do ERA 5: https://code.earthengine.google.com/47f05149a37c665650b4e520edebfff0

# Preparar os dados para o GEE=====

iffsc <- read_csv("./data_pts_stage_forest.csv")|>
  select(UA, Long, Lat) |>
  rename(plot_id = UA) |>
  rename(longitude = Long) |>
  rename(latitude = Lat)

## Transformar as corrdenadas de UTM para long lat

pts.coord.utm <- SpatialPoints(iffsc[,2:3], proj4string=CRS("+proj=utm +south +zone=22 +datum=WGS84"))
pts.coord.longLat <- spTransform(pts.coord.utm, CRS("+proj=longlat +datum=WGS84"))

rm(pts.coord.utm)

iffsc <- iffsc |> mutate(longitude = pts.coord.longLat$longitude, latitude = pts.coord.longLat$latitude)
#write_csv(iffsc, "/home/mcure/Documents/IFFSC/coordenadas_iffsc.csv")

## Preparar a lista de coordenadas para serem lidas em JavaScript e salvar em um arquivo
for(i in 1:dim(iffsc)[1]){
  	aux <- paste0("ee.Feature(ee.Geometry.Point([",iffsc$longitude[i],",",iffsc$latitude[i],"]), {plot_id: ",iffsc$plot_id[i],"}),")
      if (i == 1){
    	texto <- aux
     }else{
              texto <- rbind(texto, aux)
       }
       write.table(texto, "/home/mcure/Documents/IFFSC/coord_iffsc_earth_engine.txt")
}

########################### Pega os dados do Sentinel baixados do Google Drive =====
dir <- "/home/mcure/Documents/IFFSC"

s2 <- read_csv(paste0(dir, "/s2_IFFSC_2024.csv")) %>%
  # Muda o nome das variáveis
  rename(evi2 = value) %>%
  rename(time = date) %>%
  # Transforma a classe da hora para Date
  mutate(time = as.Date(time, format = "%Y-%m-%d")) %>%
  # agrupa por plot_id
  group_by(plot_id) %>%
  # Aninha os dados numa coluna de classe lista
  nest


# Junta os dois data.frames ('s2' e 'Dexter')
data <- right_join(iffsc, s2, by="plot_id")

complemento <- read_csv("./data_pts_stage_forest.csv") |>
  select(forest.type, stage)

data <- data |>
  add_column(forest.type = complemento$forest.type) |>
  add_column(stage = complemento$stage)

rm(complemento)
rm(iffsc)
rm(s2)
# Apenas um valor por mês. Se há mais de um valor, calcular a média.

# Cria uma lista vazia
ndvi.mon <- vector("list", length = length(data$plot_id))

# Para cada plot_id...

for (i in 1:dim(data)[1]) {

  dados <- data[i,] %>%
    # Pega a linha i
    # Seleciona a coluna data (que é uma lista de data.frames) e desaninha
    unnest(cols = c(data)) %>%
    # Seleciona as colunas que interessam
    dplyr::select(evi2|time)

  # Cria um objeto contendo a coluna evi2 e outro objeto contendo a coluna time
  time <- dados$time
  evi2 <- dados$evi2

  # remove o data.frame relativo ao plot_id i para pegar um próximo
  rm(dados)

  # Cria um xts a partir da série de evi2 com data por mês
  df <- data.frame(evi2) %>% xts::xts(order.by = time, "Months")

  #df %>% TSstudio::ts_plot()

  # Cria o endpoints para saber quantos valores há em cada mês
  ep <- xts::endpoints(xts::xts(df), on = "months")
  # Calcula a média mensal ignorando os NAs
  aux <- xts::period.apply(as.matrix(df$evi2), INDEX = ep,
                           FUN = mean, na.action=na.pass)

  # armazena a série para cada plot_id como um ítem da lista criada
  ndvi.mon[[i]] <- aux

  # Printa o i para controlar o andamento do loop
  print(paste0("Finalizados: ",i))
}

# Cria função para renomear as datas
rname <- function(lndvi){
  # Pega o nome atual onde o primeiro dia do mês varia mês-a-mês
  rownames(lndvi) <- substr(rownames(lndvi), 1, 10)
  # Seleciona só o ano e o mês e substitui os dia por '01'
  rownames(lndvi) <- paste(substr(rownames(lndvi), 1, 8),"01", sep = "")
  rname <- lndvi
}

# Aplica a função para a lista
ndvi.mon.n  <- lapply(ndvi.mon, FUN = rname)

# Pega a lista
ndvi.mon.n <- ndvi.mon.n %>%
  # transforma cada ítem em data.frame
  map(as.data.frame) %>%
  # Para todos os ítens da lista (data.frames), cria uma coluna (rowname) que antes era o nome das linhas
  map(rownames_to_column) %>%
  # Muda o nome desta coluna para 'date' em cada item da lista
  map(dplyr::rename, date=rowname)

# par(mfrow = c(3,3))
# for (i in 1:9) {
#   teste <- ndvi.mon.n |> pluck(i)
#   plot(teste); lines(teste)
# }

mean_evi2 <- vector("list", length = length(data$data))
for(i in 1:length(data$data)){

mean_evi2[i] <- data$data |>
  pluck(i) |>
  select(evi2) |>
  unlist() |>
  as.numeric() |>
  mean(na.rm = T) |>
  unlist()

}

mean_evi2 <- mean_evi2 |> unlist()


cv <- function(x){
  return(sd(x, na.rm = T)/mean(x, na.rm = T))
}

cv_evi2 <- vector("list", length = length(data$data))
for(i in 1:length(data$data)){

  cv_evi2[i] <- data$data |>
    pluck(i) |>
    select(evi2) |>
    unlist() |>
    as.numeric() |>
    cv() |>
    unlist()

}

cv_evi2 <- cv_evi2 |> unlist()


sd_evi2 <- vector("list", length = length(data$data))
for(i in 1:length(data$data)){

  sd_evi2[i] <- data$data |>
    pluck(i) |>
    select(evi2) |>
    unlist() |>
    as.numeric() |>
    sd(na.rm = T) |>
    unlist()

}

sd_evi2 <- sd_evi2 |> unlist()


data <- data |>
  add_column(mean_evi2 = mean_evi2) |>
  add_column(sd_evi2 = sd_evi2) |>
  add_column(cv_evi2 = cv_evi2)


# Ciclo anual
tabela <- data$data

# Calcula a proporção de NAs
proporção_NA <- function(vetor) {
  return(sum(is.na(vetor)) / dim(vetor)[1])
}

prop_na <- vector("list", length = length(tabela))
for(i in 1:length(tabela)){
  prop_na[[i]] <- tabela %>%
    pluck(i) %>%
    dplyr::select(evi2) %>% proporção_NA %>% round(digits = 2)
}

prop_na <- prop_na %>% unlist

range(prop_na)

# Usando na.approx para completar as séries temporais

library(zoo)

evi2_aux <- vector("list", length = length(tabela))
for (i in seq_along(tabela)) {
  evi2_aux[[i]] <- na.approx(zoo(tabela %>%
                                   pluck(i) %>%
                                   dplyr::select(evi2) %>%
                                   as.matrix), tabela %>%
                               pluck(i) %>%
                               dplyr::select(time) %>%
                               as.matrix %>%
                               as.Date, rule=2)
}


#prop_without <- evi2_aux %>% map(proporção_NA) %>% unlist

# Adiciona coluna em cada tabela da lista
dados1 <- vector("list", length = length(tabela))

for (i in seq_along(tabela)) {
  #  if(prop_without[i] == 0){
  dados1[[i]] <- tabela[[i]] %>%
    add_column(without_missing = evi2_aux[[i]] %>% as.data.frame)
  #}else{
  #    dados1[[i]] <- NULL
  #  }
}


# ANNUAL CYCLE STORED FOR MEANS====
#mon <- matrix(NA, nrow = dim(exp.leaf)[1], ncol = 13)
mon <- matrix(NA, nrow = length(tabela)[1], ncol = 12)

for (i in 1:length(tabela)[1]){
  print(i)
  # creating a df to plot the boxplots
  dfaux <- data.frame(tabela |> pluck(i) |> select(time) |> rename(date = time),
#                      dados1 |> pluck(i) |> select(rainfall),
                      dados1 |> pluck(i) |> select(evi2),
                      factor(substr(dados1 |> pluck(i) |> select(time) |> rename(date = time) |> as.matrix(), 6, 7)))
#  colnames(dfaux) <- c("date", "rain", "evi2", "month")
  colnames(dfaux) <- c("date", "evi2", "month")

  # subsetting by month and saving
  for (j in 1:length(levels(dfaux$month))){
    aux <- subset(dfaux, month == levels(dfaux$month)[j])
    mon[i,j] <- mean(aux$evi2)
    #mon[i,j+1] <- mean(aux$evi2)
  }
}


colnames(mon) <- c("Jan", "Feb", "Mar",
                   "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep",
                   "Oct", "Nov", "Dec")


mon <- as.data.frame(mon)

month <- rep(colnames(mon), dim(mon)[2])
data1 <- reshape2::melt(mon  %>%
                         add_column(plot_id = data$plot_id),
                       id.vars = "plot_id",
                       variable.name = "month",
                       value.name = "evi2")

data1 <- data1 %>%
  add_column(forest.type = rep(data$forest.type, 12)) %>%
  add_column(stage = rep(data$stage, 12))

#write_csv(data1,file = "./annual_cycle_evi2.csv")

# Processa dados de chuva do Chirps
chirps <- read_csv("./rainfall_iffsc.csv")

chirps <- chirps |> add_column(month = chirps |>
  select(date) |>
  as.matrix() |>
  substr(6,7) |>
  as.numeric(),
  year = chirps |>
    select(date) |>
    as.matrix() |>
    substr(1,4) |>
    as.numeric())


# Junta EVI2 e precipitação na mesma tabela de dados

rainfall_evi2 <- left_join(data|>
            unnest(cols = data) |>
            mutate(time = paste0(data |>
                                   unnest(cols = data) |>
                                   select(time) |>
                                   as.matrix() |>
                                   substr(1,7),"-01"))|>
            group_by(plot_id,time) |>
            summarise(monthly_evi2 = mean(evi2)),
          chirps |>
            group_by(plot_id, year, month) |>
            summarise(rainfall = sum(value)) |> #precipitação acumulado no mês
            mutate(time = paste0(year, ifelse(month>9, "-","-0"), month, "-01")) |>
            select(plot_id, rainfall, time),
          by = c("time", "plot_id"))


#### Junta

tabela <- rainfall_evi2 |> group_by(plot_id) |> nest()

# Usando na.approx para completar as séries temporais

evi2_aux <- vector("list", length = length(tabela$data))
for (i in seq_along(tabela$data)) {
  evi2_aux[[i]] <- na.approx(zoo(tabela$data %>%
                                   pluck(i) %>%
                                   dplyr::select(monthly_evi2) %>%
                                   as.matrix), tabela$data %>%
                               pluck(i) %>%
                               dplyr::select(time) %>%
                               as.matrix %>%
                               as.Date, rule=2)
}

# Adiciona coluna em cada tabela da lista
dados1 <- vector("list", length = length(tabela$data))

for (i in seq_along(tabela$data)) {
  #  if(prop_without[i] == 0){
  dados1[[i]] <- tabela$data[[i]] %>%
    add_column(without_missing = evi2_aux[[i]] %>% as.data.frame)
  #}else{
  #    dados1[[i]] <- NULL
  #  }
}

dados1 <- dados1 |>
  map(mutate, monthly_evi2 = without_missing$monthly_evi2)
dados1 |> head()


# Acoplamento entre evi2 e chuva =====
#===============
library(quantmod)


# Usando o EVI2 (ndvi_aux) gerado com na.approx
coup_0 <- vector("list", length = length(dados1))
for (i in 1:length(dados1)) {
  evi2_aux <- pluck(dados1,i)[2] %>% as.matrix
  rainfall_aux <- pluck(dados1,i)[4] %>% as.matrix
  try(aux <- cor.test(purrr::map(rainfall_aux, Lag,0) %>%
                        unlist %>%
                        as.numeric,
                      evi2_aux %>% unlist %>% as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros



  if ( i == 1){
    lag.0 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.0 <- c(lag.0, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_0 <- cbind(lag.0,p.value)
}

coup_0 %>% head

lag.0 %>% head

coup_1 <- vector("list", length = length(dados1))

for (i in 1:length(dados1)) {

  try(aux <- cor.test(purrr::map(pluck(dados1,i)[2], Lag,1) %>%
                        unlist %>%
                        as.numeric,
                      pluck(dados1,i)[4] %>%
                        unlist %>%
                        as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros

  if ( i == 1){
    lag.1 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.1 <- c(lag.1, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_1 <- cbind(lag.1,p.value)
}
coup_1
lag.1 %>% head


coup_2 <- vector("list", length = length(dados1))
for (i in 1:length(dados1)) {

  try(aux <- cor.test(purrr::map(pluck(dados1,i)[2], Lag,2) %>%
                        unlist %>%
                        as.numeric,
                      pluck(dados1,i)[4] %>%
                        unlist %>%
                        as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros

  if ( i == 1){
    lag.2 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.2 <- c(lag.2, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_2 <- cbind(lag.2,p.value)
}
coup_2
lag.2 %>% head

coup_3 <- vector("list", length = length(dados1))
for (i in 1:length(dados1)) {

  try(aux <- cor.test(purrr::map(pluck(dados1,i)[2], Lag,3) %>%
                        unlist %>%
                        as.numeric,
                      pluck(dados1,i)[4] %>%
                        unlist %>%
                        as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros

  if ( i == 1){
    lag.3 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.3 <- c(lag.3, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_3 <- cbind(lag.3,p.value)
}
coup_3
lag.3 %>% head

coup_4 <- vector("list", length = length(dados1))
for (i in 1:length(dados1)) {

  try(aux <- cor.test(purrr::map(pluck(dados1,i)[2], Lag,4) %>%
                        unlist %>%
                        as.numeric,
                      pluck(dados1,i)[4] %>%
                        unlist %>%
                        as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros

  if ( i == 1){
    lag.4 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.4 <- c(lag.4, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_4 <- cbind(lag.4,p.value)
}

lag.4 %>% head

lags <- cbind(as.data.frame(coup_0 %>% as.data.frame(row.names = "none") %>% select(lag.0)),as.data.frame(coup_1 %>% as.data.frame(row.names = "none") %>% select(lag.1)),as.data.frame(coup_2 %>% as.data.frame(row.names = "none") %>% select(lag.2)),as.data.frame(coup_3 %>% as.data.frame(row.names = "none") %>% select(lag.3)),as.data.frame(coup_4 %>% as.data.frame(row.names = "none") %>% select(lag.4)))


p_value <- cbind(as.data.frame(coup_0 %>% as.data.frame(row.names = "none") %>% select(p.value)),as.data.frame(coup_1 %>% as.data.frame(row.names = "none") %>% select(p.value)),as.data.frame(coup_2 %>% as.data.frame(row.names = "none") %>% select(p.value)),as.data.frame(coup_3 %>% as.data.frame(row.names = "none") %>% select(p.value)),as.data.frame(coup_4 %>% as.data.frame(row.names = "none") %>% select(p.value)))

colnames(p_value) <- c("p.value_0","p.value_1","p.value_2","p.value_3","p.value_4")



# Maior greenness-rainfall coupling dentre todos os lags
z <- data |>
  add_column(lags, p_value) |> select(10:14) |> as.matrix()
for (i in 1:dim(z)[1]){
  aux <- z[i,order(-abs(z[i,]))[1]]
  if (i == 1){
    coup_max <- aux
  }else{
    coup_max <- c(coup_max, aux)
  }
}
coup_max <- coup_max %>% unlist

p.val <- vector("list", dim(p_value)[1])
for(i in 1:dim(p_value)[1]){
  p.val[[i]] <- p_value[i,coup_max[i] %>% unlist %>% names %>% substr(5,5) %>% as.numeric +1]
}

p.val <- p.val %>% unlist %>%
  as.data.frame

# Lag at maximum coupling
for (i in 1:dim(z)[1]){
  aux <- order(-abs(z[i,]))[1]
  if (i == 1){
    lag <- aux-1
  }else{
    lag <- c(lag, aux-1)
  }
}

# Adiciona o maior acoplamento e o lag em que ele ocorreu
data <- data %>%
  add_column(coupling = coup_max) %>%
  add_column(p_value = p.val) %>%
  add_column(lag_max = lag)


# Temperatura  ====

era5 <- read_csv("./temperatura_iffsc.csv") |>
  rename(temperature = value, time = date) |>
  mutate(temperature = (temperature - 273.15))# Transforma Kelvin para graus Celsius

era5 |> head()

plot_id = era5 |>
  group_by(plot_id) |>
  nest() |>
  select(plot_id)


dados <- vector("list", length = length(dados1))
for (i in 1:length(dados1)) {
  dados[[i]] <- dados1 |> pluck(i) |> add_column(plot_id = plot_id[i,])
}

dados <- dados |>
  map(select,1,2,4,6) |>
  map(mutate,plot_id = plot_id$plot_id)|>
  map(mutate,time = as.Date(time))


dados |> map(head) |> head(n = 1)
era5 |> map(head) |> head(n = 1)


tabela <- vector("list", length = length(dados1))
for (i in 1:length(dados1)) {
  temp_aux <- era5 |>
    filter(plot_id == dados |>
             pluck(i) |>
             select(4) |>
             unique() |>
             as.numeric())

  aux <- left_join(dados |> pluck(i), temp_aux, by = "time") |>
    rename(plot_id = plot_id.y) |> select(1,2,3,5,6)

  tabela[[i]] <- aux
}

tabela |> map(head) |> head(n = 1)


## Acoplamento do EVI2 com a temperatura =====

coup_0 <- vector("list", length = length(tabela))
for (i in 1:length(tabela)) {
  evi2_aux <- pluck(tabela,i)[2] %>% as.matrix
  temperature_aux <- pluck(tabela,i)[4] %>% as.matrix
  try(aux <- cor.test(purrr::map(temperature_aux, Lag,0) %>%
                        unlist %>%
                        as.numeric,
                      evi2_aux %>% unlist %>% as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros



  if ( i == 1){
    lag.0 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.0 <- c(lag.0, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_0 <- cbind(lag.0,p.value)
}

coup_0 %>% head

lag.0 %>% head

coup_1 <- vector("list", length = length(tabela))

for (i in 1:length(tabela)) {

  try(aux <- cor.test(purrr::map(pluck(tabela,i)[2], Lag,1) %>%
                        unlist %>%
                        as.numeric,
                      pluck(tabela,i)[4] %>%
                        unlist %>%
                        as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros

  if ( i == 1){
    lag.1 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.1 <- c(lag.1, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_1 <- cbind(lag.1,p.value)
}
coup_1
lag.1 %>% head


coup_2 <- vector("list", length = length(tabela))
for (i in 1:length(tabela)) {

  try(aux <- cor.test(purrr::map(pluck(tabela,i)[2], Lag,2) %>%
                        unlist %>%
                        as.numeric,
                      pluck(tabela,i)[4] %>%
                        unlist %>%
                        as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros

  if ( i == 1){
    lag.2 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.2 <- c(lag.2, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_2 <- cbind(lag.2,p.value)
}
coup_2
lag.2 %>% head

coup_3 <- vector("list", length = length(tabela))
for (i in 1:length(tabela)) {

  try(aux <- cor.test(purrr::map(pluck(tabela,i)[2], Lag,3) %>%
                        unlist %>%
                        as.numeric,
                      pluck(tabela,i)[4] %>%
                        unlist %>%
                        as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros

  if ( i == 1){
    lag.3 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.3 <- c(lag.3, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_3 <- cbind(lag.3,p.value)
}
coup_3
lag.3 %>% head

coup_4 <- vector("list", length = length(tabela))
for (i in 1:length(tabela)) {

  try(aux <- cor.test(purrr::map(pluck(tabela,i)[2], Lag,4) %>%
                        unlist %>%
                        as.numeric,
                      pluck(tabela,i)[4] %>%
                        unlist %>%
                        as.numeric,
                      method = "kendall",
                      na.action = na.pass,
                      exact = F)) # função 'try' ignora os erros

  if ( i == 1){
    lag.4 <- c(aux$estimate)
    p.value <- c(aux$p.value)
  }else{
    lag.4 <- c(lag.4, aux$estimate)
    p.value <- c(p.value, aux$p.value)
  }
  coup_4 <- cbind(lag.4,p.value)
}
coup_4 |> head()

lags_temp <- cbind(as.data.frame(coup_0 %>% as.data.frame(row.names = "none") %>% select(lag.0)),as.data.frame(coup_1 %>% as.data.frame(row.names = "none") %>% select(lag.1)),as.data.frame(coup_2 %>% as.data.frame(row.names = "none") %>% select(lag.2)),as.data.frame(coup_3 %>% as.data.frame(row.names = "none") %>% select(lag.3)),as.data.frame(coup_4 %>% as.data.frame(row.names = "none") %>% select(lag.4)))


p_value_temp <- cbind(as.data.frame(coup_0 %>% as.data.frame(row.names = "none") %>% select(p.value)),as.data.frame(coup_1 %>% as.data.frame(row.names = "none") %>% select(p.value)),as.data.frame(coup_2 %>% as.data.frame(row.names = "none") %>% select(p.value)),as.data.frame(coup_3 %>% as.data.frame(row.names = "none") %>% select(p.value)),as.data.frame(coup_4 %>% as.data.frame(row.names = "none") %>% select(p.value)))

colnames(p_value_temp) <- c("p.value_0_t","p.value_1_t","p.value_2_t","p.value_3_t","p.value_4_t")

p_value_temp %>% head


data |>
  add_column(lags_temp, p_value_temp) |>
  colnames()


# Maior coupling dentre todos os lags
z <- data |>
  add_column(lags_temp, p_value_temp) |> select(13:17) |> as.matrix()
for (i in 1:dim(z)[1]){
  aux <- z[i,order(-abs(z[i,]))[1]]
  if (i == 1){
    coup_max_t <- aux
  }else{
    coup_max_t <- c(coup_max_t, aux)
  }
}
coup_max_t <- coup_max_t %>% unlist

p.val <- vector("list", dim(p_value_temp)[1])
for(i in 1:dim(p_value_temp)[1]){
  p.val[[i]] <- p_value_temp[i,coup_max_t[i] %>% unlist %>% names %>% substr(5,5) %>% as.numeric +1]
}

p.val <- p.val %>% unlist %>%
  as.data.frame

# Lag at maximum coupling
for (i in 1:dim(z)[1]){
  aux <- order(-abs(z[i,]))[1]
  if (i == 1){
    lag <- aux-1
  }else{
    lag <- c(lag, aux-1)
  }
}

# Adiciona o maior acoplamento e o lag em que ele ocorreu
data <- data %>%
  add_column(coup_temp = coup_max_t) %>%
  add_column(p_value_temp = p.val) %>%
  add_column(lag_max_temp = lag) |>
  add_column(tabela) |>
  select(-data) |>
  janitor::clean_names()

MAT <- vector("list", length = length(tabela))
for (i in 1:length(tabela)) {

  MAT[i] <- data |> select(tabela) |> pluck(1) |> pluck(i) |>
    mutate(year = substr(time, 1,4)) |>
    filter(year != 2024) |>
    group_by(year) |>
    summarise(MAT = mean(temperature)) |>
    select(MAT) |>
    unlist() |>
    mean(na.rm = T)

}
MAT |> unlist()

MAP <- vector("list", length = length(tabela))
for (i in 1:length(tabela)) {

  MAP[i] <- data |> select(tabela) |> pluck(1) |> pluck(i) |>
    mutate(year = substr(time, 1,4)) |>
    filter(year != 2024) |>
    group_by(year) |>
    summarise(MAP = sum(rainfall)) |>
    select(MAP) |>
    unlist() |>
    mean(na.rm = T)

}
MAP |> unlist()


data <- data |>
  add_column(MAT = MAT |> unlist()) |>
  add_column(MAP = MAP |> unlist())

MinT <- vector("list", length = length(tabela))
for (i in 1:length(tabela)) {

  MinT[i] <- data |> select(tabela) |>
    pluck(1) |>
    pluck(i) |>
    mutate(year = substr(time, 1,4)) |>
    filter(year != 2024) |>
    filter(year != 2019) |>
    group_by(year) |>
    summarise(MinT = min(temperature)) |>
    select(MinT) |>
    unlist() |>
    min(na.rm = T)

}
MinT |> unlist() |>

beepr::beep(8)

data |>
  add_column(MinT = MinT |> unlist()) |>
  filter(p_value <= 0.05) |>
  ggplot(aes(x = MinT, y = coupling, color = factor(forest.type)))+
  geom_point()

data |>
  add_column(MinT = MinT |> unlist()) |>
  ggplot(aes(x = forest.type, y = MinT))+
  geom_boxplot()+labs(y = "Minimum annual temperature (ºC)")|data |>
#  add_column(MinT = MinT |> unlist()) |>
  ggplot(aes(x = forest.type, y = MAT))+
  geom_boxplot()+
  geom_boxplot()+labs(y = "Mean annual temperature (ºC)")|data |>
  #  add_column(MinT = MinT |> unlist()) |>
  ggplot(aes(x = forest.type, y = MAP))+
  geom_boxplot()+
  geom_boxplot()+labs(y = "Mean annual rainfall (ºC)")

SeasRain <- vector("list", length = length(tabela))
for (i in 1:length(tabela)) {

  SeasRain[i] <-
    data |> select(tabela) |>
    pluck(1) |>
    pluck(i) |>
    mutate(month = substr(time, 6,7) |> as.numeric()) |>
    mutate(year = substr(time, 1,4) |> as.numeric()) |>
    filter(year != 2024) |>
    filter(year != 2019) |>
    group_by(year,month) |>
#    filter(rainfall <= 100) |>
    summarise(seca = rainfall[rainfall <= 100]) |>
    group_by(year) |>
    summarise(n_months = n()) |>
    summarise(seasR = mean(n_months)*100/(12)) |>
    select(seasR) |>
    unlist()

}

SeasRain |> unlist()

SeasTemp <- vector("list", length = length(tabela))
for (i in 1:length(tabela)) {

  SeasTemp[i] <-
    data |> select(tabela) |>
    pluck(1) |>
    pluck(i) |>
    mutate(month = substr(time, 6,7) |> as.numeric()) |>
    mutate(year = substr(time, 1,4) |> as.numeric()) |>
    filter(year != 2024) |>
    filter(year != 2019) |>
    group_by(year,month) |>
    mutate(clima = ifelse(temperature <= 20, "frio", "calor")) |>
    group_by(year) |>
    summarise(n_months = sum(clima == "frio")) |>
    summarise(seasT = mean(n_months)*100/(12)) |>
    select(seasT) |>
    unlist()
}

SeasTemp |> unlist()

png("seasonality.png", res = 300, width = 2200, height = 2100)
  (data |>
  add_column(SeasRain = SeasRain |> unlist()) |>
  ggplot(aes(x = forest.type, y = SeasRain, fill = forest.type))+
  geom_boxplot(show.legend = F)+
#    geom_jitter(show.legend = F)+
    scale_fill_manual(values = c("purple", "green3", "orange"))+
    labs(y = "% months with rainfall <= 100mm", x = NULL, fill = "Forest type", title = "a")+
    theme(legend.position = "bottom",text = element_text(size = 14),
          #axis.ticks = element_blank(),
          #axis.text.x = element_blank(),
          axis.title.y = element_text(size = 11))|data |>
     #  add_column(MinT = MinT |> unlist()) |>
     ggplot(aes(x = forest.type, y = MAP, fill = forest.type))+
     geom_boxplot(show.legend = F)+
     labs(y = "Mean annual rainfall (mm)", x = NULL, title = "b")+
     scale_fill_manual(values = c("purple", "green3", "orange"))+
     theme(text = element_text(size = 14),
           axis.title.y = element_text(size = 11)))/(data |>
    add_column(SeasTemp = SeasTemp |> unlist()) |>
    ggplot(aes(x = forest.type, y = SeasTemp |> sqrt(), fill = forest.type))+
    geom_boxplot(show.legend = F)+
#    geom_jitter(show.legend = F)+
    scale_fill_manual(values = c("purple", "green3", "orange"))+
    labs(y = "Squared % months with temp <= 20ºC", x = "Forest type", title = "c")+
      theme(text = element_text(size = 14),
            #axis.ticks = element_blank(),
            #axis.text.x = element_blank(),
            axis.title.y = element_text(size = 11))+
  ylim(c(0,15))|
              data |>
                add_column(MinT = MinT |> unlist()) |>
                ggplot(aes(x = forest.type, y = MinT,fill= forest.type))+
                geom_boxplot(show.legend = F)+
                labs(y = "Minimum annual temperature (ºC)", x = "Forest type", title = "d")+
                scale_fill_manual(values = c("purple", "green3", "orange"))+
                theme(legend.position = "bottom",
                      text = element_text(size = 14),
                      axis.title.y = element_text(size = 11))+
                      ylim(c(0,30))
                |data |> ggplot(aes(x = forest.type, y = MAT, fill = forest.type))+
                geom_boxplot(show.legend = F)+
                labs(y = "Mean annual temperature (ºC)", x = "Forest type", title = "e")+
                scale_fill_manual(values = c("purple", "green3", "orange"))+
                theme(text = element_text(size = 14),
                      axis.title.y = element_text(size = 11))+
                ylim(c(0,30))
  )

dev.off()

# Calcula o Markham Seasonality Index (MSI)

# Função para calcular o Markham Seasonality Index (MSI) para um único dataframe
calculate_MSI_for_dataframe <- function(df) {
  # Calcular a precipitação total anual
  annual_precipitation <- df %>%
    group_by(year) %>%
    summarise(total_precipitation = sum(rainfall))

  # Função interna para calcular o MSI de um único ano
  calculate_MSI_for_year <- function(precipitation) {
    P_total <- sum(precipitation)
    P_mean <- P_total / 12
    MSI <- 0.5 * sum(abs((precipitation / P_total) - (1 / 12)))
    return(MSI)
  }

  # Calcular o MSI para cada ano
  msi_results <- df %>%
    group_by(year) %>%
    summarise(MSI = calculate_MSI_for_year(rainfall))

  return(msi_results)
}

# Função para calcular o MSI para uma lista de dataframes
calculate_MSI_for_list <- function(data_list) {
  lapply(data_list, calculate_MSI_for_dataframe)
}

MSI <- data$tabela |>
  map(mutate, year = substr(time,1,4)) |>
  map(mutate, month = substr(time,6,7)) |>
  map(filter, year != 2018) |>
  map(filter, year != 2024) |>
  calculate_MSI_for_list() |>
  map(summarise,msi = mean(MSI)) |>
  unlist() |>
  as.numeric()

# Função para calcular o coeficiente de variação (CV) da precipitação anual para um único dataframe
calculate_CV_for_dataframe <- function(df) {
  annual_precipitation <- df %>%
    group_by(year) %>%
    summarise(total_precipitation = sum(rainfall))

  mean_precipitation <- mean(annual_precipitation$total_precipitation)
  sd_precipitation <- sd(annual_precipitation$total_precipitation)

  CV <- (sd_precipitation / mean_precipitation) * 100
  return(CV)
}

# Função para calcular o CV para uma lista de dataframes
calculate_CV_for_list <- function(data_list) {
  sapply(data_list, calculate_CV_for_dataframe)
}

calculate_CV_for_list(data$tabela)

CV <- data$tabela |>
  map(mutate, year = substr(time,1,4)) |>
  map(mutate, month = substr(time,6,7)) |>
  map(filter, year != 2018) |>
  map(filter, year != 2024) |>
  calculate_CV_for_list()


data |> colnames()

data |>
  # add_column(CV = CV) |>
  # add_column(MSI = MSI) |>
  ggplot(aes(x = forest.type, y = MSI, fill = forest.type))+
  geom_boxplot(show.legend = F)+data |>
  # add_column(CV = CV) |>
  # add_column(MSI = MSI) |>
  ggplot(aes(x = forest.type, y = CV, fill = forest.type))+
  geom_boxplot(show.legend = F)


data <- data |>
  add_column(CV = CV) |>
  add_column(MSI = MSI)


#write_rds(data, file = "./dados_completos.rds")

# Referência dos dados usados

# Muñoz Sabater, J., (2019): ERA5-Land monthly averaged data from 1981 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). (<date of access>), doi:10.24381/cds.68d2bb30

# Funk, Chris, Pete Peterson, Martin Landsfeld, Diego Pedreros, James Verdin, Shraddhanand Shukla, Gregory Husak, James Rowland, Laura Harrison, Andrew Hoell & Joel Michaelsen. "The climate hazards infrared precipitation with stations-a new environmental record for monitoring extremes". Scientific Data 2, 150066. doi:10.1038/sdata.2015.66 2015.

# Copernicus Sentinel Data

