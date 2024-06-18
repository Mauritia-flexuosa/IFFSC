library(tidyverse)

setwd("/home/mcure/Documents/IFFSC/")
source("./PCA_environmental_variables.R")
data <- read_rds("./dados_completos.rds")
data1 <- read_csv("./annual_cycle_evi2.csv")

# Obs.: A coluna 'tabela' dentro de 'data' é uma lista de tibbles contendo as séries temporais das médias mensais de EVI2, chuva e temperatura de Jan 2019 até Jan 2024.

# Mapa --------------------------------------------------------------------

library(rnaturalearth)
library(rnaturalearthhires)
#remotes::install_github("ropensci/rnaturalearthhires")
BRA <- ne_states(country = "Brazil",
                 returnclass = "sf")

#png(paste0(dir, "/mapa1.png"), res = 300, width = 2000, height = 2000)
mapa <- BRA |> ggplot()+
  geom_sf()+
  coord_sf()+
  geom_point(data = data, aes(x = longitude, y = latitude, color = factor(forest.type)))+
  xlim(c(-58,-47))+
  ylim(c(-30, -25.5))+
  labs(color = "Forest type")+
  scale_color_manual(values = c("purple", "green4", "orange3"))+
  theme(legend.position = "bottom")
#dev.off()



# Figura Média e desvio ---------------------------------------------------

library(patchwork)
fig1 <-
  data |> ggplot(aes(y = mean_evi2, x = factor(forest.type), fill = factor(forest.type)))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~factor(stage), labeller = grupos_labeller)+
  labs(x = "Forest type", y = "Mean EVI2")+
  scale_fill_manual(values = c("purple", "green4", "orange3"))|data |> ggplot(aes(y = sd_evi2, x = factor(forest.type), fill = factor(forest.type)))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~factor(stage), labeller = grupos_labeller)+
  labs(x = "Forest type", y = "Standard deviation")+
  scale_fill_manual(values = c("purple", "green4", "orange3"))

fig_cv <-
  data |> ggplot(aes(y = cv_evi2, x = factor(forest.type), fill = factor(forest.type)))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~factor(stage), labeller = grupos_labeller)+
  labs(x = "Forest type", y = "cv EVI2")+
  scale_fill_manual(values = c("purple", "green4", "orange3"))|data |> ggplot(aes(y = sd_evi2, x = factor(forest.type), fill = factor(forest.type)))+
  geom_boxplot(show.legend = F)+
  facet_wrap(~factor(stage), labeller = grupos_labeller)+
  labs(x = "Forest type", y = "Standard deviation EVI2")+
  scale_fill_manual(values = c("purple", "green4", "orange3"))


# Ciclo anual -----------------------------------------------------------

grupos <- list("1" = "Estágio avançado" , "2" = "Estágio médio")

grupos_labeller <- function(variable,value){
  return(grupos[value])
}

annual_cycle <- data1 |>
  mutate(month = c(rep(1, 331), rep(2, 331), rep(3, 331), rep(4, 331), rep(5, 331), rep(6, 331), rep(7, 331), rep(8, 331), rep(9, 331), rep(10, 331), rep(11, 331), rep(12, 331))) |>
  janitor::clean_names() |>
  group_by(month, forest_type, stage) %>%
  summarise(`Mean EVI2` = mean(evi2,na.rm = T)) %>% #print(n = 51)
  ggplot(aes(y = `Mean EVI2`, x = round(month), color = factor(forest_type), alpha = 0.6))+
  geom_point(show.legend = T, size = 2.5)+
  geom_line(show.legend = F)+
  scale_color_manual(values = c("purple", "green3", "orange"))+
  labs(color = "Forest type")+
  guides(size = "none", alpha = "none")+
  facet_wrap(~factor(stage), labeller = grupos_labeller)+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.position = "bottom")+
  ggtitle(NULL)+
  xlab("Month")

annual_cycle+scale_x_date(date_labels = "%b", date_breaks = "1 month")

data1  |> janitor::clean_names() |>
  group_by(month, forest_type, stage) %>%
  summarise(`Median EVI2` = median(evi2,na.rm = T)) %>%
  ggplot(aes(fill = `Median EVI2`, x = month, y = factor(forest_type), alpha = 0.6))+
  geom_tile(show.legend = T, size = 2.5)+
  scale_fill_viridis_c()+
  labs(y = "Forest type")+
  guides(size = "none", alpha = "none")+
  facet_wrap(~factor(stage), labeller = grupos_labeller)+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(angle = 90, size = 10),
        legend.position = "bottom")+
  ggtitle(NULL)+
  xlab("Month")

data1  |>
  group_by(month, forest_type, stage) %>%
  summarise(`sd EVI2` = sd(evi2,na.rm = T)) %>%
  ggplot(aes(fill = `sd EVI2`, x = month, y = factor(forest_type), alpha = 0.6))+
  geom_tile(show.legend = T, size = 2.5)+
  scale_fill_continuous()+
  labs(y = "Forest type")+
  guides(size = "none", alpha = "none")+
  facet_wrap(~factor(stage), labeller = grupos_labeller)+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 90, size = 9),
        legend.position = "bottom")+
  ggtitle(NULL)+
  xlab("Month")




# png(paste0(dir, "/Fig_3.png"), res = 300, width = 3000, height = 2500)
# annual_cycle
# dev.off()


# Figuras acoplamento entre o greenness e a chuva ---------------------------

a <- data |>
    filter(p_value_temp <= 0.05) |>
  ggplot(aes(x = factor(forest.type), y = coup_temp, fill = factor(forest.type), alpha = 0.7))+
  geom_boxplot(show.legend = F)+
  geom_jitter(show.legend = F)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red4")+
  facet_wrap(~factor(stage), labeller = labeller())+
  scale_fill_manual(values = c("purple", "green3", "orange"))+
  labs(color = "Forest type", x = "Forest type", y = "Greenness-temperature Coupling")+
  guides(size = "none", alpha = "none")+
  theme(axis.text = element_text(size = 12),
        axis.title =  element_text(size = 14))

b <- data |>
  filter(p_value <= 0.05) |>
  ggplot(aes(x = factor(forest.type), y = coupling, fill = factor(forest.type), alpha = 0.7))+
  geom_boxplot(show.legend = F)+
  geom_jitter(show.legend = F)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red4")+
  facet_wrap(~factor(stage), labeller = labeller())+
  scale_fill_manual(values = c("purple", "green3", "orange"))+
  labs(color = "Forest type", x = "Forest type", y = "Greenness-Rainfall Coupling")+
  guides(size = "none", alpha = "none")+
  theme(axis.text = element_text(size = 12),
        axis.title =  element_text(size = 14))+
  ylim(c(-.7,.7))
a|b

# png("/home/mcure/Documents/IFFSC/coupling_iffsc.png", res = 300, width = 1600, height = 1400)
# a|b
# dev.off()



# Figuras acoplamento entre o greenness e a temperatura ------------------


c <- data |>
  #  filter(p_value >= 0.05) |>
  ggplot(aes(x = factor(forest.type), y = coup_temp, fill = factor(forest.type), alpha = 0.7))+
  geom_boxplot(show.legend = F)+
  geom_point(show.legend = F)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red4")+
  facet_wrap(~factor(stage), labeller = labeller())+
  scale_fill_manual(values = c("purple", "green3", "orange"))+
  labs(color = "Forest type", x = "Forest type", y = "Coupling")+
  guides(size = "none", alpha = "none")+
  theme(axis.text = element_text(size = 12),
        axis.title =  element_text(size = 14))

d <- data |>
  filter(p_value_temp <= 0.05) |>
  ggplot(aes(x = factor(forest.type), y = coup_temp, fill = factor(forest.type), alpha = 0.7))+
  geom_boxplot(show.legend = F)+
  geom_point(show.legend = F)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red4")+
  facet_wrap(~factor(stage), labeller = labeller())+
  scale_fill_manual(values = c("purple", "green3", "orange"))+
  labs(color = "Forest type", x = "Forest type", y = "Greenness-Temperature Coupling")+
  guides(size = "none", alpha = "none")+
  theme(axis.text = element_text(size = 12),
        axis.title =  element_text(size = 14))+
  ylim(c(-.7,.7))

# png("./couplings.png", res = 300, width = 2750, height = 1400)
# d|b
# dev.off()


# Figuras da sazonalidade e variabilidade interanual da chuva ------------------------------

png("Fig_7.png", res = 300, width = 1800, height = 1300)
dados_1 |>
  ggplot(aes(x = forest.type, y = MSI, fill = forest.type))+
  geom_boxplot(show.legend = F)+
  scale_fill_manual(values = c("purple", "green3", "orange"))+
  labs(fill = "Forest type", x = "Forest type", y = "Markham Seasonality Index (MSI) - CHIRPS")+
  guides(size = "none", alpha = "none")+
  theme(axis.text = element_text(size = 12),
        axis.title =  element_text(size = 14))+
  dados_1 |>
  ggplot(aes(x = forest.type, y = CV, fill = forest.type))+
  geom_boxplot(show.legend = F)+
  scale_fill_manual(values = c("purple", "green3", "orange"))+
  labs(fill = "Forest type", x = "Forest type", y = "CV (Interannual variability - CHIRPS)")+
  guides(size = "none", alpha = "none")+
  theme(axis.text = element_text(size = 12),
        axis.title =  element_text(size = 14))
dev.off()
#+
# dados_1 |>
# ggplot(aes(x = forest.type, y = bio15, fill = forest.type))+
# geom_boxplot(show.legend = F)+
# scale_fill_manual(values = c("purple", "green3", "orange"))+
# labs(fill = "Forest type", x = "Forest type", y = "bio15 (Rainfall seasonality) - CHELSEA")+
# guides(size = "none", alpha = "none")+
# theme(axis.text = element_text(size = 12),
#       axis.title =  element_text(size = 14))

# Figuras dos lags --------------------------------------------------------

e <- data |>
  filter(p_value <= 0.05) |>
  ggplot(aes(x = lag_max, fill = factor(forest.type), alpha = 0.7))+
  geom_bar(show.legend = F)+
  facet_wrap(~factor(stage), labeller = labeller())+
  scale_fill_manual(values = c("purple", "green3", "orange"))+
  labs(fill = "Forest type", y = "Count", x = "Lag to the maximum Greenness-Rainfall Coupling")+
  guides(size = "none", alpha = "none")+
  theme(axis.text = element_text(size = 12),
        axis.title =  element_text(size = 14),
        legend.position = "bottom")

f <-
  data |>
  filter(p_value_temp <= 0.05) |>
  ggplot(aes(x = lag_max_temp, fill = factor(forest.type), alpha = 0.7))+
  geom_bar(show.legend = T)+
  facet_wrap(~factor(stage), labeller = labeller())+
  scale_fill_manual(values = c("purple", "green3", "orange"))+
  labs(fill = "Forest type", y = "Count", x = "Lag to the maximum Greenness-Temperature Coupling")+
  guides(size = "none", alpha = "none")+
  theme(axis.text = element_text(size = 12),
        axis.title =  element_text(size = 14),
        legend.position = "bottom")


data |> janitor::clean_names() |>
  filter(p_value <= 0.05) |>
  filter(p_value_temp <= 0.05) |>
  ggplot(aes(x = coupling, y = coup_temp, color = factor(forest_type)))+
  geom_point()+
  facet_wrap(~factor(stage), labeller = labeller())+
  scale_color_manual(values = c("purple", "green3", "orange"))+
  labs(color = "Forest type", y = "Greenness-temperature coupling", x = "Greenness-rainfall Coupling")+
  guides(size = "none", alpha = "none")+
  theme(axis.text = element_text(size = 12),
        axis.title =  element_text(size = 14),
        legend.position = "bottom")


# Plota time series chuva e greenness -------------------------------------

teste <- data |>
  filter(coupling < 0) |> select(tabela) |>  pluck(1) |>
  pluck(1) |>
  select(time, rainfall, temperature, monthly_evi2)

library(plotly)

#plot_2 <-
plotly::plot_ly(teste) %>%
  plotly::add_lines(x=~time,y=~monthly_evi2,name = 'Mean monthly EVI2',connectgaps = F, color = I("green4")) %>%
  plotly::add_lines(x=~time,y=~rainfall,name = 'Mean monthly rainfall',type = 'scatter',mode = 'lines+markers',connectgaps = FALSE,yaxis = "y2", color = I("blue")) %>%
  plotly::layout(title = 'A',
         xaxis = list(title = "Month"),
         yaxis2 = list(side = 'right', overlaying = "y", title = 'Mean monthly precipitation (mm)', showgrid = FALSE, zeroline = FALSE))

dados1 <- df1 |> group_by(plot_id, month, forest_type) |>
  summarise(
   mmvi = mean(MMVI),
   mmr = mean(MMP)
  ) |>
  group_by(month) |>
  summarise(
    mmvi1 = mean(mmvi),
    mmr1 = mean(mmr)
  )

# Médias por forest type
plotly::plot_ly(dados1) %>%
  plotly::add_lines(x=~month,y=~mmvi1,name = 'Mean monthly EVI2',connectgaps = F, color = I("green")) %>%
  plotly::add_lines(x=~month,y=~mmr1,name = 'Mean monthly rainfall',type = 'scatter',mode = 'lines+markers',connectgaps = FALSE,yaxis = "y2", color = I("blue")) %>%
  plotly::layout(title = 'A',
                 xaxis = list(title = "Month"),
                 yaxis2 = list(side = 'right', overlaying = "y", title = 'Mean monthly precipitation (mm)', showgrid = FALSE, zeroline = FALSE))



# Breaking points ---------------------------------------------------------

library(strucchange)
library(changepoint)

change_point <- cpt.mean(teste$temperature, method = "BinSeg")

cpt.var(teste$temperature) |> plot()
plot(change_point)

# strucchange -------------------------------------------------------------

quebras1 <- breakpoints(monthly_evi2 ~ 1, data = teste)
quebras2 <- breakpoints(rainfall ~ 1, data = teste)

library(patchwork)
teste1 |> remove_missing() |>
  mutate(
    segmentos = breakfactor(quebras1)
  ) |>
  ggplot(aes(x = time, y = evi2, color = segmentos))+
  geom_point()+
  geom_line()


# teste Série completa --------------------------------------------------

teste1 <- data$tabela |> pluck(3) |> remove_missing()


plotly::plot_ly(teste1) %>%
  plotly::add_lines(x=~time,y=~evi2,name = 'Mean monthly EVI2',connectgaps = F, color = I("green4"))

plotly::plot_ly(data$tabela |> pluck(1)) %>%
  plotly::add_lines(x=~time,y=~monthly_evi2,name = 'Mean monthly EVI2', connectgaps = F, color = I("green4"),linetype = "dashed") |>
  plotly::add_lines(x=~time,y=~rainfall,name = 'Mean monthly rainfall (mm)', connectgaps = F, yaxis = "y2", color = I("blue")) %>%
  plotly::layout(title = 'A',
                 xaxis = list(title = "Month"),
                 yaxis2 = list(side = 'right', overlaying = "y", title = 'Mean monthly rainfall (mm)', showgrid = FALSE, zeroline = FALSE))

# Pacote changepoint
change_point <- cpt.mean(teste1$evi2, method = "BinSeg")

cpt.var(teste1$evi2) |> plot()
plot(change_point)

# Pacote strucchange
quebras1 <- breakpoints(evi2 ~ time, data = teste1)

teste1 |> remove_missing() |>
  mutate(
    segmentos = breakfactor(quebras1)
  ) |>
  ggplot(aes(x = time, y = evi2, color = segmentos))+
  geom_point()+
  geom_line()
