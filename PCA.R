# Load required libraries
library(tidyverse)
library(vegan)

data <- read_rds("./dados_completos.rds")
data1 <- read_csv("./annual_cycle_evi2.csv") |>
  janitor::clean_names()

data_aux <- vector("list", length = dim(data)[1])
for (i in 1:dim(data)[1]) {

  data_aux[[i]] <- data$tabela |>
    pluck(i) |>
    select(plot_id, time, rainfall, temperature, monthly_evi2) |>
    mutate(month = substr(time,6,7)) |>
    mutate(year = substr(time,1,4))

}

# Junta os data.frames da lista em apenas um data.frame
data_analise <- do.call(rbind, data_aux) %>% group_by(month)

# Calcular a anomalia do EVI2 -----------------------------------
# Cria uma lista vazia
# lista <- vector("list", length = dim(data)[1])
#
# # para todos os pontos...
# for(i in 1:dim(data)[1]){
#
#   # Seleciona cada um dos data.frames na coluna dados1
#   df <- data$tabela %>%
#     pluck(i)
#
#   # Separa a coluna com a data em mês e ano
#   df$year <- format(df$time, "%Y")
#   df$month <- format(df$time, "%m")
#
#   # Calcula o EVI2 mensal e mantém a coluna Ano
#   df_mensal <- df %>%
#     group_by(month, year, plot_id) %>%
#     summarize(EVI2_mensal_por_ano = mean(monthly_evi2, na.rm = T))
#
#   # Calcula a média anual de EVI2
#   df_media_anual <- df %>%
#     group_by(year, plot_id) %>%
#     summarize(EVI2_media_anual = mean(monthly_evi2, na.rm = T))
#
#   # Para todos os valores no data.frame...
#   for (j in 1:dim(df_mensal)[1]) {
#
#     # Calcula a anomalia no EVI2 para cada mês
#     anomalia_aux <- df_media_anual[which(df_media_anual[,1] %>%
#                                            unlist %>%
#                                            as.numeric == df_mensal[j,2] %>%
#                                            as.numeric),3] - df_mensal[j,4]
#
#     # Salva os valores em um vetor
#     if(j == 1){
#       anomalia <- anomalia_aux %>% as.numeric
#     }else{
#       anomalia <- c(anomalia, anomalia_aux %>% as.numeric)
#     }
#
#     # Transforma em data.frame
#     anomalia %>%
#       as.data.frame
#   }
#
#   # Junta os resultados no data.frame que continha os dados usados no cálculo
#   df$anomalia <- anomalia
#
#   # Calcula a anomalia media mensal e salva um  data.frame para cada ponto em uma lista
#   lista[[i]] <- df %>%
#     group_by(month,plot_id) %>%
#     summarise(anomalia_mensal = mean(anomalia, na.rm = T)) |>
#     arrange(plot_id,"increasing")
#
# }
#
# # Junta os data.frames da lista em apenas um data.frame
# df_anomalia <- do.call(rbind, lista) %>% group_by(month) |>
#   mutate(month = as.numeric(month)) |>
#   arrange(plot_id,"increasing")
#
# df_anomalia |>  filter(month == 1)
#

# Standardiza os dados ------------------------------------
dados_std <- data[c(1,6:9,11:12,14)] %>%
  select(-1) %>%
  decostand(method = "standardize") |> tibble::as_tibble()

# PCA

PCA <- dados_std %>%
  prcomp

PCA %>% summary
# Eigenvalues

PC1 <- PCA %>%
  broom::tidy() %>%
  dplyr::filter(PC==1) %>%
  dplyr::select("value")

PC2 <- PCA %>%
  broom::tidy() %>%
  dplyr::filter(PC==2) %>%
  dplyr::select("value")

dados_pca <- data.frame(PC1 = PC1$value, PC2 = PC2$value)

# Eigenvetors
PCA$rotation[,1] %>%
  broom::tidy() %>%
  rename(variables = names, PC1 = x) %>%
  knitr::kable()

PCA$rotation[,2] %>%
  broom::tidy() %>%
  rename(variables = names, PC2 = x) %>%
  knitr::kable()

# Loadings
PCA %>% plot

# Prepara pro biplot

dados_pca <- dados_pca %>%
  add_column(Stage = data$stage)%>%
  add_column(Forest_type = data$forest_type)
loadings <- as.data.frame(PCA$rotation)
scores <- as.data.frame(PCA$x)
label <- rownames(loadings)

# Faz um biplot
pca_plot <- ggplot()+
  geom_jitter(data = dados_pca,
              aes(x = PC1, y = PC2, color = factor(Stage), shape = Forest_type),
              show.legend = T) +
  scale_color_manual(values = c("orange2", "brown")) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = 10*PC1, yend = 10*PC2, alpha = 0.8),
               arrow = arrow(length = unit(0.7, "cm")), show.legend = F, color = "red") +
  geom_text(data = loadings, aes(x = 10*PC1, y = 10*PC2, label = label), color = "black", size = 4, nudge_x = 0.03, nudge_y = 0.04) +
  xlab("PC1 (28 %)") +
  ylab("PC2 (27.5 %)") +
  ggtitle("PCA of vegetation grenness metrics")+
  guides(alpha = "none")+
  labs(color = "Stage",
       shape = "Forest Type",
       alpha = NULL) +
  theme(legend.position = c(0.82, 0.83))
pca_plot

png("Fig_5.png", res = 300, height = 1990, width = 2500)
pca_plot
dev.off()

