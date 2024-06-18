library(tidyverse)

setwd("/home/mcure/Documents/IFFSC/")

data <- read_rds("./dados_completos.rds")

env <- read_csv("preditores.csv") |>
  select(UA, freq.fire, bio15, cecmean, sandmean, claymean, nitrogenmean, elev) |>
  rename(plot_id = "UA", CEC = "cecmean", sand = "sandmean", clay = "claymean", N = "nitrogenmean", fire_freq = "freq.fire", elevation = "elev")

glimpse(env)
glimpse(data)


dados_1 <- inner_join(data, env, by = "plot_id")
glimpse(dados_1)
dim(dados_1)

# Standardiza os dados ------------------------------------
library(vegan)

dados_1 |> select(plot_id, MAT) |> filter(is.na(MAT))

dados_std <- dados_1[c(16:20,22:26)] %>%
  decostand(method = "standardize") |>
  tibble::as_tibble() |>
  add_column(plot_id = dados_1$plot_id) |>
  filter(plot_id != 1024)


# PCA
PCA <- dados_std %>%
  select(-plot_id) |>
  prcomp()

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
  as_tibble() |>
  add_column(Stage = dados_1 |> filter(plot_id != 1024) |> select(stage))%>%
  add_column(Forest_type = dados_1 |> filter(plot_id != 1024) |> select(forest.type))
loadings <- as.data.frame(PCA$rotation)
scores <- as.data.frame(PCA$x)
label <- rownames(loadings)

# Faz um biplot
pca_plot <- ggplot()+
  geom_point(data = dados_pca,
              aes(x = PC1, y = PC2, shape = factor(Stage$stage), color = Forest_type$forest.type),
              show.legend = T) +
  scale_color_manual(values = c("purple", "green3", "orange")) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = 10*PC1, yend = 10*PC2, alpha = 0.8),
               arrow = arrow(length = unit(0.7, "cm")), show.legend = F, color = "red") +
  geom_text(data = loadings, aes(x = 10*PC1, y = 10*PC2, label = label), color = "black", size = 4, nudge_x = 0.03, nudge_y = 0.04) +
  xlab("PC1 (33.42 %)") +
  ylab("PC2 (17.83 %)") +
  ggtitle("PCA of environmental variables")+
  guides(alpha = "none")+
  labs(shape = "Stage",
       color = "Forest Type",
       alpha = NULL) +
  theme(legend.position = "bottom")


# png("Fig_6.png", res = 300, height = 1990, width = 2500)
# pca_plot
# dev.off()




library(patchwork)
dados_1 |> ggplot(aes(y = fire_freq, x = forest.type))+
  geom_boxplot()+dados_1 |> ggplot(aes(y = MSI, x = forest.type))+
  geom_boxplot()+dados_1 |> ggplot(aes(y = elevation, x = forest.type))+
  geom_boxplot()


#+
  # dados_1 |>
  # ggplot(aes(x = forest.type, y = bio15, fill = forest.type))+
  # geom_boxplot(show.legend = F)+
  # scale_fill_manual(values = c("purple", "green3", "orange"))+
  # labs(fill = "Forest type", x = "Forest type", y = "bio15 (Rainfall seasonality) - CHELSEA")+
  # guides(size = "none", alpha = "none")+
  # theme(axis.text = element_text(size = 12),
  #       axis.title =  element_text(size = 14))
