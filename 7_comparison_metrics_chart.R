  # Carregar pacotes necessários
  library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(writexl)

# Carregar os dados
file_path <- "./results/all_results.csv"
data <- read.csv(file_path)

current_date <- Sys.Date()

# Definir a ordem dos níveis dos biomas
biome_levels <- c("Brasil", "Amazônia", "Mata Atlântica", "Caatinga", "Cerrado", "Pantanal", "Pampa")

# Definir a ordem dos níveis das versões do mapa
map_version_levels <- c("v0-0-0", "v0-0-1", "v0-0-2", "v0-0-3", "v0-1-3", "v0-1-4", "v0-1-5", "v0-1-6" )

# Transformar os dados e definir a ordem dos biomas e das versões do mapa
data_long <- data %>%
  pivot_longer(cols = c(ME_mean, ME_sd, MAE_mean, MAE_sd, MSE_mean, MSE_sd, RMSE_mean, RMSE_sd, NSE_mean, NSE_sd, Rsquared_mean, Rsquared_sd), 
               names_to = c("Metric", ".value"), 
               names_pattern = "(.*)_(mean|sd)") %>%
  mutate(biome = factor(biome, levels = biome_levels),
         map_version = recode(map_version, 
                              "v0-0-0" = "v0-0-0", 
                              "v0-0-1" = "v0-0-1", 
                              "v0-0-2" = "v0-0-2", 
                              "v0-1-3" = "v0-1-3", 
                              "v0-1-4" = "v0-1-4",
                              "v0-1-5" = "v0-1-5",
                              "v0-1-6" = "v0-1-6",)) %>%
  filter(map_version %in% c("v0-0-0", "v0-0-1", "v0-0-2", "v0-0-3", "v0-1-3", "v0-1-4", "v0-1-5", "v0-1-6"))  # Filtrar as versões renomeadas

# Diretório para salvar os gráficos
output_dir <- "./figure/"

# Função para salvar gráficos com tabela
save_metric_plot <- function(metric_name, current_date) {
  # Filtrar os dados para a métrica atual
  metric_data <- data_long %>% filter(Metric == metric_name)
  
  # Definir os limites dos eixos y
  y_limits <- range(metric_data$mean + metric_data$sd, metric_data$mean - metric_data$sd, na.rm = TRUE)
  
  # Criar o gráfico
  p <- ggplot(metric_data, 
              aes(x = map_version, y = mean, color = map_version)) +
    geom_point(size = 3) +  # Aumentar o tamanho dos pontos
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    geom_line(aes(group = interaction(biome, map_version))) +
    facet_wrap(~ biome, scales = "free_y") +
    scale_y_continuous(limits = y_limits) +  # Padronizar os valores dos eixos
    labs(title = paste("Métrica:", metric_name),
         x = paste("COMPARATIVO DAS VERSÕES", metric_name),
         y = metric_name) +
    theme_bw() +  # Define fundo branco
    theme(axis.title = element_text(size = 18),  # Aumenta a fonte dos títulos dos eixos
          axis.text = element_text(size = 16),  # Aumenta a fonte dos rótulos dos eixos
          strip.text = element_text(size = 16),  # Aumenta a fonte dos rótulos das faixas
          plot.title = element_text(size = 20, face = "bold"),  # Aumenta a fonte do título do gráfico
          legend.title = element_text(size = 18),  # Aumenta a fonte do título da legenda
          legend.text = element_text(size = 16),  # Aumenta a fonte do texto da legenda
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  # Criar a tabela de dados e arredondar os valores para 3 casas decimais
  table_data <- metric_data %>%
    select(biome, map_version, mean) %>%
    spread(key = map_version, value = mean) %>%
    arrange(biome) %>%
    mutate(across(where(is.numeric), round, 3))
  
  # Adicionar o nome da métrica como cabeçalho da coluna
  colnames(table_data) <- c("Bioma", "v0-0-0", "v0-0-1", "v0-0-2", "v0-0-3", "v0-1-3", "v0-1-4", "v0-1-5", "v0-1-6")  # Renomear as colunas com os novos nomes
  
  # Criar a tabela com tableGrob
  table_grob <- tableGrob(table_data, theme = ttheme_default(base_size = 16))
  
  # Aplicar as cores aos cabeçalhos das colunas
  colors <- scales::hue_pal()(length(map_version_levels))
  for (i in seq_along(map_version_levels)) {
    col_index <- which(table_grob$layout$name == "colhead" & table_grob$layout$l == (i + 2)) # offset by 2 to match table_grob layout
    if (length(col_index) > 0) {
      table_grob$grobs[[col_index]]$gp <- gpar(fill = colors[i], col = "black")
    }
  }
  
  # Adicionar a tabela ao gráfico
  combined_plot <- grid.arrange(p, table_grob, ncol = 1, heights = c(4, 1))
  
  # Salvar o gráfico como PNG
  ggsave(filename = paste0(output_dir, current_date, "_", metric_name, ".png"), plot = combined_plot, width = 12, height = 12)
}

# Salvar gráficos para cada métrica
metrics <- c("ME", "MAE", "MSE", "RMSE", "NSE", "Rsquared")

for (metric in metrics) {
  save_metric_plot(metric, current_date)
}
