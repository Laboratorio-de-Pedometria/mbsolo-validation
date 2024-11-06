# Carregar as bibliotecas necessárias
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra) # para adicionar tabelas ao gráfico

# Carregar os dados
dados <- read_excel("D:/0_Projetos/1_mbsolos-validation/results/2024_06_04_cv_C1_v003/all_predictions.xlsx")

# Calcular a média das previsões para cada dataset_id em g/m²
dados_media_gm2 <- dados %>%
  group_by(dataset_id) %>%
  summarise(mean_observed_gm2 = mean(Observed, na.rm = TRUE), mean_predicted_gm2 = mean(Predicted, na.rm = TRUE))

# Converter os valores para t/ha (1 g/m² = 0.01 t/ha)
dados_media_tha <- dados_media_gm2 %>%
  mutate(mean_observed_tha = mean_observed_gm2 * 0.01,
         mean_predicted_tha = mean_predicted_gm2 * 0.01)

# Função para criar gráfico e calcular métricas
create_plot <- function(data, observed, predicted, units) {
  obs <- data[[observed]]
  pred <- data[[predicted]]

  # Calcular as métricas
  ME <- round(mean(pred - obs, na.rm = TRUE), digits = 3)
  MSE <- round(mean((pred - obs)^2, na.rm = TRUE), digits = 3)
  MAE <- round(mean(abs(pred - obs), na.rm = TRUE), digits = 3)
  RMSE <- round(sqrt(mean((pred - obs)^2, na.rm = TRUE)), digits = 3)
  r2 <- round((cor(pred, obs, method = 'spearman', use = 'pairwise.complete.obs')^2), digits = 3)
  NSE <- round((1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)), digits = 3)

  # Criar gráfico
  plot <- ggplot(data, aes_string(x = observed, y = predicted)) +
    geom_point(color = "black", size = 1.5, alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(x = paste("prediction_soc_v003", units), y = paste("observed_soc_9649_samples", units), title = paste("Comparative Observed_Predicted SOC Stock", units)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  # Criar uma tabela de métricas
  metrics_table <- tibble(
    Stat = c("ME", "MAE", "RMSE", "NSE", "R2"),
    Value = c(ME, MAE, RMSE, NSE, r2)
  )

  # Combinar gráfico e tabela
  combined_plot <- grid.arrange(
    plot,
    tableGrob(metrics_table, rows = NULL),
    ncol = 2, widths = c(3, 1)
  )

  return(combined_plot)
}

# Gerar gráficos
plot_gm2 <- create_plot(dados_media_gm2, "mean_observed_gm2", "mean_predicted_gm2", "(g/m²)")
plot_tha <- create_plot(dados_media_tha, "mean_observed_tha", "mean_predicted_tha", "(t/ha)")

# Salvar gráficos
ggsave("D:/0_Projetos/1_mbsolos-validation/results/2024_06_04_cv_C1_v003/scatter_plot_with_metrics_gm2.png", plot_gm2, width = 12, height = 8, dpi = 300)
ggsave("D:/0_Projetos/1_mbsolos-validation/results/2024_06_04_cv_C1_v003/scatter_plot_with_metrics_tha.png", plot_tha, width = 12, height = 8, dpi = 300)
