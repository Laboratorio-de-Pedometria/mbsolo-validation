# Instalar e carregar os pacotes necessários
library(readxl)
library(writexl)
library(dplyr)

# Definir o diretório onde os arquivos XLSX estão localizados
dir_path <- "./results/1_results_all/"

# Listar todos os arquivos XLSX no diretório
file_list <- list.files(path = dir_path, pattern = "*.xlsx", full.names = TRUE)

# Função para calcular a média e o desvio padrão das métricas fornecidas em um arquivo XLSX, separando por map_version e biome
calculate_stats <- function(file) {
  data <- read_excel(file)
  
  # Verificar se a coluna 'biome' existe e adicionar 'Brasil' onde estiver vazia
  if ("biome" %in% colnames(data)) {
    data$biome[is.na(data$biome) | data$biome == ""] <- "Brasil"
  } else {
    data$biome <- "Brasil"
  }
  
  # Filtrar as colunas de interesse
  metrics <- data %>%
    select(map_version, biome, ME, MAE, MSE, RMSE, NSE, Rsquared)
  
  # Calcular a média e o desvio padrão das métricas separadas por map_version e biome
  stats <- metrics %>%
    group_by(map_version, biome) %>%
    summarise(across(c(ME, MAE, MSE, RMSE, NSE, Rsquared), list(mean = mean, sd = sd), na.rm = TRUE))
  
  return(stats)
}

# Aplicar a função a todos os arquivos e combinar os resultados em um único data frame
all_stats <- lapply(file_list, calculate_stats) %>%
  bind_rows()

# Salvar os resultados em um novo arquivo XLSX
output_file_xlsx <- "./results/all_results.xlsx"
write_xlsx(all_stats, output_file_xlsx)

output_file_csv <- "./results/all_results.csv"
write.csv(all_stats, output_file_csv)
