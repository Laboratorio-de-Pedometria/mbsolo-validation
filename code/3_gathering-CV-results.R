### Script para Validação Cruzada Standard e Spatial por Bioma ###

#Data: 2023-12-12
#Autores: Erli dos Santos, Marcos Cardoso Taciara Horst
#Contato: erlipinto@gmail.com, cardoso.mvs@gmail.com 

## Objetivo:
# Este script importa os resultados e calcula métricas para avaliar o desempenho dos modelos em cada bioma e, em seguida, 
# os resultados são escritos separados por bioma, permitindo uma avaliação abrangente do desempenho dos modelos em diferentes ambientes.

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pacotes necessários -------------------------------------------------------
require(dplyr)       # Para manipulação de dados
require(readxl)      # Para leitura de arquivos Excel
require(writexl)    # Para escrita de arquivos Excel
require(readr)       # Para leitura de dados
require(tidyr)       # Para manipulação de dados (e.g., spread, gather)
require(tibble)      # Para criação de tibbles (data frames)
require(yardstick)   # Para avaliação de modelos de aprendizado de máquina
require(caret)       # Para treinamento e avaliação de modelos
require(ranger)      # Para treinamento de modelos de florestas aleatórias
library(magrittr)    # Para operações pipe (%>%)

# Data --------------------------------------------------------------------
# Definindo a versão dos dados em uso
data_version <- "v0-1-6"

# Criando o caminho para os resultados de validação cruzada
cv_results_path <- paste0("./results/", data_version, "/")

# Listando arquivos .xlsx no caminho especificado
list.files(path = cv_results_path, pattern = ".xlsx")

# Cross-validation results ------------------------------------------------
# Carregando e organizando resultados da validação cruzada de diferentes modelos
cv_results <- list(
  # Resultados da validação cruzada k-Fold comum
  rf__kFold_results = read_excel(
    paste0(cv_results_path, data_version, "_rf-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Standard k-Fold CV", model_for = "random forest", n_clusters = 0),
  
 #  # Resultados da validação cruzada Spatial com diferentes números de clusters
#   ## 10 clusters
  # read_excel(
  #   paste0(cv_results_path, "/rf-10_clusters-spatial-kFold-results.xlsx")) %>%
  #   mutate(cross_validation = "Spatial k-Fold CV", model_for = "random forest", n_clusters = 10),
  # ## 20 clusters
  # read_excel(
  #   paste0(cv_results_path, "/rf-20_clusters-spatial-kFold-results.xlsx")) %>%
  #   mutate(cross_validation = "Spatial k-Fold CV", model_for = "random forest", n_clusters = 20),
  # ## 30 clusters
  read_excel(
    paste0(cv_results_path, "/rf-30_clusters-spatial-kFold-results.xlsx")) %>%
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "random forest", n_clusters = 30)
)

# Combinando todos os resultados em um único dataframe
cv_results <- bind_rows(cv_results)

# Escrevendo os resultados em um arquivo Excel
write_xlsx(cv_results, paste0(cv_results_path, data_version, "_cv_clusters_results.xlsx"), col_names = TRUE)
  
# Biome-wise standard cross-validation statistics -------------------------

# Carregando dados originais e definindo novamente a versão dos dados
input_data_path <- "data/"
original_data <- read_csv(paste0(input_data_path, "/matriz-", data_version ,".csv"))

# Classificando os dados por bioma
original_data <- original_data %>% mutate(
  biome = ifelse(Amazonia == 1, "Amazônia",
                 ifelse(Caatinga == 1, "Caatinga",
                        ifelse(Mata_Atlantica == 1, "Mata Atlântica",   
                               ifelse(Cerrado == 1, "Cerrado",
                                      ifelse(Pampa == 1, "Pampa",
                                             ifelse(Pantanal == 1, "Pantanal", "NA")))))),
  
  # Removendo o caminho dos dados de entrada para limpar a memória
  remove(input_data_path)
)

# Auxiliary functions -----------------------------------------------------
  # Funções auxiliares para manipulação de strings
  
  # Função para remover a última parte de uma string
  remove_tail <- function(x, sep = ".R", del = 1){
    sapply(strsplit(x, split = sep, fixed = TRUE),
           function(i) paste(head(i, -del), collapse = sep))
  }
  
  # Função para manter a última parte de uma string
  keep_tail <- function(x, sep = "_", del = 4){
    sapply(strsplit(x, split = sep, fixed = TRUE),
           function(i) paste(tail(i, -del), collapse = sep))
  }
  
  # Função para avaliação de regressão
  regression_eval <- function(pred, obs){
    # Calculando várias métricas estatísticas
    ME <- round(mean(pred - obs, na.rm = TRUE), digits = 4)   # Mean Error
    MSE <- round(mean((pred - obs)^2, na.rm = TRUE), digits = 4)  # Mean Square Error
    MAE <- round(mean(abs(pred - obs), na.rm = TRUE), digits = 4)  # Mean Absolute Error
    RMSE <- round(sqrt(mean((pred - obs)^2, na.rm = TRUE)), digits = 4)  # Root Mean Square Error
    r2 <- round((cor(pred, obs, method = 'spearman', use = 'pairwise.complete.obs')^2), digits = 4)  # Pearson's correlation squared
    NSE <- round((1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)), digits = 4)  # Nash–Sutcliffe model efficiency coefficien
    VAR <- round(var(pred - obs, na.rm = TRUE), digits = 4)
    SD <- round(sqrt(VAR), digits = 4)
    out <- c(ME, MAE, MSE, RMSE, NSE, r2, VAR, SD)
    names(out) <- c("ME", "MAE", "MSE", "RMSE", "NSE", "Rsquared", "VAR", "SD")
    if (any(is.nan(out))) 
      out[is.nan(out)] <- NA
    out
  }
# Computing statistical metrics for samples from each biome separa --------
# Computing same goodness-of-fit metrics from models tuning

models_path <- paste0("./models/", data_version, "/") # Define o caminho para os modelos ajustados

model_names <- list.files(path = models_path, pattern = "RF_cv_model") # Obtém os nomes dos arquivos de modelo

model_number <- keep_tail(remove_tail(model_names)) %>% as.numeric()  # Extrai números de modelo dos nomes dos arquivos

# Define uma função para calcular métricas estatísticas para cada bioma e modelo
check_statistical_results <- function(
    dataset = biomes[["Amazônia"]],
    biome = "Amazônia",
    map_version = data_version, model_for = "Both stable and unstable",
    cross_validation = "Standard k-Fold CV", n_clusters = 0, model_number = 1) {
  
  regression_eval(pred = dataset %>% pull(pred_estoque), # Avalia as previsões
                  obs = dataset %>% pull(estoque)) %>%
    as.data.frame() %>% t() %>% as_tibble() %>% 
    mutate(model = model_number, 
           map_version = map_version,
           n_clusters = n_clusters,
           cross_validation = cross_validation,
           model_for = model_for,
           biome = biome)
  
}
# Inicializa listas para armazenar resultados de validação cruzada para cada bioma
amazonia_cv_results <- list()
caatinga_cv_results <- list()
cerrado_cv_results <- list()
mata_atlantica_cv_results <- list()
pampa_cv_results <- list()
pantanal_cv_results <- list()

# Loop sobre os modelos para calcular métricas estatísticas para cada bioma
for (i in 1:100) {
  
  load(paste0(models_path, # Carrega o modelo correspondente
              model_names[grep(paste0("_", model_number[i], ".RData"), model_names)]))
  
  original_data$pred_estoque <- tuned_RF_kfold_cv[["finalModel"]][["predictions"]] # Adiciona as previsões ao conjunto de dados original
  
  amazonia_cv_results[[i]] <- check_statistical_results( 
    dataset = original_data %>% filter(biome == "Amazônia"), biome = "Amazônia",
    model_number = i) # Calcula métricas estatísticas para a Amazônia
  
  caatinga_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Caatinga"), biome = "Caatinga",
    model_number = i)
  
  cerrado_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Cerrado"), biome = "Cerrado",
    model_number = i)
  
  mata_atlantica_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Mata Atlântica"), biome = "Mata Atlântica",
    model_number = i)
  
  pampa_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Pampa"), biome = "Pampa",
    model_number = i)
  
  pantanal_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Pantanal"), biome = "Pantanal",
    model_number = i)
  
  original_data <- original_data %>% select(-pred_estoque) # Remove as previsões do conjunto de dados original
  
  remove(tuned_RF_kfold_cv) # Remove o modelo carregado da memória
  gc() # Limpa a memória
  
}

# Combina os resultados de validação cruzada para cada bioma em um único dataframe
biome_cv_results <- bind_rows(amazonia_cv_results, caatinga_cv_results,
                              cerrado_cv_results, mata_atlantica_cv_results, 
                              pampa_cv_results, pantanal_cv_results)

remove(amazonia_cv_results, caatinga_cv_results,
       cerrado_cv_results, mata_atlantica_cv_results, 
       pampa_cv_results, pantanal_cv_results) # Remove as listas de resultados individuais

# Escreve os resultados de validação cruzada em arquivos Excel separados por bioma
write_xlsx(biome_cv_results,
           path = paste0(cv_results_path,"/", data_version, "_each_biome.xlsx"),
           col_names = TRUE)
