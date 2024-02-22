### Script para Validação Espacial ###

#Data: 2023-12-12
#Autores: Erli dos Santos, Marcos Cardoso Taciara Horst
#Contato: erlipinto@gmail.com, cardoso.mvs@gmail.com 

## Objetivo:
# Este script realiza modelagem estatística usando o algoritmo Random Forest para predizer a variável de resposta 'estoque'. 
# Inclui etapas de preparação dos dados, treinamento do modelo com validação espacial, avaliação de métricas de desempenho e 
# salvamento dos resultados.

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Define o número de clusters a serem considerados
number_of_clusters <- c(10, 20, 30)

for (k in seq(along.with = number_of_clusters)) { # Loop sobre os diferentes números de clusters
  
# Pacotes necessários -------------------------------------------------------
  
  require(readr)     #Para leitura de dados
  require(dplyr)     #Para manipulação de dados
  require(writexl)   #Para escrita em arquivos Excel
  require(random)    #Para funções aleatórias
  require(geosphere) #Para cálculos geoespaciais
  library(jsonlite)  #Para manipulação de JSON
  require(CAST)      #Para análise espacial
  require(caret)     #Para treinamento de modelos de aprendizado de máquina
  require(ranger)    #Implementação de Random Forest otimizada para desempenho.
  
  library(doParallel)  # Carrega o pacote doParallel para computação paralela

# Define os caminho de saída dos dados --------------------------------
 
#Define o caminho para salvar os modelos ajustados   
  output_models_path <- "C:/Users/CODEVASF/Desktop/tuned-models_v1-7-3/"
  
#Define o caminho para salvar os resultados
  output_models_results_path <- "C:/Users/CODEVASF/Desktop/cross-validation-results_v1-7-3/"
  
# Importação e divisão dos conjuntos de dados -----------------------------
  
  original_data <- read_csv( # Lê os dados originais
    "C:/Users/CODEVASF/Desktop/2023-04-25-cross_validation-study/01-data/matriz_rf_prediction_v1-7-3.csv")
  
  data_version <- "v1-7-3" # Define a versão dos dados
  
# Agrupamento das amostras por posição geográfica --------------------------------
  
  extract_latitude <- function(geojson) { # Define a função para extrair a latitude de um objeto GeoJSON
    # Converte o GeoJSON em um objeto JSON
    geojson_obj <- parse_json(geojson)
    # Retorna a latitude
    return(geojson_obj$coordinates[[2]]) 
  }
  
  extract_longitude <- function(geojson) { # Define a função para extrair a latitude de um objeto GeoJSON
    # Converte o GeoJSON em um objeto JSON
    geojson_obj <- parse_json(geojson)
    # Retorna a latitude
    return(geojson_obj$coordinates[[1]])
    
  }
  
  original_data <- original_data%>% # Aplica as funções de extração de latitude e longitude
    mutate(LAT = apply(original_data %>% select(.geo), 1, extract_latitude)) %>%
    mutate(LON = apply(original_data %>% select(.geo), 1, extract_longitude))
  
  # Calcula a matriz de distância entre as amostras
  dist_mat <- distm(original_data[, c("LON", "LAT")])
  
  # apply k-means clustering to the distance matrix
  #randomNumbers(n = 1, min = 100, max = 9999, col = 1) %>% as.vector()
  
  set.seed(3019) # Define a semente aleatória para reprodução
  k <- number_of_clusters[k] # Define o número de clusters
  kmeans_clusters <- kmeans(dist_mat, k) # Aplica o algoritmo de k-means para agrupar as amostras
  
  # Adiciona o ID do cluster ao conjunto de dados original
  original_data$cluster <- kmeans_clusters$cluster
  
# Visualizando os clusters ----------------------------------------------------
  
  #ggplot2::ggplot(original_data, aes(LON, LAT))+
  #  geom_point(size = 3, alpha = 0.1)
  
  #clustered_samples <- ggplot2::ggplot(original_data,
  #                aes(LON, LAT, color = as.factor(cluster),
  #                    shape = as.factor(cluster)))+
  #  geom_point(size = 3, alpha = 0.3)+
  #  scale_color_manual(aesthetics = "color",
  #                     values = c("#ffd700", "#ffb14e", "#fa8775",
  #                                "#ea5f94", "#ef0000", "#9d02d7",
  #                                "#0000ff", "#04590d", "#00e8ff", "#000000"))+
  #  scale_shape_manual(values = c(15, 18, 17, 16, 3, 8, 0, 6, 9, 12))+
  #  theme(legend.position = "none")
  
  #ggsave("clustered_points.png", plot = clustered_samples,
  #       width = 210, height = 250, units = "mm", dpi = 300)               
  
  remove(dist_mat, kmeans_clusters, clustered_samples,
         extract_latitude, extract_longitude) # Remove objetos desnecessários da memória
  
# Trabalhando com os clusters de processamento (opcional) -----------------------------------------
  
  cl <- makeCluster(detectCores()-2) # Cria clusters para computação paralela
  registerDoParallel(cl) # Registra os clusters para uso

# Define vetores de sementes aleatórias --------------------------------------------
  
  #randomNumbers(n = 100, min = 100, max = 9999, col = 1) %>% as.vector()
  
  random_seeds <- c(6842, 7045, 1359, 4109, 7947, 9122, 2050, 6646, 8143, 8444,
                    6402, 1721, 6955, 3744, 3144, 3681, 9588, 3807, 4464, 1034,
                    950, 8778, 163, 7249, 3181, 9938, 1564, 685, 8560, 8504, 3092,
                    7722, 6351, 2368, 5969, 6367, 3921, 8767, 9040, 1415, 428,
                    4837, 8263, 1631, 4249, 1411, 4747, 3158, 7846, 430, 6366,
                    6428, 1305, 8981, 3461, 6489, 1580, 8997, 8685, 5944, 991,
                    3630, 4472, 9304, 8411, 4961, 6877, 1325, 1507, 6748, 9408,
                    5790, 8395, 6161, 8942, 8907, 329, 2263, 9397, 3317, 6359,
                    8121, 2416, 1121, 9781, 4723, 5186, 3671, 7715, 4939, 4640,
                    9268, 5138, 6258, 8862, 2386, 6146, 879, 6644, 1821) 
  
# Covariáveis -------------------------------------------------------------
  
  # Define variáveis co-variáveis para modelagem  
  covariables <- c(
    # Soilgrids WRB probability classes
    'Ferralsols',
    'Histosols',
    'Sandysols',
    'Humisols',
    'Thinsols',
    'Wetsols',
    
    # Soilgrids Soil Properties
    'bdod',
    'cec',
    'cfvo',
    'clay',
    'nitrogen',
    'phh2o',
    'sand',
    'silt',
    'soc',
    
    'oxides',
    'clayminerals',
    
    # Black Soil
    'black_soil_prob',
    
    # Geomorphometry
    'convergence',
    'cti',
    'eastness',
    'northness',
    'pcurv',
    'roughness',
    'slope',
    'spi',
    'elevation',
    
    # Lat-Long
    'latitude',
    'longitude',
    
    # Koppen
    'lv1_Humid_subtropical_zone',
    'lv1_Tropical',
    'lv2_monsoon',
    'lv2_oceanic_climate_without_sry_season',
    'lv2_with_dry_summer',
    'lv2_with_dry_winter',
    'lv2_with_dry_winter_1',
    'lv2_without_dry_season',
    'lv3_with_hot_summer',
    'lv3_with_temperate_summer',
    
    # Indices
    'ndvi_mean',
    'evi_mean',
    'savi_mean',
    
    # # MapBiomas - Col.7.1
    # 'formacaoCampestre',
    # 'formacaoFlorestal',
    # 'formacaoSavanica',
    # 'mosaicoAgriculturaPastagem',
    # 'outrasFormacoesNaoFlorestais',
    # 'pastagem',
    # 'lavouras',
    # 'antropico',
    # 'natural',
    # 'Area_Estavel',
    
    # MapBiomas - Col. 8.0
    # 'campoAlagado-areaPantanosa', #Não rodou na matriz C1-v1_0_0
    'formacaoCampestre',
    'formacaoFlorestal',
    'formacaoSavanica',
    'lavouras',
    'mosaicoDeUsos',
    'outrasFormacoesFlorestais',
    'pastagem',
    'restingas',
    'silvicultura',
    'antropico',
    'natural',
    
    # Biomas 
    'Amazonia',
    'Caatinga',
    'Cerrado',
    'Mata_Atalntica',
    'Pampa',
    'Pantanal',
    
    # Fitofisionomia
    'Floresta_Ombrofila_Aberta',
    'Floresta_Estacional_Decidual',
    'Floresta_Ombrofila_Densa',
    'Floresta_Estacional_Semidecidual',
    'Campinarana',
    'Floresta_Ombrofila_Mista',
    'Formacao_Pioneira',
    'Savana',
    'Savana_Estepica',
    'Contato_Ecotono_e_Encrave',
    'Floresta_Estacional_Sempre_Verde',
    'Estepe',
    
    # Quarta Comunicação Nacional
    "cagb",
    "cbgb",
    "cdw",
    "clitter",
    "ctotal"
    )
  
  
# Definição das métricas usadas (script "statistical_functions.R) -----------------------------------------------------
  
  source("./my_statistical_functions.R") # Fonte de um script externo contendo funções estatísticas
  
  # Modelagem ----------------------------------------------------------------

  rf_kFold_cross_validation <- list() # Inicializa uma lista para resultados de validação cruzada espacial
  rf_kFold_best_models <- list() # Inicializa uma lista para os melhores modelos
  
  for (i in seq(along.with = random_seeds)) { # Loop sobre as sementes aleatórias
    
    ti <- Sys.time() # Registra o tempo inicial
    print(paste("Start time:", Sys.time(),"; random seed:", random_seeds[i])) 
    
    # Define a semente aleatória
    set.seed(random_seeds[i])
    
    # Cria folds espaciais-temporais
    LCOCV_obj <- CreateSpacetimeFolds(
      original_data,
      spacevar = "cluster", class = "cluster",
      k = 10)
    
    LCOCV_trCtrl <- trainControl( # Define configurações para treinamento do modelo
      method = "cv",
      number = 10,
      savePredictions = TRUE,
      index = LCOCV_obj$index,
      indexOut = LCOCV_obj$indexOut,
      summaryFunction = my_summary_metrics,
      returnResamp = 'all'
    )
    
    # Treina o modelo random forest com validação cruzada espacial
    tuned_RF_spatial_kfold_cv <- train(
      as.formula(paste("estoque", "~",
                       paste(covariables, collapse = '+'))),
      data = original_data,
      method = "ranger",
      num.trees = 1000,
      replace = TRUE,
      sample.fraction = 0.632,
      importance = "permutation",
      trControl = LCOCV_trCtrl,
      tuneGrid = expand.grid(mtry = 25, min.node.size = 5,
                             splitrule = "variance")
    )
    
    remove(LCOCV_obj, LCOCV_trCtrl) # Remove objetos desnecessários da memória

#Obtendo as métricas ---------------------------
    
    # Extrai os resultados da validação cruzada espacial
    cv <- tuned_RF_spatial_kfold_cv[["resample"]] %>% 
      mutate(model = i) %>% mutate(map_version = data_version, n_clusters = k) # Adiciona informações sobre o modelo
    
    rf_kFold_cross_validation[[i]] <- cv # Armazena os resultados da validação cruzada espacial
    
    # Obtém os hiperparâmetros ótimos do modelo
    hyperparameters <- tuned_RF_spatial_kfold_cv[["bestTune"]] 
    
    # Obtém os resultados do modelo
    result <- tuned_RF_spatial_kfold_cv[["results"]] %>%
      filter(mtry == hyperparameters[1, 1])
    
    rf_kFold_best_models[[i]] <- result %>% mutate(model = i) %>% # Armazena os melhores modelos
      mutate(map_version = data_version, n_clusters = k) # Adiciona informações sobre o modelo
    
    # Remove objetos desnecessários da memória
    remove(cv, hyperparameters, result)
    gc() 
    
    ## Saving tuned models
    
    if (!dir.exists(output_models_path)){ # Verifica se o diretório de saída existe
      
      dir.create(output_models_path) # Cria o diretório se não existir
      
      save(tuned_RF_spatial_kfold_cv, # Salva o modelo ajustado
           file = paste0(output_models_path,
                         "tuned_RF-", k, "_clusters-spatial_cv_model_", i, ".RData"))
      
    } else {
      
      save(tuned_RF_spatial_kfold_cv, # Salva o modelo ajustado
           file = paste0(output_models_path,
                         "tuned_RF-", k, "_clusters-spatial_cv_model_", i, ".RData"))
    }
    
    tf <- Sys.time()  # Registra o tempo final
    print(paste0("Time spent training the model ", i, ":")) # Imprime o tempo gasto no treinamento do modelo
    print(tf - ti) # Calcula e imprime o tempo total de treinamento
    
    # Remove objetos desnecessários da memória
    remove(tuned_RF_spatial_kfold_cv, i, ti, tf)
    gc()
    
  }
  
  rf_all_cv_results <- bind_rows(rf_kFold_cross_validation) # Combina todos os resultados de validação cruzada espacial
  rf_all_best_models <- bind_rows(rf_kFold_best_models) # Combina todos os melhores modelos
  
  remove(rf_kFold_cross_validation, rf_kFold_best_models) # Remove objetos desnecessários da memória
  
  if (dir.exists(output_models_results_path)) { # Verifica se o diretório de saída existe
    
    write_xlsx(rf_all_cv_results, 
               paste0(output_models_results_path,
                      paste0("rf-inner-", k, "_clusters-spatial-kFold-results.xlsx")),
               col_names = TRUE) # Escreve os resultados de validação cruzada em um arquivo Excel
    
    write_xlsx(rf_all_best_models,
               paste0(output_models_results_path,
                      paste0("rf-", k, "_clusters-spatial-kFold-results.xlsx")),
               col_names = TRUE) # Escreve os melhores modelos em um arquivo Excel
    
  } else { # Se o diretório de saída não existir, cria-o
    
    dir.create(output_models_results_path) # Cria o diretório
    
    write_xlsx(rf_all_cv_results,
               paste0(output_models_results_path,
                      paste0("rf-inner-", k, "_clusters-spatial-kFold-results.xlsx")),
               col_names = TRUE) # Escreve os resultados de validação cruzada em um arquivo Excel
    
    write_xlsx(rf_all_best_models,
               paste0(output_models_results_path,
                      paste0("rf-", k, "_clusters-spatial-kFold-results.xlsx")),
               col_names = TRUE)  # Escreve os melhores modelos em um arquivo Excel
    
  }

  remove(rf_all_cv_results, rf_all_best_models, train_sets, random_seeds,
         covariables, j, output_models_path, output_models_results_path,
         k, data_version) # Remove objetos desnecessários da memória
  
# Finalizando --------------------------------------
  
  if (exists("cl")) { # Verifica se os clusters de processamento foram criados
    print("Closing clusters.") # Imprime mensagem indicando que os clusters estão sendo fechados
    stopCluster(cl) # Fecha os clusters
    rm(cl) # Remove os clusters
  } else {
    print("Code is not working with clusters.") # Imprime mensagem indicando que o código não está utilizando clusters
  }
  
}
