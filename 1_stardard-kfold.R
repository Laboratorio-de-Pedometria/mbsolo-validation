### Script para Validação Cruzada ###

#Data: 2023-12-12
#Autores: Erli dos Santos, Marcos Cardoso Taciara Horst
#Contato: erlipinto@gmail.com, cardoso.mvs@gmail.com 

## Objetivo:
# Este script realiza modelagem estatística usando o algoritmo Random Forest para prever a variável de resposta 'estoque'. 
# Inclui etapas de preparação dos dados, treinamento do modelo com validação cruzada k-fold, avaliação de métricas de desempenho e 
# salvamento dos resultados.

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## Pacotes necessários -------------------------------------------------------

require(readr) # Para leitura de arquivos CSV
require(dplyr) # Para manipulação de dados
require(writexl) # Para escrever dados em Excel

require(random) # Biblioteca aleatória

require(caret) # Para treinamento de modelos de aprendizado de máquina
#require(randomForest) #Alternativa ao ranger para modelagem com Random Forest 
require(ranger) # Implementação de Random Forest otimizada para desempenho.

## Dividindo núcleos de processamento (optional) --------------------------------------------------------------

library(doParallel) #Para trabalhar com clusters

# Criando um cluster com um número de núcleos igual ao número de núcleos
cl <- makeCluster(detectCores()-2) #Esta função retorna o número de núcleos disponíveis no seu sistema.

# Registrando o cluster para paralelizar operações
registerDoParallel(cl) #Esta função registra o cluster criado anteriormente, permitindo que as operações sejam executadas em paralelo nos núcleos disponíveis.

#A razão para subtrair dois núcleos geralmente é deixar alguns núcleos livres para outras tarefas do sistema operacional, garantindo que o processamento paralelo não sobrecarregue totalmente o sistema.
#Melhora potencialmente o desempenho e reduz o tempo de execução.

## Definindo caminhos para os diretórios de entrada e saída ----------------------------------------------------

#Define o caminho para entrada dos dados
input_data_path <- "C:/Users/marco/OneDrive/Área de Trabalho/matriz"

#Define o caminho para salvar os modelos ajustados
output_models_path <- "C:/Users/marco/OneDrive/Área de Trabalho/tuned-models_Cbeta_v1-7-4/"

#Define o caminho para salvar os resultados
output_models_results_path <- "C:/Users/marco/OneDrive/Área de Trabalho/cross-validation-results_Cbeta_v1-7-4/"

## Importação e divisão de datasets. ---------------------------------------------------------------------------

#Lê o arquivo CSV para a variável original_data.
original_data <- read_csv(paste0(input_data_path,
                                 "/matriz_soc_pred_Cbeta_v1-7-4.csv"))

#Armazena a versão dos dados analisados.
data_version <- "Cbeta_v1-7-4"
#Define a variável k para o número de folds na validação cruzada (ainda não definido).
k = NA

## Criação do vetor de sementes aleatórias --------------------------------------------------------------------

# randomNumbers(n = 100, min = 100, max = 9999, col = 1) %>% as.vector()  # código original para gerar 100 sementes aleatórias entre 100 e 9999.

#Define o vetor random_seeds com 100 sementes aleatórias.
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

## Covariáveis --------------------------------------------------------------------------------------------

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
  'evi_mean',
  'savi_mean',
  'ndvi_mean',
  
  # MapBiomas - Col.7.1
  'formacaoCampestre',
  'formacaoFlorestal',
  'formacaoSavanica',
  'mosaicoAgriculturaPastagem',
  'outrasFormacoesNaoFlorestais',
  'pastagem',
  'lavouras',
  'antropico',
  'natural',
  'Area_Estavel',
  
  # MapBiomas - Col. 8.0
  # 'campoAlagado-areaPantanosa', #Não rodou na matriz C1-v1_0_0
  #'formacaoCampestre',
  #'formacaoFlorestal',
  #'formacaoSavanica',
  #'lavouras',
  #'mosaicoDeUsos',
  #'outrasFormacoesFlorestais',
  #'pastagem',
  #'restingas',
  #'silvicultura',
  #'antropico',
  #'natural',

  #'Area_Estavel',
  
  # Biomas 
  'Amazonia',
  'Caatinga',
  'Cerrado',
  'Mata_Atalntica', #Col. 7.1
  #'Mata_Atlantica', #Col. 8.0
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

## Definição das métricas usadas (script "statistical_functions.R) -----------------------------------------------------

source("C:/Users/marco/OneDrive/Área de Trabalho/Scripts/statistical_functions.R") # Fonte de um script externo contendo funções estatísticas

## Modelagem-----

# Inicialização de listas para armazenar resultados de validação cruzada
rf_kFold_cross_validation <- list() #Lista vazia para armazenar resultados
rf_kFold_best_models <- list() #Lista vazia para armazenar os melhores modelos

for (i in seq(along.with = random_seeds)) { #Este loop itera sobre todas as sementes aleatórias armazenadas em random_seeds
  
  ti <- Sys.time() #Armazena tempo de inicio
  print(paste("Start time:", Sys.time(),"; random seed:", random_seeds[i])) #Para mostrar as mensagens Start time: tempo atual; Random seed: seed usada
  
  ## Configura a semente de randomização para garantir a reprodutibilidade dos resultados
  set.seed(random_seeds[i]) 
  
  ## Prepara um objeto de controle kfold
  cv_control_object <- trainControl(method = "cv", number = 10, #Define o método (Validação cruzada) e npumero de folds (10)
                                    summaryFunction = my_summary_metrics, #É especificada como a função de resumo para  para calcular as métricas estatísticas
                                    returnResamp = 'all') #Para retornar todas as amostras de treinamento e validação
  
  ## Treinando o modelo
  tuned_RF_kfold_cv <- train( #Treina um modelo Random Forest
    as.formula(paste("estoque", "~",
                     paste(covariables, collapse = '+'))), #Define a fórmula do modelo, especificando a variável de resposta ("estoque") e as covariáveis como variáveis explicativas
    data = original_data,  # Especifica o conjunto de dados utilizado para treinamento
    method = "ranger",  # Especifica o método de treinamento como "ranger" (Random Forest)
    num.trees = 1000, #Número de árvores de regressão
    replace = TRUE, #Especifica se a reposição deve ser usada na amostragem de dados para treinamento.
    sample.fraction = 0.632, #Especifica a fração de dados a serem usados para treinamento.
    importance = "permutation", #Método para calcular a importância das variáveis: "permutation" permuta valores de uma variável para medir a importância
    trControl = cv_control_object, #Especifica o objeto de controle de validação cruzada a ser usado. O objeto cv_control_object foi criado anteriormente e especifica que a validação cruzada será realizada com 10 folds.
    tuneGrid = expand.grid( ## Grade de valores a serem ajustados durante o processo de ajuste (tuning)
                          mtry = 25, #Número de variáveis consideradas para a divisão em cada nó
                          min.node.size = 5, #Número mínimo de observações em um nó termina
                          splitrule = "variance") #Método para dividir os nós da árvore
  )

# Remove o objeto de controle após o treinamento do modelo para liberar memória
  remove(cv_control_object)
  
## Obtenção de métricas de treinamento -------
  
  ## Obtenção de métricas de validação cruzada
#Extrai o objeto resample do modelo treinado (tuned_RF_kfold_cv) e adiciona novas colunas
  cv <- tuned_RF_kfold_cv[["resample"]] %>%       #Armazena os resultados no dataframe "cv"
   mutate(model = i) %>%                          #Indica o número do modelo atual
   mutate(map_version = data_version,             #Armazena a versão do mapa de solos utilizada
           n_clusters = k)                        #Armazena o número de clusters utilizados

#Os resultados da validação cruzada (cv) são armazenados na lista rf_kFold_cross_validation na posição correspondente ao índice i
  rf_kFold_cross_validation[[i]] <- cv 
  
  # Obtém as estatísticas dos melhores modelos ajustados 
  hyperparameters <- tuned_RF_kfold_cv[["bestTune"]] #Extrai o objeto bestTune do modelo treinado (tuned_RF_kfold_cv), que contém os hiperparâmetros selecionados como os melhores.
  
  #Seleciona os o resultado que corresponde ao melhor mtry
  result <- tuned_RF_kfold_cv[["results"]] %>% #Filtra o objeto results do modelo treinado
    filter(mtry == hyperparameters[1, 1]) #Seleciona apenas o resultado que corresponde ao melhor valor de mtry encontrado nos hiperparâmetros
 
  # Adiciona informações aos resultados armazenados na lista
  rf_kFold_best_models[[i]] <- result %>% mutate(model = i) %>% 
    mutate(map_version = data_version, n_clusters = k)
  
  ## Limpeza de memória
  remove(cv, hyperparameters, result)
  gc() #Executa a coleta de lixo para liberar memória não utilizada
  
## Salvamento os modelos ajustados----------------------------------------------------------------------------------------------------------------------------------------------
  
  if (!dir.exists(output_models_path)){ #Verifica se o diretório existe 
    
    dir.create(output_models_path) #Cria o diretório caso não exita
    
    save(tuned_RF_kfold_cv, #Cria arquivo com os resultados
         file = paste0(output_models_path,
                       "tuned_RF_cv_model_", i, ".RData")) #Se não existir, o diretório é criado e o modelo treinado tuned_RF_kfold_cv é salvo no arquivo tuned_RF_cv_model_i.RData, onde i é o número do modelo atual.
    
  } else { #Sobrescreve qualquer arquivo na versão anterior se já existir
    
    save(tuned_RF_kfold_cv, 
         file = paste0(output_models_path,
                       "tuned_RF_cv_model_", i, ".RData")) 
  }

#Calcula e imprime o tempo gasto no treinamento do modelo.
  tf <- Sys.time() #Armazena o tempo atual
  print(paste0("Time spent training the model ", i, ":"))
  print(tf - ti)
  
  ## Limpeza de memória
  remove(tuned_RF_kfold_cv, i, ti, tf)
  gc() #Executa a coleta de lixo para liberar memória não utilizada
  
}

# Combina os resultados da validação cruzada em um dataframe
rf_all_cv_results <- bind_rows(rf_kFold_cross_validation) #bind_rows> é uma função do pacote dplyr em R e é usada para combinar data frames por linhas (concatenar verticalmente). Ela é útil quando você tem vários data frames com as mesmas colunas e deseja agrupá-los em um único data frame, mantendo a estrutura de colunas.

# Combina os resultados dos melhores modelos em um dataframe
rf_all_best_models <- bind_rows(rf_kFold_best_models)

#Remove as listas pois seus dados foram combinados nos dataframes acima
remove(rf_kFold_cross_validation, rf_kFold_best_models)

#Verifica se o diretório existe
if (dir.exists(output_models_results_path)) { #Verifica se já existe
  
  write_xlsx(rf_all_cv_results,
             paste0(output_models_results_path,
                    "rf-inner-kFold-results.xlsx"), #Escreve o dataframe em arquivo Excel e nomeia
             col_names = TRUE) #Indica que o nome das colunas devem ser incluídos no arquivo
  
  write_xlsx(rf_all_best_models,
             paste0(output_models_results_path,
                    "rf-kFold-results.xlsx"), 
             col_names = TRUE) 
  
} else { #Se o diretório não existir
  
  dir.create(output_models_results_path) #Cria o diretório especificado
  
  write_xlsx(rf_all_cv_results,
             paste0(output_models_results_path,
                    "rf-inner-kFold-results.xlsx"),  #Escreve o dataframe em arquivo Excel e nomeia
             col_names = TRUE) #Indica que o nome das colunas devem ser incluídos no arquivo
  
  write_xlsx(rf_all_best_models,
             paste0(output_models_results_path,
                    "rf-kFold-results.xlsx"),
             col_names = TRUE)  
  
}

#Remove da memória vários objetos que não são mais necessários
remove(rf_all_cv_results, rf_all_best_models, train_sets, random_seeds,
       covariables, j, output_models_path, output_models_results_path)

# Finalizing --------------------------------------------------------------

if (exists("cl")) { #Verifica se existe um objeto chamado "cl" no ambiente. Este objeto provavelmente representa um cluster de computação criado anteriormente.
  print("Closing clusters.") 
  stopCluster(cl) #Encerra o cluster paralelo
  rm(cl) #Remove o objeto "cl" do ambiente de trabalho 
} else { #Se o objeto "cl" não existe, imprime a mensagem
  print("Code is not working with clusters.")
}
