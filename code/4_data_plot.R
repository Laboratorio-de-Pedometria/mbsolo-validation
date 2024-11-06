### Script para gerar os gráficos e mapa de clusters ###

#Data: 2023-12-12
#Autores: Erli dos Santos, Marcos Cardoso e Taciara Horst
#Contato: erlipinto@gmail.com, cardoso.mvs@gmail.com 

## Objetivo:
# Este script gera os gráficos e o mapa de clusters a partir dos resultados da validação cruzada stardard e spatial.

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
require(dplyr) #Para manipulação de dados
require(readxl) #Para leitura de arquivos Excel
require(ggplot2) #Para criação de gráficos
require(patchwork) #Para combinar gráficos

#Define o caminho para o diretório com os arquivos de resultados
data_path <- "D:/0_Projetos/1_mbsolos-validation/results/"

#Define o caminho para o diretório onde serão salvos os arquivos com as figuras
output_path <- "D:/0_Projetos/1_mbsolos-validation/results/figure/"

#Lista todos os arquivos com extensão ".xlsx" no diretório path
list.files(path = data_path, pattern = ".xlsx")

cv_results <- list(                                                                  #Cria uma lista
  # read_excel(paste0(data_path, "cross_validation-results_v1-5-0.xlsx")),           #Faz a leitura dos arquivos .xlsx
  # read_excel(paste0(data_path, "cross_validation-results_v1-6-0.xlsx")),
  # read_excel(paste0(data_path, "cross_validation-results_v1-7-2.xlsx")),
  read_excel(paste0(data_path, "2023_12_12_cv_C1_v1_0_0/uniao_cross_validation-results_C1_v1_0_0.xlsx")),     
  read_excel(paste0(data_path, "2024_03_18_cv_C1_v2_0_0/uniao_cross_validation-results_C1_v2_0_0.xlsx")))%>%
  bind_rows() %>%                                                                    #Combina os dataframes lidos da lista em um único dataframe
  mutate(cv_nclusters = ifelse(cross_validation == "Standard k-Fold CV",             #Se "cross_validation" for "Standard k-Fold CV", a coluna mantém o mesmo valor.
                               cross_validation,
                               paste(cross_validation, "with", n_clusters, "clusters"))) #Se "cross_validation" for diferente, agrega o número de clusters utilizado (e.g., "KMeans with 30 clusters").

# Visualização das métricas -----------------------------------------------------
# # Cada conjunto de código representa uma figura para cada versão.

## ME e MAE para v1-7-3--------------
# (cv_results %>% 
#     filter(map_version == "v1-7-3", n_clusters %in% c(0, 30)) %>% 
#     ggplot(aes(x = model_for, y = ME))+
#     geom_errorbar(stat = "boxplot", width = 0.25)+
#     geom_boxplot(width = 0.5, size = 0.7)+
#     facet_grid(.~cv_nclusters)+
#     scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 100),
#                        labels = seq(0, 700, 100)/1000)+
#     labs(x = NULL, y = expression(atop("ME", (kg~m^-2))),
#          title = "Simulations for the version data: v1-7-3 (n = 9531)")+
#     theme_classic()+
#     theme(text = element_text(family = "serif", size = 12),
#           axis.text = element_text(family = "serif", size = 12),
#           axis.text.x = element_blank(),
#           axis.ticks.x = element_blank(),
#           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
#           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
#   (cv_results %>% 
#      filter(map_version == "v1-7-3", n_clusters %in% c(0, 30)) %>% 
#      ggplot(aes(x = model_for, y = MAE))+
#      geom_errorbar(stat = "boxplot", width = 0.25)+
#      geom_boxplot(width = 0.5, size = 0.7)+
#      facet_grid(.~cv_nclusters)+
#      scale_y_continuous(limits = c(1500, 2500), breaks = seq(1500, 2500, 200),
#                         labels = seq(1500, 2500, 200)/1000)+
#      labs(x = NULL, y = expression(atop("MAE", (kg~m^-2))))+
#      theme_classic()+
#      theme(text = element_text(family = "serif", size = 12),
#            axis.text = element_text(family = "serif", size = 12),
#            axis.text.x = element_blank(),
#            axis.ticks.x = element_blank(),
#            axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
#            axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))
# 
# ggsave(filename = paste0(output_path, "me_and_mae-v1-7-3.png"),
#        width = 148, height = 148, units = "mm", dpi = 600)

## MSE, RMSE e NSE para v1-7-3 --------------
# library(scales)
# 
# (cv_results %>% 
#     filter(map_version == "v1-7-3", n_clusters %in% c(0, 30)) %>% 
#     ggplot(aes(x = model_for, y = MSE))+
#     geom_errorbar(stat = "boxplot", width = 0.25)+
#     geom_boxplot(width = 0.5, size = 0.7)+
#     facet_grid(.~cv_nclusters)+
#     scale_y_continuous(limits = c(13000000, 25000000), labels = scientific)+
#     labs(x = NULL,
#          title = "Simulations for the version data: v1-7-3 (n = 9531)")+
#     theme_classic()+
#     theme(text = element_text(family = "serif", size = 12),
#           axis.text = element_text(family = "serif", size = 12),
#           axis.text.x = element_blank(),
#           axis.ticks.x = element_blank(),
#           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
#           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
#   (cv_results %>% 
#      filter(map_version == "v1-7-3", n_clusters %in% c(0, 30)) %>% 
#      ggplot(aes(x = model_for, y = RMSE))+
#      geom_errorbar(stat = "boxplot", width = 0.25)+
#      geom_boxplot(width = 0.5, size = 0.7)+
#      facet_grid(.~cv_nclusters)+
#      scale_y_continuous(limits = c(3000, 5000),
#                         breaks = round(seq(3000, 5000, 400), 3),
#                         labels = round(seq(3000, 5000, 400)/1000, 3))+
#      labs(x = NULL, y = expression(atop("RMSE", (kg~m^-2))))+
#      theme_classic()+
#      theme(text = element_text(family = "serif", size = 12),
#            axis.text = element_text(family = "serif", size = 12),
#            axis.text.x = element_blank(),
#            axis.ticks.x = element_blank(),
#            axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
#            axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
#   (cv_results %>% 
#      filter(map_version == "v1-7-3", n_clusters %in% c(0, 30)) %>% 
#      ggplot(aes(x = model_for, y = NSE))+
#      geom_errorbar(stat = "boxplot", width = 0.25)+
#      geom_boxplot(width = 0.5, size = 0.7)+
#      facet_grid(.~cv_nclusters)+
#      labs(x = NULL)+
#      scale_y_continuous(limits = c(-0.2, 0.6), breaks = round(seq(-0.2, 0.6, 0.2), 1),
#                         labels = round(seq(-0.2, 0.6, 0.2), 1))+
#      theme_classic()+
#      theme(text = element_text(family = "serif", size = 12),
#            axis.text = element_text(family = "serif", size = 12),
#            axis.text.x = element_blank(),
#            axis.ticks.x = element_blank(),
#            axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
#            axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))
# 
# ggsave(filename = paste0(output_path, "mse_rmse_nse-v1-7-3.png"),
#        width = 148, height = 220, units = "mm", dpi = 300)

## ME e MAE para C1_v000----------

(cv_results %>% 
   filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% #Filtra os dados
   ggplot(aes(x = model_for, y = ME))+ #Cria o ggplot e define os eixos "x" e "y" 
   geom_errorbar(stat = "boxplot", width = 0.25)+
   geom_boxplot(width = 0.5, size = 0.7)+
   facet_grid(.~cv_nclusters)+
   scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 100),
                      labels = seq(0, 700, 100)/1000)+
   labs(x = NULL, y = expression(atop("ME", (kg~m^-2))),
        title = "Simulations for the version data: C1_v000 (n = 9531)")+
   theme_classic()+
   theme(text = element_text(family = "serif", size = 12),
         axis.text = element_text(family = "serif", size = 12),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
         axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = model_for, y = MAE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     scale_y_continuous(limits = c(1500, 2500), breaks = seq(1500, 2500, 200),
                        labels = seq(1500, 2500, 200)/1000)+
     labs(x = NULL, y = expression(atop("MAE", (kg~m^-2))))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = paste0(output_path, "me_and_mae-C1_v000.png"),
       width = 148, height = 148, units = "mm", dpi = 600)

## MSE, RMSE e NSE para C1_v000-----
library(scales)

(cv_results %>% 
    filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% 
    ggplot(aes(x = model_for, y = MSE))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    facet_grid(.~cv_nclusters)+
    scale_y_continuous(limits = c(13000000, 25000000), labels = scientific)+
    labs(x = NULL,
         title = "Simulations for the version data: C1_v000 (n = 9531)")+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = model_for, y = RMSE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     scale_y_continuous(limits = c(3000, 5000),
                        breaks = round(seq(3000, 5000, 400), 3),
                        labels = round(seq(3000, 5000, 400)/1000, 3))+
     labs(x = NULL, y = expression(atop("RMSE", (kg~m^-2))))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = model_for, y = NSE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     labs(x = NULL)+
     scale_y_continuous(limits = c(-0.2, 0.6), breaks = round(seq(-0.2, 0.6, 0.2), 1),
                        labels = round(seq(-0.2, 0.6, 0.2), 1))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = paste0(output_path, "mse_rmse_nse-C1_v000.png"),
       width = 148, height = 220, units = "mm", dpi = 300)



## ME e MAE para C1_v001----
(cv_results %>% 
   filter(map_version == "C1_v001", n_clusters %in% c(0, 30)) %>% 
   ggplot(aes(x = model_for, y = ME))+
   geom_errorbar(stat = "boxplot", width = 0.25)+
   geom_boxplot(width = 0.5, size = 0.7)+
   facet_grid(.~cv_nclusters)+
   scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 100),
                      labels = seq(0, 700, 100)/1000)+
   labs(x = NULL, y = expression(atop("ME", (kg~m^-2))),
        title = "Simulations for the version data: C1_v001 (n = 9531)")+
   theme_classic()+
   theme(text = element_text(family = "serif", size = 12),
         axis.text = element_text(family = "serif", size = 12),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
         axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "C1_v001", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = model_for, y = MAE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     scale_y_continuous(limits = c(1500, 2500), breaks = seq(1500, 2500, 200),
                        labels = seq(1500, 2500, 200)/1000)+
     labs(x = NULL, y = expression(atop("MAE", (kg~m^-2))))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = paste0(output_path, "me_and_mae-C1_v001.png"),
       width = 148, height = 148, units = "mm", dpi = 600)

## MSE, RMSE e NSE para C1_v001-----
library(scales)

(cv_results %>% 
    filter(map_version == "C1_v001", n_clusters %in% c(0, 30)) %>% 
    ggplot(aes(x = model_for, y = MSE))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    facet_grid(.~cv_nclusters)+
    scale_y_continuous(limits = c(13000000, 25000000), labels = scientific)+
    labs(x = NULL,
         title = "Simulations for the version data: C1_v001 (n = 9531)")+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "C1_v001", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = model_for, y = RMSE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     scale_y_continuous(limits = c(3000, 5000),
                        breaks = round(seq(3000, 5000, 400), 3),
                        labels = round(seq(3000, 5000, 400)/1000, 3))+
     labs(x = NULL, y = expression(atop("RMSE", (kg~m^-2))))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "C1_v001", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = model_for, y = NSE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     labs(x = NULL)+
     scale_y_continuous(limits = c(-0.2, 0.6), breaks = round(seq(-0.2, 0.6, 0.2), 1),
                        labels = round(seq(-0.2, 0.6, 0.2), 1))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = paste0(output_path, "mse_rmse_nse-C1_v001.png"),
       width = 148, height = 220, units = "mm", dpi = 300)


# Standard cross-validation statistics for territories --------------------

list.files(path = data_path, pattern = ".xlsx")

territories_cv_results <- list(
  read_excel(paste0("D:/0_Projetos/1_mbsolos-validation/results/2023_12_12_cv_C1_v1_0_0/each_biome-cross_validation-results_C1_v1_0_0.xlsx"))) %>% 
  bind_rows()

## ME e MAE para C1_v000
(territories_cv_results %>% 
    filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% 
    ggplot(aes(x = biome, y = ME))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    scale_y_continuous(limits = c(-200, 800), breaks = seq(-200, 800, 200),
                       labels = seq(-200, 800, 200)/1000)+
    labs(x = NULL, y = expression(atop("ME", (kg~m^-2))),
         title = "Simulations for the version data: C1_v000 (n = 9531)")+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.ticks.x = element_blank(),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (territories_cv_results %>% 
     filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = biome, y = MAE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     scale_y_continuous(limits = c(1000, 3500), breaks = seq(1000, 3500, 500),
                        labels = seq(1000, 3500, 500)/1000)+
     labs(x = NULL, y = expression(atop("MAE", (kg~m^-2))))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = paste0(output_path, "biomes-me_and_mae-C1_v000.png"),
       width = 220, height = 148, units = "mm", dpi = 600)

## MSE, RMSE e NSE para C1_v000
library(scales)

(territories_cv_results %>% 
    filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% 
    ggplot(aes(x = biome, y = MSE))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    scale_y_continuous(limits = c(4000000, 60000000), labels = scientific)+
    labs(x = NULL,
         title = "Simulations for the version data: C1_v000 (n = 9531)")+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.ticks.x = element_blank(),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (territories_cv_results %>% 
     filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = biome, y = RMSE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     scale_y_continuous(limits = c(2000, 8000),
                        breaks = signif(seq(2000, 8000, 500), 3),
                        labels = signif(seq(2000, 8000, 500)/1000, 3))+
     labs(x = NULL, y = expression(atop("RMSE", (kg~m^-2))))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (territories_cv_results %>% 
     filter(map_version == "C1_v000", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = biome, y = NSE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     labs(x = NULL)+
     scale_y_continuous(limits = c(-0.3, 0.6), breaks = round(seq(-0.3, 0.6, 0.1), 1),
                        labels = round(seq(-0.3, 0.6, 0.1), 1))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = paste0(output_path, "biomes-mse_rmse_nse-C1_v000.png"),
       width = 220, height = 220, units = "mm", dpi = 300)

# # Viewing clusters --------------------------------------------------------
# 
# require(readr)
# require(dplyr)
# require(geosphere)
# library(jsonlite)
# library(RColorBrewer)
# 
# # Importing and splitting datasets ----------------------------------------
# input_data_path <- "C:/Users/erlis/OneDrive/Documentos/MEGA/Parcerias_Laboratorios/11_MapBiomas_GT-Solos/2023-04-25-cross_validation-study/01-data/"
# 
# original_data <- read_csv(paste0(input_data_path, "matriz_rf_prediction_v1-7-3.csv"))
# 
# data_version <- "v1-7-3"
# 
# # Grouping samples by geographical position -------------------------------
# 
# extract_latitude <- function(geojson) {
#   # parse GeoJSON string as JSON object
#   geojson_obj <- parse_json(geojson)
#   # extract latitude from JSON object
#   return(geojson_obj$coordinates[[2]])
# }
# 
# extract_longitude <- function(geojson) {
#   # parse GeoJSON string as JSON object
#   geojson_obj <- parse_json(geojson)
#   # extract longitude from JSON object
#   return(geojson_obj$coordinates[[1]])
#   
# }
# 
# original_data <- original_data%>%
#   mutate(LAT = apply(original_data %>% select(.geo), 1, extract_latitude)) %>%
#   mutate(LON = apply(original_data %>% select(.geo), 1, extract_longitude))
# 
# # compute distance matrix 
# dist_mat <- distm(original_data[, c("LON", "LAT")])
# 
# # apply k-means clustering to the distance matrix
# #randomNumbers(n = 1, min = 100, max = 9999, col = 1) %>% as.vector()
# set.seed(3019)
# k <- 30 # number of clusters
# kmeans_clusters <- kmeans(dist_mat, k)
# 
# # add cluster ID to original data frame
# original_data$cluster <- kmeans_clusters$cluster
# 
# # Visualizing clusters ----------------------------------------------------
# 
# n <- 20
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))
# 
# clustered_samples <- ggplot2::ggplot(original_data,
#                 aes(LON, LAT, color = as.factor(cluster),
#                     shape = as.factor(cluster)))+
#   geom_point(size = 3, alpha = 0.3)+
#   #scale_color_manual(aesthetics = "color",
#   #                   values = c("#ffd700", "#ffb14e", "#fa8775",
#   #                              "#ea5f94", "#ef0000", "#9d02d7",
#   #                              "#0000ff", "#04590d", "#00e8ff", "#000000"))+
#   #scale_shape_manual(values = c(15, 18, 17, 16, 3, 8, 0, 6, 9, 12))+
#   #scale_color_manual(values = sample(col_vector, 20))+
#   #scale_shape_manual(values = rep(c(15, 18, 17, 16, 3, 8, 0, 6, 9, 12), 2))+
#   scale_color_manual(values = sample(col_vector, 30))+
#   scale_shape_manual(values = rep(c(15, 18, 17, 16, 3, 8, 0, 6, 9, 12), 3))+
#   labs(y = "Lat (º)", x = "Long (º)",
#        title = "Point data from version v1-7-3 (n = 9531): 30 clusters of points")+
#   theme_bw()+
#   theme(legend.position = "none",
#         axis.title = element_text(family = "serif", size = 12, color = "#000000"),
#         axis.title.y = element_text(angle = 0, vjust = 0.5),
#         axis.text = element_text(family = "serif", size = 12, color = "#000000"),
#         title = element_text(family = "serif", size = 12, color = "#000000"),
#         panel.background = element_rect(fill = "#B9B9B9"),
#         plot.background = element_rect(fill = "#B9B9B9"))
# 
# ggsave(paste0(output_path, "clustered_points-v1_7_3-30_clusters.png"),
#        plot = clustered_samples,
#        width = 210, height = 250, units = "mm", dpi = 300) 
# 
# 
# 
