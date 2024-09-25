### Script com as funções para Validação de Modelos de Regressão ###

#Data: 2023-12-12
#Autores: Erli dos Santos, Marcos Cardoso, Taciara Horst
#Contato: erlipinto@gmail.com, cardoso.mvs@gmail.com

## Objetivo:
# Este script contém funções para calcular métricas de avaliação de modelos de regressão,
#focando em métricas como ME, MSE, MAE, RSME, R² e NSE.

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

require(chillR)

# Definindo uma função calcula estatísticas de validação.
my_summary_metrics <- function(data, lev = NULL, model = NULL) {
  
#-- data: DataFrame contendo as colunas "pred" (predições do modelo) e "obs" (observações reais).
#-- lev: Níveis da variável de resposta (usada para converter "obs" em fator, caso necessário).
#-- model: Objeto do modelo de regressão (para recuperar informações adicionais, não utilizado no código atual).
  
# Esta função calcula diversas métricas a partir de vetores de predisões e observações
  regression_eval <- function(pred, obs){
    
    # Erro médio
    ME <- round(mean(pred - obs, na.rm = TRUE), digits = 4)
    
    # Erro médio quadrático
    MSE <-   round(mean((pred - obs)^2, na.rm = TRUE), digits = 4)
    
    # Erro absoluto médio
    MAE <-   round(mean(abs(pred - obs), na.rm = TRUE), digits = 4)
    
    # Raiz do erro médio quadrático
    RMSE <-   round(sqrt(mean((pred - obs)^2, na.rm = TRUE)), digits = 4)
    
    # Coeficiente de correlação de Pearson ao quadrado
    r2 <-  round((cor(pred, obs, method = 'spearman', use = 'pairwise.complete.obs')^2), digits = 4)
    
    # Coeficiente de eficiência do modelo Nash–Sutcliffe
    # NSE <- round(1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2), digits = 4)
    NSE <- round((1 - (sum((obs - pred)^2) / sum((obs - mean(obs))^2))), digits = 4)
    
        # Coeficiente de correlação de concordância de Lin (Paramos de utilizar desde o beta)
    CCC <- round(yardstick::ccc_vec(truth = obs, estimate = pred), digits = 4)

    # Variância
    VAR <- round(var(pred - obs, na.rm = TRUE), digits = 4)

    # Desvio padrão
    SD <- round(sqrt(VAR), digits = 4)
    
# Criando um vetor 'out' com os resultados
    out <- c(ME, MAE, MSE, RMSE, NSE, r2, CCC, VAR, SD) #, CCC
    names(out) <- c("ME", "MAE", "MSE", "RMSE", "NSE", "Rsquared", "CCC", "VAR", "SD") #, "CCC"
    
# Criando um vetor 'out' com os resultados
    out <- c(ME, MAE, MSE, RMSE, NSE, r2, CCC) #, CCC
    names(out) <- c("ME", "MAE", "MSE", "RMSE", "NSE", "Rsquared", "CCC") #, "CCC"
    
# Substituindo valores NaN por NA, se houver
    if (any(is.nan(out))) 
      out[is.nan(out)] <- NA
# Retornando o vetor 'out' como resultado   
    out
    
  }
# Verificando se a variável 'obs' em 'data' é do tipo character e, se for, convertendo-a para factor  
  if (is.character(data$obs)) 
    data$obs <- factor(data$obs, levels = lev)
  
# Chamando a função 'regression_eval' com as colunas 'pred' e 'obs' de 'data'  
  regression_eval(data[, "pred"], data[, "obs"])
  
}

