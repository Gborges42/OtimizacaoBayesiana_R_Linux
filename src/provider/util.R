#===============================================#
#Função objetivo, retorna o valor a ser minimizado
resultadosRMSEs <- function(simulacao, paramSim, inputList) {
  
  #==========================================================================#
  # Executando SSE 
  run = simulationFunction(paramSim, gsub(":", "", sprintf("iteration_%s", format(Sys.time(), "%H:%M:%OS3"))), inputList)
  run$Origem = simulacao
  return(run)
  # Obtendo os valores de RMSE
  calibration = as.character(inputList$calibration)
  
  # Calculando RMSE individuais
  rmse.list = lapply(calibration, function(calibration, evaluateData){
    
    # Obtendo index variaveis de calibracao
    variable.index = grep(paste0(calibration, collapse = "|"), names(evaluateData))
    
    # Obtendo variaveis simuladas
    calibrationData = evaluateData[, ..variable.index]
    calibrationData[calibrationData == -99] = NA
    
    # Fazendo RMSE da variável
    rmse <- sqrt(mean((calibrationData[[1]] - calibrationData[[2]])^2))
    
    return(c(calibration, rmse))
  }, run)
  
  # Iniciando o data.table
  dt = data.table("Origem" = simulacao, t(paramSim)) 
  
  # Inserindo RMSEs
  for(rmse in rmse.list){
    dt[1, sprintf("RSME_%s", rmse[1])] = round(as.numeric(rmse[2]), 3)
  }
  
  # Inserindo RMSE médio
  # Remover a primeira string de cada concatenação, mantendo a segunda
  resultados <- sapply(rmse.list, function(x) x[2])
  # Média RMSE
  dt$RMSE_med = round(mean(unlist(as.numeric(resultados))), 3)
  
  return(dt)
}


#===============================================#
# Lê os limites das variáveis a serem calibradas
load.limites <- function(input){
  
  # Adquirindo limites
  limites <- list()
  for(i in input$limites){
    
    # Separando string em nome e valores
    coef_limit <- strsplit(i, ":")[[1]]
    coef_name <- coef_limit[1]
    limits <- as.numeric(strsplit(coef_limit[2], ";")[[1]])
    
    # Removendo hífen
    coef_name <- gsub("-", "", coef_name)
    
    # Atribuindo os limites à lista com o nome do coeficiente
    limites[[coef_name]] <- limits
  }
  
  return(limites)
}
#===============================================#
calcular_tempo_dec <- function(start_time){
  # Tempo de fim
  end_time <- Sys.time()
  
  # Diferença total em segundos
  time_diff_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Conversão para dias, horas, minutos e segundos
  days <- floor(time_diff_sec / (24 * 3600))
  remainder <- time_diff_sec %% (24 * 3600)
  hours <- floor(remainder / 3600)
  remainder <- remainder %% 3600
  minutes <- floor(remainder / 60)
  seconds <- remainder %% 60
  
  # Mensagem de tempo
  tempo_decorrido = sprintf("")
  if(days != 0){
    tempo_decorrido = paste(tempo_decorrido, sprintf("D: %s", days), sep = " ")
  }
  if(hours != 0){
    tempo_decorrido = paste(tempo_decorrido, sprintf("H: %s", hours), sep = " ")
  }
  if(minutes != 0){
    tempo_decorrido = paste(tempo_decorrido, sprintf("M: %s", minutes), sep = " ")
  }
  if(seconds != 0){
    tempo_decorrido = paste(tempo_decorrido, sprintf("S: %s", round(seconds)), sep = " ")
  }

  return(tempo_decorrido)
}
#===============================================#

#===============================================#
# Função que salva os resultados da otimização
salvar_resultados_bo <- function(resultado_bo, caminho_output, valoresSufixo) {
  # Salvando o objeto RDS em completo
  rdsFileDir = sprintf("%s/bayeopt_%s_%s_%s.rds", caminho_output, valoresSufixo[1], valoresSufixo[2], valoresSufixo[3])
  saveRDS(resultado_bo, file = rdsFileDir)
  
  # Salvando todas as rodadas
  rodadas = resultado_bo$scoreSummary
  rodadasFileDir = sprintf("%s/todas_rodadas_%s_%s_%s.csv", caminho_output, valoresSufixo[1], valoresSufixo[2], valoresSufixo[3])
  fwrite(rodadas, rodadasFileDir)
  
  melhor_res = rodadas[Score == max(rodadas$Score)]
  melhor_resFileDir = sprintf("%s/melhor_resultado_%s_%s_%s.csv", caminho_output, valoresSufixo[1], valoresSufixo[2], valoresSufixo[3])
  fwrite(melhor_res, melhor_resFileDir)
}
#===============================================#

#===============================================#
# Função para ler o resultado da otimização como um objeto R
carregar_resultados_bo <- function(caminho_arquivo) {
  # Lendo o objeto RDS em completo
  resultado_bo <- readRDS(file = caminho_arquivo)
  return(resultado_bo)
}
#===============================================#