#===============================================#
# Função que dita a simulação da safra com o DSSAT, dado os parâmetros
simulationFunction = function(paramSim, templateId, inputList) {
  # Iniciando timer
  startTime = Sys.time() |> as.numeric()
  
  # Criando diretorios das simulacoes
  simulation.list = createSimulationDirectories(paramSim, templateId, inputList)
  
  # Iniciando paralelismo caso esteja ativado
  noCores = as.numeric(inputList$simulationCores)

  # Obtendo modelo e calibracao
  model = as.character(inputList$model)
  calibration = as.character(inputList$calibration)

  # Obtendo arquivo de execussao do dssat
  dssatFile = as.character(inputList$dssatFile)

  # Executando Dssat em serie
  run = runDssat(simulation.list, model, dssatFile, calibration)
  
  # Encerrando timer
  endTime = Sys.time() |> as.numeric()
  
  # Retornando valores da run
  return(run)
}
#===============================================#