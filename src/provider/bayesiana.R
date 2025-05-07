#===============================================#
#Função objetivo, retorna o valor a ser minimizado
scoringFunction <- function(paramSim, inputList) {
  
  # Trycatch para possíveis erros
  tryCatch({
    # Salvando os parâmetros no log
    mensagem = ("Rodando Simulação para os valores:")
    for(valor in paramSim){
      mensagem = paste(mensagem, round(valor,2), sep = " ")
    }
    logfile <- "output/log_execucao.txt"
    cat(mensagem, file = logfile, append = TRUE)
    
    #==========================================================================#
    # Executando SSE 
    run = simulationFunction(paramSim, gsub(":", "", sprintf("iteration_%s", format(Sys.time(), "%H:%M:%OS3"))), inputList)
    
    # Obtendo o valor de RMSE
    calibration = as.character(inputList$calibration)
    RMSE = evaluateDifference(run, calibration)

    # salvando no log os resultados
    cat(sprintf(" - Valor do RMSE para rodada: %s\n", round(RMSE, 2)), file = logfile, append = TRUE)
    if(is.na(RMSE)){
      return(list(Score = -99))
    }else{
      return(list(Score = -(RMSE+rnorm(1,0,0.0001)))) # Queremos minimizar o erro, então retornamos negativo
    }
    
  }, error = function(e) {

    # Se der erro, retorne valor ruim (pior possível)
    return(list(Score = -99))
  })
}
#===============================================#

#===============================================#
# Função que configura e ativa a otimização bayesiana
runSimulationBaye = function(arq.config) {
  
  # Lendo o arquivo de configuração para adquirir o input
  input = config.treatment(arq.config)
  
  # Tratando parametros de entrada
  bounds = load.limites(input)
  
  # Destacando as variáveis de configuração
  initPoints = as.integer(input$initPoints)
  iters.n = as.integer(input$iters.n)
  iters.k = as.integer(input$iters.k)
  acq = input$acq
  
  environment(customBayesOpt) <- asNamespace('ParBayesianOptimization')
  assignInNamespace("bayesOpt", customBayesOpt, ns = "ParBayesianOptimization")
  bayesOpt <- customBayesOpt
  
  # Iniciando o arquivo de log
  logfile <- "output/log_execucao.txt"
  tempo_inicio <- # Tempo de início
  start_time <- Sys.time()
  mensagem <- sprintf("*******************************************************\n** Iniciando o processo de otimização. Tempo: %s\n", tempo_inicio)
  cat(mensagem, file = logfile, append = TRUE)
  
  # Realizando otimização bayesiana
  opt_result <- bayesOpt(
    FUN = function(EMFL = NA, FLSH = NA, FLSD = NA, SDPM = NA, FLLF = NA){
      # Vetor nomeado com as variáveis escolhidas, sem NAs
      paramSim = c(EMFL = EMFL, FLSH = FLSH, FLSD = FLSD, SDPM = SDPM, FLLF = FLLF)
      paramSim = na.omit(paramSim)
      
      scoringFunction(paramSim, input)
    },
    bounds = bounds,
    initPoints = initPoints,  # Testes iniciais
    iters.n = iters.n,  # Iterações de otimização
    iters.k = iters.k,
    acq = acq,
    verbose = 2,
    parallel = FALSE
  )
  
  # Salvando os resultados obtidos na calibração
  salvar_resultados_bo(opt_result, "output/bayesiana_opt_result.rds")
  resultado_carregado <- carregar_resultados_bo("output/bayesiana_opt_result.rds")
  
  # Salvar log do fim do programa
  tempo_decorrido = calcular_tempo_dec(start_time)
  mensagem <- sprintf("*******************************************************
** Fim do processo de otimização.
** Tempo decorrido: %s\n\n", tempo_decorrido)
  cat(mensagem, file = logfile, append = TRUE)
}
#===============================================#

#===============================================#
# Versão customizada da otimização bayesiana
customBayesOpt <- function (FUN, bounds, saveFile = NULL, initGrid, initPoints = 4, 
                            iters.n = 3, iters.k = 1, otherHalting = list(timeLimit = Inf, 
                                                                          minUtility = 0), acq = "ucb", kappa = 2.576, eps = 0, 
                            parallel = FALSE, gsPoints = pmax(100, length(bounds)^3), 
                            convThresh = 1e+08, acqThresh = 1, errorHandling = "continue", 
                            plotProgress = FALSE, verbose = 1, ...) 
{
  startT <- Sys.time()
  optObj <- list()
  class(optObj) <- "bayesOpt"
  optObj$FUN <- FUN
  optObj$bounds <- bounds
  optObj$iters <- 0
  optObj$initPars <- list()
  optObj$optPars <- list()
  optObj$GauProList <- list()
  optObj <- changeSaveFile(optObj, saveFile)
  checkParameters(bounds, iters.n, iters.k, otherHalting, 
                  acq, acqThresh, errorHandling, plotProgress, parallel, 
                  verbose)
  boundsDT <- boundsToDT(bounds)
  otherHalting <- formatOtherHalting(otherHalting)
  if (missing(initGrid) + missing(initPoints) != 1) 
    stop("Please provide 1 of initGrid or initPoints, but not both.")
  if (!missing(initGrid)) {
    setDT(initGrid)
    inBounds <- checkBounds(initGrid, bounds)
    inBounds <- as.logical(apply(inBounds, 1, prod))
    if (any(!inBounds)) 
      stop("initGrid not within bounds.")
    optObj$initPars$initialSample <- "User Provided Grid"
    initPoints <- nrow(initGrid)
  }
  else {
    initGrid <- randParams(boundsDT, initPoints)
    optObj$initPars$initialSample <- "Latin Hypercube Sampling"
  }
  optObj$initPars$initGrid <- initGrid
  if (nrow(initGrid) <= 2) 
    stop("Cannot initialize with less than 3 samples.")
  optObj$initPars$initPoints <- nrow(initGrid)
  if (initPoints <= length(bounds)) 
    stop("initPoints must be greater than the number of FUN inputs.")
  sinkFile <- file()
  on.exit({
    while (sink.number() > 0) sink()
    close(sinkFile)
  })
  `%op%` <- ParMethod(parallel)
  if (parallel) 
    Workers <- getDoParWorkers()
  else Workers <- 1
  if (verbose > 0) 
    cat("\nRunning initial scoring function", nrow(initGrid), 
        "times in", Workers, "thread(s)...")
  sink(file = sinkFile)
  tm <- system.time(scoreSummary <- foreach(iter = 1:nrow(initGrid), 
                                            .options.multicore = list(preschedule = FALSE), .combine = list, 
                                            .multicombine = TRUE, .maxcombine = 100*nrow(initGrid), .inorder = FALSE, .errorhandling = "pass", 
                                            .verbose = FALSE) %op% {
                                              Params <- initGrid[get("iter"), ]
                                              Elapsed <- system.time(Result <- tryCatch({
                                                do.call(what = FUN, args = as.list(Params))
                                              }, error = function(e) e))
                                              if (any(class(Result) %in% c("simpleError", "error", 
                                                                           "condition")))
                                                return(Result)
                                              if (!inherits(x = Result, what = "list")) 
                                                stop("Object returned from FUN was not a list.")
                                              resLengths <- lengths(Result)
                                              if (!any(names(Result) == "Score")) 
                                                stop("FUN must return list with element 'Score' at a minimum.")
                                              if (!is.numeric(Result$Score)) 
                                                stop("Score returned from FUN was not numeric.")
                                              if (any(resLengths != 1)) {
                                                badReturns <- names(Result)[which(resLengths != 
                                                                                    1)]
                                                stop("FUN returned these elements with length > 1: ", 
                                                     paste(badReturns, collapse = ","))
                                              }
                                              data.table(Params, Elapsed = Elapsed[[3]], as.data.table(Result))
                                            })[[3]]
  while (sink.number() > 0) sink()
  if (verbose > 0) 
    cat(" ", tm, "seconds\n")
  se <- which(sapply(scoreSummary, function(cl) any(class(cl) %in% 
                                                      c("simpleError", "error", "condition"))))
  if (length(se) > 0) {
    print(data.table(initGrid[se, ], errorMessage = sapply(scoreSummary[se], 
                                                           function(x) x$message)))
    stop("Errors encountered in initialization are listed above.")
  }
  else {
    scoreSummary <- rbindlist(scoreSummary)
  }
  scoreSummary[, `:=`(("gpUtility"), rep(as.numeric(NA), nrow(scoreSummary)))]
  scoreSummary[, `:=`(("acqOptimum"), rep(FALSE, nrow(scoreSummary)))]
  scoreSummary[, `:=`(("Epoch"), rep(0, nrow(scoreSummary)))]
  scoreSummary[, `:=`(("Iteration"), 1:nrow(scoreSummary))]
  scoreSummary[, `:=`(("inBounds"), rep(TRUE, nrow(scoreSummary)))]
  scoreSummary[, `:=`(("errorMessage"), rep(NA, nrow(scoreSummary)))]
  extraRet <- setdiff(names(scoreSummary), c("Epoch", "Iteration", 
                                             boundsDT$N, "inBounds", "Elapsed", "Score", "gpUtility", 
                                             "acqOptimum"))
  setcolorder(scoreSummary, c("Epoch", "Iteration", boundsDT$N, 
                              "gpUtility", "acqOptimum", "inBounds", "Elapsed", "Score", 
                              extraRet))
  if (any(scoreSummary$Elapsed < 1) & acq == "eips") {
    cat("\n   FUN elapsed time is too low to be precise. Switching acq to 'ei'.\n")
    acq <- "ei"
  }
  optObj$optPars$acq <- acq
  optObj$optPars$kappa <- kappa
  optObj$optPars$eps <- eps
  optObj$optPars$parallel <- parallel
  optObj$optPars$gsPoints <- gsPoints
  optObj$optPars$convThresh <- convThresh
  optObj$optPars$acqThresh <- acqThresh
  optObj$scoreSummary <- scoreSummary
  optObj$GauProList$gpUpToDate <- FALSE
  optObj$iters <- nrow(scoreSummary)
  optObj$stopStatus <- "OK"
  optObj$elapsedTime <- as.numeric(difftime(Sys.time(), startT, 
                                            units = "secs"))
  saveSoFar(optObj, 0)
  optObj <- addIterations(optObj, otherHalting = otherHalting, 
                          iters.n = iters.n, iters.k = iters.k, parallel = parallel, 
                          plotProgress = plotProgress, errorHandling = errorHandling, 
                          saveFile = saveFile, verbose = verbose, ...)
  return(optObj)
}
