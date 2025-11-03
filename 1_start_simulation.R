# Carregando funcoes de inicializacao
source(".//src//loader.R")

# Carregando pacotes
load.packages()

# Compilando funcoes
compile.functions()

# Carregando funcoes compiladas
load.functions()

# Arquivo de Configuração
# ATENÇÃO!!! Ao mudar o arquivo de configurção, será necessário mudar também dentro da função 
arq.config = ".//StartValues_bean.config"

# Vetores com os valores desejados
pontosIniciais <- c(10) #, 100, 200, 300, 400, 500)
NIteracoes <- c(2) #, 100, 200, 300, 400, 500)

# Gerar todas as combinações possíveis (produto cartesiano)
dt_combinacoes <- data.table(initPoints = pontosIniciais, iters.n = NIteracoes)

#===================================#
# Executar calibracao
apply(dt_combinacoes, FUN = runSimulationBaye, MARGIN = 1, arq.config = arq.config)
#===================================#