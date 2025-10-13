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

#===================================#
# Executar calibracao
runSimulationBaye(arq.config)
#===================================#
