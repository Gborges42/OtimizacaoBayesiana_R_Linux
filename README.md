# Instalação e Execução da Aplicação R no Ambiente Linux via WSL

Este guia orienta sobre como configurar e executar uma aplicação R no Windows utilizando o Windows Subsystem for Linux (WSL) e o Ubuntu.

## Etapa 1: Instalar o WSL no Windows

Para instalar o WSL no Windows, siga as instruções oficiais da Microsoft:

-   [Instalar o WSL no Windows](https://docs.microsoft.com/pt-br/windows/wsl/install)

Após a instalação, reinicie o computador conforme solicitado.

## Etapa 2: Instalar o R no Ubuntu

Abra o terminal do Ubuntu e execute os seguintes comandos para instalar o R:

``` bash
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
sudo apt update
sudo apt install r-base
```

Para mais detalhes, consulte a documentação oficial do CRAN:

-   [Instalação do R no Ubuntu](https://cran.r-project.org/bin/linux/ubuntu/)

## Etapa 3: Preparar o Ambiente da Aplicação

1.  Faça o download do arquivo `.zip` contendo o código da aplicação.

2.  Extraia o conteúdo para o diretório `home` do seu usuário:

``` bash
unzip calibration-dssat-main.zip calibration-dssat-main
```

4.  Conceda permissões de leitura, escrita e execução para todos os arquivos:

``` bash
sudo chmod -R 777 calibration-dssat-main/
```

> **Nota:** Após cada extração do arquivo `.zip`, será necessário redefinir as permissões.

## Etapa 4: Instalar os Pacotes Necessários

1.  Para instalar os pacotes apropriadamente, primeiro deve-se instalar o cmake, necessário para compilação.

``` bash
sudo apt update
sudo apt install cmake
```

> **Dica:** Para que a instalação de pacotes R seja global (para todos os usuários), conceda permissões de escrita ao diretório `/usr/local/lib/R`:

> `bash sudo chmod -R 777 /usr/local/lib/R`

> Caso contrário, os pacotes serão instalados localmente para o usuário atual.

2.  Navegue até o diretório extraído:

``` bash
cd calibration-dssat-main
```

3.  Execute o script para instalar os pacotes necessários:

``` bash
./pacotes.sh
```

## Etapa 5: Configurar a Aplicação

1.  Localize o arquivo `StartValues_bean.config` no diretório do projeto.

2.  Edite o arquivo conforme necessário para configurar os parâmetros iniciais da aplicação.

## Etapa 6: Executar a Aplicação

> **Nota:** Certifique-se de estar dentro do diretório cd `calibration-dssat-main`

1.  No terminal, estando no diretório `calibration-dssat-main`, execute o seguinte comando para iniciar a aplicação:

``` bash
./Run.sh
```
