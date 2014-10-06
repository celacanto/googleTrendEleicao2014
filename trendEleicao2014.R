# Pacotes
# -------------------------------------------------------------------------------
library(MASS) 


# Input
# -------------------------------------------------------------------------------
googleTrendCsv  <- "~/Desktop/trendDir/report.csv"
outputImageDir  <- "~/Desktop/trendDir/"


# Variáveis do gráfico
# -------------------------------------------------------------------------------

### Fonte
family  <- "NewCenturySchoolbook"


#### Pontos e linhas
# Cores
cor_dilma  <- "#A6483F"
cor_marina  <- "#7BB38E"
cor_aecio  <- "#355B8C"
alpha_points  <- 0.5
pch_datafolha  <- 21
pch_ibope  <- 23
pch_votacao  <- 4

# Formatos
grossura_linhas  <- 4
tamanho_pontos  <- 2


### Eixos
range_y  <- c(0, 80)
nXticks  <- 5
lwdAxis  <- 0.4
lasY  <- 2
yAxisLabel  <- "% sobre o total\ndos 3 candidatos"
xAxisLabel  <- "Data"
 
#### Lowess
# f de 2/3 é o default
trend_f  <- 2/3



# Functions
# -------------------------------------------------------------------------------

formatColumnHeader  <- function(x){
  splited  <- strsplit(x, ",")
  vec  <- unlist(splited)
  semAcento  <- iconv(vec, to="ASCII//TRANSLIT")
  semEspaco  <- gsub(" ", ".", semAcento) 
}

extractTrendTable  <- function(file){
  startTable  <- 5
  endTable  <- which(file == "")[2] - 1  
  table  <- file[startTable:endTable]
}

formatColumnsClasses  <- function(df){
  df[,1]  <- as.Date(df[,1])
  nCol  <- ncol(df)
  for(i in 2:nCol){
    df[, i]  <- as.integer(df[,i])
  }
  df
}

getTrendDf  <- function(trendCsv, minDate){
  file  <- readLines(trendCsv)
  table <- extractTrendTable(file)
  
  header  <- table[1]
  data  <- table[-1]
  
  mtx <- do.call(rbind, strsplit(data, ","))
  df  <- as.data.frame(mtx, stringsAsFactors = FALSE)
  
  colnames(df)  <- formatColumnHeader(header)
  df <- formatColumnsClasses(df)
  dfRowsComplete  <- df[complete.cases(df),]
  dfRowsComplete[dfRowsComplete[,1] > minDate,]
}



dados2InstitutoDf  <- function(dados){
  mtx  <- matrix(dados, ncol = 4, byrow = TRUE)
  df  <- as.data.frame(mtx, stringsAsFactors = FALSE)
  colnames(df)  <- c("Day", "Marina.Silva", "Aecio.Neves" , "Dilma.Rousseff")
  df[, 1]  <- as.Date(df[,1])    
  for(i in 2:4){
    df[, i]  <- as.integer(df[,i])
  }
  return(df)
}

makePercentageDf  <- function(df){
  totalDia  <- rowSums(df[,2:4])
  df[,2:4]  <- (df[,2:4]/totalDia) * 100
  df
}



# Format Trend
# ---------------------------------------------------------------------------------------


data_marinaAparece  <-  "2014-08-13"
trend  <- getTrendDf(trendCsv = googleTrendCsv, minDate=data_marinaAparece)
trend_percentage  <- makePercentageDf(trend)



# Research Data
# ---------------------------------------------------------------------------------------------
# instituto = (Data, Marina, Aécio, Dilma)

list_dados  <- list(ibope  = c("2014-08-26", 29, 19, 34, 
                               "2014-09-03", 33, 15, 37,
                               "2014-09-12", 31, 15, 39,
                               "2014-09-16", 30, 19, 36,
                               "2014-09-23", 29, 19, 38,
                               "2014-09-30", 25, 19, 39,
                               "2014-10-02", 24, 19, 40,
                               "2014-10-04", 21, 24, 40),
                    
                    dataFolha  = c("2014-08-18", 21, 20, 36, 
                                   "2014-08-29", 34, 15, 34,
                                   "2014-09-03", 34, 14, 35,
                                   "2014-09-10", 33, 15, 36, 
                                   "2014-09-19", 30, 17, 37,
                                   "2014-09-26", 27, 18, 40,
                                   "2014-09-30", 25, 20, 40,
                                   "2014-10-02", 24, 21, 40,
                                   "2014-10-04", 22, 24, 40),
          
                    votacao  = c("2014-10-05", 21, 33, 41))

dados_df  <- lapply(list_dados, dados2InstitutoDf)
dados_df_percentage  <- lapply(dados_df, makePercentageDf)



# Plot
# ------------------------------------------------------------------------------------------------

### Formata variáveis
candidatos  <- c("Marina.Silva", "Aecio.Neves", "Dilma.Rousseff")

cores  <- c(Marina.Silva = cor_marina, 
            Aecio.Neves = cor_aecio, 
            Dilma.Rousseff = cor_dilma)

institutos  <- c("ibope", "dataFolha")
formasInstitutos  <- c(ibope = pch_ibope, dataFolha = pch_datafolha )


par(family = family)
datas  <- c(trend_percentage[,1], dados_df_percentage$votacao$Day)
range_x  <- range(datas)


#### Faz o gráfico

# F do lowess
# 1/1000 é como não fazer lowess (já que tem menos que 1000 pontos)
fs  <- c(trend_f, 1/1000)

for(f in fs){
  
  # abre png
  if(f == 1/1000){
    filename  <- paste0(outputImageDir, "/trendEleicao.png")
  } else {
    filename  <- paste0(outputImageDir, "/trendEleicao_lowess.png")
  }
  png(filename = filename, width = 1118, height = 906)
  
  
  
  # Vazio
  plot(x = range_x, 
       y = range_y,  
       type = "n", 
       axes = FALSE, 
       xlab = "", 
       ylab = "")
  
  # eixos
  axisTicks  <- seq(from=datas[1], to=tail(datas, 1), length.out = nXticks)
  axis.Date(side = 1, at=  axisTicks, lwd = lwdAxis)
  axis(2 ,las = 1, lwd= lwdAxis)
  
  
  for(cand in candidatos){
    
    corCand  <- cores[cand]
    
    # Trend
    lowessTrend  <- lowess(trend_percentage$Day, trend_percentage[,cand], f = f)
    lines(lowessTrend, 
          col = corCand, 
          lwd = grossura_linhas)
    
    
    # Institutos
    for(inst in institutos){
      
      instDf  <- dados_df_percentage[[inst]]
      corPonto  <- adjustcolor(corCand, alpha_points)
      formatoInstituto  <- formasInstitutos[inst]
      numeroCandidato  <- instDf[,cand]
      datas_pesquisas  <-  instDf$Day
      
      
      points(x = datas_pesquisas, 
             y = numeroCandidato, 
             pch = formatoInstituto, 
             bg = corPonto, 
             cex = tamanho_pontos)
    }
    
    
    # Votacao
    diaVotacao  <- dados_df_percentage$votacao$Day
    votacao_cand  <- dados_df_percentage$votacao[,cand]
    points(x= diaVotacao, 
           y = votacao_cand, 
           pch = pch_votacao,
           col = corCand,
           cex = tamanho_pontos)
    
  }
  
  
  #### Legendas
  
  # Formas
  legendaX_formas  <- ((par("usr")[1] + par("usr")[2])/2) - 15
  legendaY_formas  <- par("usr")[4]
  fFraction  <- as.fractions(trend_f)
  
  if(f == 1/1000){
    googletrendLegenda  <- "Google Trend"
  } else {
    googletrendLegenda  <- paste0("Google Trend (lowess, f = ", fFraction, ")")
  }
  labelslegendas_formas  <- c("IBOPE", 
                              "DataFolha", 
                              "Votação",
                              googletrendLegenda)
  pchsLegenda_formas  <- c(formasInstitutos, pch_votacao, NA)
  ltyLegenda_formas  <- c(NA, NA, NA, 1)
    
  legend(x = legendaX_formas,
         y = legendaY_formas, 
         xpd = TRUE,
         pch = pchsLegenda_formas, 
         lty = ltyLegenda_formas,
         legend = labelslegendas_formas,
         box.lwd = 0) 
  
  
  ### Candidatos
  legendaX_cand  <- legendaX_formas + 20 
  legend(x = legendaX_cand,
         y = legendaY_formas,
         box.lwd = 0, 
         border = NA,
         fill = cores,
         xpd = TRUE,
         legend=c("Marina Silva", "Aécio Neves", "Dilma Rousseff")) 
  
  
  #### Títutlo dos Eixos
  mtext(text = yAxisLabel, 
        side = 2, 
        las = 1, 
        at = par("usr")[4] + 2, 
        adj = 0.5)
  
  mtext(text = xAxisLabel, 
        line = 3,
        side = 1)
  
  dev.off()
}
