#################################################################################################
# plot1.R
# -----------------------------------------------------------------------------------------------
# QUESTION: Le emissioni totali di PM2.5 sono decrementate in US dal 1999 al 2008. Utilizzando il 
#           BASE plotting system, mostrare il TOTALE PM2.5 da tutte le source per ognuno degli anni.
#################################################################################################
# REQUIRE(START) ---------------------------------------------------------------------------------
require(dplyr)
# GLOBAL(START) ---------------------------------------------------------------------------------
dew4p2.g_dfSourceClassCode <- NULL
dew4p2.g_dfSummarySCC <- NULL

dew4p2.g_fncInit <- function(){
  
  dew4p2.g_dfSummarySCC <<- readRDS("summarySCC_PM25.rds")
  dew4p2.g_dfSourceClassCode <<- readRDS("Source_Classification_Code.rds")
  
}
# UNIT-TEST(START) ---------------------------------------------------------------------------------
dew4p2.ut_g_fncInit <- function(){
  #rm(dew4p2.g_dfSummarySCC)
  #rm(dew4p2.g_dfSourceClassCode)
  dew4p2.g_fncInit()
  View(head(dew4p2.g_dfSummarySCC))
  View(head(dew4p2.g_dfSourceClassCode))
}

dew4p2.ut_l_summaryByCountryAndYear <- function(){
  
  print("TEST dew4p2.l_summaryByCountryAndYear")
  if(length(dew4p2.g_dfSummarySCC) == 0 ){
    print("Devo ricaricare i file ...")
    dew4p2.g_fncInit()
    print("File ricaricati !")
  }
  print("File già pronti, effettuo il summarize")
  tst <- dew4p2.l_fncSummaryByCountryAndYear(dew4p2.g_dfSummarySCC)
  print("Stampo il summarize ...")
  View(tst)
  
}

dew4p2.ut_l_fncPlot <- function(){

  print("TEST dew4p2.l_summaryByCountryAndYear")
  if(length(dew4p2.g_dfSummarySCC) == 0 ){
    print("Devo ricaricare i file ...")
    dew4p2.g_fncInit()
    print("File ricaricati !")
  }
  print("File già pronti, effettuo il summarize")
  tst <- dew4p2.l_fncSummaryByCountryAndYear(dew4p2.g_dfSummarySCC) 
  print("Effettuo il plot")
  dew4p2.l_fncPlot(tst)
  
}

dew4p2.ut_l_fncSavePlot <- function(){
  
  dew4p2.ut_l_fncPlot()
  print("Salvo con ut_plot1.png")
  dew4p2.l_fncSavePlot("ut_plot1.png")
  
}

# LOCAL(START)  ----------------------------------------------------------------------------------
dew4p2.l_mainTitle <- "Total Emission of PM2.5 over years"
dew4p2.l_ylab <- "10^6 TONS Of PM2.5"
dew4p2.l_xlab <- "Year of Measurements"
dew4p2.l_yrefactor <- 1000000
dew4p2.l_fileName <- "plot1.png"
dew4p2.l_yearFrom <- c(1,1999)
dew4p2.l_yearTo <- c(4,2008)

dew4p2.l_fncSummaryByCountryAndYear <- function(df){
  
  return (df %>% group_by(year) %>% summarize(totale = sum(Emissions)))
  
}

dew4p2.l_fncPlot <- function(dfEmissionByYear){
  
  #dev.off()
  barplot(dfEmissionByYear$totale/dew4p2.l_yrefactor,
          names.arg=dfEmissionByYear$year,
          ylab = dew4p2.l_ylab,
          xlab = dew4p2.l_xlab ,
          main = dew4p2.l_mainTitle)  
  
  dew4p2.l_fncPlotSegment(
    dfEmissionByYear = dfEmissionByYear,
    xYearFrom = dew4p2.l_yearFrom[1] ,
    xYearTo = dew4p2.l_yearTo[1],
    yearFrom = dew4p2.l_yearFrom[2],
    yearTo = dew4p2.l_yearTo[2],
    factor = dew4p2.l_yrefactor
  )
  
}

dew4p2.l_fncGetValueByYear <- function(dfEmissionByYear,searchYear){
  
  return(dfEmissionByYear %>% filter(year==searchYear) %>% select(totale));
  
}

dew4p2.l_fncPlotSegment <- function(dfEmissionByYear,
                                    xYearFrom,xYearTo,yearFrom,yearTo,
                                    factor){
  
  l_valYearFrom <- dew4p2.l_fncGetValueByYear(dfEmissionByYear,yearFrom) / factor
  l_valYearTo <- dew4p2.l_fncGetValueByYear(dfEmissionByYear,yearTo) / factor
  segments(
    xYearFrom,l_valYearFrom[[1]],
    xYearTo,l_valYearTo[[1]],
    col="blue",
    lwd=5
  )
  
}

dew4p2.l_fncSavePlot <- function(fileName){
  
  #dev.off()
  dev.copy(png,fileName)
  dev.off()
  
}

dew4p2.l_fncMain <- function(){
  dew4p2.g_fncInit()
  l_emissionByYear <- dew4p2.l_fncSummaryByCountryAndYear(dew4p2.g_dfSummarySCC)
  dew4p2.l_fncPlot(l_emissionByYear)
  dew4p2.l_fncSavePlot(dew4p2.l_fileName)
  
}

# MAIN(START)  ----------------------------------------------------------------------------------
dew4p2.l_fncMain()

