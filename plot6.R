#################################################################################################
# plot6.R
# -----------------------------------------------------------------------------------------------
# QUESTION: Confronta le emissioni di motor-vehicle in Baltimore City con le emissioni di 
#           motor-vehicle in Los Angeles County (fips == "06037"). 
#           Quale città ha visto i più grandi cambiamenti nel tempo ?
#################################################################################################
# REQUIRE(START) ---------------------------------------------------------------------------------
require(dplyr)
require(ggplot2)
# GLOBAL(START) ---------------------------------------------------------------------------------
dew4p2.g_dfSourceClassCode <- NULL
dew4p2.g_dfSummarySCC <- NULL

dew4p2.g_fncInit <- function(){
  
  dew4p2.g_dfSummarySCC <<- readRDS("summarySCC_PM25.rds")
  dew4p2.g_dfSourceClassCode <<- readRDS("Source_Classification_Code.rds")
  
}
# UNIT-TEST(START) ---------------------------------------------------------------------------------
dew4p2.ut_g_fncInit <- function(){
  
  dew4p2.g_fncInit()
  View(head(dew4p2.g_dfSummarySCC))
  View(head(dew4p2.g_dfSourceClassCode))
}


# LOCAL(START)  ----------------------------------------------------------------------------------
dew4p2.l_fileName <- "plot6.png"
dew4p2.l_ylab <- "PM2.5"
dew4p2.l_mainTitle <- "Emission of PM2.5 about motor-vehicle in Baltimora(24510) vs LosAngeles(06037)"
dew4p2.l_xlab <- "Year of Measurements"
dew4p2.l_countryVector <- c("24510", "06037")

dew4p2.l_fncSummaryByYearSelectedSCCAndCountry <- function(df,sourceVector,countryVector){
  
  df1 <- df %>% 
    filter(SCC %in% sourceVector & fips %in% countryVector ) %>% 
    select(Emissions,year,fips) %>% 
    group_by(year,fips) %>% 
    summarize(totale = sum(Emissions)) %>%
    select(year,fips,totale)
  return(df1)
  
}

dew4p2.l_fncAddModelCoeff <- function(dfEmissionByYear){
  
  dfCalcModel <- dfEmissionByYear %>% 
    group_by(fips) %>% 
    do(model=lm(year ~ totale,data=.))
  
  vCoeff <- sapply(dfCalcModel$model,function(x) { x$coefficients[[2]] })
  vCoeff[is.na(vCoeff)] <- 0
  dfNew <- data.frame(fips=dfCalcModel$fips,coeff=vCoeff)
  dfNew <- merge(dfEmissionByYear,dfNew)
  
  return(dfNew)
}

dew4p2.l_fncMain <- function(){
  dew4p2.g_fncInit()
  #Select the correct scc
  l_sccSelected <- dew4p2.l_fncGetSCCOfCoalCombRelated(dew4p2.g_dfSourceClassCode)
  l_emissionByYear <- dew4p2.l_fncSummaryByYearSelectedSCCAndCountry(
    dew4p2.g_dfSummarySCC,
    l_sccSelected,
    dew4p2.l_countryVector)
  dew4p2.l_fncPlot(l_emissionByYear)
  dew4p2.l_fncSavePlot(dew4p2.l_fileName)
  
}

dew4p2.l_fncSavePlot <- function(fileName){
  
  #dev.off()
  dev.copy(png,fileName)
  dev.off()
  
}

dew4p2.l_fncGetSCCOfCoalCombRelated <- function(dfSCC){
  
  return
  (
    dfSCC$SCC[
      grep(".*([M|m]otor).*([V|v]ehicle).*",
           dfSCC$Short.Name)]
    
  )
  
}


dew4p2.l_fncPlot <- function(dfEmissionByYear){
  
  dfWithCoeff <- dew4p2.l_fncAddModelCoeff(dfEmissionByYear)
  
  
  dfpositivi <- dfWithCoeff %>% 
    filter(coeff <= 0) %>%
    select(year,fips,totale)
  dfnegativi <- dfWithCoeff %>%
    filter(coeff > 0) %>%
    select(year,fips,totale)

  #dev.off()
  g <- qplot(
    x=as.character(year),
    y=totale,
    data=dfEmissionByYear,
    geom="bar",
    fill=I("gray70"),
    ylab=dew4p2.l_ylab,
    xlab=dew4p2.l_xlab,
    facets = .~ fips,
    stat="identity") + geom_smooth( #POSITIVI
      data = dfpositivi,
      method="lm",
      aes(group=1),
      size=2,
      colour=I("lightgreen"),
      weight=3,
      level=.25
    )+ geom_smooth( #NEGATIVI
      data = dfnegativi,
      method="lm",
      aes(group=1),
      size=2,
      colour=I("red"),
      weight=3,
      level=.25
    )+ labs(title = dew4p2.l_mainTitle)
  plot(g)
  
}

# MAIN(START)  ----------------------------------------------------------------------------------
dew4p2.l_fncMain()
