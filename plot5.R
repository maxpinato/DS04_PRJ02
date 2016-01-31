#################################################################################################
# plot5.R
# -----------------------------------------------------------------------------------------------
# QUESTION: 	Come sono cambiate le emissioni da motor-vehicle dal 1999-2008 in Baltimore City ?
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
dew4p2.l_fileName <- "plot5.png"
dew4p2.l_ylab <- "PM2.5"
dew4p2.l_mainTitle <- "Emission of PM2.5 about motor-vehicle in Baltimora"
dew4p2.l_xlab <- "Year of Measurements"
dew4p2.l_baltimoraFIPS <- "24510"

dew4p2.l_fncSummaryByYearSelectedSCCAndCountry <- function(df,sourceVector,countrySelected){
  
  df1 <- df %>% 
    filter(SCC %in% sourceVector & fips == countrySelected ) %>% 
    select(Emissions,year) %>% 
    group_by(year) %>% 
    summarize(totale = sum(Emissions)) %>%
    select(year,totale)
  return(df1)
  
}


dew4p2.l_fncMain <- function(){
  dew4p2.g_fncInit()
  #Select the correct scc
  l_sccSelected <- dew4p2.l_fncGetSCCOfCoalCombRelated(dew4p2.g_dfSourceClassCode)
  l_emissionByYear <- dew4p2.l_fncSummaryByYearSelectedSCCAndCountry(
    dew4p2.g_dfSummarySCC,
    l_sccSelected,
    dew4p2.l_baltimoraFIPS)
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
  
 l_color <- I("lightgreen")
  #dev.off()
  g <- qplot(
    x=as.character(year),
    y=totale,
    data=dfEmissionByYear,
    geom="bar",
    fill=I("gray70"),
    ylab=dew4p2.l_ylab,
    xlab=dew4p2.l_xlab,
    stat="identity") + geom_smooth( 
      method="lm",
      aes(group=1),
      size=2,
      colour=l_color,
      weight=3,
      level=.25
    )+ labs(title = dew4p2.l_mainTitle)
  plot(g)
  
}

# MAIN(START)  ----------------------------------------------------------------------------------
dew4p2.l_fncMain()
