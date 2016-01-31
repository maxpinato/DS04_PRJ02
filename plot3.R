#################################################################################################
# plot3.R
# -----------------------------------------------------------------------------------------------
# QUESTION: 	Dei quattro tipi di source (type), per quali di questi source abbiamo 
#             visto DECREMENTI dal 1999-2008 in Baltimore City ? 
#             Quale di questi è INCREMENTATO. Usare ggplot2.
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

dew4p2.ut_l_fncAddModelCoeff <- function(){
  
  print("TEST dew4p2.ut_l_fncAddModelCoeff")
  if(length(dew4p2.g_dfSummarySCC) == 0 ){
    print("Devo ricaricare i file ...")
    dew4p2.g_fncInit()
    print("File ricaricati !")
  }
  print("File già pronti, effettuo il summarize")
  tst <- dew4p2.l_fncSummaryByYearTypeSelectCountry(dew4p2.g_dfSummarySCC,dew4p2.l_baltimoraFIPS)
  print("Stampo il summarize ...")
  View(tst)
  tst2 <- dew4p2.l_fncAddModelCoeff(tst)
  return(tst2)
  
}


dew4p2.ut_l_fncSummaryByYearTypeSelectCountry <- function(){
  
  print("TEST dew4p2.ut_l_summaryByYearSelectCountry")
  if(length(dew4p2.g_dfSummarySCC) == 0 ){
    print("Devo ricaricare i file ...")
    dew4p2.g_fncInit()
    print("File ricaricati !")
  }
  print("File già pronti, effettuo il summarize")
  tst <- dew4p2.l_fncSummaryByYearTypeSelectCountry(dew4p2.g_dfSummarySCC,dew4p2.l_baltimoraFIPS)
  View(tst)
  
}
# LOCAL(START)  ----------------------------------------------------------------------------------
dew4p2.l_baltimoraFIPS <- "24510"
dew4p2.l_fileName <- "plot3.png"
dew4p2.l_ylab <- "TONS Of PM2.5"
dew4p2.l_mainTitle <- "Emission of PM2.5 for each type, over years in Baltimora City"
dew4p2.l_xlab <- "Year of Measurements"

dew4p2.l_fncSummaryByYearTypeSelectCountry <- function(df,country){
  
  return (df %>% 
            filter(fips==country) %>%
            group_by(year,type) %>% 
            summarize(totale = sum(Emissions)))
  
}

dew4p2.l_fncMain <- function(){
  dew4p2.g_fncInit()
  l_emissionByYearCountry <- dew4p2.l_fncSummaryByYearTypeSelectCountry(
    df = dew4p2.g_dfSummarySCC,
    country = dew4p2.l_baltimoraFIPS
  )
  dew4p2.l_fncPlot(l_emissionByYearCountry)
  dew4p2.l_fncSavePlot(dew4p2.l_fileName)
  
}

dew4p2.l_fncSavePlot <- function(fileName){
  
  #dev.off()
  dev.copy(png,fileName)
  dev.off()
  
}

dew4p2.l_fncAddModelCoeff <- function(dfEmissionByYear){
  
  dfCalcModel <- dfEmissionByYear %>% 
    group_by(type) %>% 
    do(model=lm(year ~ totale,data=.))
  
  vCoeff <- sapply(dfCalcModel$model,function(x) { x$coefficients[[2]] })
  dfNew <- data.frame(type=dfCalcModel$type,coeff=vCoeff)
  dfNew <- merge(dfEmissionByYear,dfNew)
  
  return(dfNew)
}
  

dew4p2.l_fncPlot <- function(dfEmissionByYear){

    
  dfWithCoeff <- dew4p2.l_fncAddModelCoeff(dfEmissionByYear)
  dfpositivi <- dfWithCoeff %>% 
    filter(coeff <= 0) %>%
    select(type,year,totale)
  dfnegativi <- dfWithCoeff %>%
    filter(coeff > 0) %>%
    select(type,year,totale)

  
  #dev.off()
  g <- qplot(
    x=as.character(year),
    y=totale,
    data=dfEmissionByYear,
    geom="bar",
    fill=I("gray70"),
    ylab=dew4p2.l_ylab,
    xlab=dew4p2.l_xlab, 
    facets=.~type,
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
        ) + labs(title = dew4p2.l_mainTitle)
  plot(g)
  
}

# MAIN(START)  ----------------------------------------------------------------------------------
dew4p2.l_fncMain()
