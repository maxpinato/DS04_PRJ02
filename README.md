# Data Exploration - Week 4 - Project 2
Primo step, leggere i dati.
Il codice deve essere autoconsistente. In ogni file R riporto una sezione con le funzioni da riutilizzare. Il file R avrà le seguenti sezioni.

\# GLOBAL (START) 
\# here your global code 
\# GLOBAL (END) 

Aggiungo anche un area "UNIT-TEST" in cui inserisco tutte le funzioni di unit-test.

## Naming Convention
variable name: [namespace].[scope]_[type][VariableName]
namespace: dew4p2
scope: g (global), l (local), ut (unit-test), 
type:
- df: dataframe
- str: string
- vec: vector
- fnc: function

Example: 
- dew4p2.g_dfSourceClassCode
- dew4p2.g_dfSummarySCC

## Variabili Globali
- dew4p2.g\_dfSourceClassCode: DataFrame del file "Source_Classification_Code.rds"
- dew4p2.g\_dfSummarySCC: Dataframe del file "summarySCC_PM25.rds"


## Funzioni Globali
- dew4p2.g_fncInit: inizializzazione delle variabili globali

## Funzioni Unit Test

## Funzioni Main
- dew4p2.l_fncMain: funzione che realizza lo scopo del modulo

