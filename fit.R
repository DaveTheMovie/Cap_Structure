
rm(list = ls())
source('tools.R')
# file.edit('tools.R')
# file.edit('clean.R')
library(readr)
library(dplyr)

d <- read_csv("capIQ.csv")

n = dim(d)[1]
d_iis = d[1:(n/2),]
d_oos = d[round(n/2+1):n,]


PlotFit(formulaStr = 'EL ~ Industry',d_iis = d_iis,d_oos = d_oos,targetName = 'EL')

PlotFit(formulaStr = 'EL ~ BL',d_iis = d_iis,d_oos = d_oos,targetName = 'EL')

PlotFit(formulaStr = 'EL ~ LM',d_iis = d_iis,d_oos = d_oos,targetName = 'EL')

PlotFit(formulaStr = 'EL ~ Vol',d_iis = d_iis,d_oos = d_oos,targetName = 'EL')

PlotFit(formulaStr = 'EL ~ BL*AR',d_iis = d_iis,d_oos = d_oos,targetName = 'EL')

PlotFit(formulaStr = 'EL ~ BL:AR + LM:AP - 1',d_iis = d_iis,d_oos = d_oos,targetName = 'EL')

PlotFit(formulaStr = 'EL ~ BL:AR + Vol:AP - 1',d_iis = d_iis,d_oos = d_oos,targetName = 'EL')

PlotFit(formulaStr = 'EL ~ BL + BL:AR + AP:Vol -1',d_iis = d_iis,d_oos = d_oos,targetName = 'EL')
