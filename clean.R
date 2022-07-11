
library(dplyr)
source('read.R')

d0_Old = d0
# d0 = d0_Old

d0 %>%
  filter(Exchange == "New York Stock Exchange (NYSE)") %>%
  filter(Rating != "-") %>%
  filter(Rating != "NR") ->
  d0

# Mean Over Time ####
idx_col_nonNumeric = 1:8

d0 %>%
  colnames() %>%
  stringr::str_replace(pattern = "__.*", replacement = '') %>%
  unique() ->
  cnames_all
  
cnames = cnames_all[-idx_col_nonNumeric] 
d1 = d0[,idx_col_nonNumeric]

for (cname in cnames){
  d0 %>%
  select(starts_with(cname)) %>%
    rowMeans(na.rm = T) ->
    newcol
  d1 = cbind(d1, newcol)
}
names(d1) = cnames_all

# Features ####

d_EV = select(d0,starts_with('EV'))
d_Debt = select(d0,starts_with('Debt__'))
d_nDebt = select(d0,starts_with('Debt_Net'))
d_Assets = select(d0,starts_with('Assets'))
d_EBITDA = select(d0,starts_with('EBITDA'))
d_Cash = select(d0,starts_with('Cash'))

d1$EBITDA_sd = apply(d_EBITDA,1,function(x) sd(x,na.rm=T))
d1$Vol = d1$EBITDA_sd / d1$EV

d_EL = d_Debt/d_EV
d_BL = d_Debt/d_Assets
d_LM = d_Debt/d_EBITDA
d_AP = d_EV/d_Assets
d_AR = 1/d_AP
d_RoD = d_EBITDA/d_Debt

d_nAssets = d_Assets - d_Cash
d_nEL = d_nDebt/d_EV
d_nBL = d_nDebt/d_nAssets
d_nLM = d_nDebt/d_EBITDA
d_nAP = d_EV/d_nAssets
d_nAR = 1/d_nAP
d_nRoD = d_EBITDA/d_nDebt

d1$EL = rowMeans(d_EL,na.rm = T)
d1$BL = rowMeans(d_BL,na.rm = T)
d1$LM = rowMeans(d_LM,na.rm = T)
d1$AP = rowMeans(d_AP,na.rm = T)
d1$AR = rowMeans(d_AR,na.rm = T)
d1$RD = rowMeans(d_RoD,na.rm = T)

d1$nEL = rowMeans(d_nEL,na.rm = T)
d1$nBL = rowMeans(d_nBL,na.rm = T)
d1$nLM = rowMeans(d_nLM,na.rm = T)
d1$nAP = rowMeans(d_nAP,na.rm = T)
d1$nAR = rowMeans(d_nAR,na.rm = T)
d1$nRD = rowMeans(d_nRoD,na.rm = T)

# Outliers ####
CleanNPlot = function(xcol, lo = -Inf, hi = Inf){
  par(mfrow = c(2, 1))
  hist(xcol,main ='',20)
  ind_outlier = xcol < lo | xcol > hi | xcol == Inf
  xcol[ind_outlier] = NA
  hist(xcol,main ='',20)
  par(mfrow = c(1, 1))
  print(sum(ind_outlier,na.rm = T))
  return(xcol)
}

# net calcs


tempview = CleanNPlot(d1$nEL, 0,1)
tempview = CleanNPlot(d1$nBL, 0,5)
tempview = CleanNPlot(d1$nLM,-10,10)
tempview = CleanNPlot(d1$nAR,-5,5)
tempview = CleanNPlot(d1$nAP,-5,20)
tempview = CleanNPlot(d1$nRD,-10,10)

tempview = CleanNPlot(d1$EL, 0,1)
tempview = CleanNPlot(d1$BL, 0,5)
tempview = CleanNPlot(d1$LM,-10,10)
tempview = CleanNPlot(d1$AR,-5,5)
tempview = CleanNPlot(d1$AP,-5,20)
tempview = CleanNPlot(d1$RD,-10,10)
tempview = CleanNPlot(d1$Vol,0,2)

d1$nEL = CleanNPlot(d1$nEL, 0,1)
d1$nBL = CleanNPlot(d1$nBL, 0,5)
d1$nLM = CleanNPlot(d1$nLM,-10,10)
d1$nAR = CleanNPlot(d1$nAR,-5,5)
d1$nAP = CleanNPlot(d1$nAP,-5,20)
d1$nRD = CleanNPlot(d1$nRD,-10,10)

d1$EL = CleanNPlot(d1$EL,-5,5)
d1$BL = CleanNPlot(d1$BL, 0,5)
d1$LM = CleanNPlot(d1$LM,-10,10)
d1$AR = CleanNPlot(d1$AR,-5,5)
d1$AP = CleanNPlot(d1$AP,-5,20)
d1$RD = CleanNPlot(d1$RD,-10,10)
d1$Vol = CleanNPlot(d1$Vol,0,2)

# Industry Names ####
d1$Industry = gsub(' \\(Primary)','',d1$Industry)

ind_name = d1$Industry == 'Real Estate Management and Development'
d1$Industry[ind_name] = 'Real Estate'

ind_name = d1$Industry == 'Communication Services'
d1$Industry[ind_name] = 'Communication'

ind_name = d1$Industry == 'Information Technology'
d1$Industry[ind_name] = 'IT'

ind_name = d1$Industry == 'Consumer Discretionary'
d1$Industry[ind_name] = 'Consumer'

# Complete Cases ####

VIM::aggr(d1)
VIM::aggr(d1,plot = F)

idx_complete = complete.cases(d1)
sum(idx_complete)

# Export ####
d2 = d1[idx_complete,]

d = select(d2, -one_of(c('Status','Exchange_2nd','Type')))

write.csv(d,'capIQ.csv',row.names = F)
