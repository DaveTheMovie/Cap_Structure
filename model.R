
library(readr)
d <- read_csv("capIQ.csv")


d %>%
  transmute(EL = Debt/EV,
            BL = Debt/Assets,
            LM = Debt/EBITDA,
            AR = Assets / EV,
            AP = 1 - AR,
            Industry,
            Rating) -> dmini 

n = dim(dmini)[1]
d_iis = dmini[1:(n/2),]
d_oos = dmini[round(n/2+1):n,]

