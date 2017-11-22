library(quantmod)
library(Quandl)
library(PerformanceAnalytics)
library(fPortfolio)
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")
Sys.setenv(TZ = 'UTC')

#### Close data from TC2000 Convert TXT to XTS ####
TRtxt <- read.table('tdetfc', header = FALSE, sep = ",")

TRzoo <- read.zoo(TRtxt, sep = ",", format = "%m/%d/%Y", split = 3)
TRxts <- as.xts(TRzoo)
TRxts <- TRxts['2014-11-07:/']

ETFSector <- read.csv("TDETFNames.csv", header = FALSE)
SectName <- as.data.frame(ETFSector[,2:5])
rownames(SectName) <- ETFSector[,1]
colnames(SectName) <- c("Asset Class","Category","Sub-Cat", "Factor")

#### Volatility and Momentum Measure Component
stkreg <- na.locf(TRxts, na.rm = FALSE, fromLast = TRUE)
stkreg.d <- cbind(TRxts, "Days" = 1:nrow(TRxts))

reg.r2 <- do.call(merge, lapply(stkreg.d, function(x) na.omit(rollSFM(log(x),
                                                                      stkreg.d$Days, 90)$r.squared)))
r.sqrd <- (reg.r2[,1:ncol(stkreg.d)-1])
names(r.sqrd) <- colnames(TRxts)

slope.b <- do.call(merge, lapply(stkreg.d, function(x) na.omit(rollSFM(log(x), stkreg.d$Days,
                                                                       90)$beta)))
Be.ta <- slope.b[,1:ncol(stkreg.d)-1]
names(Be.ta) <- colnames(TRxts)

Ann.sl <- ((exp(Be.ta)^250)-1)*100
names(Ann.sl) <- colnames(TRxts)
Adj.sl <- round((r.sqrd * Ann.sl),4)
names(Adj.sl) <- colnames(TRxts)

### RANK Adjusted Slope (the -Adj.sl is so the asset with the greatest value will receive the rank of 1###
AdSl.Rk <- as.xts(t(apply(-Adj.sl,1,rank)))

#### Convert Components to Monthly Data ####
Adjsl.mo <- Adj.sl[ endpoints(Adj.sl, on="months", k=1), ] #convert to monthly

ExpReg.rk <- as.xts(t(apply(-Adjsl.mo,1, rank)))

#### Signals for the Model ####
CurrSignal <- rbind(round(last(TRxts),2),last(ExpReg.rk))
Signal <- as.data.frame(cbind(SectName,t(CurrSignal)))
names(Signal) <- c( "Asset Class","Asset Cat", "Sub-Cat","Factor", "Close","Rank")

write.csv(as.data.frame(ExpReg.rk), file = "ExpRegRank.csv", row.names = TRUE)
write.csv(Signal, file = "ExpRegSignal.csv", row.names = TRUE)


