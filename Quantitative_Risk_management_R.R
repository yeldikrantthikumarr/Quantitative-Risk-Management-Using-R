install.packages("qrmdata")
install.packages("qrmtools")
install.packages("xts")
library(qrmdata)
library(qrmtools)
library(xts)
# Risk measure is utmost important, prior to this, we need to understand how to manage risk,
# Risk management can be done by a) Selling assets, b) Diversifying portfolio c) Hedging with derivatives d) being cash rich

# one of the popular measure of risk, is VAR.
data("DJ_const")
DJ_const
names(DJ_const)
head(DJ_const)

stocks <- DJ_const["2008/2009",c(1,10)]
plot.zoo(stocks)


#library(xts)
#Lets explore exisiting datasets from QRMDATA
data("SP500")
#Loading existing dataset of R, S&P500 index and save it variable 'sp500' for better naming convention.
head(SP500, n=3)
tail(SP500, n=3)


plot(SP500)
data("FTSE")
plot(FTSE)
#From FTSE data select particular data range from 2000-04-01 to 2006-06-30

ftse00 <- FTSE["2004-04-01/2006-06-30"]
#similarly, on SP500's GSPC .....
gspc <- SP500
#data from dates gspc_1950-01-01_to_1980-04-04 
gspc_data <- gspc["2008/2015"]
plot(gspc_data)

plot(ftse00)

data("DJ")
plot(DJ)
head(DJ, n=3)
tail(DJ, n=3)

dj0809 <- DJ["2008/2009"]
plot(dj0809)

#Exploring risk-factor time series: individual equities
data("DJ_const")
names(DJ_const)
head("DJ_const")
str(DJ_const)
stocks <- DJ_const["2008/2009", c(1,10)]
plot.zoo(stocks) # Two companies are plotted sepearately and neatly.

#Exploring risk-factor data: exchange rates
#data(gb)
data("GBP_USD")
data("EUR_USD")
names(GBP_USD)
plot(GBP_USD)
plot(EUR_USD)

####################
# Load exchange rate data
data("GBP_USD")
data("EUR_USD")

# Plot the two exchange rates
plot(GBP_USD)
plot(EUR_USD)

#Plotting Inverse i.e y-x instead of x-y # Plot a USD_GBP exchange rate
plot(1/ GBP_USD)

# Merge the two exchange rates GBP_USD and EUR_USD
merge1 <- merge(GBP_USD,EUR_USD,all = TRUE)
plot.zoo(merge1)

fx <- merge(GBP_USD,EUR_USD,all = TRUE)
fx

# Extract 2010-15 data from fx and assign to fx0015
fx0015 <- fx["2010/2015",]
plot(fx0015)
#or plot.zoo(fx0015)

# NOTE:  Changes in risk factors are risk-factor returns or returns
#Simple returns : date wise -->> Todays date - yesterday date
#Relative returns: date wuse ---> Todays date - yesterday date/ yesterdays date
#log returns : date wise --> log(Todays date) - log(yesterdays date)

#Log returns are very close to log returns

#  Xt stores return values
#  Zt denote a time series of risk factor values
#  Xt = Zt - Zt-1 Simple return

# Log returns are approximately equals to relative returns

# model used for pricing derivatives
#Geometric Brownian Motion (GBM)
#if pricer series follows GBM then log returns will be normally distributed.

data("SP500")
sp500x <- diff(log(SP500))
head(sp500x)
# remove fist row with #NA value
# NOte: if price distribution follows GBM then log returns will be normally distributed
#In practise, log returns are calculated on normally distributed, at least short time horizons
sp500x <- diff(log(SP500))[-1]
head(sp500x)
plot(sp500x)
sp500_2 <- diff(log(SP500))[-1]
head(sp500_2)
plot(sp500_2)
plot(SP500)
#EXploing returning Series:
#Fluctuations in price are known as "returns".
data("FTSE")
plot(FTSE)
ftse_x <- diff(log(FTSE))
head(ftse_x)
#remove fist row with NA value
ftse_x <- diff(log(FTSE))[-1]
head(ftse_x)
plot(ftse_x)
# The return series often just look like noise with some periods of larger fluctuations.
#plot.zoo(ftse_x,sp500_2)
#Different ways of plotting risk-factor and return series

#plot(ftse_x,plot.type="single") +legend(julian(x = as.Date("2009-01-01")), y = 70, legend = names(ftse_x)[1:4], fill = 1:4)
# Plot djstocks in four separate plots

plot.zoo(djstocks)

# Plot djstocks in one plot and add legend
plot.zoo(djstocks,plot.type="single",col=1:4)
legend(julian(x = as.Date("2009-01-01")), y = 70, legend = names(DJ_const)[1:4], fill = 1:4)

# Compute log-returns and assign to djstocks_x
djstocks_x <- log(DJ_const)

# Plot djstocks_x in four separate plots
plot.zoo(djstocks_x)

# Plot djstocks_x with vertical bars
plot.zoo(djstocks_x,type="h")

# Plot djstocks in four separate plots
plot.zoo(djstocks)

# Plot djstocks in one plot and add legend
plot.zoo(djstocks,plot.type="single",col=1:4)
legend(julian(x = as.Date("2009-01-01")), y = 70, legend = names(DJ_const)[1:4], fill = 1:4)

# Compute log-returns and assign to djstocks_x
djstocks_x <- diff(log(djstocks))

# Plot djstocks_x in four separate plots
plot.zoo(djstocks_x)

# Plot djstocks_x with vertical bars
plot.zoo(djstocks_x,type="h")

#Nice plots! Note how in late 2008 there were large returns for all series. That was the height of the financial crisis.
plot.zoo(dj0809)
hdfc <- read.csv("C:\\Users\\DELL-pc\\Documents\\stockdata1.csv")
head(hdfc)
sa <- hdfc[,8]
sa[1] <- as.POSIXct.date(sa[1],"%Y-%m-%d")
sa1 <- hdfc["2017-0"]
#sa1 <- hdfc(,)[-1]
head(sa1)

#Aggregating log returns
# aggregate Daily log
# Xt is log returns, Zt is risk factor values            
# Log returns for a trading week, sum of log returns for each trading day

# log(Zt+5) - log(Zt) == Xt+i
# Aggregating log return series
djreturns <- diff(log(DJ_const["2000/2015"]))

djx <- diff(log(DJ))
# Plot djx
plot(djx)

# Plot weekly log-returns of djx
plot(apply.weekly(djx, sum), type = "h")

# Plot monthly log-returns of djx
plot(apply.monthly(djx, sum), type = "h")

# Plot djreturns
plot.zoo(djreturns)

# Plot monthly log-returns of djreturns
plot.zoo(apply.monthly(djreturns, colSums), type = "h")

#A test on Aggregation of log returns
# object sp consists of daily log returns for the S&P 500 index for the period 1960-2015
sp <- apply.daily(diff(log(SP500)),sum)
sp_qtly <- apply.quarterly(diff(log(SP500["1990/2010",][-1])),sum)
sp_q <- sp_qtly[,-1]
mean(sp_qtly)

mean(apply.quarterly(sp["1990/2010",],sum))

#Aggregating daily log returns to obtain weekly, monthly, quarterly, yearly log returns
#Commodities data
as.zoo(GBP_USD)
pairs(GBP_USD)
plot(EUR_USD,type = "h")

# Plot gold and oil prices
plot(gold)
plot(oil)
# Calculate daily log-returns
goldx <- diff(log(gold))
oilx <- diff(log(oil))
# Calculate monthly log-returns
goldx_m <- apply.monthly(goldx,sum)
oilx_m <- apply.monthly(oilx,sum)

# Merge goldx_m and oilx_m into coms
coms <- merge(goldx_m,oilx_m)
# Plot coms with vertical bars
plot.zoo(coms,type = "h")

# Make a pairwise scatterplot of coms
pairs(as.zoo(coms))

# Plot gold and oil prices
plot(gold)
plot(oil)
# Calculate daily log-returns
goldx <- diff(log(gold))
oilx <- diff(log(oil))
# Calculate monthly log-returns
goldx_m <- apply.monthly(goldx,sum)
oilx_m <- apply.monthly(oilx,sum)

# Merge goldx_m and oilx_m into coms
coms <- merge(goldx_m,oilx_m)
# Plot coms with vertical bars
plot.zoo(coms,type = "h")

# Make a pairwise scatterplot of coms
pairs(as.zoo(coms))
# Identify and create vector containing column names for 1, 5, 10 year yields
names(zcb)
yield_cols = c("1.00y", "5.00y", "10.00y")

# Compute log-returns as zcb_x and plot them for same maturities
zcb_x <- diff(log(zcb))
plot.zoo(zcb_x[, yield_cols])

# Compute simple returns as zcb_x2 and plot them for same maturities
zcb_x2 <- diff(zcb)
plot.zoo(zcb_x2[, yield_cols])

# Make a vector containing the maturities                             
maturity <- (1:120)/4

# Plot the yield curve for the first day of zcb
plot(maturity, zcb[1, ], ylim = range(zcb), type = "l", ylab = "yield (%)", col = "red")

# Add a line for the last day of zcb
lines(maturity, zcb[nrow(zcb), ])
























