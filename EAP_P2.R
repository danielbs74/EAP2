pacman::p_load(vroom, tidyverse, lubridate, xts, sandwich, dplyr,
               pracma, stargazer)

rm(list = ls())

gc()
if(Sys.info()['login'] == 'batista1'){
  setwd('C:/Users/batista1/OneDrive/PhD/Empirical Asset Pricing/Project 2')
} else {
  setwd('C:/Users/user/Documents/OneDrive/PhD/Empirical Asset Pricing/Project 2/')
}

#######################Building variables#######################
crsp  = vroom('crsp.txt',
                  delim = '\t',
                  col_types = cols(.default = col_character()))

ccmfund = vroom('compustat.txt',
                    delim = '\t')

colnames(ccmfund) <- str_to_lower(colnames(ccmfund))

compustat <- ccmfund %>%
  transmute(
    gvkey = as.integer(gvkey),         # firm identifier
    revt = as.numeric(revt),           # Total Revenue
    cogs = as.numeric(cogs),           # Cost of goods sold
    permno = as.integer(lpermno),      # stock identifier
    date = ymd(datadate),          # date of report
    linktype = as.character(linktype), # link type
    linkenddt = ymd(linkenddt),        # date when link ends to be valid
    seq = as.numeric(seq),             # stockholders' equity
    ceq = as.numeric(ceq),             # total common/ordinary equity
    at = as.numeric(at),               # total assets
    lt = as.numeric(lt),               # total liabilities
    txditc = as.numeric(txditc),       # deferred taxes and investment tax credit
    txdb = as.numeric(txdb),           # deferred taxes
    itcb = as.numeric(itcb),           # investment tax credit
    pstkrv = as.numeric(pstkrv),       # preferred stock redemption value
    pstkl = as.numeric(pstkl),         # preferred stock liquidating value
    pstk = as.numeric(pstk),           # preferred stock par value
    sec = as.numeric(gsector)
  ) 

compustat <- compustat %>%
  mutate(year = year(date)) %>%
  group_by(permno, year) %>%
  filter(date == max(date)) %>%
  ungroup()

month(compustat$date) = 12

# Calculate book value of preferred stock and equity
compustat <- compustat %>%
  mutate(be = coalesce(seq, ceq + pstk, at - lt) + 
           coalesce(txditc, txdb + itcb, 0) - 
           coalesce(pstkrv, pstkl, pstk, 0),
         be = if_else(be <= 0, as.numeric(NA), be))

compustat <- compustat %>%
  group_by(gvkey) %>%
  mutate(gp = (revt - cogs)/at,
         gp = ifelse(is.infinite(gp), NA, gp)) %>%
  ungroup()


compustat <- compustat %>%
  group_by(gvkey) %>%
  mutate(ag = lag(at)/lag(at, 2) - 1,
         ag = ifelse(is.infinite(ag), NA, ag)) %>%
  ungroup()



colnames(crsp) <- str_to_lower(colnames(crsp))

# Parse only relevant variables
crsp <- crsp %>%
  transmute(permno = as.integer(permno),     # security identifier
            date = ymd(date),                # month identifier
            ret = as.numeric(ret) * 100,     # return (converted to percent)
            shrout = as.numeric(shrout),     # shares outstanding (in thousands)
            altprc = as.numeric(altprc),     # last traded price in a month
            exchcd = as.integer(exchcd),     # exchange code
            shrcd = as.integer(shrcd),       # share code
            siccd = as.integer(siccd),       # industry code
            dlret = as.numeric(dlret) * 100, # delisting return (converted to percent)
            dlstcd = as.integer(dlstcd)      # delisting code
  ) 


# Keep only US-based common stocks (10 and 11)
crsp <- crsp %>%
  filter(shrcd %in% c(10, 11)) %>%
  select(-shrcd)

# Keep only distinct observations to avoid multiple counting
crsp <- crsp %>%
  distinct(permno, date, .keep_all = TRUE) # remove duplicates 

# Compute market cap
# Note: altprc is the negative of average of bid and ask from last traded price
#       for which the data is available if there is no last traded price
crsp_size <- crsp  %>% 
  group_by(permno) %>%
  mutate(size = abs(lag(shrout)) * abs(lag(altprc)) / 1000, # in millions of dollars
         size = if_else(size == 0, as.numeric(NA), size)) %>%
  filter(month(date) == 7) %>%
  ungroup() %>%
  select(permno, date, size)

crsp <- crsp %>%
  left_join(crsp_size, by = c("permno", "date")) %>%
  mutate(size_bm = abs(shrout * altprc) / 1000, # in millions of dollars
         size_bm = if_else(size_bm == 0, as.numeric(NA), size_bm)) 

crsp <- crsp %>%
  mutate(exchange = case_when(exchcd %in% c(1, 31) ~ "NYSE",
                              exchcd %in% c(2, 32) ~ "AMEX",
                              exchcd %in% c(3, 33) ~ "NASDAQ",
                              TRUE ~ "Other"))

crsp = crsp %>% group_by(permno) %>% fill(size)

stocks <- crsp %>%
  select(permno, date, exchange, altprc, ret, size, size_bm) %>%
  na.omit

compustat <- compustat %>% select(permno, date, be, gp, ag, sec)

day(stocks$date) = 1
day(compustat$date) = 1
compustat$date = compustat$date %m+% months(6)

stocks <- stocks %>%
  left_join(compustat %>% na.omit, by = c("permno", "date"))


stocks <- stocks %>%
  group_by(permno) %>%
  mutate(rev = lag(ret),
         mom = 100*(abs(lag(altprc, 2))/abs(lag(altprc, 12)) - 1),
         bm = log(be/lag(size_bm, 6)),
         size = log(size)) %>%
  ungroup() %>% 
  select(-be, -size_bm)

stocks = stocks %>% group_by(permno) %>% fill(bm, gp, ag, sec)

stocks = stocks %>% na.omit

breaks = stocks %>% 
  filter(month(date) == 7 & exchange == 'NYSE') %>%
  group_by(date) %>%
  mutate(q20 = quantile(size, 0.2),
         q50 = quantile(size, 0.5)) %>%
  select(date, q20, q50) %>% distinct

stocks <- stocks %>%
  left_join(breaks, by = c("date"))

stocks = stocks %>% group_by(permno) %>% fill(q20, q50)
stocks = stocks %>% group_by(date) %>% fill(q20, q50, .direction = 'downup')

stocks = stocks %>% 
  mutate(Size_sort = case_when(size < q20 ~ 'Micro',
                               size > q50 ~ 'Large',
                               TRUE ~ 'Small'))

stocks = stocks[stocks$date < '2020-12-31' & stocks$date > '1962-12-31',]

table(stocks$Size_sort)/length(unique(stocks$date))

sum(table(stocks$Size_sort)/length(unique(stocks$date)))


######################Excess returns - Annually##################
rf = readRDS('rf.rds')*100

###All stocks
betas = c()

dates = stocks$date %>% unique %>% sort %>% as.character

for(i in dates){
  aux = stocks[stocks$date == i,]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betas = rbind(betas, c(mod$coefficients))
}


tstats_betas = sqrt(nrow(betas))*colMeans(betas)/apply(betas, 2, sd)
tstats_betas

###Micro, small, large caps
betas_small = c()
betas_micro = c()
betas_large = c()

for(i in dates){
  aux = stocks[stocks$date == i & stocks$Size_sort == 'Micro',]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betas_micro = rbind(betas_micro, c(mod$coefficients))
  
  aux = stocks[stocks$date == i & stocks$Size_sort == 'Small',]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betas_small = rbind(betas_small, c(mod$coefficients))
  
  aux = stocks[stocks$date == i & stocks$Size_sort == 'Large',]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betas_large = rbind(betas_large, c(mod$coefficients))
}



tstats_betas_micro = sqrt(nrow(betas_micro))*colMeans(betas_micro)/apply(betas_micro, 2, sd)
tstats_betas_micro

tstats_betas_small = sqrt(nrow(betas_small))*colMeans(betas_small)/apply(betas_small, 2, sd)
tstats_betas_small

tstats_betas_large = sqrt(nrow(betas_large))*colMeans(betas_large)/apply(betas_large, 2, sd)
tstats_betas_large


###All stocks without Financials and Utilities

betasFinUtil = c()

dates = stocks$date %>% unique %>% sort %>% as.character

for(i in dates){
  aux = stocks[stocks$date == i & !(stocks$sec %in% c(40, 55)),]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betasFinUtil = rbind(betasFinUtil, c(mod$coefficients))
}


tstats_betasFinUtil = sqrt(nrow(betasFinUtil))*colMeans(betasFinUtil)/apply(betasFinUtil, 2, sd)
tstats_betasFinUtil


res = rbind(colMeans(betas) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betas %>% round(digits = 2), ')')),
            colMeans(betas_micro) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betas_micro %>% round(digits = 2), ')')),
            colMeans(betas_small) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betas_small %>% round(digits = 2), ')')),
            colMeans(betas_large) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betas_large %>% round(digits = 2), ')')),
            colMeans(betasFinUtil) %>% round(digits = 3),
            paste0('(', paste0(tstats_betasFinUtil %>% round(digits = 2), ')')))

stargazer(res)

###Testing interactions###

betasI1 = c()

dates = stocks$date %>% unique %>% sort %>% as.character

for(i in dates){
  aux = stocks[stocks$date == i,]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag + aux$mom*aux$size)
  betasI1 = rbind(betasI1, c(mod$coefficients))
}


tstats_betasI1 = sqrt(nrow(betasI1))*colMeans(betasI1)/apply(betasI1, 2, sd)
tstats_betasI1 #Works. MOM becomes 0 and MOM*Size is significant pos.

betasI2 = c()

dates = stocks$date %>% unique %>% sort %>% as.character

for(i in dates){
  aux = stocks[stocks$date == i,]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag + aux$gp*aux$size)
  betasI2 = rbind(betasI2, c(mod$coefficients))
}


tstats_betasI2 = sqrt(nrow(betasI2))*colMeans(betasI2)/apply(betasI2, 2, sd)
tstats_betasI2
tstats_betas


betasI3 = c()

dates = stocks$date %>% unique %>% sort %>% as.character

for(i in dates){
  aux = stocks[stocks$date == i,]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag + aux$mom*aux$ag)
  betasI3 = rbind(betasI3, c(mod$coefficients))
}


tstats_betasI3 = sqrt(nrow(betasI3))*colMeans(betasI3)/apply(betasI3, 2, sd)
tstats_betasI3
tstats_betas


res = rbind(colMeans(betasI1) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasI1 %>% round(digits = 2), ')')),
            colMeans(betasI2) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasI2 %>% round(digits = 2), ')')),
            colMeans(betasI3) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasI3 %>% round(digits = 2), ')')))
stargazer(res)


######################Factor adjusted returns (FF5) - Annually##################

###Running FF5 and getting factor adjusted returns
if(!file.exists('stocksFA.rds')){
  ff5 = vroom('ff5.csv')
  ff5 = xts(ff5[, 2:ncol(ff5)], as.Date(paste0(ff5$Date, '01'), "%Y%m%d"))
  ff5 = ff5[, 1:5]
  stocksFA = stocks[stocks$date >= '1963-07-01',]
  
  for(i in unique(stocks$permno)){
    aux = stocksFA[stocksFA$permno == i, c('date', 'ret')]
    aux = xts(aux$ret, aux$date) - rf
    mod = lm(aux ~ ff5[index(aux)])
    stocksFA[stocksFA$permno == i, 'ret'] = mod$coef[1] + mod$res %>% coredata
    (which(unique(stocks$permno) == i)/length(unique(stocks$permno))) %>% round(digits = 4) %>% print
  }
  
  saveRDS(stocksFA, 'stocksFA.rds')
  
} else { 
  stocksFA = readRDS('stocksFA.rds')
}


betasFA = c()

dates = stocksFA$date %>% unique %>% sort %>% as.character


for(i in dates){
  aux = stocksFA[stocksFA$date == i,]
  aux$ret = aux$ret
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betasFA = rbind(betasFA, c(mod$coefficients))
}


tstats_betasFA = sqrt(nrow(betasFA))*colMeans(betasFA)/apply(betasFA, 2, sd)
tstats_betasFA

###Micro, small, large caps

betasFA_small = c()
betasFA_micro = c()
betasFA_large = c()

for(i in dates){
  aux = stocksFA[stocksFA$date == i & stocksFA$Size_sort == 'Micro',]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betasFA_micro = rbind(betasFA_micro, c(mod$coefficients))
  
  aux = stocksFA[stocksFA$date == i & stocksFA$Size_sort == 'Small',]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betasFA_small = rbind(betasFA_small, c(mod$coefficients))
  
  aux = stocksFA[stocksFA$date == i & stocksFA$Size_sort == 'Large',]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betasFA_large = rbind(betasFA_large, c(mod$coefficients))
}


tstats_betasFA_micro = sqrt(nrow(betasFA_micro))*colMeans(betasFA_micro)/apply(betasFA_micro, 2, sd)
tstats_betasFA_micro

tstats_betasFA_small = sqrt(nrow(betasFA_small))*colMeans(betasFA_small)/apply(betasFA_small, 2, sd)
tstats_betasFA_small

tstats_betasFA_large = sqrt(nrow(betasFA_large))*colMeans(betasFA_large)/apply(betasFA_large, 2, sd)
tstats_betasFA_large



###All stocks without Financials and Utilities

betasFinUtilFA = c()

dates = stocksFA$date %>% unique %>% sort %>% as.character

for(i in dates){
  aux = stocksFA[stocksFA$date == i & !(stocksFA$sec %in% c(40, 55)),]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag)
  betasFinUtilFA = rbind(betasFinUtilFA, c(mod$coefficients))
}


tstats_betasFinUtilFA = sqrt(nrow(betasFinUtilFA))*colMeans(betasFinUtilFA)/apply(betasFinUtilFA, 2, sd)
tstats_betasFinUtilFA


res = rbind(colMeans(betasFA) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasFA %>% round(digits = 2), ')')),
            colMeans(betasFA_micro) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasFA_micro %>% round(digits = 2), ')')),
            colMeans(betasFA_small) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasFA_small %>% round(digits = 2), ')')),
            colMeans(betasFA_large) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasFA_large %>% round(digits = 2), ')')),
            colMeans(betasFinUtilFA) %>% round(digits = 3),
            paste0('(', paste0(tstats_betasFinUtilFA %>% round(digits = 2), ')')))

stargazer(res)

###Testing interactions###

betasFAI1 = c()

dates = stocksFA$date %>% unique %>% sort %>% as.character

for(i in dates){
  aux = stocksFA[stocksFA$date == i,]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag + aux$mom*aux$size)
  betasFAI1 = rbind(betasFAI1, c(mod$coefficients))
}


tstats_betasFAI1 = sqrt(nrow(betasFAI1))*colMeans(betasFAI1)/apply(betasFAI1, 2, sd)
tstats_betasFAI1 #Works. MOM becomes 0 and MOM*Size is significant pos.

betasFAI2 = c()

dates = stocksFA$date %>% unique %>% sort %>% as.character

for(i in dates){
  aux = stocksFA[stocksFA$date == i,]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag + aux$gp*aux$size)
  betasFAI2 = rbind(betasFAI2, c(mod$coefficients))
}


tstats_betasFAI2 = sqrt(nrow(betasFAI2))*colMeans(betasFAI2)/apply(betasFAI2, 2, sd)
tstats_betasFAI2
tstats_betasFA


betasFAI3 = c()

dates = stocksFA$date %>% unique %>% sort %>% as.character

for(i in dates){
  aux = stocksFA[stocksFA$date == i,]
  aux$ret = aux$ret - rf[i][[1]]
  mod = lm(aux$ret ~ aux$rev + aux$mom + aux$size + aux$bm + aux$gp + aux$ag + aux$mom*aux$ag)
  betasFAI3 = rbind(betasFAI3, c(mod$coefficients))
}


tstats_betasFAI3 = sqrt(nrow(betasFAI3))*colMeans(betasFAI3)/apply(betasFAI3, 2, sd)
tstats_betasFAI3
tstats_betasFA


res = rbind(colMeans(betasFAI1) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasFAI1 %>% round(digits = 2), ')')),
            colMeans(betasFAI2) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasFAI2 %>% round(digits = 2), ')')),
            colMeans(betasFAI3) %>% round(digits = 3), 
            paste0('(', paste0(tstats_betasFAI3 %>% round(digits = 2), ')')))
stargazer(res)
