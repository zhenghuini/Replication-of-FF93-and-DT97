# AP homework 4
# July 2019
# Zed Zhenghui Ni
# Data: CRSP/COMPUSTAT
#
# replication of FF 1993 and DT 1997
#
##################################

library(rio)
library(foreign)
library(tidyverse)
library(zoo)
library(lubridate)
library(fst)
##### import data
##
##
#Directory
setwd("/Users/nizhenghui1/Desktop/Teaching Assistant/Investment Strategy/Data")

###
#    step 1 import data and merge data
#    
#### import CRSP/COMPUSTAT

Compustat_19602018 <-  read_csv(gzfile("COMPUSTAT_1960_2018.csv.gz"))
#Crsp_19942018 <- read_csv(gzfile("CRSP_1994_2018.csv.gz"))
Crsp_19602018 <- read_csv(gzfile("CRSP_1960_2018.csv.gz"))
FF_5factor <- read_csv("F-F_Research_Data_5_Factors_2x3.csv")

## only nyse/nasdaq/amex only common stocks
## set delisting returns
Crsp_19602018 <- Crsp_19602018 %>% filter(SHRCD %in% c(10,11) & EXCHCD %in% c(1,2,3) ) %>%
  mutate(RET =as.numeric(RET)) %>% 
  group_by(PERMNO) %>%
  mutate(ret=if_else(date != max(date), RET,
                     ifelse(date == max(date) & !is.na(DLRET), DLRET,
                            if_else(date == max(date) & is.na(DLRET) & DLSTCD %in% c(500,520,551:573,574,580,584), -0.3, -1 ))))  %>%     
  filter(!is.na(ret)) %>% 
  mutate(date = ymd(date),
         month = month(date),
         year = year(date)) %>% 
  ungroup() 

summary(Crsp_19602018)


###
#    step 2 calculate B/M Ratio
#    
#### 

## calculate BE

summary(Compustat_19602018)

Compustat_19602018 <- Compustat_19602018 %>% filter( !is.na(fyear) & !is.na(seq) & !is.na(txdb)  ) %>% 
  mutate(itcb = if_else(is.na(itcb), 0, itcb),
         bvps = if_else(!is.na(pstkrv), pstkrv, 
                        if_else(!is.na(pstkl), pstkl,
                                if_else(!is.na(pstk), pstk, 0))),
         be = seq + txdb + itcb - bvps) %>% 
  filter(be >= 0) %>% distinct()

## calculate ME

summary(Crsp_19602018)

Crsp_me <- Crsp_19602018  %>% 
  filter(month == 12) %>% 
  mutate(me = abs(ALTPRC*SHROUT)/1000)
summary(Crsp_me)

## merge to get bm ratio

BM_ratio_annual <- Compustat_19602018 %>% left_join(Crsp_me, by = c("LPERMNO" = "PERMNO", "fyear" = "year")) %>% 
  mutate(bm_ratio = be/me,
         bm_ratio_ln = log(bm_ratio)) %>% filter(bm_ratio>0) %>% 
  select(LPERMNO, fyear, bm_ratio, bm_ratio_ln, be, me) %>%   
  group_by(LPERMNO, fyear) %>% 
  mutate( bm_ratio_ln = mean(bm_ratio_ln, na.rm = TRUE),
          bm_ratio = mean(bm_ratio, na.rm = TRUE),
          be = mean(be, na.rm = TRUE),
          me = mean(me, na.rm = TRUE)) %>% 
  distinct()

summary(BM_ratio_annual)

test <- BM_ratio_annual %>% select(LPERMNO ,fyear) %>% mutate(test =1) %>%  
  distinct()

###
#    step 3 calculate size/beta and merge to ff factors
#    
#### 

Return_19602018 <- Crsp_19602018 %>% 
  distinct() %>% 
  left_join(BM_ratio_annual, by = c("PERMNO" = "LPERMNO", "year" = "fyear")) %>% 
  mutate(size = log(abs(ALTPRC*SHROUT)/1000) ) %>% 
  select(PERMNO, year, month, ret, size, date, bm_ratio_ln, EXCHCD) %>% 
  arrange(PERMNO, year, month) %>% 
  group_by(PERMNO) %>%  
  mutate(lead_ret = dplyr::lead(ret)) %>% 
  ungroup() %>% distinct()

## winsorize
Return_19602018$bm_ratio_ln <- pmax(pmin(Return_19602018$bm_ratio_ln, 
                                         quantile(Return_19602018$bm_ratio_ln , .995,na.rm = TRUE)), quantile(Return_19602018$bm_ratio_ln , .005,na.rm = TRUE)) 
summary(Return_19602018)

Return_19602018 <- FF_5factor %>% mutate(year = date%/%100,
                                                 month = date%%100,
                                                 yearmonth = year + (month -1)/12) %>% 
  right_join(Return_19602018, by = c("year", "month"))

summary(FF_5factor)

Return_19602018 <- Return_19602018 %>% 
  arrange(PERMNO, yearmonth) %>% 
  group_by(PERMNO) %>%  
  mutate(lead_ret = dplyr::lead(ret,17),
         lag_size = dplyr::lag(size,1),
         weight = dplyr::lag(size,1),
         lag_bm_ratio = dplyr::lag(bm_ratio_ln,17)) %>% 
  ungroup()

length(unique(Return_19602018$PERMNO))

write_fst(Return_19602018, "Return_19602018.fst")

summary(Return_19602018)

###
#    step 4 Fame and French 1993 replication
#    
#### 


Form_CharSizePorts2 <- function(df) { # streamlined version
  # forms (size x specificed-characteristc) and forms the 25 portfolios
  # variable broken by 20/40/60/80 percentiles, (breakpoints uses NYSE data only)
  # requires Date and exchcd
  # outputs portfolio returns for each period,
  
  main.cln <- Return_19602018 %>%
    filter(!is.na(weight)&!is.na(lag_size)&!is.na(ret)&!is.na(lag_bm_ratio)&!is.infinite(lag_size)&!is.infinite(weight)) %>% 
    mutate_at(vars(c(ret, lag_size, lag_bm_ratio, weight)), 
              funs(DescTools::Winsorize(x=., probs = c(0.01, 0.99), na.rm = T))) %>% 
    select(date.x, PERMNO, EXCHCD, lag_size, lag_bm_ratio, weight, ret, RF)
  
  Bkpts.NYSE <- main.cln %>% # create size and var breakpoints based on NYSE stocks only
    filter(EXCHCD == 1) %>% # NYSE exchange
    group_by(date.x) %>%
    dplyr::summarize(var.P20 = quantile(lag_bm_ratio, probs=.2, na.rm=TRUE),
              var.P40 = quantile(lag_bm_ratio, probs=.4, na.rm=TRUE),
              size.P20 = quantile(lag_size, probs=.2, na.rm=TRUE),
              var.P60 = quantile(lag_bm_ratio, probs=.6, na.rm=TRUE),
              var.P80 = quantile(lag_bm_ratio, probs=.8, na.rm=TRUE),
              size.P40 = quantile(lag_size, probs=.4, na.rm=TRUE),
              size.P60 = quantile(lag_size, probs=.6, na.rm=TRUE),
              size.P80 = quantile(lag_size, probs=.8, na.rm=TRUE)) %>% 
    ungroup() %>% filter( date.x%%10==6 ) 
  
  # calculate size and var portfolio returns
  main.rank <- main.cln %>%
    mutate(date.y = if_else(date.x%%100 <= 5, date.x - 100 - date.x%%100 + 6,  date.x - date.x%%100 + 6)) %>% 
    left_join(Bkpts.NYSE, by=c("date.y"="date.x"), all.x=TRUE) %>%
    mutate(Size = if_else(lag_size<size.P20, "1", 
                          if_else(lag_size<size.P40, "2",
                                  if_else(lag_size<size.P60, "3",
                                          if_else(lag_size<size.P80, "4", "5")  )  )  ),
           Var = if_else(lag_bm_ratio<var.P20, "1", 
                          if_else(lag_bm_ratio<var.P40, "2",
                                  if_else(lag_bm_ratio<var.P60, "3",
                                          if_else(lag_bm_ratio<var.P80, "4", "5")  )  )  )) %>% 
    dplyr::filter(!is.na(Size)&!is.na(Var))
  
  Ret <- main.rank %>% # name 2 x 3 size-var portfolios
    group_by(date.x, Size, Var) %>%
    dplyr::summarize(ret.port = weighted.mean(ret*100 - RF, weight, na.rm=TRUE)) %>% # calc value-weighted returns
    ungroup()
  
  return(Ret)
}

FF1993_portfolio <- Form_CharSizePorts2(Return_19602018) %>% 
  filter(!is.na(date.x))

summary(FF1993_portfolio)

# merge to ff 5 factors
FF1993_portfolio <- FF_5factor %>% mutate(year = date%/%100,
                                         month = date%%100,
                                         yearmonth = year + (month -1)/12) %>% 
  right_join(FF1993_portfolio, by = c("date"="date.x")) 

library(stringr)
#pattern <- "[.]"
# select time period
FF_portfolio_1992 <- FF1993_portfolio %>% 
  filter(year>1962&year<1992) %>% 
  rename(rank_size = Size,
         rank_bm_ratio_ln = Var)
  
# valid_column_names <- make.names(names=names(FF_portfolio_1992), unique=TRUE, allow_ = TRUE)
# names(FF_portfolio_1992) <- valid_column_names

summary(FF_portfolio_1992)
library(dplyr)
library(broom)
Form_FF_rable <- function(df) {

FF1993_portfolio_1 <- df %>% 
  nest(-rank_size,-rank_bm_ratio_ln) %>% 
  mutate(fit = map(data, ~ lm( ret.port ~ `Mkt-RF` , data =.)),
         results1 = map(fit, tidy)) %>% 
  unnest(results1) %>% 
  filter_all( all_vars(!is.na(.)) ) %>% 
  filter(term == "(Intercept)") %>% 
  select(rank_size, rank_bm_ratio_ln, estimate) %>% 
  spread(key=rank_bm_ratio_ln, value=estimate) 

FF1993_portfolio_2 <- df %>% 
    nest(-rank_size,-rank_bm_ratio_ln) %>% 
    mutate(fit = map(data, ~ lm( ret.port ~ `Mkt-RF` , data =.)),
           results1 = map(fit, tidy)) %>% 
    unnest(results1) %>% 
    filter_all( all_vars(!is.na(.)) ) %>% 
    filter(term == "(Intercept)") %>% 
    select(rank_size, rank_bm_ratio_ln, statistic) %>% 
    spread(key=rank_bm_ratio_ln, value=statistic) 

FF1993_portfolio_3 <- df %>% 
  nest(-rank_size,-rank_bm_ratio_ln) %>% 
  mutate(fit = map(data, ~ lm( ret.port ~ `Mkt-RF` + SMB + HML , data =.)),
         results1 = map(fit, tidy)) %>% 
  unnest(results1) %>% 
  filter_all( all_vars(!is.na(.)) ) %>% 
  filter(term == "(Intercept)") %>% 
  select(rank_size, rank_bm_ratio_ln, estimate) %>% 
  spread(key=rank_bm_ratio_ln, value=estimate) #%>% 
#rename(alpha=`(Intercept)`)

FF1993_portfolio_4 <- df %>% 
  nest(-rank_size,-rank_bm_ratio_ln) %>% 
  mutate(fit = map(data, ~ lm( ret.port ~ `Mkt-RF` + SMB + HML  , data =.)),
         results1 = map(fit, tidy)) %>% 
  unnest(results1) %>% 
  filter_all( all_vars(!is.na(.)) ) %>% 
  filter(term == "(Intercept)") %>% 
  select(rank_size, rank_bm_ratio_ln, statistic) %>% 
  spread(key=rank_bm_ratio_ln, value=statistic) 

FF1993_portfolio_5 <- rbind(cbind(FF1993_portfolio_1,FF1993_portfolio_2), cbind(FF1993_portfolio_3,FF1993_portfolio_4)  ) 
return(FF1993_portfolio_5)
}

FF_table_1992 <- Form_FF_rable(FF_portfolio_1992)
rio::export(FF_table_1992, "FF1965-1991_table.csv")


FF_portfolio_all <- FF1993_portfolio %>% 
  filter(year>1962) %>% 
  rename(rank_size = Size,
         rank_bm_ratio_ln = Var)

FF_table_all <- Form_FF_rable(FF_portfolio_all)
rio::export(FF_table_all, "FF1964-2018_table.csv")

FF_portfolio_after1991 <- FF1993_portfolio %>% 
  filter(year>1991) %>% 
  rename(rank_size = Size,
         rank_bm_ratio_ln = Var)

FF_table_after1991 <- Form_FF_rable(FF_portfolio_after1991)
rio::export(FF_table_after1991, "FF1992-2018_table.csv")


###
#    step 5 Daniel and Titman 1997 replication
#    
#### 
library(zoo)
library(purrr)
library(tibbletime)

Return_19602018 <-  read_fst( "Return_19602018.fst")

Form_VarSizeCoef2 <- function(df) { # streamlined version
  # forms (size x specificed-characteristc) and forms the 25 portfolios
  # variable broken by 20/40/60/80 percentiles, (breakpoints uses NYSE data only)
  # requires Date and exchcd
  # outputs portfolio returns for each period,
  
  main.cln <- df %>%
    filter(!is.na(weight)&!is.na(lag_size)&!is.na(ret)&!is.na(lag_bm_ratio)&!is.infinite(lag_size)&!is.infinite(weight)) %>% 
    mutate_at(vars(c(ret, lag_size, lag_bm_ratio, weight)), 
              funs(DescTools::Winsorize(x=., probs = c(0.01, 0.99), na.rm = T))) %>% 
    select(date.x, PERMNO, EXCHCD, lag_size, lag_bm_ratio, weight, ret, RF, SMB, HML, `Mkt-RF`)
  
  Bkpts.NYSE <- main.cln %>% # create size and var breakpoints based on NYSE stocks only
    filter(EXCHCD == 1) %>% # NYSE exchange
    group_by(date.x) %>%
    dplyr::summarize(var.P33 = quantile(lag_bm_ratio, probs=.333, na.rm=TRUE),
                     var.P67 = quantile(lag_bm_ratio, probs=.667, na.rm=TRUE),
                     size.P33 = quantile(lag_size, probs=.333, na.rm=TRUE),
                     size.P67 = quantile(lag_size, probs=.667, na.rm=TRUE)) %>% 
    ungroup() %>% filter( date.x%%10==6 ) 
  
  # calculate size and var portfolio returns
  main.rank <- main.cln %>%
    mutate(date.y = if_else(date.x%%100 <= 5, date.x - 100 - date.x%%100 + 6,  date.x - date.x%%100 + 6)) %>% 
    left_join(Bkpts.NYSE, by=c("date.y"="date.x"), all.x=TRUE) %>%
    mutate(Size = if_else(lag_size<size.P33, "1", 
                          if_else(lag_size<size.P67, "2", "3" ) ),
           Var = if_else(lag_bm_ratio<var.P33, "1", 
                         if_else(lag_bm_ratio<var.P67, "2", "3" )),
           Port = paste(Size, Var, sep="."),
           ret = ret*100 - RF)
  
  lm_roll <- rollify( .f = function(x,y,z,o) { coef(lm(x ~ y + z + o)) }, window = 37, unlist = FALSE)
  
  main.rank2 <- main.rank %>% 
    filter(!is.na(RF)) %>% 
    dplyr::arrange(PERMNO, date.x) %>% 
    dplyr::group_by(PERMNO) %>% 
    filter(n()>37) %>% 
    dplyr::mutate(rolling_lm = lm_roll(ret, SMB, HML, `Mkt-RF`)  ) %>% 
    ungroup() %>%
    filter(!is.na(rolling_lm))
  
  save(main.rank2, file = "mainrank2.RData")
  load("mainrank2.RData")
  
  main.rank3 <- data.frame()
  
  for ( i in seq(1, 41, by = 1)){
  main.rank2.sub <- main.rank2 %>% filter(row_number()>40000*(i-1)&row_number()<=40000*i)
  main.rank3.sub <- main.rank2.sub  %>%
    unnest(rolling_lm) %>% 
    filter(row_number()%%4 == 3)
  main.rank3 <- rbind(main.rank3, main.rank3.sub)
  }
  
  
  DT_portfolio <-  main.rank3 %>% 
    arrange(PERMNO, date.x) %>% 
    group_by(PERMNO) %>% 
    mutate( HML_factor_loading = dplyr::lag(rolling_lm, 6) ) %>% 
    ungroup()

# Ret <- main.rank %>% # name 2 x 3 size-var portfolios
  #   group_by(date.x, Port) %>%
  #   dplyr::summarize(ret.port = weighted.mean(ret, weight, na.rm=TRUE)) %>% # calc value-weighted returns
  #   ungroup()
  
  return(DT_portfolio)
}

DT_portfolio <- Form_VarSizeCoef2(Return_19602018)

write_fst(main.rank2, "main_rank2.fst")

write_fst(DT_portfolio, "DT_portfolio.fst")

library(dplyr)
Size.bm <- Return_19602018 %>% select(size, bm_ratio_ln, PERMNO, date.x) %>% 
  rename(Market_value = size)

#
# generate returns from 1973-1993
#

DT_portfolio_73.93 <- DT_portfolio %>%  
  filter( date.x >= 197306 & date.x <= 199312 & !is.na(HML_factor_loading) ) %>% 
  group_by(date.x, Size, Var) %>% 
  mutate( HML_loading_rank = ntile(HML_factor_loading, 5) ) %>% 
  ungroup() %>% 
  left_join(Size.bm, by = c("PERMNO", "date.x")) %>% 
  mutate(bm_ratio_ln = exp(bm_ratio_ln))

Form_Ave_SiveBM_1 <- function(df) {
  Ave_SiveBM_weighted <- df %>% filter(date.x%%10 == 6) %>% 
    group_by(date.x, Size, Var, HML_loading_rank) %>% 
    dplyr::summarize(bm.weighted = weighted.mean( bm_ratio_ln , Market_value, na.rm=TRUE)) %>% # calc value-weighted returns
    ungroup()

  Ave_SiveBM_not_weighted <- df %>% filter(date.x%%10 == 6 & EXCHCD == 1) %>% 
    group_by(date.x) %>% 
    dplyr::summarize(bm.not.weighted = median( bm_ratio_ln , na.rm=TRUE)) %>% # calc value-weighted returns
    ungroup()
    
  Ave_SiveBM <- Ave_SiveBM_weighted %>% 
    left_join(Ave_SiveBM_not_weighted, by = c("date.x")) %>% 
    group_by( Var,Size, HML_loading_rank) %>% 
    summarise( bm_relative_to_mean = mean(bm.weighted/bm.not.weighted, na.rm = TRUE)  ) %>% 
    ungroup() %>% 
    spread(key = HML_loading_rank, value = bm_relative_to_mean) %>% 
    arrange(Var, Size)
  
  return(Ave_SiveBM)
}

Table1 <- Form_Ave_SiveBM_1(DT_portfolio_73.93) 

Form_Ave_FactorLoading_3 <- function(df) {

  Ave_SiveBM <- df %>% 
    group_by( Var,Size, HML_loading_rank) %>% 
    summarise( ret.weight = weighted.mean(ret, weight, na.rm = TRUE)  ) %>% 
    ungroup() %>% 
    spread(key = HML_loading_rank, value = ret.weight) %>% 
    arrange(Var, Size)
  return(Ave_SiveBM)
}

Table3 <- Form_Ave_FactorLoading_3(DT_portfolio_73.93) 

library(broom)
Form_Ave_PostFormationHML_2 <- function(df) {
  
  Ave_HML <- DT_portfolio_73.93 %>% 
    group_by( date.x, Var,Size, HML_loading_rank) %>% 
    summarise( ret.weight = weighted.mean(ret, weight, na.rm = TRUE),
               `Mkt-RF` = mean(`Mkt-RF`, na.rm = TRUE),
               SMB = mean(SMB, na.rm = TRUE),
               HML = mean(HML, na.rm = TRUE) ) %>% 
    ungroup() %>%   
    nest(-Size,-Var,-HML_loading_rank) %>% 
    mutate(fit = map(data, ~ lm( ret.weight ~ `Mkt-RF` + SMB + HML , data =.)),
           results1 = map(fit, tidy)) %>% 
    unnest(results1) %>% 
    filter(term == "HML") %>% 
    select(Var,Size, HML_loading_rank,estimate)
  
  Ave_HML <- Ave_HML %>% 
    spread(key = HML_loading_rank, value = estimate) %>% 
    arrange(Var, Size)
  return(Ave_HML) 
}

Table2 <- Form_Ave_PostFormationHML_2(DT_portfolio_73.93) 

Form_Ave_Char_BalancedL_4 <- function(df) {
  
  Ave_HML <- DT_portfolio_73.93 %>% 
    group_by( date.x, Var,Size, HML_loading_rank) %>% 
    summarise( ret.weight = weighted.mean(ret, weight, na.rm = TRUE),
               `Mkt-RF` = mean(`Mkt-RF`, na.rm = TRUE),
               SMB = mean(SMB, na.rm = TRUE),
               HML = mean(HML, na.rm = TRUE) ) %>% 
    ungroup() %>%  
    spread(key = HML_loading_rank, value = ret.weight) %>% 
    mutate(ret.weight = `1` + `2` - `4` - `5`) 
  
  Ave_HML1 <- Ave_HML %>% 
    nest(-Size,-Var) %>% 
    mutate(fit = map(data, ~ lm( ret.weight ~ `Mkt-RF` + SMB + HML , data =.)),
           results1 = map(fit, tidy)) %>% 
    unnest(results1) %>% 
    select(Var,Size, term,statistic) %>% 
    spread(key = term, value = statistic) 

  Ave_HML2 <- Ave_HML %>% 
    nest(-Size,-Var) %>% 
    mutate(fit = map(data, ~ lm( ret.weight ~ `Mkt-RF` + SMB + HML , data =.)),
           results1 = map(fit, glance)) %>% 
    unnest(results1) %>% 
    select(Var,Size, r.squared) 
    
  Ave_HML3 <- Ave_HML1 %>%
    left_join(Ave_HML2, by = c("Size","Var")) %>% 
    rename(Alpha = `(Intercept)`)
  
  return(Ave_HML3) 
}

Table4 <- Form_Ave_Char_BalancedL_4(DT_portfolio_73.93) 

#
# generate returns from 1973-now
#

DT_portfolio_73.now <- DT_portfolio %>%  
  filter( date.x >= 197306 & !is.na(HML_factor_loading) ) %>% 
  group_by(date.x, Size, Var) %>% 
  mutate( HML_loading_rank = ntile(HML_factor_loading, 5) ) %>% 
  ungroup() %>% 
  left_join(Size.bm, by = c("PERMNO", "date.x")) %>% 
  mutate(bm_ratio_ln = exp(bm_ratio_ln))

Table1now <- Form_Ave_SiveBM_1(DT_portfolio_73.now) 
Table2now <- Form_Ave_FactorLoading_2(DT_portfolio_73.now) 
Table3now <- Form_Ave_PostFormationHML_3(DT_portfolio_73.now) 
Table4now <- Form_Ave_Char_BalancedL_4(DT_portfolio_73.now) 

#
# generate returns from 1993-now
#

DT_portfolio_93.now <- DT_portfolio %>%  
  filter( date.x >= 199312 & !is.na(HML_factor_loading) ) %>% 
  group_by(date.x, Size, Var) %>% 
  mutate( HML_loading_rank = ntile(HML_factor_loading, 5) ) %>% 
  ungroup() %>% 
  left_join(Size.bm, by = c("PERMNO", "date.x")) %>% 
  mutate(bm_ratio_ln = exp(bm_ratio_ln))

Table1_93 <- Form_Ave_SiveBM_1(DT_portfolio_93.now) 
Table2_93 <- Form_Ave_FactorLoading_2(DT_portfolio_93.now) 
Table3_93 <- Form_Ave_PostFormationHML_3(DT_portfolio_93.now) 
Table4_93 <- Form_Ave_Char_BalancedL_4(DT_portfolio_93.now) 


T73_93 <- rbind(Table1, Table2, Table3)
T73_now <- rbind(Table1now, Table2now, Table3now)
T93_now <- rbind(Table1_93, Table2_93, Table3_93)

export(T73_93, "T73_93.csv")
export(T73_now, "T73_now.csv")
export(T93_now, "T93_now.csv")
export(Table4, "Table473_93.csv")
export(Table4now, "Table4now.csv")
export(Table4_93, "Table493.csv")






