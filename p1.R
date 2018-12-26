### load packages
library(dplyr)
library(ggplot2)
library(survey)
library(convey)
library(dbplyr)
library(RPostgreSQL)

#establish data connection (do NOT push with password inside!!!)  
pg <- src_postgres(dbname="datacube", host="ineq.wu.ac.at", user='lvineq',password = '', options="-c search_path=silc")

###FETCH ALL VARIABLES IN ORDER TO BUILD PROPER INCOME CONCEPTS LATER ON

##PERSONAL DATA
# best: remove PW instantly after getting data --------------------------------------
#available from pp
silc.p <- tbl(pg, 'pp') %>% filter(pb010 %in% c(2004:2017) & pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, pl060, pl100, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g) %>% collect(n=Inf)


#fetch information about cars for 2005 to 2006 (2004 not available) from py020g but rename it to py021g fro later matching purposes:
#py021g04 <- tbl(pg, 'c04p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py020g) %>% collect(n=Inf)
#py021g04 <- rename(py021g04, 'py021g'= py020g)

py021g05 <- tbl(pg, 'c05p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py020g) %>% collect(n=Inf)
py021g05 <- rename(py021g05, 'py021g'= py020g)

py021g06 <- tbl(pg, 'c06p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py020g) %>% collect(n=Inf)
py021g06 <- rename(py021g06, 'py021g'= py020g)

py021g07 <- tbl(pg, 'c07p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py021g) %>% collect(n=Inf)
py021g08 <- tbl(pg, 'c08p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py021g) %>% collect(n=Inf)
py021g09 <- tbl(pg, 'c09p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py021g) %>% collect(n=Inf)
py021g10 <- tbl(pg, 'c10p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py021g) %>% collect(n=Inf)
py021g11 <- tbl(pg, 'c11p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py021g) %>% collect(n=Inf)
py021g12 <- tbl(pg, 'c12p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py021g) %>% collect(n=Inf)
py021g13 <- tbl(pg, 'c13p') %>% filter(pb020=='PL') %>% select(pb030, px030, pb010, py021g) %>% collect(n=Inf)

#stack all 'py021gXX'-objects to one single vector
py021g <- bind_rows(py021g05, py021g06, py021g07, py021g08, py021g09, py021g10, py021g11, py021g12, py021g13)

#matching the data with main dataset silc.p according to year, household ID and personal ID
silc.p <- left_join(silc.p, py021g, by=c('pb010','pb030', 'px030'))

#for the years 2014-2017, data has to be fetched one-by-one from the 'cXXp'-sets
silc.p_14 <- tbl(pg, 'c14p') %>% filter(pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, pl060, pl100, pl073, pl074, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, py021g, ) %>% collect(n=Inf)
silc.p_15 <- tbl(pg, 'c15p') %>% filter(pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, pl060, pl100, pl073, pl074, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, py021g) %>% collect(n=Inf)
silc.p_16 <- tbl(pg, 'c16p') %>% filter(pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, pl060, pl100, pl073, pl074, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, py021g) %>% collect(n=Inf)
silc.p_17 <- tbl(pg, 'c17p') %>% filter(pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, pl060, pl100, pl073, pl074, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, py021g) %>% collect(n=Inf)

#we stack those objects again to one single frame, and attach it to the main set from "below" (newest observations)
silc.p_14to17 <- bind_rows(silc.p_14, silc.p_15, silc.p_16, silc.p_17)

silc.p <- bind_rows(silc.p, silc.p_14to17)

# HOUSEHOLD DATA ----------------------------------------------------------

#Available from hh:
silc.h <- tbl(pg, 'hh') %>% filter(hb010 %in% c(2004:2017) & hb020 %in% c('PL')) %>% select(hb010, hy110g, hy040g, hy090g, hy050g, hy060g, hy070g, hy080g, hy120g, hy130g, hy140g, hb030, hy020, hx050, hx040) %>% collect(n=Inf)

#for the years 2014-2017, data has to be fetched one-by-one from the 'cXXp'-sets
silc.h_14 <- tbl(pg, 'c14h') %>% filter(hb020=='PL') %>% select(hb010, hy110g, hy040g, hy090g, hy050g, hy060g, hy070g, hy080g, hy120g, hy130g, hy140g, hb030, hy020, hx050, hx040) %>% collect(n=Inf)
silc.h_15 <- tbl(pg, 'c15h') %>% filter(hb020=='PL') %>% select(hb010, hy110g, hy040g, hy090g, hy050g, hy060g, hy070g, hy080g, hy120g, hy130g, hy140g, hb030, hy020, hx050, hx040) %>% collect(n=Inf)
silc.h_16 <- tbl(pg, 'c16h') %>% filter(hb020=='PL') %>% select(hb010, hy110g, hy040g, hy090g, hy050g, hy060g, hy070g, hy080g, hy120g, hy130g, hy140g, hb030, hy020, hx050, hx040) %>% collect(n=Inf)
silc.h_17 <- tbl(pg, 'c17h') %>% filter(hb020=='PL') %>% select(hb010, hy110g, hy040g, hy090g, hy050g, hy060g, hy070g, hy080g, hy120g, hy130g, hy140g, hb030, hy020, hx050, hx040) %>% collect(n=Inf)

silc.h_14to17 <- bind_rows(silc.h_14, silc.h_15, silc.h_16, silc.h_17)

silc.h <- bind_rows(silc.h, silc.h_14to17)

#for every household, give us now the summed up income of all household members
silc.p_sum <- silc.p %>% group_by(pb010, px030) %>% summarise_at(vars(py010g, py021g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g), funs(sum)) 

#now we join this with silc.h
silc.h <- left_join(silc.h, silc.p_sum, by=c('hb010'='pb010', 'hb030'='px030'))

#inside silc.h there is now all the information we need in order to calculate our aggregates. Remember that with hx040 we have the number of all household members that have an entry in file r


# NOW BUILD THE INCOME AGGREGATES P1----------------------------------------

##P1 (EUROSTAT)


#Pre-tax factor income = income from employment(py010g + py021g + py050g + hy110g) + property income(hy040g + hy090g + py080g)
#make vectors for convenience, hope that works out

silc.h<- silc.h %>% mutate(emplinc=py010g + py021g + py050g + hy110g)
silc.h<- silc.h %>% mutate(propinc=hy040g + hy090g + py080g)
silc.h<- silc.h %>% mutate(pretaxfactorinc = emplinc + propinc)

#pretax national income:  pre-tax factor income + (unemployment benefits(py090g) + old-age benefits(py100g)) 

silc.h <- silc.h %>% mutate(pretax_nationalinc =  pretaxfactorinc + py090g + py100g)
summary(silc.h$pretax_nationalinc)

#check just4fun for differences
fun <- silc.h$pretax_nationalinc - silc.h$pretaxfactorinc

#now post-tax disp income: pretaxnatinc + all other transfers(means all additional transfers and taxes)
# first all transfers :  pretaxinc + (survivor benefits (PY110G) + sickness benefits (PY120G)
#  + disability benefits (PY130G) + education-related allowances (PY140G) + 
#  family/children related allowances (HY050G) + social exclusion not elsewhere classified (HY060G)
#   + housing allowances (HY070G) +	regular inter-household cash transfers received (HY080G)

silc.h <- silc.h %>% mutate(transfers = py110g + py120g + py130g
                            + py140g + hy050g + hy060g + hy070g + hy080g)

# second: substract all taxes & social security taxes: regular taxes on wealth (HY120G) +	
# regular inter-household cash transfer paid (HY130G) + tax on income and social insurance contributions (HY140G)

silc.h <- silc.h %>% mutate(taxes = hy120g + hy130g + hy140g)

# now finish it

silc.h <- silc.h %>% mutate(posttax_transfers = transfers - taxes)
silc.h <- silc.h %>% mutate(posttax_dispinc = pretax_nationalinc + posttax_transfers)


#check if aligns with hy020
identical(silc.h[['silc.h$posttax_dispinc']],silc.h[['silc.h$hy020']])

#it does