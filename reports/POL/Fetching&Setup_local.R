### load packages
library(dplyr)
library(ggplot2)
library(survey)
library(convey)

#establish data connection (do NOT push with password inside!!!)
pg <- src_postgres(dbname="datacube", host="ineq.wu.ac.at", user='lvineq',password = password, options="-c search_path=silc")

###FETCH ALL VARIABLES IN ORDER TO BUILD PROPER INCOME CONCEPTS LATER ON

##PERSONAL DATA
#available from pp
silc.p <- tbl(pg, 'pp') %>% filter(pb010 %in% c(2004:2017) & pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, pl060, pl100, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, pe040, pl150, pl190, pl200) %>% collect(n=Inf)

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
silc.p_14 <- tbl(pg, 'c14p') %>% filter(pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, py021g, pe040, pl150, pl190, pl200) %>% collect(n=Inf)
silc.p_15 <- tbl(pg, 'c15p') %>% filter(pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, py021g, pe040, pl150, pl190, pl200) %>% collect(n=Inf)
silc.p_16 <- tbl(pg, 'c16p') %>% filter(pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, py021g, pe040, pl150, pl190, pl200) %>% collect(n=Inf)
silc.p_17 <- tbl(pg, 'c17p') %>% filter(pb020=='PL') %>% select(pb010, pb030, pb040, pb140, pb150, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g, py021g, pe040, pl150, pl190, pl200) %>% collect(n=Inf)

#we stack those objects again to one single frame, and attach it to the main set from "below" (newest observations)
silc.p_14to17 <- bind_rows(silc.p_14, silc.p_15, silc.p_16, silc.p_17)

silc.p <- bind_rows(silc.p, silc.p_14to17)
silc.p[is.na(silc.p)] <- 0




# replace NAs in working hours by 0
#silc.p$pl060[is.na(silc.dp$pl060)] <- 0
#silc.p$pl100[is.na(silc.dp$pl100)] <- 0

#create total working hours
#silc.p <- silc.p %>% mutate(totalhours = pl060+pl100)


##HOUSEHOLD DATA

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

silc.h[is.na(silc.h)] <- 0

## Personal Register Data

#available from rr
silc.r <- tbl(pg, 'rr') %>% filter(rb010 %in% c(2004:2017) & rb020=='PL') %>% select(rb010, rb030, rb050, rb080, rb090, rx030) %>% collect(n=Inf)

#for the years 2014-2017, data has to be fetched one-by-one from the 'cXXr'-sets
silc.r_14 <- tbl(pg, 'c14r') %>% filter(rb020=='PL') %>% select(rb010, rb030, rb050, rb080, rb090, rx030) %>% collect(n=Inf)
silc.r_15 <- tbl(pg, 'c15r') %>% filter(rb020=='PL') %>% select(rb010, rb030, rb050, rb080, rb090, rx030) %>% collect(n=Inf)
silc.r_16 <- tbl(pg, 'c16r') %>% filter(rb020=='PL') %>% select(rb010, rb030, rb050, rb080, rb090, rx030) %>% collect(n=Inf)
silc.r_17 <- tbl(pg, 'c17r') %>% filter(rb020=='PL') %>% select(rb010, rb030, rb050, rb080, rb090, rx030) %>% collect(n=Inf)

silc.r_14to17 <- bind_rows(silc.r_14, silc.r_15, silc.r_16, silc.r_17)

silc.r <- bind_rows(silc.r, silc.r_14to17)

#get education into the r-file
educ<-select(silc.p, pe040, pb010, px030, pb030)
silc.r<-left_join(silc.r, educ, by=c('rb010'='pb010', 'rx030'='px030', 'rb030'='pb030'))

silc.r[is.na(silc.r)] <- 0


###NOW BUILD THE INCOME AGGREGATES

##P1 (EUROSTAT)

#Pre-tax factor income
silc.h <- silc.h %>% mutate(pretaxfacinc_sum=py010g+py021g+py050g+hy110g+hy040g+hy090g+py080g)
silc.h <- silc.h %>% mutate(pretaxfacinc=pretaxfacinc_sum/(hx050))

#pretax national income
silc.h <- silc.h %>% mutate(pretaxnational_sum=pretaxfacinc_sum+py090g+py100g)
silc.h <- silc.h %>% mutate(pretaxnational=pretaxnational_sum/hx050)

#post-tax disposable income
silc.h <- silc.h %>% mutate(posttax_sum=pretaxnational_sum+py110g+py120g+py130g+py140g+hy050g+hy060g+hy070g+hy080g-hy120g-hy130g-hy140g)
silc.h <- silc.h %>% mutate(posttax=posttax_sum/hx050)

#now we put silc.h and silc-r together in order to appoint the weighted income aggregates to all household members.
silc.rh <- left_join(silc.r, silc.h, by=c('rb010'='hb010', 'rx030'='hb030'))

#we can take now this table and cut out only the collumns that we need and rename them
silc.rh <- rename(silc.rh, 'year'=rb010, 'persID'=rb030, 'perswght'=rb050, 'brthyr'=rb080, 'sex'=rb090, 'houseID'=rx030, 'incu16'=hy110g, 'incrental'=hy040g, 'returninv'=hy090g, 'childallow'=hy050g, 'socex'=hy060g, 'houseallow'=hy070g, 'interhouse_rec'=hy080g, 'wealthtax'=hy120g, 'interhouse_paid'=hy130g, 'inctax'=hy140g, 'houseinc'=hy020, 'eqhousesize'=hx050, 'housesize'=hx040, 'empcashinc'=py010g, 'compcar'=py021g, 'selfcashinc'=py050g, 'privatepens'=py080g, 'unempben'=py090g, 'oldageben'=py100g, 'survivorben'=py110g, 'sicknessben'=py120g, 'disabilben'=py130g, 'educationben'=py140g, 'highed'=pe040)

#generate age and recode gender to factor variable
silc.rh <- silc.rh %>% mutate(age = year-brthyr,
                               gender = factor(sex,labels = c('Male','Female')),
                               edu = factor(highed, levels=c(0:5), labels = c("pre-primary","primary","lower secondary","upper secondary","post secondary","tertiary")))


#get cpi from eurostat
library(eurostat)
library(dplyr)
infl <- get_eurostat(id = "prc_hicp_aind")

infl <- infl %>% filter(geo == "PL"  &         
                          coicop == "CP00" &
                          unit == "INX_A_AVG" &
                          time > "2003-01-01")

infl$year<-format(infl$time, format="%Y")

infl<-infl[,5:6]

#shift the index by one year, because the income reported in 2017 was actually earned in 2016 (we thus have to use the 2016-CPI for income in 2017)
ones<-matrix(1,14,1)
infl$year<-as.integer(infl$year)
infl[,2]<-infl[,2]+ones


P1<-merge(silc.rh,infl, by='year')

P1<-rename(P1, 'CPI'=values)


save(P1, file='./data/P1.rda',compress = 'xz')

#silc.rhp <- left_join(silc.rh, silc.p, by=c('rb010'='pb010','rx030'='px030' ,'rb030'='pb030'))
#cancelled because the p-file has less observation than the r-file


##P2 (wid.world)

#create age in silc.p so we know which persons to exclude
silc.p <- silc.p %>% mutate(age=pb010 - pb140)
silc.p_adults <- filter(silc.p, age>= 20)

#now we count for every household-ID how many members are left
hh_size <- silc.p_adults %>% group_by(pb010) %>% count(px030)

#the resulting length of hh_size is slightly smaller than silc.p, because we have cut out the households where all members are <20 years old.
#we attatch hh_size to silc.h and delete all cases where there is no match, because these are the households with all members <20 years old.
silc.h <- left_join(silc.h, hh_size, by=c('hb010'='pb010', 'hb030'='px030'))
silc.h_adults <- na.omit(silc.h)

#now we build the aggregates from the h-file and divide them by n for every household

#aggregate 1
silc.h_adults <- mutate(silc.h_adults, pretax_h=(hy110g+hy040g+hy090g)/n)

#aggregate 2 is the same as aggregate 1 regarding the positions from the h-file

#aggregate 3
silc.h_adults <- mutate(silc.h_adults, posttax_h=pretax_h+(hy050g+hy060g+hy070g+hy080g-hy120g-hy130g-hy140g)/n)

#we attatch silc.h_adults (only the columns of interest to us) to silc.p_adults and add up the full aggregates
silc.ph_adults <- left_join(silc.p_adults, select(silc.h_adults, c('hb010', 'hb030', 'pretax_h', 'posttax_h')), by=c('pb010'='hb010', 'px030'='hb030'))

#pre-tax factor income
silc.ph_adults <- mutate(silc.ph_adults, pretaxfactor=py010g+py021g+py050g+pretax_h)

#pre-tax national income
silc.ph_adults <- mutate(silc.ph_adults, pretaxnational=pretaxfactor+py090g+py100g)

#post-tax disposable income
silc.ph_adults <- mutate(silc.ph_adults, posttax=pretaxnational+py110g+py120g+py130g+py140g+posttax_h)

#we can take now this table and cut out only the collumns that we need and rename them
silc.ph_adults <- rename(silc.ph_adults, 'year'=pb010, 'persID'=pb030, 'perswght'=pb040, 'brthyr'=pb140, 'sex'=pb150, 'houseID'=px030, 'empcashinc'=py010g, 'noncash'=py020g, 'selfcashinc'=py050g, 'privatepens'=py080g, 'unempben'=py090g, 'oldageben'=py100g, 'survivorben'=py110g, 'sicknessben'=py120g, 'disabilben'=py130g, 'educationben'=py140g, 'compcar'=py021g, 'highed'=pe040, 'supervise'=pl150, 'firstjob'=pl190, 'yrsworkd'=pl200)

#recode gender and education variables to factor variable
silc.ph_adults <- silc.ph_adults %>% mutate(edu = factor(highed, levels=c(0:5), labels = c("pre-primary","primary","lower secondary","upper secondary","post secondary","tertiary")), gender = factor(sex,labels = c('Male','Female')))

#create column with CPI
P2<-merge(silc.ph_adults,infl, by='year')

P2<-rename(P2, 'CPI'=values)

save(P2, file='./data/P2.rda',compress = 'xz')