### load packages
library(dplyr)
library(ggplot2)
library(survey)
library(convey)

#establish data connection (do NOT push with password inside!!!)
pg <- src_postgres(dbname="datacube", host="ineq.wu.ac.at", user='lvineq',password = '', options="-c search_path=silc")

###FETCH ALL VARIABLES IN ORDER TO BUILD PROPER INCOME CONCEPTS LATER ON

##PERSONAL DATA
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


###NOW BUILD THE INCOME AGGREGATES

##P1 (EUROSTAT)

#Pre-tax factor income
silc.h <- silc.h %>% mutate(pretaxfacinc_sum=py010g+py021g+py050g+hy110g+hy040g+hy090g+py080g)
silc.h <- silc.h %>% mutate(pretaxfacinc=pretaxfacinc_sum/(hx050))

#pretax national income
silc.h <- silc.h %>% mutate(pretaxnational_sum=pretaxfacinc_sum+py090g+py100g)
silc.h <- silc.h %>% mutate(pretaxnational=pretaxnational_sum/hx050)

#post-tax disposable income
silc.h <- silc.h %>% mutate(posttax_sum=pretaxnational_sum+py110g+py120g+py130g+py140g+hy050g+hy060g+hy070g+hy080g+hy120g+hy130g+hy140g)
silc.h <- silc.h %>% mutate(posttax=posttax_sum/hx050)

#now we put silc.h and silc-r together in order to appoint the weighted income aggregates to all household members.
silc.rh <- left_join(silc.r, silc.h, by=c('rb010'='hb010', 'rx030'='hb030'))

#we can take now this table and cut out only the collumns that we need and rename them
silc.rhp <- left_join(silc.rh, silc.p, by=c('rb010'='pb010','rx030'='px030' ,'rb030'='pb030'))

####hier weitermachen






# join register & data
silc.dp <- right_join(silc.d, silc.p, by=c('db020'='pb020', 'db030'='px030'))

# replace NAs in working hours by 0
silc.dp$pl060[is.na(silc.dp$pl060)] <- 0
silc.dp$pl100[is.na(silc.dp$pl100)] <- 0

silc.dp <- silc.dp %>% mutate(totalhours = pl060+pl100)

#anderer Weg mit dplyr-Logik: 
#silc.dp <- silc.dp %>% mutate(totalhours = coalesce(pl060, 0L) + coalesce(pl100, 0L))

# calculate hourly wage
silc.dp <- silc.dp %>% mutate(hwages = py010g / ((pl060+pl100)*52/12*pmin(12,pl073+pl074)) )

#NaN weil divisionen durch Null, daher:
silc.dp <- silc.dp %>% filter(py010g>0 & pl060>0 & (pl073+pl074)>0)

# filter only observations with income and working time
silc.dp <- silc.dp %>% filter(py010g>0 & pl060>0 & (pl073+pl074)>0)

summary(silc.dp)

# generate age and recode gender to factor variable
silc.dp <- silc.dp %>% mutate( age = 2013-pb140,
                               gender = factor(pb150,labels = c('Male','Female')),
                               edu = factor(pe040, levels=c(0:5),  labels = c("pre-primary","primary","lower secondary","upper secondary","post secondary","tertiary")))
class(silc.dp$gender)

## some regression specifications
ols1 <- lm(hwages ~ age + gender, data=silc.dp)
summary(ols1)

# log(y): distribution of residuals look more like normal distr.
ols2 <- lm(log(hwages) ~ age + gender, data=silc.dp)
summary(ols2)

# include age^2: non-linear relationship between inc and age
ols3 <- lm(log(hwages) ~ I(age-min(age)) + I(age^2) + gender, data=silc.dp)
summary(ols3)

# factor: includes dummies for qualitative variables
ols4 <- lm(log(hwages) ~ age + I(age^2) + gender + factor(pe040) + edu, data=silc.dp)
summary(ols4)


# svy design
library(survey)
library(convey)
silc.dp <- silc.dp %>% mutate(id_h=paste0(db020,db030))
silc.p.svy <- svydesign(ids=~id_h,
                        strata=~db040,
                        weights=~db090,
                        data=silc.dp) %>% convey_prep()

# weighted ols
svyols <- svyglm(log(hwages) ~ age + I(age^2) + gender + factor(pe040) , silc.p.svy)

# logit model: family=binomial()!!
svylogit <- svyglm(I(hwages>10) ~ age + I(age^2) + gender + factor(pe040) , silc.p.svy, family=binomial())

# marginal effects: caveat! weights currently ignored
library(margins)
margins(svylogit)

summary(svyols)


# scatterplot with ggplot
library(ggplot2)
silc.dp %>% ggplot(aes(x=age, y=log(hwages))) + geom_point(aes(color=gender), aplha=0.25) + scale_color_brewer('Geschlecht', palette = 'Set1') + facet_wrap(~edu)#+ geom_smooth(method='lm')
