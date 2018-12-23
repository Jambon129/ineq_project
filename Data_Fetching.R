### load packages
library(dplyr)
library(ggplot2)
library(survey)
library(convey)



#PRE-TAX FACTOR INCOME

#personal income data
silc.p <- tbl(pg, 'pp') %>% filter(pb010 %in% c(2004:2017) & pb020=='PL') %>%
  select(pb010, pb030, px030, py010g, py020g, py050g, py080g, py090g, py100g, py110g, py120g, py130g, py140g) %>% collect(n=Inf)

#fetch information about cars for 2005 to 2006 (0224 not available) from py020g but rename it to py021g fro later matching purposes:
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

# years after 2013 are still missing

#stack all 'py021gXX'-object to one single vector

py021g <- bind_rows(py021g04, py021g05, py021g06, py021g07, py021g08, py021g09, py021g10, py021g11, py021g12, py021g13)

#matching the data with main dataset silc.p according to year, household ID and personal ID

silc.p <- left_join(silc.p, py021g, by=c('pb010','pb030', 'px030'))

#household data:

silc.h <- tbl(pg, 'hh') %>% filter(hb010 %in% c(2004:2017) & hb020 %in% c('PL')) %>% select(hb010, hy110g, hy040g, hy090g, hy50g, hy60g, hy70g, hy080g, hy120g, hy130g, hy140g, hb030, hy020, hx050)




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
