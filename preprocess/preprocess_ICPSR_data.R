library(data.table)
library(ggplot2)
library(ggsci)
library(ggpubr)
dir <- '~/Desktop/Stanford/medicaid_transitions_sim/'

# read in dictionary for state codes
state.codes <- fread(paste0(dir, 'data/ICPSR.state.codes.csv'))

# read in individual-level release data from 1991-2016
releases <- read.table(file = paste0(dir, 'data/ICPSR_37021/DS0003/37021-0003-Data.tsv'),
                       header=T, sep='\t')
releases <- data.table(releases)
rel_summary <- releases[,.(rel.year = RELYR,
                           SEX)]

# variable RPTYEAR (reporting year) seems to be always consistent with RELYR (release year)
# for observations where RELYR is missing (9999), assume RELYR = RPTYEAR
table(releases$RPTYEAR, releases$RELYR)
releases[RELYR == 9999, RELYR := RPTYEAR]

# get the latest year with reporting per state
setorder(releases, STATE, RELYR)
lastyearperstate <- releases[,.SD[.N], by='STATE'][,.(STATE, RELYR)]
lastyearperstate <- merge(lastyearperstate, state.codes, by.x='STATE', by.y='code',
                               all=T)
table(lastyearperstate$RELYR)
# adjust year for certain states based on manual examination of missing race/ethnicity data (for some states, more missingness in last year of reporting)
lastyearperstate[state == 'Idaho', RELYR := 2015]
lastyearperstate[state == 'North Dakota', RELYR := 2014]
lastyearperstate[state == 'South Dakota', RELYR := 2012]
lastyearperstate$RELYR <- as.factor(lastyearperstate$RELYR)

# summarize by subgroup in last year per state
yrs2012.2016 <- xtabs(~RACE + AGERLSE + SEX + STATE + RELYR, data=releases[RELYR %in% seq(2012, 2016)])
rel2012.2016 <- data.table(ftable(yrs2012.2016))
rel2012.2016$STATE <- as.integer(as.character(rel2012.2016$STATE))
rel.lastyear.perstate <- merge(lastyearperstate, rel2012.2016, all.x=T, by=c('STATE','RELYR'), allow.cartesian = T)
rel.lastyear.perstate$SEX <- factor(rel.lastyear.perstate$SEX, levels = c(1,2),
                                    labels = c('Male','Female'))
rel.lastyear.perstate$AGERLSE <- factor(rel.lastyear.perstate$AGERLSE, levels = c(1,2,3,4,5,0),
                                    labels = c('18-24','25-34','35-44','45-54','55+','NA age'))
rel.lastyear.perstate$RACE <- factor(rel.lastyear.perstate$RACE, levels = c(1,2,3,4,9),
                                        labels = c('White, non-Hispanic',
                                                   'Black, non-Hispanic',
                                                   'Hispanic, any race',
                                                   'Other race(s), non-Hispanic',
                                                   'Missing race'))
rel.lastyear.perstate[,subgroup := paste(SEX, AGERLSE, RACE, sep='; ')]
table(rel.lastyear.perstate$subgroup)
setorder(rel.lastyear.perstate, STATE, SEX, AGERLSE, RACE)
rel.lastyear.perstate.cast <- dcast.data.table(rel.lastyear.perstate, state~subgroup, value.var = 'Freq')
# reorder
rel.lastyear.perstate.cast <- rel.lastyear.perstate.cast[,c('state',unique(rel.lastyear.perstate$subgroup)),with=F]
rel.lastyear.perstate.cast <- rel.lastyear.perstate.cast[,!'NA; NA; NA']

# annotate with which year the data are from, for each state
rel.lastyear.perstate.cast <- merge(rel.lastyear.perstate.cast, lastyearperstate[,c('state','RELYR')], by='state')
write.csv(rel.lastyear.perstate.cast, file = paste0(dir, 'data/prisonrel.lastyr.perstate.subgroups.csv'),
          row.names = F)

# trends over time per state for age, sex, and race/ethnicity
trends.age.state <- xtabs(~AGERLSE + RELYR + STATE, data = releases)
trends.age.state <- ftable(trends.age.state)
trends.age.state <- data.table(trends.age.state)
trends.age.state[,prop := Freq/sum(Freq), by=c('RELYR','STATE')]
trends.age.state$STATE <- as.integer(as.character(trends.age.state$STATE))
trends.age.state$RELYR <- as.integer(as.character(trends.age.state$RELYR))
trends.age.state <- merge(trends.age.state, state.codes, by.x='STATE', by.y='code', allow.cartesian = T)
trends.age.state <- trends.age.state[RELYR != 9999 & !(state == 'Virginia' & RELYR == 2009)] # drop unknown year andone year of data from VA where most are unknown age

trends.age.natl <- trends.age.state[,.(Freq=sum(Freq),
                                       state='Total'), by=c('RELYR','AGERLSE')]
trends.age.natl[,prop := Freq/sum(Freq), by=c('RELYR')]
trends.age.comb <- rbind(trends.age.state, trends.age.natl, fill=TRUE)
trends.age.comb$state <- factor(trends.age.comb$state, 
                                levels = c(unique(trends.age.state$state),'Total'))
ggplot(trends.age.comb, 
       aes(x=RELYR, y=prop, color=AGERLSE)) +
  geom_line() + facet_wrap(~state, nrow=6) + theme_bw() +
  labs(x='Year', y='Proportion', color='Age at Release') +
  scale_color_locuszoom(labels = c('NA','18-24','25-34','35-44','45-54','55+'))
ggsave(file = paste0(dir, 'figs/prison.rel.age.state.trends.pdf'),
       height=7, width=16)

trends.race.state <- xtabs(~RACE + RELYR + STATE, data = releases)
trends.race.state <- ftable(trends.race.state)
trends.race.state <- data.table(trends.race.state)
trends.race.state[,prop := Freq/sum(Freq), by=c('RELYR','STATE')]
trends.race.state$STATE <- as.integer(as.character(trends.race.state$STATE))
trends.race.state$RELYR <- as.integer(as.character(trends.race.state$RELYR))
trends.race.state <- merge(trends.race.state, state.codes, by.x='STATE', by.y='code', allow.cartesian = T)
trends.race.state <- trends.race.state[RELYR != 9999] # drop unknown year
trends.race.state[is.na(prop) & RACE == 9, prop := 1]

trends.race.natl <- trends.race.state[,.(Freq=sum(Freq),
                                       state='Total'), by=c('RELYR','RACE')]
trends.race.natl[,prop := Freq/sum(Freq), by=c('RELYR')]
trends.race.comb <- rbind(trends.race.state, trends.race.natl, fill=TRUE)
trends.race.comb$state <- factor(trends.race.comb$state, 
                                levels = c(unique(trends.race.state$state),'Total'))

ggplot(trends.race.comb,
       aes(x=RELYR, y=prop, color=RACE)) +
  geom_line() + facet_wrap(~state, nrow=6) + theme_bw() +
  labs(x='Year', y='Proportion', color='Race') +
  scale_color_locuszoom(labels = c('White, non-Hispanic',
                                   'Black, non-Hispanic',
                                   'Hispanic, any race',
                                   'Other race(s), non-Hispanic',
                                   'Missing'))
ggsave(file = paste0(dir, 'figs/prison.rel.race.state.trends.pdf'),
       height=7, width=16)


trends.sex.state <- xtabs(~SEX + RELYR + STATE, data = releases)
trends.sex.state <- ftable(trends.sex.state)
trends.sex.state <- data.table(trends.sex.state)
trends.sex.state[,prop := Freq/sum(Freq), by=c('RELYR','STATE')]
trends.sex.state$STATE <- as.integer(as.character(trends.sex.state$STATE))
trends.sex.state$RELYR <- as.integer(as.character(trends.sex.state$RELYR))
trends.sex.state <- merge(trends.sex.state, state.codes, by.x='STATE', by.y='code', allow.cartesian = T)
trends.sex.state <- trends.sex.state[RELYR != 9999] # drop unknown year

trends.sex.natl <- trends.sex.state[,.(Freq=sum(Freq),
                                         state='Total'), by=c('RELYR','SEX')]
trends.sex.natl[,prop := Freq/sum(Freq), by=c('RELYR')]
trends.sex.comb <- rbind(trends.sex.state, trends.sex.natl, fill=TRUE)
trends.sex.comb$state <- factor(trends.sex.comb$state, 
                                 levels = c(unique(trends.sex.state$state),'Total'))

ggplot(trends.sex.comb,
       aes(x=RELYR, y=prop, color=SEX)) +
  geom_line() + facet_wrap(~state, nrow=6) + theme_bw() +
  labs(x='Year', y='Proportion', color='sex') +
  scale_color_locuszoom(labels = c('Male','Female'))
ggsave(paste0(dir, 'figs/prison.rel.sex.state.trends.pdf'),
       height=7, width=16)
