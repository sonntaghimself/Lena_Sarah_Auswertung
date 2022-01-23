library(tidyverse)
library(haven)
library(naniar)

getwd()
setwd("~/Desktop/Lena_Sarah_Auswertung")

###############################################################################
#                             reading in the data                             #
###############################################################################

# dat <- read.spss("ZA6748_v1-0-0.sav", to.data.frame=TRUE)
dat <- read_dta("ZA6748_v1-0-0.dta")

###############################################################################
#                             sorting out the data                             #
###############################################################################
dat$sex <- dat$a9_gesch %>% as_factor()
levels(dat$sex) <- list("f" = "weiblich", "m" = "männlich")

dat <- dat %>% relocate(id, sex)

## dropping some useless columns

dat %>% select(-c(za_nr, doi, a9_gesch))


# dat_a8 <- dat %>% select(starts_with("a8"))

# dat_a4 <- dat %>% select(starts_with("a4"))

