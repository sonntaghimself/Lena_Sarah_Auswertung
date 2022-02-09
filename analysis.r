library("tidyverse")
library("haven")
library("naniar")

getwd()
setwd("~/Desktop/Lena_Sarah_Auswertung")

###############################################################################
#                             reading in the data                             #
###############################################################################
dat <- read_dta("ZA6748_v1-0-0.dta")

###############################################################################
#                            sorting out the data                             #
###############################################################################
dat$sex <- dat$a9_gesch %>% as_factor()
levels(dat$sex) <- list("f" = "weiblich", "m" = "männlich")

##############################
#  dropping useless columns  #
##############################

dat %>% 
    select(c(
             id, 
             sex, 
             starts_with(c(
                           "a4", 
                           "a8", 
                           "a10", 
                           "a11"
                           )
             ))
    )



# dat <-
    # dat %>%
        # unite(a11, c("a11_kauf", "a11_gew"))

###############
#  case_when  #
###############

dat_1 <-
    dat %>%
        mutate(a11 = case_when(a11_kauf == 1 & a11_gew == 0 ~ "kauf",
                               a11_kauf == 0 & a11_gew == 2 ~ "gew",
                               TRUE ~ "sonst"
                               )
        )

###############################################################################
#                                  analysis                                   #
###############################################################################

#######################
#  variables from a4  #
#######################
a4 <-
    dat_1 %>%
    select(c(
             id,
             sex,
             a11,
             starts_with(c("a4_ap")
                        )
             )
    )

names(a4) <- c("id", "sex", "kauf_gew","nicht", "Unterricht", "Arbeitsplatz", "Freizeit")
################
#  statistics  #
################

############
#  graphs  #
############


###############################
#  example variables from a8  #
###############################
dat$a8_web %>% na.omit() %>% str() 

dat$a8_soft %>% na.omit() %>% str()
