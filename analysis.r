library("tidyverse")
library("haven")

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

dat %>% nrow()

###############################################################################
#      eexclusion; dropping useless columns and giving meaningful names       #
###############################################################################
dat <-
    dat %>%
        mutate(a11 = case_when(a11_kauf == 1 & a11_gew == 0 ~ "kauf",
                               a11_kauf == 0 & a11_gew == 2 ~ "gew",
                               TRUE ~ "sonst"
                               ),
        ) %>%
        select(c(
                 id,
                 sex,
                 a11,
                 starts_with(c("a4_ap",
                               "a6_druck",
                               "a8_selb")
                 )
                 )
        )
             
names(dat) <- c("id",
               "sex",
               "kauf_gew",
               "alt_nicht",
               "alt_Unterricht",
               "alt_Arbeitsplatz",
               "alt_Freizeit",
               "a6",
               "a8"
            )

##############################
#  getting the right format  #
##############################

########
#  a4  #
########

dat <-
    dat %>%
        mutate(nicht = case_when(alt_nicht == 1 ~ 1,
                               TRUE ~ 0
                               ),
               Unterricht = case_when(alt_Unterricht == 2 ~ 1,
                                      TRUE ~ 0
                                ),
               Arbeitsplatz = case_when(alt_Arbeitsplatz == 3 ~ 1,
                                      TRUE ~ 0
                                ),
               Freizeit = case_when(alt_Freizeit == 4 ~ 1,
                                      TRUE ~ 0
                                ),
        )

dat  <-
    dat %>%
    select(!(starts_with("alt")))

########
#  a6  #
########

dat$a6 <-
    dat$a6 %>%
        as_factor()

########
#  a8  #
########

dat$a8 <-
    dat$a8 %>%
        as_factor()

dat %>% nrow()

dat %>%
    na.omit() %>%
    nrow()


### done to this point

###############################################################################
#                           few descriptive things                            #
###############################################################################
####################################################
#  exclusion should be done at this point already  #
####################################################

#########
#  age  #
#########

#########
#  sex  #
#########

#############
#  N vs. n  #
#############

###############################################################################
#                                  analysis (a4)                              #
###############################################################################
#######################
#  variables from a4  #
#######################
a4 <-
    dat %>%
    select(c(
             id,
             sex,
             a11,
             starts_with(c("a4_ap"),

                        )
             )
    )

names(a4) <- c("id",
               "sex",
               "kauf_gew",
               "nicht_alt",
               "Unterricht_alt",
               "Arbeitsplatz_alt",
               "Freizeit_alt"
            )

##################
#  right format  #
##################
a4 %>% str

a4 <-
    a4 %>%
        mutate(nicht = case_when(nicht_alt == 1 ~ 1,
                               TRUE ~ 0
                               ),
               Unterricht = case_when(Unterricht_alt == 2 ~ 1,
                                      TRUE ~ 0
                                ),
               Arbeitsplatz = case_when(Arbeitsplatz_alt == 3 ~ 1,
                                      TRUE ~ 0
                                ),
               Freizeit = case_when(Freizeit_alt == 4 ~ 1,
                                      TRUE ~ 0
                                ),
        )

####################################################
#  aggregating across Kaufmännisch vs. Gewerblich  #
####################################################
a4_kg <- a4 %>%
    select(kauf_gew, nicht, Unterricht, Arbeitsplatz, Freizeit) %>%
    group_by(kauf_gew) %>%
    summarise(across(c(nicht, Unterricht, Arbeitsplatz, Freizeit)
                     ,mean))

#####################
#  aggregating Sex  #
#####################
a4_mf <- a4 %>%
    select(sex, nicht, Unterricht, Arbeitsplatz, Freizeit) %>%
    group_by(sex) %>%
    summarise(across(c(nicht, Unterricht, Arbeitsplatz, Freizeit)
                     ,mean))

###############################################################################
#                                 statistics                                  #
###############################################################################
a4_1 <-
    a4 %>%
        subset(
            kauf_gew != "sonst"
        )

a4_1$kauf_gew <- as.factor(a4_1$kauf_gew)

## NOTE: I have to apply na.omit. df are all messed up.

#######################################################
#  Unterschied; zwischen Kaufmännisch vs. Gewerblich  #
#######################################################
## H0 : kauf = gew
## H1 : kauf != gew

##################
#  nutzen nicht  #
##################
t.test(a4_1$nicht ~ a4_1$kauf_gew)

        # Welch Two Sample t-test

# data:  a4_1$nicht by a4_1$kauf_gew
# t = -3.0317, df = 951.62, p-value = 0.002498
# alternative hypothesis: true difference in means between group gew and group 
# kauf is not equal to 0
# 95 percent confidence interval:
#  -0.13629572 -0.02917998
# sample estimates:
#  mean in group gew mean in group kauf
        #  0.6497890         0.7325269

# kauf benutzen öfter nicht; als gew

################
#  Unterricht  #
################
t.test(a4_1$Unterricht ~ a4_1$kauf_gew)

        # Welch Two Sample t-test

# data:  a4_1$Unterricht by a4_1$kauf_gew
# t = 2.5363, df = 802.43, p-value = 0.01139
# alternative hypothesis: true difference in means between group gew and group 
# kauf is not equal to 0
# 95 percent confidence interval:
 # 0.008226657 0.064558153
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.07805907         0.04166667

##################
#  Arbeitsplatz  #
##################
t.test(a4_1$Arbeitsplatz ~ a4_1$kauf_gew)

        # Welch Two Sample t-test

# data:  a4_1$Arbeitsplatz by a4_1$kauf_gew
# t = 1.3941, df = 851.13, p-value = 0.1637
# alternative hypothesis: true difference in means between group gew and group 
# kauf is not equal to 0
# 95 percent confidence interval:
 # -0.006246572  0.036871317
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.04219409         0.02688172

##############
#  Freizeit  #
##############
t.test(a4_1$Freizeit ~ a4_1$kauf_gew)

        # Welch Two Sample t-test

# data:  a4_1$Freizeit by a4_1$kauf_gew
# t = 2.8541, df = 928.62, p-value = 0.004412
# alternative hypothesis: true difference in means between group gew and group
# kauf is not equal to 0
# 95 percent confidence interval:
 # 0.0224546 0.1213116
# sample estimates:
 # mean in group gew mean in group kauf
        #  0.2721519          0.2002688


########################################
#  Unterschied zwischen Geschlechtern  #
########################################
# H0: m = f
# H1: m != f

##################
#  nutzen nicht  #
##################
t.test(a4$nicht ~ a4$sex)

        # Welch Two Sample t-test

# data:  a4$nicht by a4$sex
# t = -0.96441, df = 1651.5, p-value = 0.335
# alternative hypothesis: true difference in means between group f and group m 
# is not equal to 0
# 95 percent confidence interval:
 # -0.06624214  0.02257241
# sample estimates:
# mean in group f mean in group m
      # 0.6800000       0.7018349

################
#  Unterricht  #
################
t.test(a4$Unterricht ~ a4$sex)

        # Welch Two Sample t-test

# data:  a4$Unterricht by a4$sex
# t = -2.4842, df = 1632, p-value = 0.01309
# alternative hypothesis: true difference in means between group f and group m 
# is not equal to 0
# 95 percent confidence interval:
 # -0.049315817 -0.005798861
# sample estimates:
# mean in group f mean in group m
     # 0.04125000      0.06880734

##################
#  Arbeitsplatz  #
##################
t.test(a4$Arbeitsplatz ~ a4$sex)

        # Welch Two Sample t-test

# data:  a4$Arbeitsplatz by a4$sex
# t = -2.5976, df = 1569.9, p-value = 0.009474
# alternative hypothesis: true difference in means between group f and group m 
# is not equal to 0
# 95 percent confidence interval:
 # -0.039187827 -0.005468137
# sample estimates:
# mean in group f mean in group m
     # 0.02125000      0.04357798

##############
#  Freizeit  #
##############
t.test(a4$Freizeit ~ a4$sex)

        # Welch Two Sample t-test

# data:  a4$Freizeit by a4$sex
# t = 1.2072, df = 1645.1, p-value = 0.2275
# alternative hypothesis: true difference in means between group f and group m is not equal to 0
# 95 percent confidence interval:
 # -0.01569817  0.06595046
# sample estimates:
# mean in group f mean in group m
      # 0.2487500       0.2236239

# plots {{{{{ #
###############################################################################
#                                   plots                                     #
###############################################################################
##############################
#  Kaufmännisch; Gewerblich  #
##############################

# benutzen nicht #
p_a4_kg_nicht <- a4_kg %>% ggplot(aes(x = kauf_gew, y = nicht)) +
    geom_bar(stat = "identity") +
    labs(title = "Anteil der Azubis in Kaufmännisch oder gewerblicher
         Ausbildung, die Lern-Apps nicht benutzen.",
    x = "Ausbildungszweig",
    y = "Anteil"
    )

p_a4_kg_nicht

# am Arbeitsplatz #
p_a4_kg_ar <- a4_kg %>% ggplot(aes(x = kauf_gew, y = Arbeitsplatz)) +
    geom_bar(stat = "identity") +
    labs(title = "Anteil
         der Azubis in Kaufmännisch oder gewerblicher Ausbildung, die Lern-Apps
         am Arbeitsplatz benutzen",
    x = "Ausbildungszweig",
    y = "Anteil"
    )

p_a4_kg_ar

# im Unterricht #
p_a4_kg_un <- a4_kg %>% ggplot(aes(x = kauf_gew, y = Unterricht)) +
    geom_bar(stat = "identity") +
    labs(title = "Anteil der Azubis in Kaufmännisch oder gewerblicher
         Ausbildung, die Lern-Apps im Unterricht benutzen",
    x = "Ausbildungszweig",
    y = "Anteil"
    )

p_a4_kg_un

# in Freizeit #
p_a4_kg_fr <- a4_kg %>% ggplot(aes(x = kauf_gew, y = Freizeit)) +
    geom_bar(stat = "identity") +
    labs(title = "Anteil der Azubis in Kaufmännisch oder gewerblicher 
         Ausbildung, die Lern-Apps in ihrer Freizeit benutzen",
    x = "Ausbildungszweig",
    y = "Anteil"
    )

p_a4_kg_fr

#########
#  Sex  #
#########


# benutzen nicht #
p_a4_mf_nicht <- a4_mf %>% ggplot(aes(x = sex, y = nicht)) +
    geom_bar(stat = "identity") +
    labs(title = "Benutzung der Lern-Apps in Abhängigkeit vom Geschlecht",
    x = "Geschlecht",
    y = "Anteil"
    )

p_a4_mf_nicht

# am Arbeitsplatz #
p_a4_mf_ab <- a4_mf %>% ggplot(aes(x = sex, y = Arbeitsplatz)) +
    geom_bar(stat = "identity") +
    labs(title = "Benutzung der Lern-Apps in Abhängigkeit vom Geschlecht",
    x = "Geschlecht",
    y = "Anteil"
    )

p_a4_mf_ab

# im Unterricht #
p_a4_mf_un <- a4_mf %>% ggplot(aes(x = sex, y = Unterricht)) +
    geom_bar(stat = "identity") +
    labs(title = "Benutzung der Lern-Apps in Abhängigkeit vom Geschlecht",
    x = "Geschlecht",
    y = "Anteil"
    )

p_a4_mf_un

# in Freizeit #
p_a4_mf_fr <- a4_mf %>% ggplot(aes(x = sex, y = Freizeit), colors = sex) +
    geom_bar(stat = "identity")
    labs(title = "Benutzung der Lern-Apps in Abhängigkeit vom Geschlecht",
    x = "Geschlecht",
    y = "Anteil"
    )

p_a4_mf_fr

# }}}}} plots #

##########################################################################
#  geg: 4 im Unterricht benutzen, kauf_gew & Sex -> Aufg. 6 und Aufg. 8  #
##########################################################################

##############################################################
#  Achtung! 6 vs. 8 sind verschieden gepolte Likert Skalen.  #
##############################################################
