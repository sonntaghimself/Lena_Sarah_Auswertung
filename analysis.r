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
                 a10_alt,
                 a11,
                 starts_with(c("a4_ap",
                               "a6_druck",
                               "a8_selb")
                 )
                 )
        )
             
names(dat) <- c("id",
               "sex",
               "age",
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

#########
#  age  #
#########
dat$age <-
    dat$age %>%
        as_factor()

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

dat <-
    dat %>%
    na.omit()

### done to this point
#########################################################
#  excluding everyone who isn't kaufmännisch oder gew.  #
#########################################################

dat <-
    dat %>%
        subset(
            kauf_gew != "sonst"
        )

dat$kauf_gew <- as.factor(dat$kauf_gew)

###############################################################################
#                           few descriptive things                            #
###############################################################################
####################################################
#  exclusion should be done at this point already  #
####################################################
#########
#  age  #
#########
dat$age %>%
    table()

# unter 16 Jahre     16 bis 18 Jahre     19 bis 21 Jahre     22 bis 24 Jahre
#              7                 317                 507                 201
# 25 bis 27 Jahre 28 Jahre oder älter
#                 64               73

pl_age <-
    dat %>%
        ggplot(aes(age)) +
        geom_bar(stat="count") +
        theme_minimal() +
        labs(title = "Alter der Teilnehmenden",
             x = "Altersgruppe",
             y = "Anzahl in Gruppe"
        )

pl_age

#########
#  sex  #
#########
dat$sex %>%
    table()

  # f   m
# 519 650
#############
#  N vs. n  #
#############

dat %>%
    nrow()
# [1] 1169

###############################################################################
#                                  analysis (a4)                              #
###############################################################################
####################################################
#  aggregating across Kaufmännisch vs. Gewerblich  #
####################################################
# a4_kg <- dat %>%
#     select(kauf_gew, nicht, Unterricht, Arbeitsplatz, Freizeit) %>%
#     group_by(kauf_gew) %>%
#     summarise(across(c(nicht, Unterricht, Arbeitsplatz, Freizeit)
#                      ,mean))

#####################
#  aggregating Sex  #
#####################
# a4_mf <- dat %>%
#     select(sex, nicht, Unterricht, Arbeitsplatz, Freizeit) %>%
#     group_by(sex) %>%
#     summarise(across(c(nicht, Unterricht, Arbeitsplatz, Freizeit)
#                      ,mean))

###############################################################################
#                                 statistics                                  #
###############################################################################

dat %>%
    nrow()

dat %>%
    subset(kauf_gew == "gew") %>%
    nrow()

# 454

dat %>%
    subset(kauf_gew == "kauf") %>%
    nrow()

# 715
#######################################################
#  Unterschied; zwischen Kaufmännisch vs. Gewerblich  #
#######################################################
## H0 : kauf = gew
## H1 : kauf != gew

##################
#  nutzen nicht  #
##################
##################################
#  Testen von Varianzgleichheit  #
##################################
var.test(dat$nicht ~ dat$kauf_gew)

        # F test to compare two variances

# data:  dat$nicht by dat$kauf_gew
# F = 1.1647, num df = 453, denom df = 714, p-value = 0.0703
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.9874642 1.3781326
# sample estimates:
# ratio of variances
        #   1.164723

# F-Test was not significant, equality of variances can be assumed

t.test(dat$nicht ~ dat$kauf_gew, var.equal=TRUE)

        # Two Sample t-test

# data:  dat$nicht by dat$kauf_gew
# t = -2.9585, df = 1167, p-value = 0.003154
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # -0.13417678 -0.02717377
# sample estimates:
 # mean in group gew mean in group kauf
        #  0.6563877          0.7370629

################
#  Unterricht  #
################
var.test(dat$Unterricht ~ dat$kauf_gew)

        # F test to compare two variances

# data:  dat$Unterricht by dat$kauf_gew
# F = 1.8776, num df = 453, denom df = 714, p-value = 4.885e-14
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 1.591863 2.221648
# sample estimates:
# ratio of variances
        #   1.877616

t.test(dat$Unterricht ~ dat$kauf_gew, var.equal=FALSE)

        # Welch Two Sample t-test

# data:  dat$Unterricht by dat$kauf_gew
# t = 2.6377, df = 756.32, p-value = 0.008519
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # 0.009906372 0.067565055
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.07929515         0.04055944

##################
#  Arbeitsplatz  #
##################
var.test(dat$Arbeitsplatz ~ dat$kauf_gew)

        # F test to compare two variances

# data:  dat$Arbeitsplatz by dat$kauf_gew
# F = 1.3945, num df = 453, denom df = 714, p-value = 7.515e-05
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 1.182269 1.650008
# sample estimates:
# ratio of variances
        #   1.394497

t.test(dat$Arbeitsplatz ~ dat$kauf_gew, var.equal=FALSE)

        # Welch Two Sample t-test

# data:  dat$Arbeitsplatz by dat$kauf_gew
# t = 1.0103, df = 847.92, p-value = 0.3126
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # -0.01024929  0.03199231
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.03744493         0.02657343


##############
#  Freizeit  #
##############
var.test(dat$Freizeit ~ dat$kauf_gew)

        # F test to compare two variances

# data:  dat$Freizeit by dat$kauf_gew
# F = 1.2608, num df = 453, denom df = 714, p-value = 0.005884
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 1.068933 1.491832
# sample estimates:
# ratio of variances
        #   1.260816

t.test(dat$Freizeit ~ dat$kauf_gew, var.equal=TRUE)

        # Two Sample t-test

# data:  dat$Freizeit by dat$kauf_gew
# t = 3.1395, df = 1167, p-value = 0.001735
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # 0.02960451 0.12825877
# sample estimates:
 # mean in group gew mean in group kauf
        #  0.2775330          0.1986014


########################################
#  Unterschied zwischen Geschlechtern  #
########################################
# H0: m = f
# H1: m != f

##################
#  nutzen nicht  #
##################
var.test(dat$nicht ~ dat$sex)

        # F test to compare two variances

# data:  dat$nicht by dat$sex
# F = 1.0023, num df = 518, denom df = 649, p-value = 0.9758
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.8517698 1.1811709
# sample estimates:
# ratio of variances
        #   1.002276

t.test(dat$nicht ~ dat$kauf_gew, var.equal=TRUE)

        # Two Sample t-test

# data:  dat$nicht by dat$kauf_gew
# t = -2.9585, df = 1167, p-value = 0.003154
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # -0.13417678 -0.02717377
# sample estimates:
 # mean in group gew mean in group kauf
        #  0.6563877          0.7370629

################
#  Unterricht  #
################
var.test(dat$Unterricht ~ dat$sex)

        # F test to compare two variances

# data:  dat$Unterricht by dat$sex
# F = 0.57521, num df = 518, denom df = 649, p-value = 6.855e-11
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.4888306 0.6778739
# sample estimates:
# ratio of variances
        #  0.5752062

t.test(dat$Unterricht ~ dat$kauf_gew, var.equal=FALSE)

        # Welch Two Sample t-test

# data:  dat$Unterricht by dat$kauf_gew
# t = 2.6377, df = 756.32, p-value = 0.008519
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # 0.009906372 0.067565055
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.07929515         0.04055944

##################
#  Arbeitsplatz  #
##################
var.test(dat$Arbeitsplatz ~ dat$sex)

        # F test to compare two variances

# data:  dat$Arbeitsplatz by dat$sex
# F = 0.63543, num df = 518, denom df = 649, p-value = 7.918e-08
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.5400070 0.7488415
# sample estimates:
# ratio of variances
        #  0.6354254

t.test(dat$Arbeitsplatz ~ dat$kauf_gew, var.equal=FALSE)

        # Welch Two Sample t-test

# data:  dat$Arbeitsplatz by dat$kauf_gew
# t = 1.0103, df = 847.92, p-value = 0.3126
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # -0.01024929  0.03199231
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.03744493         0.02657343

##############
#  Freizeit  #
##############
var.test(dat$Freizeit ~ dat$sex)

        # F test to compare two variances

# data:  dat$Freizeit by dat$sex
# F = 1.0438, num df = 518, denom df = 649, p-value = 0.6049
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.8870267 1.2300625
# sample estimates:
# ratio of variances
        #   1.043763

t.test(dat$Freizeit ~ dat$kauf_gew, var.equal=TRUE)

        # Two Sample t-test

# data:  dat$Freizeit by dat$kauf_gew
# t = 3.1395, df = 1167, p-value = 0.001735
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # 0.02960451 0.12825877
# sample estimates:
 # mean in group gew mean in group kauf
        #  0.2775330          0.1986014

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

########################
#  geg. im Unterricht  #
########################
dat_1 <-
    dat %>%
        subset(Unterricht == 1)

dat_1 %>%
    group_by(kauf_gew, sex) %>%
        summarise(
        count = n(),
        mean = mean(a6, na.rm = TRUE),
        sd = sd(a6, na.rm = TRUE)
  )
##############################################################
#  Achtung! 6 vs. 8 sind verschieden gepolte Likert Skalen.  #
##############################################################

