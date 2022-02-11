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
    as.character() %>%
    # replace_na("0") %>%
    as.numeric

# 4 = stimme überhaupt nicht zu
# 1 = voll und ganz
# 0 = weiß nicht

########
#  a8  #
########

dat$a8 <-
    dat$a8 %>%
    as.character() %>%
    # replace_na("0") %>%
    as.numeric

# 4 = überhaupt nicht
# 1 = motiviert sehr

# dat$a8 <-
#     dat$a8 %>%
#         as_factor()

dat %>% nrow()

dat %>%
    na.omit() %>%
    nrow()

dat <-
    dat %>%
    na.omit()

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

# unter 16 Jahre     16 bis 18 Jahre     19 bis 21 Jahre     22 bis 24 Jahre     25 bis 27 Jahre 28 Jahre oder älter
#              6                 201                 298                 120                  34                  40

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
# 299 400
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

# 290

dat %>%
    subset(kauf_gew == "kauf") %>%
    nrow()

# 409
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
# F = 1.1221, num df = 289, denom df = 408, p-value = 0.2854
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.908211 1.391904
# sample estimates:
# ratio of variances
        #   1.122127

# F-Test was not significant, equality of variances can be assumed

t.test(dat$nicht ~ dat$kauf_gew, var.equal = TRUE)

        # Two Sample t-test

# data:  dat$nicht by dat$kauf_gew
# t = -2.4548, df = 697, p-value = 0.01434
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # -0.16105837 -0.01791473
# sample estimates:
 # mean in group gew mean in group kauf
        #  0.6000000          0.6894866

################
#  Unterricht  #
################
var.test(dat$Unterricht ~ dat$kauf_gew)

        # F test to compare two variances

# data:  dat$Unterricht by dat$kauf_gew
# F = 1.6808, num df = 289, denom df = 408, p-value = 1.416e-06
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 1.360363 2.084862
# sample estimates:
# ratio of variances
        #   1.680778

t.test(dat$Unterricht ~ dat$kauf_gew, var.equal=FALSE)

        # Welch Two Sample t-test

# data:  dat$Unterricht by dat$kauf_gew
# t = 2.0958, df = 518.86, p-value = 0.03659
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # 0.002803248 0.086733890
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.10344828         0.0586797

##################
#  Arbeitsplatz  #
##################
var.test(dat$Arbeitsplatz ~ dat$kauf_gew)

        # F test to compare two variances

# data:  dat$Arbeitsplatz by dat$kauf_gew
# F = 1.4852, num df = 289, denom df = 408, p-value = 0.0002417
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 1.202074 1.842271
# sample estimates:
# ratio of variances
        #   1.485205

t.test(dat$Arbeitsplatz ~ dat$kauf_gew, var.equal=FALSE)

        # Welch Two Sample t-test

# data:  dat$Arbeitsplatz by dat$kauf_gew
# t = 1.1048, df = 543.13, p-value = 0.2697
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # -0.01361082  0.04859944
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.05172414         0.03422983


##############
#  Freizeit  #
##############
var.test(dat$Freizeit ~ dat$kauf_gew)

        # F test to compare two variances

# data:  dat$Freizeit by dat$kauf_gew
# F = 1.2053, num df = 289, denom df = 408, p-value = 0.08353
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.9755627 1.4951254
# sample estimates:
# ratio of variances
        #   1.205343


t.test(dat$Freizeit ~ dat$kauf_gew, var.equal=TRUE)

        # Two Sample t-test

# data:  dat$Freizeit by dat$kauf_gew
# t = 2.4526, df = 697, p-value = 0.01443
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # 0.01666135 0.15039033
# sample estimates:
 # mean in group gew mean in group kauf
        #  0.3206897          0.2371638


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
# F = 1.0328, num df = 298, denom df = 399, p-value = 0.7615
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.8364136 1.2795743
# sample estimates:
# ratio of variances
        #   1.032816

t.test(dat$nicht ~ dat$kauf_gew, var.equal=TRUE)

        # Two Sample t-test

# data:  dat$nicht by dat$kauf_gew
# t = -2.4548, df = 697, p-value = 0.01434
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # -0.16105837 -0.01791473
# sample estimates:
 # mean in group gew mean in group kauf
        #  0.6000000          0.6894866

################
#  Unterricht  #
################
var.test(dat$Unterricht ~ dat$sex)

        # F test to compare two variances

# data:  dat$Unterricht by dat$sex
# F = 0.69139, num df = 298, denom df = 399, p-value = 0.0007749
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.5599104 0.8565702
# sample estimates:
# ratio of variances
        #  0.6913857

t.test(dat$Unterricht ~ dat$kauf_gew, var.equal=FALSE)

        # Welch Two Sample t-test

# data:  dat$Unterricht by dat$kauf_gew
# t = 2.0958, df = 518.86, p-value = 0.03659
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # 0.002803248 0.086733890
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.10344828         0.05867971

##################
#  Arbeitsplatz  #
##################
var.test(dat$Arbeitsplatz ~ dat$sex)

        # F test to compare two variances

# data:  dat$Arbeitsplatz by dat$sex
# F = 0.52393, num df = 298, denom df = 399, p-value = 5.821e-09
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.4242952 0.6491013
# sample estimates:
# ratio of variances
        #  0.5239259

t.test(dat$Arbeitsplatz ~ dat$kauf_gew, var.equal=FALSE)

        # Welch Two Sample t-test

# data:  dat$Arbeitsplatz by dat$kauf_gew
# t = 1.1048, df = 543.13, p-value = 0.2697
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # -0.01361082  0.04859944
# sample estimates:
 # mean in group gew mean in group kauf
        # 0.05172414         0.03422983

##############
#  Freizeit  #
##############
var.test(dat$Freizeit ~ dat$sex)

        # F test to compare two variances

# data:  dat$Freizeit by dat$sex
# F = 1.0659, num df = 298, denom df = 399, p-value = 0.5521
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
 # 0.8631705 1.3205080
# sample estimates:
# ratio of variances
        #   1.065856

t.test(dat$Freizeit ~ dat$kauf_gew, var.equal=TRUE)

        # Two Sample t-test

# data:  dat$Freizeit by dat$kauf_gew
# t = 2.4526, df = 697, p-value = 0.01443
# alternative hypothesis: true difference in means between group gew and group kauf is not equal to 0
# 95 percent confidence interval:
 # 0.01666135 0.15039033
# sample estimates:
 # mean in group gew mean in group kauf
        #  0.3206897          0.2371638


##########################################################################
#  geg: 4 im Unterricht benutzen, kauf_gew & Sex -> Aufg. 6 und Aufg. 8  #
##########################################################################

########################
#  geg. im Unterricht  #
########################
dat_1 <-
    dat %>%
        subset(Unterricht == 1)

# dat_1 %>%
#     group_by(kauf_gew, sex) %>%
#         summarise(
#         count = n(),
#         mean = mean(a6, na.rm = TRUE),
#         sd = sd(a6, na.rm = TRUE)
#   )

########
#  a6  #
########

a6_anova  <- aov(a6 ~ kauf_gew + sex, dat_1)
summary(a6_anova)

            # Df Sum Sq Mean Sq F value Pr(>F)
# kauf_gew     1    0.3  0.3000   0.411  0.524
# sex          1    0.5  0.5048   0.692  0.409
# Residuals   51   37.2  0.7293

########
#  a8  #
########

a8_anova  <- aov(a8 ~ kauf_gew + sex, dat_1)
summary(a8_anova)

            # Df Sum Sq Mean Sq F value Pr(>F)
# kauf_gew     1   0.01  0.0148   0.023  0.880
# sex          1   0.15  0.1453   0.226  0.637
# Residuals   51  32.82  0.6436

##############################################################
#  Achtung! 6 vs. 8 sind verschieden gepolte Likert Skalen.  #
##############################################################

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
