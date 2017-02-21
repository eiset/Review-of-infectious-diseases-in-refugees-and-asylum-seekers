# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Title: R code for the research project "Review of infectious diseases in
# refugees and asylum seekers – current status and going forward"
# Author: Andreas Halgreen Eiset
# System: x86_64-pc-linux-gnu; Linux 4.2.0-35-generic; R version: 3.3.2
# Packages: dplyr_0.5.0 ; tidyr_0.6.1; ggplot2_2.2.1
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# All code and data are available from github under CC-BY-4.0 license
# From http://choosealicense.com/licenses/cc-by-4.0/:
# "Permits almost any use subject to providing credit and license notice."

# Packages used
lapply(c("dplyr", "tidyr", "ggplot2"), library, character.only = TRUE)

# Read in the data base file (.csv). Attention: This will open an internet
# connection to the GitHub repo
dataurl <- file.path("https://raw.githubusercontent.com/eiset/Review-of-infectious-diseases-in-refugees-and-asylum-seekers/master/data_base.csv")
dta <- read.csv(dataurl, strip.white = TRUE, na.strings = c("NA", ""))

# Initial data management -------------------------------------------------

dta$id <- as.character(levels(dta$id))[dta$id]
dta$aim <- as.character(levels(dta$aim))[dta$aim]
dta$results <- as.character(levels(dta$results))[dta$results]

all.inc <- filter(dta, ! is.na(inc.screen))
all.inc.read <- filter(dta, inc.final == 1)

# Tidying up --------------------------------------------------------------

intrm1 <- gather(all.inc.read, tmp, est, est.1:est.6) %>%
        separate(tmp, into = c("tmp", "obs.nr"), sep = "\\.") %>%
        select(- tmp)

intrm2 <- gather(all.inc.read, tmp, dis, dis.1:dis.6) %>%
        separate(tmp, into = c("tmp", "obs.nr"), sep = "\\.") %>%
        select(- tmp)

dta <- full_join(intrm1, intrm2) %>%
        select(- contains("est.")) %>%
        select(- contains("dis."))

dta$obs.nr <- factor(dta$obs.nr)
dta$id <- factor(dta$id)
dta$dis <- factor(dta$dis)

rm(intrm1, intrm2, dataurl)

# Summaries for the flow chart (fig 1) -------------------------------------

# Step 1: total number of articles screened
table(all.inc$found.by.ref, useNA = c("always"))
# "1" = article found by searching reference list of other article

# excluding those found by searching reference list of other article
datbas.srch <- filter(all.inc, found.by.ref == 0)

# Step 2: Number excluded after screening
table(datbas.srch$inc.screen, datbas.srch$inc.final)
# "0, 0" = those that were excluded directly after screening (where the two
# first authors agreed)

# Reasons for exclusion
excl.1 <- filter(datbas.srch, inc.screen == 0)
table(excl.1$reason)
# Subsets
# not relevant stratification
filter(excl.1, reason == 1) %>% summarise(n())
# not a relevant study population
filter(excl.1, reason == 2) %>% summarise(n())
# not relevant design:
filter(excl.1, reason == 3) %>% summarise(n())
# unable to obtain further information:
filter(excl.1, reason == 4) %>% summarise(n())

# Step 3: Number of articles included for full assessment after initial data base
# search
table(datbas.srch$inc.screen)
# "1" are articles read in full

# Step 4: Studies found in reference lists
excl.2 <- filter(all.inc, found.by.ref == 1)
# number and reason for exclusion of these articles
table(excl.2$inc.screen, excl.2$reason, useNA = "always")
# "NA" (horisontal) is number of articles included.

# Step 5: Articles read in full
excl.3 <- filter(all.inc, inc.screen == 1)
# Number of articles still included after reading through the full article:
nrow(excl.3)

# Reasons for exclusion:
table(excl.3$reason)

# Step 6: Articles included for analysis, stratified on coding system
table(all.inc$inc.final)

rm(excl.1, excl.2, excl.3, datbas.srch, all.inc, all.inc.srch)


# Plot (fig 2) --------------------------------------------------------------------

# Data frame for plot
dtap <- dta[!(is.na(dta$measure) | dta$measure == ""), ]
dtap$study.site <- as.character(levels(dtap$study.site))[dtap$study.site]

dtap <- dtap %>%
        filter(!dis %in% c("", "chlamydis", "entamoeba_histolytica",
                                 "chlamydia", "giardia_intestinalis", "gonorrhea",
                                 "leishmaniasis", "malaria", "syphilis")) %>%
        filter(measure == "prev") %>%
        mutate(est = est * 100) %>%
        mutate(study.site = ifelse(is.na(study.site), "Not stated", study.site)) %>%
        mutate(dis = gsub("ltbi", "Latent tuberculosis", .$dis)) %>%
        mutate(dis = gsub("tb", "Tuberculosis", .$dis)) %>%
        mutate(dis = gsub("hiv", "HIV", .$dis)) %>%
        mutate(dis = gsub("hcv", "Hepatitis C", .$dis)) %>%
        mutate(dis = gsub("hbv", "Hepatitis B", .$dis)) %>%
        mutate(mig.def = ifelse(.$mig.def1 == 0, "Not specified", ifelse(
                        .$mig.def1 == 1, "Country of birth", ifelse(
                                .$mig.def1 == 2, "Refugee", ifelse(
                                        .$mig.def1 == 3, "Family reunified", ifelse(
                                                .$mig.def1 == 4, "Asylum seeker", ifelse(
                                                        .$mig.def1 == 5, "Border-crosser", "ERROR"
                                                        )
                                                )
                                        )
                                )
                        )
                )
               )

g <- ggplot(dtap, aes(dis, est)) +
        geom_point(aes(colour = study.site, shape = mig.def), size = 5, alpha = 0.8) +
        scale_colour_brewer(palette = "Paired", name = "Study site") +
        scale_shape_discrete(name = "Migrant sub-population") +
        labs(x = "Disease", y = "Prevalence estimate, (%)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
g


