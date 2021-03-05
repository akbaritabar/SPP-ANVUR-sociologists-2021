# ============================
#### R script to replicate analysis ####
# ============================
options(scipen = 999)
require(pander)
require(knitr)
require(tidyverse)
require(stargazer)
require(lme4)
require(car)
require(kableExtra)
require(texreg)
require(broom)
require(broom.mixed)
require(dotwhisker)
require(brms)
# for stargazer tables (here PDF format)
rmd_format_take <- "latex"

# ============================
#### Read & prepare data ####
# ============================
anvur <- read_csv("./DPANVREP.csv") %>% 
  mutate(level_changed = factor(level_changed, levels = c('same_level', 'not_same_level')),
         universityrank = factor(universityrank, levels = c('low', 'medium', 'high')))

anvur_wide <- anvur %>% 
  select(everything(), -c(year:int_score, c_avg, fascia_journal, fascia_pre_post, proportion_fascia)) %>%
  distinct(joinname_author, ANVUR, .keep_all = T) %>%
  pivot_wider(names_from = ANVUR, values_from = c(fss_pre_post, pub_pre_post, cit_pre_post, int_pre_post, coauthors_pre_post))

anvur_long <- anvur %>% 
  select(everything(), -c(year:int_score, c_avg, fascia_journal, fascia_pre_post, proportion_fascia)) %>% 
  distinct(joinname_author, ANVUR, .keep_all = T)

anvur_long_fascia <- anvur %>% 
  select(everything(), -c(year:int_score, c_avg)) %>% 
  distinct(joinname_author, ANVUR, fascia_journal, .keep_all = T) %>% 
  mutate(ANVUR = factor(ANVUR, levels = c('Before_ANVUR', 'Post_ANVUR'), ordered = T),
         fascia_journal = factor(fascia_journal, levels = c('not_fascia_A', 'fascia_A'), ordered = T),
         id = as.factor(id),
         uniDept = as.factor(uniDept),
         fasciaA = if_else(fascia_journal == "fascia_A", 1, 0))

# ============================
#### Fig 1 ####
# ============================
anvur %>% 
  distinct(joinname_author, .keep_all = T) %>% 
  ggplot(aes(x = total_pub)) +
  geom_bar(stat = 'count') +
  labs(x = 'Number of publications', y = 'Authors count') +
  theme_bw() + scale_fill_grey() +
  theme(legend.position = "none", axis.text.x = element_text(hjust = 1, size = 15), axis.text.y = element_text(hjust = 1, size = 15), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

# ============================
#### Tab 1 ####
# ============================
geo_report <- anvur_wide %>% 
  group_by(georegion) %>% 
  summarise(variable = "Geo Region", meanFSS_bf = mean(fss_pre_post_Before_ANVUR, na.rm = T), medianFSS_bf = median(fss_pre_post_Before_ANVUR, na.rm = T), meanPub_bf = mean(pub_pre_post_Before_ANVUR, na.rm = T), medianPub_bf = median(pub_pre_post_Before_ANVUR, na.rm = T), meanFSS_af = mean(fss_pre_post_Post_ANVUR, na.rm = T), medianFSS_af = median(fss_pre_post_Post_ANVUR, na.rm = T), meanPub_af = mean(pub_pre_post_Post_ANVUR, na.rm = T), medianPub_af = median(pub_pre_post_Post_ANVUR, na.rm = T)) %>% 
  select(mainvar = georegion, everything())

level_report <- anvur_wide %>% 
  group_by(level_changed) %>% 
  summarise(variable = "Level change", meanFSS_bf = mean(fss_pre_post_Before_ANVUR, na.rm = T), medianFSS_bf = median(fss_pre_post_Before_ANVUR, na.rm = T), meanPub_bf = mean(pub_pre_post_Before_ANVUR, na.rm = T), medianPub_bf = median(pub_pre_post_Before_ANVUR, na.rm = T), meanFSS_af = mean(fss_pre_post_Post_ANVUR, na.rm = T), medianFSS_af = median(fss_pre_post_Post_ANVUR, na.rm = T), meanPub_af = mean(pub_pre_post_Post_ANVUR, na.rm = T), medianPub_af = median(pub_pre_post_Post_ANVUR, na.rm = T)) %>% 
  select(mainvar = level_changed, everything())

level_report2 <- anvur_wide %>% 
  group_by(level) %>% 
  summarise(variable = "Level", meanFSS_bf = mean(fss_pre_post_Before_ANVUR, na.rm = T), medianFSS_bf = median(fss_pre_post_Before_ANVUR, na.rm = T), meanPub_bf = mean(pub_pre_post_Before_ANVUR, na.rm = T), medianPub_bf = median(pub_pre_post_Before_ANVUR, na.rm = T), meanFSS_af = mean(fss_pre_post_Post_ANVUR, na.rm = T), medianFSS_af = median(fss_pre_post_Post_ANVUR, na.rm = T), meanPub_af = mean(pub_pre_post_Post_ANVUR, na.rm = T), medianPub_af = median(pub_pre_post_Post_ANVUR, na.rm = T)) %>% 
  select(mainvar = level, everything())

uni_report <- anvur_wide %>% 
  group_by(universityrank) %>% 
  summarise(variable = "University Rank", meanFSS_bf = mean(fss_pre_post_Before_ANVUR, na.rm = T), medianFSS_bf = median(fss_pre_post_Before_ANVUR, na.rm = T), meanPub_bf = mean(pub_pre_post_Before_ANVUR, na.rm = T), medianPub_bf = median(pub_pre_post_Before_ANVUR, na.rm = T), meanFSS_af = mean(fss_pre_post_Post_ANVUR, na.rm = T), medianFSS_af = median(fss_pre_post_Post_ANVUR, na.rm = T), meanPub_af = mean(pub_pre_post_Post_ANVUR, na.rm = T), medianPub_af = median(pub_pre_post_Post_ANVUR, na.rm = T)) %>% 
  select(mainvar = universityrank, everything())

sec_report <- anvur_wide %>% 
  group_by(sector) %>% 
  summarise(variable = "Sector", meanFSS_bf = mean(fss_pre_post_Before_ANVUR, na.rm = T), medianFSS_bf = median(fss_pre_post_Before_ANVUR, na.rm = T), meanPub_bf = mean(pub_pre_post_Before_ANVUR, na.rm = T), medianPub_bf = median(pub_pre_post_Before_ANVUR, na.rm = T), meanFSS_af = mean(fss_pre_post_Post_ANVUR, na.rm = T), medianFSS_af = median(fss_pre_post_Post_ANVUR, na.rm = T), meanPub_af = mean(pub_pre_post_Post_ANVUR, na.rm = T), medianPub_af = median(pub_pre_post_Post_ANVUR, na.rm = T)) %>% 
  select(mainvar = sector, everything())

dep_report <- anvur_wide %>%
  filter(!is.na(department)) %>% 
  group_by(department) %>% 
  summarise(variable = "Department", meanFSS_bf = mean(fss_pre_post_Before_ANVUR, na.rm = T), medianFSS_bf = median(fss_pre_post_Before_ANVUR, na.rm = T), meanPub_bf = mean(pub_pre_post_Before_ANVUR, na.rm = T), medianPub_bf = median(pub_pre_post_Before_ANVUR, na.rm = T), meanFSS_af = mean(fss_pre_post_Post_ANVUR, na.rm = T), medianFSS_af = median(fss_pre_post_Post_ANVUR, na.rm = T), meanPub_af = mean(pub_pre_post_Post_ANVUR, na.rm = T), medianPub_af = median(pub_pre_post_Post_ANVUR, na.rm = T)) %>% 
  select(mainvar = department, everything())

bind_rows(list(geo_report, uni_report, sec_report, dep_report, level_report2, level_report)) %>% 
  select(variable, mainvar, meanFSS_bf, meanFSS_af, medianFSS_bf, medianFSS_af, meanPub_bf, meanPub_af, medianPub_bf, medianPub_af) %>% 
  kable(caption = 'Descriptive statistics of the total number of publications and FSS, BF = Before ANVUR, AF = After ANVUR', format.args = list(big.mark = ","), digits = 3, col.names = c("Main Cat", "Sub Cat", "BF", "AF", "BF", "AF", "BF", "AF", "BF", "AF")) %>% 
  add_header_above(c(" " = 2, "Average FSS" = 2, "Median FSS" = 2, "Average pubs" = 2, "Median pubs" = 2)) %>%
  kable_styling(font_size = 9, latex_options = "striped")

# ============================
#### Base line models ####
# ============================
lme.fssr_baseline <- lmer(fss_pre_post ~ ANVUR + (1 | uniDept / id), data = anvur_long)

lme.citations_baseline <- lmer(data = anvur_long, cit_pre_post ~ ANVUR + (1 | uniDept / id))

lme.internationalization_baseline <- lmer(data = anvur_long, int_pre_post ~ ANVUR + (1 | uniDept / id))

lme.number_coauthors_baseline <- lmer(data = anvur_long, coauthors_pre_post ~ ANVUR + (1 | uniDept / id))

lme.papers_baseline <- glmer.nb(data = anvur_long, pub_pre_post ~ ANVUR + (1 | uniDept / id))

# ============================
#### FSS models ####
# ============================
lme.fssr.gender <- lmer(data = anvur_long, fss_pre_post ~ gender*ANVUR + (1 | uniDept / id))

lme.fssr.level <- lmer(data = anvur_long, fss_pre_post ~ level*ANVUR + (1 | uniDept / id))

lme.fssr.level_changed <- lmer(data = anvur_long, fss_pre_post ~ level_changed*ANVUR + (1 | uniDept / id))

lme.fssr.level_2010 <- lmer(data = anvur_long, fss_pre_post ~ level_2010*ANVUR + (1 | uniDept / id))

lme.fssr.sector <- lmer(data = anvur_long, fss_pre_post ~ sector*ANVUR + (1 | uniDept / id))

lme.fssr.universityrank <- lmer(data = anvur_long, fss_pre_post ~ universityrank*ANVUR + (1 | uniDept / id))

lme.fssr.georegion <- lmer(data = anvur_long, fss_pre_post ~ georegion*ANVUR + (1 | uniDept / id))

lme.fssr.department <- lmer(data = anvur_long, fss_pre_post ~ department*ANVUR + (1 | uniDept / id))

lme.fssr.full.model <- lmer(data = anvur_long, fss_pre_post ~ (gender+sector+level+level_2010+level_changed+universityrank+georegion+department)*ANVUR + (1 | uniDept / id))

if (rmd_format_take == "html") {
  htmlreg(list(lme.fssr.gender, lme.fssr.sector, lme.fssr.level, lme.fssr.level_2010, lme.fssr.level_changed, lme.fssr.universityrank, lme.fssr.georegion, lme.fssr.department, lme.fssr.full.model), caption = "Comparative table of repeated measures mixed effect analysis to check overall ANVUR effect on research productivity (Dependent variable FSS)", custom.coef.names = c("Constant", "Gender M", "Post-ANVUR", paste0("Gender M ", "\u00D7" ," post-ANVUR"), "SPS/08", "SPS/09", "SPS/10", "SPS/11", "SPS/12", paste0("SPS/08 ", "\u00D7" ," post-ANVUR"), paste0("SPS/09 ", "\u00D7" ," post-ANVUR"), paste0("SPS/10 ", "\u00D7" ," post-ANVUR"), paste0("SPS/11 ", "\u00D7" ," post-ANVUR"), paste0("SPS/12 ", "\u00D7" ," post-ANVUR"), "Associate professors", "Full professors", paste0("Associate prof. ", "\u00D7" ," post-ANVUR"), paste0("Full prof. ", "\u00D7" ," post-ANVUR"), "Associate prof. 2010", "Full prof. 2010", "Unknown 2010", paste0("Associate prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Full prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Unknown 2010 ", "\u00D7" ," post-ANVUR"), "Level changed from 2010", paste0("Level changed ", "\u00D7" ," post-ANVUR"), "Medium rank uni",  "High rank uni", paste0("Medium rank uni ", "\u00D7" ," post-ANVUR"), paste0("High rank uni ", "\u00D7" ," post-ANVUR"), "Isolated", "North", "South", paste0("Isolated ", "\u00D7" ," post-ANVUR"), paste0("North ", "\u00D7" ," post-ANVUR"), paste0("South ", "\u00D7" ," post-ANVUR"), "Economics", "Engineering", "Humanities", "Medicine", "Other", "Political Sci.", "Psychology", "Social Sci.", "Sociology", paste0("Economics ", "\u00D7" ," post-ANVUR"), paste0("Engineering ", "\u00D7" ," post-ANVUR"), paste0("Humanities ", "\u00D7" ," post-ANVUR"), paste0("Medicine ", "\u00D7" ," post-ANVUR"), paste0("Other ", "\u00D7" ," post-ANVUR"), paste0("Political Sci. ", "\u00D7" ," post-ANVUR"), paste0("Psychology ", "\u00D7" ," post-ANVUR"), paste0("Social Sci. ", "\u00D7" ," post-ANVUR"), paste0("Sociology ", "\u00D7" ," post-ANVUR")), custom.model.names = c("Gender", "Sector", "Level", "Level 2010", "Level Change", "Uni Rank", "Georegion", "Department", "Full model"), single.row = T, label = "tab:repeated-measures-models-FSS-1", caption.above = T)
  
} else if (rmd_format_take == "latex") {
  
  # in order to rotate the big tables of results in the latex (pdf) output of Rmarkdown, I searched and learnt how to add two latex packages to "premeable.tex" file like this "\usepackage{rotating, graphicx}" and in the stargazer function I needed to add argument "float.env = "sidewaystable"" and voila!
  texreg(list(lme.fssr.gender, lme.fssr.sector, lme.fssr.level, lme.fssr.level_2010, lme.fssr.level_changed, lme.fssr.universityrank, lme.fssr.georegion, lme.fssr.department, lme.fssr.full.model), custom.coef.names = c("Constant", "Gender M", "Post-ANVUR", paste0("Gender M ", "\u00D7" ," post-ANVUR"), "SPS/08", "SPS/09", "SPS/10", "SPS/11", "SPS/12", paste0("SPS/08 ", "\u00D7" ," post-ANVUR"), paste0("SPS/09 ", "\u00D7" ," post-ANVUR"), paste0("SPS/10 ", "\u00D7" ," post-ANVUR"), paste0("SPS/11 ", "\u00D7" ," post-ANVUR"), paste0("SPS/12 ", "\u00D7" ," post-ANVUR"), "Associate professors", "Full professors", paste0("Associate prof. ", "\u00D7" ," post-ANVUR"), paste0("Full prof. ", "\u00D7" ," post-ANVUR"), "Associate prof. 2010", "Full prof. 2010", "Unknown 2010", paste0("Associate prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Full prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Unknown 2010 ", "\u00D7" ," post-ANVUR"), "Level changed from 2010", paste0("Level changed ", "\u00D7" ," post-ANVUR"), "Medium rank uni",  "High rank uni", paste0("Medium rank uni ", "\u00D7" ," post-ANVUR"), paste0("High rank uni ", "\u00D7" ," post-ANVUR"), "Isolated", "North", "South", paste0("Isolated ", "\u00D7" ," post-ANVUR"), paste0("North ", "\u00D7" ," post-ANVUR"), paste0("South ", "\u00D7" ," post-ANVUR"), "Economics", "Engineering", "Humanities", "Medicine", "Other", "Political Sci.", "Psychology", "Social Sci.", "Sociology", paste0("Economics ", "\u00D7" ," post-ANVUR"), paste0("Engineering ", "\u00D7" ," post-ANVUR"), paste0("Humanities ", "\u00D7" ," post-ANVUR"), paste0("Medicine ", "\u00D7" ," post-ANVUR"), paste0("Other ", "\u00D7" ," post-ANVUR"), paste0("Political Sci. ", "\u00D7" ," post-ANVUR"), paste0("Psychology ", "\u00D7" ," post-ANVUR"), paste0("Social Sci. ", "\u00D7" ," post-ANVUR"), paste0("Sociology ", "\u00D7" ," post-ANVUR")), custom.model.names = c("Gender", "Sector", "Level", "Level 2010", "Level Change", "Uni Rank", "Georegion", "Department", "Full model"), caption = "Comparative table of repeated measures mixed effect analysis to check overall ANVUR effect on research productivity (Dependent variable FSS)", single.row = T, label = "tab:repeated-measures-models-FSS-1", caption.above = T, fontsize = 'tiny', table = TRUE, sideways = TRUE, use.packages	= FALSE)
  
}

# ============================
#### Fig 2 Base line models Plot ####
# ============================
all_baselines <- bind_rows(tidy(lme.fssr_baseline) %>% mutate(model = "FSS") %>% filter(term == 'ANVURPost_ANVUR'), 
                           tidy(lme.citations_baseline) %>% mutate(model = "Citations") %>% filter(term == 'ANVURPost_ANVUR'),
                           tidy(lme.internationalization_baseline) %>% mutate(model = "Internationalisation") %>% filter(term == 'ANVURPost_ANVUR'),
                           tidy(lme.number_coauthors_baseline) %>% mutate(model = "Coauthors") %>% filter(term == 'ANVURPost_ANVUR'),
                           tidy(lme.papers_baseline) %>% mutate(model = "Publications") %>% select(everything(), -`p.value`) %>% filter(term == 'ANVURPost_ANVUR')) %>% mutate(term = 'Post_ANVUR') 

dwplot(all_baselines, effects="fixed") +
  theme(legend.title=element_blank(), panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_line(colour="grey"))

# ============================
#### Publication count models ####
# ============================
lme.papers <- lmer(data = anvur_long, pub_pre_post ~ ANVUR + (1 | uniDept / id))

lme.papers.gender <- lmer(data = anvur_long, pub_pre_post ~ gender*ANVUR + (1 | uniDept / id))

lme.papers.level <- lmer(data = anvur_long, pub_pre_post ~ level*ANVUR + (1 | uniDept / id))

lme.papers.level_changed <- lmer(data = anvur_long, pub_pre_post ~ level_changed*ANVUR + (1 | uniDept / id))

lme.papers.level_2010 <- lmer(data = anvur_long, pub_pre_post ~ level_2010*ANVUR + (1 | uniDept / id))

lme.papers.sector <- lmer(data = anvur_long, pub_pre_post ~ sector*ANVUR + (1 | uniDept / id))

lme.papers.universityrank <- lmer(data = anvur_long, pub_pre_post ~ universityrank*ANVUR + (1 | uniDept / id))

lme.papers.georegion <- lmer(data = anvur_long, pub_pre_post ~ georegion*ANVUR + (1 | uniDept / id))

lme.papers.department <- lmer(data = anvur_long, pub_pre_post ~ department*ANVUR + (1 | uniDept / id))

lme.papers.full.model <- lmer(data = anvur_long, pub_pre_post ~ (gender+sector+level+level_2010+level_changed+universityrank+georegion+department)*ANVUR + (1 | uniDept / id))

if (rmd_format_take == "html") {
  
  htmlreg(list(lme.papers.gender, lme.papers.sector, lme.papers.level, lme.papers.level_2010, lme.papers.level_changed, lme.papers.universityrank, lme.papers.georegion, lme.papers.department, lme.papers.full.model), custom.coef.names = c("Constant", "Gender M", "Post-ANVUR", paste0("Gender M ", "\u00D7" ," post-ANVUR"), "SPS/08", "SPS/09", "SPS/10", "SPS/11", "SPS/12", paste0("SPS/08 ", "\u00D7" ," post-ANVUR"), paste0("SPS/09 ", "\u00D7" ," post-ANVUR"), paste0("SPS/10 ", "\u00D7" ," post-ANVUR"), paste0("SPS/11 ", "\u00D7" ," post-ANVUR"), paste0("SPS/12 ", "\u00D7" ," post-ANVUR"), "Associate professors", "Full professors", paste0("Associate prof. ", "\u00D7" ," post-ANVUR"), paste0("Full prof. ", "\u00D7" ," post-ANVUR"), "Associate prof. 2010", "Full prof. 2010", "Unknown 2010", paste0("Associate prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Full prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Unknown 2010 ", "\u00D7" ," post-ANVUR"), "Level changed from 2010", paste0("Level changed ", "\u00D7" ," post-ANVUR"), "Medium rank uni",  "High rank uni", paste0("Medium rank uni ", "\u00D7" ," post-ANVUR"), paste0("High rank uni ", "\u00D7" ," post-ANVUR"), "Isolated", "North", "South", paste0("Isolated ", "\u00D7" ," post-ANVUR"), paste0("North ", "\u00D7" ," post-ANVUR"), paste0("South ", "\u00D7" ," post-ANVUR"), "Economics", "Engineering", "Humanities", "Medicine", "Other", "Political Sci.", "Psychology", "Social Sci.", "Sociology", paste0("Economics ", "\u00D7" ," post-ANVUR"), paste0("Engineering ", "\u00D7" ," post-ANVUR"), paste0("Humanities ", "\u00D7" ," post-ANVUR"), paste0("Medicine ", "\u00D7" ," post-ANVUR"), paste0("Other ", "\u00D7" ," post-ANVUR"), paste0("Political Sci. ", "\u00D7" ," post-ANVUR"), paste0("Psychology ", "\u00D7" ," post-ANVUR"), paste0("Social Sci. ", "\u00D7" ," post-ANVUR"), paste0("Sociology ", "\u00D7" ," post-ANVUR")), custom.model.names = c("Gender", "Sector", "Level", "Level 2010", "Level Change", "Uni Rank", "Georegion", "Department", "Full model"), caption = "Comparative table of repeated measures mixed effect analysis to check overall ANVUR effect on research productivity (Dependent variable number of papers published)", single.row = T, label = "tab:repeated-measures-models-papers", caption.above = T)
  
} else if (rmd_format_take == "latex") {
  
  texreg(list(lme.papers.gender, lme.papers.sector, lme.papers.level, lme.papers.level_2010, lme.papers.level_changed, lme.papers.universityrank, lme.papers.georegion, lme.papers.department, lme.papers.full.model), custom.coef.names = c("Constant", "Gender M", "Post-ANVUR", paste0("Gender M ", "\u00D7" ," post-ANVUR"), "SPS/08", "SPS/09", "SPS/10", "SPS/11", "SPS/12", paste0("SPS/08 ", "\u00D7" ," post-ANVUR"), paste0("SPS/09 ", "\u00D7" ," post-ANVUR"), paste0("SPS/10 ", "\u00D7" ," post-ANVUR"), paste0("SPS/11 ", "\u00D7" ," post-ANVUR"), paste0("SPS/12 ", "\u00D7" ," post-ANVUR"), "Associate professors", "Full professors", paste0("Associate prof. ", "\u00D7" ," post-ANVUR"), paste0("Full prof. ", "\u00D7" ," post-ANVUR"), "Associate prof. 2010", "Full prof. 2010", "Unknown 2010", paste0("Associate prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Full prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Unknown 2010 ", "\u00D7" ," post-ANVUR"), "Level changed from 2010", paste0("Level changed ", "\u00D7" ," post-ANVUR"), "Medium rank uni",  "High rank uni", paste0("Medium rank uni ", "\u00D7" ," post-ANVUR"), paste0("High rank uni ", "\u00D7" ," post-ANVUR"), "Isolated", "North", "South", paste0("Isolated ", "\u00D7" ," post-ANVUR"), paste0("North ", "\u00D7" ," post-ANVUR"), paste0("South ", "\u00D7" ," post-ANVUR"), "Economics", "Engineering", "Humanities", "Medicine", "Other", "Political Sci.", "Psychology", "Social Sci.", "Sociology", paste0("Economics ", "\u00D7" ," post-ANVUR"), paste0("Engineering ", "\u00D7" ," post-ANVUR"), paste0("Humanities ", "\u00D7" ," post-ANVUR"), paste0("Medicine ", "\u00D7" ," post-ANVUR"), paste0("Other ", "\u00D7" ," post-ANVUR"), paste0("Political Sci. ", "\u00D7" ," post-ANVUR"), paste0("Psychology ", "\u00D7" ," post-ANVUR"), paste0("Social Sci. ", "\u00D7" ," post-ANVUR"), paste0("Sociology ", "\u00D7" ," post-ANVUR")), custom.model.names = c("Gender", "Sector", "Level", "Level 2010", "Level Change", "Uni Rank", "Georegion", "Department", "Full model"), caption = "Comparative table of repeated measures mixed effect analysis to check overall ANVUR effect on research productivity (Dependent variable number of papers published)", single.row = T, label = "tab:repeated-measures-models-papers", caption.above = T, fontsize = 'tiny', table = TRUE, sideways = TRUE, use.packages	= FALSE)
}

# ============================
#### Fascia logistic models ####
# ============================
gm.fascia.papers.baseline <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR  + (1 | uniDept / id), family=binomial)

gm.fascia.papers.gender <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR + gender + (1 | uniDept / id), family=binomial)

gm.fascia.papers.level <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR + level + (1 | uniDept / id), family=binomial)

gm.fascia.papers.level_changed <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR + level_changed + (1 | uniDept / id), family=binomial)

gm.fascia.papers.level_2010 <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR + level_2010 + (1 | uniDept / id), family=binomial)

gm.fascia.papers.sector <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR + sector + (1 | uniDept / id), family=binomial)

gm.fascia.papers.universityrank <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR + universityrank + (1 | uniDept / id), family=binomial)

gm.fascia.papers.georegion <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR + georegion + (1 | uniDept / id), family=binomial)

gm.fascia.papers.department <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR + department + (1 | uniDept / id), family=binomial)

gm.fascia.papers.full.model <- glmer(data = anvur_long_fascia, fasciaA ~ ANVUR + gender + sector + level + level_2010 + level_changed + universityrank + georegion + department + (1 | uniDept / id), family=binomial)

if (rmd_format_take == "html") {
  
  htmlreg(list(gm.fascia.papers.baseline, gm.fascia.papers.gender, gm.fascia.papers.sector, gm.fascia.papers.level, gm.fascia.papers.level_2010, gm.fascia.papers.level_changed, gm.fascia.papers.universityrank, gm.fascia.papers.georegion, gm.fascia.papers.department, gm.fascia.papers.full.model), custom.coef.names = c("Constant", "Post-ANVUR", "Gender M", "SPS/08", "SPS/09", "SPS/10", "SPS/11", "SPS/12", "Associate professors", "Full professors", "Associate prof. 2010", "Full prof. 2010", "Unknown 2010", "Level changed from 2010", "Medium rank uni",  "High rank uni", "Isolated", "North", "South", "Economics", "Engineering", "Humanities", "Medicine", "Other", "Political Sci.", "Psychology", "Social Sci.", "Sociology"), custom.model.names = c("Baseline", "Gender", "Sector", "Level", "Level 2010", "Level Change", "Uni Rank", "Georegion", "Department", "Full model"), caption = "Logistic regression models with differing control variables on publications in fascia A journals", single.row = T, label = "tab:logistic-regression-models-of-fascia-A-papers", caption.above = T)
  
} else if (rmd_format_take == "latex") {
  
  texreg(list(gm.fascia.papers.baseline, gm.fascia.papers.gender, gm.fascia.papers.sector, gm.fascia.papers.level, gm.fascia.papers.level_2010, gm.fascia.papers.level_changed, gm.fascia.papers.universityrank, gm.fascia.papers.georegion, gm.fascia.papers.department, gm.fascia.papers.full.model), custom.coef.names = c("Constant", "Post-ANVUR", "Gender M", "SPS/08", "SPS/09", "SPS/10", "SPS/11", "SPS/12", "Associate professors", "Full professors", "Associate prof. 2010", "Full prof. 2010", "Unknown 2010", "Level changed from 2010", "Medium rank uni",  "High rank uni", "Isolated", "North", "South", "Economics", "Engineering", "Humanities", "Medicine", "Other", "Political Sci.", "Psychology", "Social Sci.", "Sociology"), custom.model.names = c("Baseline", "Gender", "Sector", "Level", "Level 2010", "Level Change", "Uni Rank", "Georegion", "Department", "Full model"), caption = "Logistic regression models with differing control variables on publications in fascia A journals", single.row = T, label = "tab:logistic-regression-models-of-fascia-A-papers", fontsize = 'tiny', table = TRUE, sideways = TRUE, use.packages = FALSE, caption.above = T)
}

# ============================
#### Bayesian models ####
# ============================
bm1 <- brm(fasciaA ~ ANVUR + (1 | uniDept / id), data=anvur_long_fascia, family="bernoulli", chains=4, cores=4)

bm2 <- update(bm1, formula. = ~ . + gender + level + sector, chains=4, cores=4, newdata=anvur_long_fascia)

plot(bm2)

# ============================
#### App 1, base line models ####
# ============================
if (rmd_format_take == "html") {
  
  htmlreg(list(lme.fssr_baseline, lme.citations_baseline, lme.internationalization_baseline, lme.number_coauthors_baseline, lme.papers_baseline), caption = "Comparative table of baseline repeated measures mixed effect analysis to check overall ANVUR effect on research productivity (Different dependent variables presented)", custom.coef.names = c("Constant", "Post-ANVUR"), custom.model.names = c("FSS", "Citations", "Internationalisation", "Coauthors", "Publications"), single.row = T, label = "tab:repeated-measures-null-models", caption.above = T)
  
} else if (rmd_format_take == "latex") {
  
  texreg(list(lme.fssr_baseline, lme.citations_baseline, lme.internationalization_baseline, lme.number_coauthors_baseline, lme.papers_baseline), caption = "Comparative table of baseline repeated measures mixed effect analysis to check overall ANVUR effect on research productivity (Different dependent variables presented)", custom.coef.names = c("Constant", "Post-ANVUR"), custom.model.names = c("FSS", "Citations", "Internationalisation", "Coauthors", "Publications"), single.row = T, label = "tab:repeated-measures-null-models", caption.above = T, fontsize = 'small')
  
}

# ============================
#### App 2, Negative Binomial models ####
# ============================
nb.lme.papers <- glmer.nb(data = anvur_long, pub_pre_post ~ ANVUR + (1 | uniDept / id))

nb.lme.papers.gender <- glmer.nb(data = anvur_long, pub_pre_post ~ gender*ANVUR + (1 | uniDept / id))

nb.lme.papers.level <- glmer.nb(data = anvur_long, pub_pre_post ~ level*ANVUR + (1 | uniDept / id))

nb.lme.papers.level_changed <- glmer.nb(data = anvur_long, pub_pre_post ~ level_changed*ANVUR + (1 | uniDept / id))

nb.lme.papers.level_2010 <- glmer.nb(data = anvur_long, pub_pre_post ~ level_2010*ANVUR + (1 | uniDept / id))

nb.lme.papers.sector <- glmer.nb(data = anvur_long, pub_pre_post ~ sector*ANVUR + (1 | uniDept / id))

nb.lme.papers.universityrank <- glmer.nb(data = anvur_long, pub_pre_post ~ universityrank*ANVUR + (1 | uniDept / id))

nb.lme.papers.georegion <- glmer.nb(data = anvur_long, pub_pre_post ~ georegion*ANVUR + (1 | uniDept / id))

nb.lme.papers.department <- glmer.nb(data = anvur_long, pub_pre_post ~ department*ANVUR + (1 | uniDept / id))

if (rmd_format_take == "html") {
  
  htmlreg(list(nb.lme.papers, nb.lme.papers.gender, nb.lme.papers.sector, nb.lme.papers.level, nb.lme.papers.level_2010, nb.lme.papers.level_changed, nb.lme.papers.universityrank, nb.lme.papers.georegion, nb.lme.papers.department), caption = "Comparative table of repeated measures negative binomial models to check overall ANVUR effect on research productivity (Dependent variable count of total papers published)", custom.coef.names = c("Constant", "Post-ANVUR", "Gender M", paste0("Gender M ", "\u00D7" ," post-ANVUR"), "SPS/08", "SPS/09", "SPS/10", "SPS/11", "SPS/12", paste0("SPS/08 ", "\u00D7" ," post-ANVUR"), paste0("SPS/09 ", "\u00D7" ," post-ANVUR"), paste0("SPS/10 ", "\u00D7" ," post-ANVUR"), paste0("SPS/11 ", "\u00D7" ," post-ANVUR"), paste0("SPS/12 ", "\u00D7" ," post-ANVUR"), "Associate professors", "Full professors", paste0("Associate prof. ", "\u00D7" ," post-ANVUR"), paste0("Full prof. ", "\u00D7" ," post-ANVUR"), "Associate prof. 2010", "Full prof. 2010", "Unknown 2010", paste0("Associate prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Full prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Unknown 2010 ", "\u00D7" ," post-ANVUR"), "Level changed from 2010", paste0("Level changed ", "\u00D7" ," post-ANVUR"), "Medium rank uni",  "High rank uni", paste0("Medium rank uni ", "\u00D7" ," post-ANVUR"), paste0("High rank uni ", "\u00D7" ," post-ANVUR"), "Isolated", "North", "South", paste0("Isolated ", "\u00D7" ," post-ANVUR"), paste0("North ", "\u00D7" ," post-ANVUR"), paste0("South ", "\u00D7" ," post-ANVUR"), "Economics", "Engineering", "Humanities", "Medicine", "Other", "Political Sci.", "Psychology", "Social Sci.", "Sociology", paste0("Economics ", "\u00D7" ," post-ANVUR"), paste0("Engineering ", "\u00D7" ," post-ANVUR"), paste0("Humanities ", "\u00D7" ," post-ANVUR"), paste0("Medicine ", "\u00D7" ," post-ANVUR"), paste0("Other ", "\u00D7" ," post-ANVUR"), paste0("Political Sci. ", "\u00D7" ," post-ANVUR"), paste0("Psychology ", "\u00D7" ," post-ANVUR"), paste0("Social Sci. ", "\u00D7" ," post-ANVUR"), paste0("Sociology ", "\u00D7" ," post-ANVUR")), custom.model.names = c("Papers", "Gender", "Sector", "Level", "Level 2010", "Level Change", "Uni Rank", "Georegion", "Department"), single.row = T, label = "tab:repeated-measures-models-papers-negative-binomial", caption.above = T)
  
} else if (rmd_format_take == "latex") {
  
  texreg(list(nb.lme.papers, nb.lme.papers.gender, nb.lme.papers.sector, nb.lme.papers.level, nb.lme.papers.level_2010, nb.lme.papers.level_changed, nb.lme.papers.universityrank, nb.lme.papers.georegion, nb.lme.papers.department), caption = "Comparative table of repeated measures negative binomial models to check overall ANVUR effect on research productivity (Dependent variable count of total papers published)", custom.coef.names = c("Constant", "Post-ANVUR", "Gender M", paste0("Gender M ", "\u00D7" ," post-ANVUR"), "SPS/08", "SPS/09", "SPS/10", "SPS/11", "SPS/12", paste0("SPS/08 ", "\u00D7" ," post-ANVUR"), paste0("SPS/09 ", "\u00D7" ," post-ANVUR"), paste0("SPS/10 ", "\u00D7" ," post-ANVUR"), paste0("SPS/11 ", "\u00D7" ," post-ANVUR"), paste0("SPS/12 ", "\u00D7" ," post-ANVUR"), "Associate professors", "Full professors", paste0("Associate prof. ", "\u00D7" ," post-ANVUR"), paste0("Full prof. ", "\u00D7" ," post-ANVUR"), "Associate prof. 2010", "Full prof. 2010", "Unknown 2010", paste0("Associate prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Full prof. 2010 ", "\u00D7" ," post-ANVUR"), paste0("Unknown 2010 ", "\u00D7" ," post-ANVUR"), "Level changed from 2010", paste0("Level changed ", "\u00D7" ," post-ANVUR"), "Medium rank uni",  "High rank uni", paste0("Medium rank uni ", "\u00D7" ," post-ANVUR"), paste0("High rank uni ", "\u00D7" ," post-ANVUR"), "Isolated", "North", "South", paste0("Isolated ", "\u00D7" ," post-ANVUR"), paste0("North ", "\u00D7" ," post-ANVUR"), paste0("South ", "\u00D7" ," post-ANVUR"), "Economics", "Engineering", "Humanities", "Medicine", "Other", "Political Sci.", "Psychology", "Social Sci.", "Sociology", paste0("Economics ", "\u00D7" ," post-ANVUR"), paste0("Engineering ", "\u00D7" ," post-ANVUR"), paste0("Humanities ", "\u00D7" ," post-ANVUR"), paste0("Medicine ", "\u00D7" ," post-ANVUR"), paste0("Other ", "\u00D7" ," post-ANVUR"), paste0("Political Sci. ", "\u00D7" ," post-ANVUR"), paste0("Psychology ", "\u00D7" ," post-ANVUR"), paste0("Social Sci. ", "\u00D7" ," post-ANVUR"), paste0("Sociology ", "\u00D7" ," post-ANVUR")), custom.model.names = c("Papers", "Gender", "Sector", "Level", "Level 2010", "Level Change", "Uni Rank", "Georegion", "Department"), single.row = T, label = "tab:repeated-measures-models-papers-negative-binomial", caption.above = T, fontsize = 'tiny', table = TRUE, sideways = TRUE, use.packages	= FALSE)
  
}

# ============================
#### Session Info JUL 9 2020 ####
# ============================
sessionInfo()

# R version 3.6.2 (2019-12-12)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server 2012 R2 x64 (build 9600)
# 
# Matrix products: default
# 
# Random number generation:
#   RNG:     Mersenne-Twister 
# Normal:  Inversion 
# Sample:  Rounding 
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252   
# [3] LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] brms_2.13.0       Rcpp_1.0.4.6      dotwhisker_0.5.0  broom.mixed_0.2.6
# [5] broom_0.5.6       texreg_1.37.5     kableExtra_1.1.0  car_3.0-8        
# [9] carData_3.0-4     lme4_1.1-23       Matrix_1.2-18     stargazer_5.2.2  
# [13] forcats_0.5.0     stringr_1.4.0     dplyr_1.0.0       purrr_0.3.4      
# [17] readr_1.3.1       tidyr_1.1.0       tibble_3.0.1      ggplot2_3.3.2    
# [21] tidyverse_1.3.0   knitr_1.29        pander_0.6.3     
# 
# loaded via a namespace (and not attached):
#   [1] readxl_1.3.1         backports_1.1.8      plyr_1.8.6          
# [4] igraph_1.2.5         TMB_1.7.16           splines_3.6.2       
# [7] crosstalk_1.1.0.1    rstantools_2.0.0     inline_0.3.15       
# [10] digest_0.6.25        htmltools_0.5.0      rsconnect_0.8.16    
# [13] fansi_0.4.1          magrittr_1.5         openxlsx_4.1.5      
# [16] modelr_0.1.8         RcppParallel_5.0.2   matrixStats_0.56.0  
# [19] xts_0.12-0           prettyunits_1.1.1    colorspace_1.4-1    
# [22] blob_1.2.1           rvest_0.3.5          haven_2.3.1         
# [25] xfun_0.15            callr_3.4.3          crayon_1.3.4        
# [28] jsonlite_1.7.0       zoo_1.8-8            glue_1.4.1          
# [31] gtable_0.3.0         webshot_0.5.2        pkgbuild_1.0.8      
# [34] rstan_2.19.3         abind_1.4-5          scales_1.1.1        
# [37] mvtnorm_1.1-1        DBI_1.1.0            miniUI_0.1.1.1      
# [40] viridisLite_0.3.0    xtable_1.8-4         ggstance_0.3.4      
# [43] foreign_0.8-75       stats4_3.6.2         StanHeaders_2.21.0-5
# [46] DT_0.14              htmlwidgets_1.5.1    httr_1.4.1          
# [49] threejs_0.3.3        ellipsis_0.3.1       farver_2.0.3        
# [52] pkgconfig_2.0.3      loo_2.2.0            dbplyr_1.4.4        
# [55] utf8_1.1.4           labeling_0.3         tidyselect_1.1.0    
# [58] rlang_0.4.6          reshape2_1.4.4       later_1.1.0.1       
# [61] munsell_0.5.0        cellranger_1.1.0     tools_3.6.2         
# [64] cli_2.0.2            generics_0.0.2       ggridges_0.5.2      
# [67] evaluate_0.14        fastmap_1.0.1        processx_3.4.2      
# [70] fs_1.4.2             zip_2.0.4            packrat_0.5.0       
# [73] nlme_3.1-148         mime_0.9             xml2_1.3.2          
# [76] compiler_3.6.2       bayesplot_1.7.2      shinythemes_1.1.2   
# [79] rstudioapi_0.11      curl_4.3             reprex_0.3.0        
# [82] statmod_1.4.34       stringi_1.4.6        highr_0.8           
# [85] ps_1.3.3             Brobdingnag_1.2-6    lattice_0.20-38     
# [88] nloptr_1.2.2.1       markdown_1.1         shinyjs_1.1         
# [91] vctrs_0.3.1          pillar_1.4.4         lifecycle_0.2.0     
# [94] bridgesampling_1.0-0 data.table_1.12.8    httpuv_1.5.4        
# [97] R6_2.4.1             promises_1.1.1       gridExtra_2.3       
# [100] rio_0.5.16           boot_1.3-25          colourpicker_1.0    
# [103] MASS_7.3-51.6        gtools_3.8.2         assertthat_0.2.1    
# [106] withr_2.2.0          shinystan_2.5.0      parallel_3.6.2      
# [109] hms_0.5.3            grid_3.6.2           coda_0.19-3         
# [112] minqa_1.2.4          rmarkdown_2.3        pbdZMQ_0.3-3        
# [115] shiny_1.5.0          lubridate_1.7.9      base64enc_0.1-3     
# [118] dygraphs_1.1.1.6    