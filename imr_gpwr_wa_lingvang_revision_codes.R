#' ---
#' title: "R Notebook for *Corpus linguistic and experimental studies on meaning-preserving hypothesis in Indonesian voice alternation*"
#' author: 
#' - name: '[I Made Rajeg](https://udayananetworking.unud.ac.id/lecturer/1817-i-made-rajeg) <a itemprop="sameAs" content="https://orcid.org/0000-0001-8989-0203" href="https://orcid.org/0000-0001-8989-0203" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>^1^, [Gede Primahadi Wijaya Rajeg](https://udayananetworking.unud.ac.id/lecturer/880-gede-primahadi-wijaya-rajeg) <a itemprop="sameAs" content="https://orcid.org/0000-0002-2047-8621" href="https://orcid.org/0000-0002-2047-8621" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>^1^, [I Wayan Arka](https://researchers.anu.edu.au/researchers/arka-iww) <a itemprop="sameAs" content="https://orcid.org/0000-0002-2819-6186" href="https://orcid.org/0000-0002-2819-6186" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>^2,^ ^1^'
#'   affiliation: "Universitas Udayana ^1^, Australian National University ^2^"
#' output:
#'   html_notebook:
#'     code_folding: show
#'     fig_caption: yes
#'     fig_width: 6
#'     number_sections: yes
#'     toc: yes
#'     toc_float: no
#'   pdf_document:
#'     toc: yes
#'   bookdown::pdf_document2:
#'     df_print: tibble
#'     fig_caption: yes
#'     number_sections: yes
#'   bookdown::word_document2:
#'     df_print: kable
#'     fig_caption: yes
#'     fig_width: 6
#' bibliography: reference.bib
#' csl: unified_stylesheet_linguistics.csl
#' ---
#' 
## ----setup, include = FALSE, message = FALSE, warning = FALSE, echo = FALSE---------
knitr::opts_chunk$set(fig.width = 7,
                      fig.asp = 0.618,
                      fig.retina = 2,
                      dpi = 300,
                      dev = "pdf",
                      tidy = FALSE,
                      echo = FALSE)

library(tidyverse)
library(broom)
library(readxl)
library(vcd)

#' 
#' 
#' <!-- badges: start -->
#' <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This R Notebook and the analyses codes are licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
#' <!-- badges: end -->
#' 
#' # How to cite this R notebook {-}
#' 
#' Please cite this notebook as follows (in Unified Style Sheet for Linguistics) [@rajeg_supplementary_2021]:
#' 
#' - to be filled with the citation of the paper to appear in [*Linguistics Vanguard*](https://www.degruyter.com/journal/key/LINGVAN/html)
#' 
#' - Rajeg, I Made, Gede Primahadi Wijaya Rajeg & I Wayan Arka. 2021. R Notebook for “Corpus linguistic and experimental studies on meaning-preserving hypothesis in Indonesian voice alternation.” Open Science Framework. doi: [10.17605/OSF.IO/QF38H](https://doi.org/10.17605/OSF.IO/QF38H). url: [https://osf.io/qf38h/](https://osf.io/qf38h/).
#' 
#' 
#' # Introduction {#intro}
#' 
#' 
## ----load-elicitation-data, message = FALSE, warning = FALSE, eval = TRUE, include = TRUE----
corpsize <- readr::read_tsv('corpus_total_size_per_file.txt')
corpsizeused <- subset(corpsize, grepl("_newscrawl_", corpus_id))

memajukan_exp <- readRDS("memajukan_exp.rds")
memajukan_exp1 <- memajukan_exp %>% 
  filter(!sense %in% c("duplicate"), P_anim != "INTR")

dimajukan_exp <- readRDS("dimajukan_exp.rds")
dimajukan_exp1 <- dimajukan_exp %>% 
  filter(sense != "duplicate")

majukan_exp <- readRDS("majukan_exp.rds")
majukan_exp1 <- majukan_exp %>% 
  filter(!clause_type %in% c("IRRELEVANT", "duplicate", "unclear"))
majukan_exp1_declarative <- majukan_exp1 %>% 
  filter(clause_type %in% c("declarative"))

memundurkan_exp <- readRDS("memundurkan_exp.rds")
memundurkan_exp1 <- memundurkan_exp %>% 
  filter(!sense %in% c("IRRELEVANT", "duplicate", "unclear"))

dimundurkan_exp <- readRDS("dimundurkan_exp.rds")
dimundurkan_exp1 <- dimundurkan_exp %>% 
  filter(!sense %in% c("IRRELEVANT", "duplicate", "unclear"))

mundurkan_exp <- readRDS("mundurkan_exp.rds")
mundurkan_exp1 <- mundurkan_exp %>% filter(!sense %in% c("IRRELEVANT", "duplicate", "unclear"))
mundurkan_exp1_declarative <- mundurkan_exp1 %>% filter(clause_type == "declarative")

mengajukan_exp <- readRDS("mengajukan_exp.rds")
mengajukan_exp1 <- mengajukan_exp %>% filter(!sense %in% c("duplicate", "unclear"))

diajukan_exp <- readRDS("diajukan_exp.rds")
diajukan_exp1 <- diajukan_exp %>% 
  filter(!sense %in% c("duplicate", "IRRELEVANT")) %>% 
  mutate(sense = replace(sense, sense %in% c("report", "temporal"), "others")) %>% 
  filter(sense != "others")

ajukan_exp <- readRDS("ajukan_exp.rds")
ajukan_exp1 <- ajukan_exp %>% 
  filter(!sense %in% c("duplicate", "IRRELEVANT", "unclear"))
ajukan_exp1_declarative <- ajukan_exp1 %>% 
  filter(clause_type == "declarative")

mengundur_exp <- readRDS("mengundur_exp.rds")
mengundur_exp1 <- mengundur_exp %>% 
  filter(!sense %in% c("duplicate", "IRRELEVANT", "unclear"))

diundur_exp <- readRDS("diundur_exp.rds")
diundur_exp1 <- diundur_exp %>% 
  filter(!sense %in% c("duplicate", "IRRELEVANT", "unclear"))

mengundurkan_exp <- readRDS("mengundurkan_exp.rds")
mengundurkan_exp1 <- mengundurkan_exp %>% 
  filter(!sense %in% c("duplicate", "IRRELEVANT", "unclear"))

# diundurkan_exp <- readRDS("diundurkan_exp.rds")
# diundurkan_exp1 <- diundurkan_exp %>% 
#   filter(!sense %in% c("duplicate", "IRRELEVANT", "unclear")) %>% 
#   slice_sample(n = 100)

# load only experimental data for *diundurkan* that has been randomly sampled to 100 tokens
diundurkan_exp1 <- readRDS("diundurkan_exp_100_sample.rds")


undurkan_exp <- readRDS("undurkan_exp.rds")
undurkan_exp1 <- undurkan_exp %>% 
  filter(!sense %in% c("duplicate", "IRRELEVANT", "unclear"))
undurkan_exp1_declarative <- undurkan_exp1 %>% 
  filter(clause_type == "declarative")

exp_database <- data.frame(verbs = c("memajukan", "dimajukan", "majukan", 
                                     "memundurkan", "dimundurkan", "mundurkan",
                                     "mengajukan", "diajukan", "ajukan",
                                     "mengundur", "diundur", 
                                     "mengundurkan", "diundurkan", "undurkan"),
                           cases = c(nrow(memajukan_exp), nrow(dimajukan_exp),
                                     nrow(majukan_exp), nrow(memundurkan_exp),
                                     nrow(dimundurkan_exp), nrow(mundurkan_exp),
                                     nrow(mengajukan_exp), nrow(diajukan_exp),
                                     nrow(ajukan_exp), nrow(mengundur_exp),
                                     nrow(diundur_exp), nrow(mengundurkan_exp),
                                     nrow(diundurkan_exp), nrow(undurkan_exp)),
                           relevant = c(nrow(memajukan_exp1), nrow(dimajukan_exp1),
                                     nrow(majukan_exp1), nrow(memundurkan_exp1),
                                     nrow(dimundurkan_exp1), nrow(mundurkan_exp1),
                                     nrow(mengajukan_exp1), nrow(diajukan_exp1),
                                     nrow(ajukan_exp1), nrow(mengundur_exp1),
                                     nrow(diundur_exp1), nrow(mengundurkan_exp1),
                                     nrow(diundurkan_exp1), nrow(undurkan_exp1)))


memundurkan_exp1_sense_count <- memundurkan_exp1 %>% 
  count(sense, node, sort = TRUE)

dimundurkan_exp1_sense_count <- dimundurkan_exp1 %>% 
  count(sense, node, sort = TRUE)

mundurkan_exp1_declarative_sense_count <- count(mundurkan_exp1_declarative, 
                                                sense, node, sort = TRUE)

mundurkan_exp1_declarative_sense_count_by_voice <- count(mundurkan_exp1_declarative, 
                                                         sense, voice, node, sort = TRUE)

mundurkan_exp1_declarative_voice_count <- count(mundurkan_exp1_declarative, 
                                                voice, node, sort = TRUE)

mengundurkan_exp1_sense_count <- mengundurkan_exp1 %>% 
  count(sense, node, sort = TRUE)

diundurkan_exp1_sense_count <- diundurkan_exp1 %>% 
  count(sense, node, sort = TRUE)

undurkan_exp1_declarative_sense_count <- count(undurkan_exp1_declarative, 
                                               sense, node, sort = TRUE)

undurkan_exp1_declarative_sense_count_by_voice <- count(undurkan_exp1_declarative, 
                                                        sense, voice, node, sort = TRUE)

undurkan_exp1_declarative_voice_count <- count(undurkan_exp1_declarative, 
                                               voice, node, sort = TRUE)

mengajukan_exp1_subsense_count <- mengajukan_exp1 %>% 
  count(sense, node, sort = TRUE)

diajukan_exp1_subsense_count <- diajukan_exp1 %>% 
  count(sense, node, sort = TRUE)

ajukan_exp1_declarative_sense_count <- count(ajukan_exp1_declarative, 
                                             sense, node, sort = TRUE)

ajukan_exp1_declarative_voice_count <- count(ajukan_exp1_declarative, 
                                             voice, node, sort = TRUE)

ajukan_exp1_declarative_sense_count_by_voice <- count(ajukan_exp1_declarative, 
                                                      sense, voice, node, sort = TRUE)

dimajukan_exp1_sense_count <- count(dimajukan_exp1, 
                                    sense, node, sort = TRUE)

memajukan_exp1_sense_count <- count(memajukan_exp1, 
                                    sense, node, sort = TRUE)

majukan_exp1_declarative_sense_count <- count(majukan_exp1_declarative, 
                                              sense, node, sort = TRUE)

majukan_exp1_declarative_voice_count <- count(majukan_exp1_declarative, 
                                              voice, node, sort = TRUE)

majukan_exp1_declarative_sense_count_by_voice <- count(majukan_exp1_declarative, 
                                                       sense, voice, node, sort = TRUE)

mengundur_exp1_sense_count <- mengundur_exp1 %>% 
  count(sense, node, sort = TRUE)

diundur_exp1_sense_count <- diundur_exp1 %>% 
  count(sense, node, sort = TRUE)

#' 
#' # Data and methodology {#datamethod}
#' 
#' [[Table \@ref(tab:table1-corpus-data)](#table1-corpus-data)]). 
#' 
## ----table1-corpus-data, message = FALSE, warning=FALSE-----------------------------
## TABLE 1 ========
corpsizeused %>% 
  rename(Filenames = corpus_id,
         `Size (in word-tokens)` = total_tokens) %>% 
  mutate(`Size (in word-tokens)` = format(`Size (in word-tokens)`, big.mark = ",")) %>% 
  knitr::kable(caption = "Corpus files used and their sizes", row.names = TRUE)

#' 
#' [Table \@ref(tab:table2-lexemes-database-count)](#table2-lexemes-database-count). 
#' 
## ----table2-lexemes-database-count, message = FALSE, warning = FALSE----------------
## TABLE 2 ========
lexemes_all <- readr::read_tsv("lexemes_all_database.txt")
lexemes_all_database <- lexemes_all %>% 
  select(base, affixes, n) %>% 
  pivot_wider(names_from = affixes, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(base = fct_relevel(as.factor(base), c("majukan", "mundurkan", "aju", "ajukan", "undur", "undurkan"))) %>% 
  arrange(base) %>% 
  mutate(gloss = "gloss here",
         base = paste("*", base, "*", sep = "")) %>% 
  select(base, -gloss, unprefixed, `meN-`, `di-`) %>% 
  column_to_rownames("base") %>% 
  as.matrix()
lexemes_all_database_perc <- round(prop.table(lexemes_all_database, 1)*100, 2)
knitr::kable(format(addmargins(lexemes_all_database), big.mark = ","), caption = "Distribution of bases and their voice morphologies")

#' 
#' ## Metaphor analysis {#metapanalysis}
#' 
#' # Analysis for *majukan*, *memajukan* and *dimajukan* {#majukan-all}
#' 
#' ## Corpus data for *majukan*
#' 
## ----majukan-voice-and-sense-categorising-new---------------------------------------
# read the concordance data
majukan <- readRDS("majukan_BARE_all_data.rds") %>% 
  mutate(node = tolower(node))

#' 
## ----majukan-voice-count-new--------------------------------------------------------
majukan_voice_tb <- majukan %>% 
  filter(str_detect(sense, "^irrel", negate = TRUE)) %>% 
  count(voice, sort = TRUE) %>% 
  mutate(perc = round((n/sum(n) * 100), 2))

#' 
## ----majukan_phys_motion_examples---------------------------------------------------
majukan_phys_motion_df <- subset(majukan, sense=="phys_motion")

#' 
## ----memajukan-load-data------------------------------------------------------------
memajukan <- readRDS("majukan_AV_sample_data.rds") %>% 
  filter(senses != "duplicate") %>% 
  rename(sense = senses) %>% 
  mutate(node = tolower(node))

#' 
## ----dimajukan-load-data------------------------------------------------------------
dimajukan <- readRDS("majukan_PASS_sample_data.rds") %>% 
  filter(!sense %in% c("duplicate", "phys_motion")) %>% 
  mutate(node = tolower(node))
dimajukan_physmotion <- readRDS("majukan_PASS_sample_data.rds") %>% 
  filter(sense %in% c("phys_motion"))

#' 
## ----majukan-combined-voice---------------------------------------------------------
memajukan1 <- memajukan %>% 
  select(node, sense) %>% 
  mutate(voice = "av")
dimajukan1 <- dimajukan %>% 
  select(node, sense) %>% 
  mutate(voice = "pass")
majukan1 <- majukan %>%
  filter(str_detect(sense, "^(irrel|phys_m)", negate = TRUE)) %>% 
  select(node, sense, voice)
majukan_combined <- bind_rows(filter(majukan1, voice == "uv"), 
                              memajukan1, 
                              dimajukan1) %>% 
  mutate(sense = replace(sense, sense == "propose", "proposing"),
         sense = as.factor(sense), 
         sense = fct_relevel(sense, "proposing", after = 3))
majukan_voice_count <- table(majukan_combined$voice)
majukan_combined_goodness_of_fit <- chisq.test(majukan_voice_count)

#' 
#' Codes to generate [Figure \@ref(fig:figure1-majukan-voice-plot)](#figure1-majukan-voice-plot).
#' 
## ----figure1-majukan-voice-plot, fig.cap="Distribution of voice for the metaphoric usages of the base *majukan*"----
# saving to computer
## FIGURE 1 ========
fig1 <- majukan_voice_count %>% 
  data.frame() %>% 
  rename(voice = Var1, n = Freq) %>% 
  mutate(voice = toupper(voice)) %>% 
  ggplot(aes(x = voice, y = n, fill = voice)) +
  geom_col() +
  geom_text(aes(label = paste("N=", n, sep = "")), vjust = -.35, size = 3.5) +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(11, "RdYlBu")[1], 
                               RColorBrewer::brewer.pal(11, "RdYlBu")[8], 
                               RColorBrewer::brewer.pal(11, "RdYlBu")[5])) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(y = "Raw frequency", x = "Voice", caption = bquote(paste(italic(X)["goodness-of-fit"]^2, "=", .(round(majukan_combined_goodness_of_fit$statistic, 2)), "; ", italic(df), "=", .(majukan_combined_goodness_of_fit$parameter), "; ", italic(p)["two-tailed"], .(if(majukan_combined_goodness_of_fit$p.value < 0.001) " < 0.001"))))

# Uncomment (i.e. delete the hashtag) the following line to activate the code line to save the plot to computer
# fig1 + ggsave("figs/figure-1.jpeg", width = 6, height = 5, dpi = 600)

majukan_voice_count %>% 
  data.frame() %>% 
  rename(voice = Var1, n = Freq) %>% 
  mutate(voice = toupper(voice)) %>% 
  ggplot(aes(x = voice, y = n, fill = voice)) +
  geom_col() +
  geom_text(aes(label = paste("N=", n, sep = "")), vjust = -.35, size = 2.75) +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(11, "RdYlBu")[1], 
                               RColorBrewer::brewer.pal(11, "RdYlBu")[8], 
                               RColorBrewer::brewer.pal(11, "RdYlBu")[5])) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(y = "Raw frequency", x = "Voice", caption = bquote(paste(italic(X)["goodness-of-fit"]^2, "=", .(round(majukan_combined_goodness_of_fit$statistic, 2)), "; ", italic(df), "=", .(majukan_combined_goodness_of_fit$parameter), "; ", italic(p)["two-tailed"], .(if(majukan_combined_goodness_of_fit$p.value < 0.001) " < 0.001"))))

#' 
#' 
#' Codes for *Chi-square Test* of Independence. 
#' 
## ----majukan-combined-av-pass-chisquare---------------------------------------------
majukan_combined_av_pass <- filter(majukan_combined, voice != "uv")
majukan_combined_av_pass_chisq0 <- majukan_combined_av_pass %>% 
  count(sense, voice) %>% 
  pivot_wider(values_from = "n", names_from = voice) %>% 
  data.frame(row.names = 1) %>% 
  chisq.test() 
majukan_combined_av_pass_chisq <- majukan_combined_av_pass_chisq0 %>% 
  broom::tidy()
majukan_combined_av_pass_assocstats <- majukan_combined_av_pass %>% 
  count(sense, voice) %>% 
  pivot_wider(values_from = "n", names_from = voice) %>% 
  data.frame(row.names = 1) %>% 
  as.matrix() %>% 
  vcd::assocstats()

#' 
#' Code for [Figure \@ref(fig:figure2-majukan-combined-av-pass-plot)](#figure2-majukan-combined-av-pass-plot).
#' 
## ----figure2-majukan-combined-av-pass-plot, fig.cap="Distribution of metaphoric senses of *majukan* across AV and PASS"----
majukan_combined_av_pass_count <- majukan_combined_av_pass %>% 
  count(voice, sense) %>% 
  group_by(voice) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  ungroup()

# PERCENTAGES-----
# saving to computer
## FIGURE 2 =============
 fig2 <- majukan_combined_av_pass_count %>% 
    mutate(voice = toupper(voice), 
           sense = as.character(sense), 
           sense = replace(sense, sense=='temporal', 'cause to happen earlier'), 
           sense = as.factor(sense), 
           sense = fct_relevel(sense)) %>% 
  ggplot(aes(x = voice, y = perc, fill = sense)) + 
  geom_col(position = position_dodge(.9)) + 
  # scale_fill_brewer(palette = "PuBu", type = "qual", direction = -1) + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(9, "YlGnBu")[7], 
                               RColorBrewer::brewer.pal(9, "YlGnBu")[4], 
                               RColorBrewer::brewer.pal(9, "YlGnBu")[2])) +
  theme_bw() + 
  geom_text(aes(label = paste("N=", n, sep = "")), position = position_dodge(.9), vjust = -.35, size = 3.5) + 
  theme(legend.position = "top") +
  labs(x = "Voice", y = "Percentages", fill = "Metaphoric senses", caption = bquote(paste(italic(X)["independence"]^2, "=", .(round(majukan_combined_av_pass_chisq$statistic, 2)), "; ", italic(df), "=", .(majukan_combined_av_pass_chisq$parameter), "; ", italic(p)["two-tailed"], "=", .(if(majukan_combined_av_pass_chisq$p.value < 0.001) " < 0.001"), "; Cramér's ", italic(V), "=", .(round(majukan_combined_av_pass_assocstats$cramer, 3))))) 

# Uncomment (i.e. delete the hashtag) the following line to activate the code line to save the plot to computer
# fig2 + ggsave("figs/figure-2.jpeg", width = 7, height = 5.5, dpi = 600)

majukan_combined_av_pass_count %>% 
    mutate(voice = toupper(voice), 
           sense = as.character(sense), 
           sense = replace(sense, sense=='temporal', 'cause to happen earlier'), 
           sense = as.factor(sense), 
           sense = fct_relevel(sense)) %>% 
  ggplot(aes(x = voice, y = perc, fill = sense)) + 
  geom_col(position = position_dodge(.9)) + 
  # scale_fill_brewer(palette = "PuBu", type = "qual", direction = -1) + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(9, "YlGnBu")[7], 
                               RColorBrewer::brewer.pal(9, "YlGnBu")[4], 
                               RColorBrewer::brewer.pal(9, "YlGnBu")[2])) +
  theme_bw() + 
  geom_text(aes(label = paste("N=", n, sep = "")), position = position_dodge(.9), vjust = -.35, size = 2.75) + 
  theme(legend.position = "top") +
  labs(x = "Voice", y = "Percentages", fill = "Metaphoric senses", caption = bquote(paste(italic(X)["independence"]^2, "=", .(round(majukan_combined_av_pass_chisq$statistic, 2)), "; ", italic(df), "=", .(majukan_combined_av_pass_chisq$parameter), "; ", italic(p)["two-tailed"], "=", .(if(majukan_combined_av_pass_chisq$p.value < 0.001) " < 0.001"), "; Cramér's ", italic(V), "=", .(round(majukan_combined_av_pass_assocstats$cramer, 3)))))

#' 
#' Such asymmetric distribution effect of metaphoric senses across voice for *majukan* is statistically highly significant (*X*^2^=`r round(majukan_combined_av_pass_chisq$statistic, 2)`; *df*=`r majukan_combined_av_pass_chisq$parameter`; *p*~two-tailed~=`r if(majukan_combined_av_pass_chisq$p.value < 0.001) " < 0.001"`) with a highly strong effect size (Cramér's *V*=`r round(majukan_combined_av_pass_assocstats$cramer, 3)`)^[The interpretation of the effect size follows that given in Levshina [-@levshina_how_2015, 209]: 0.1 $\leq$ *V* $\lt$ 0.3 indicates small effect; 0.3 $\leq$ *V* $\lt$ 0.5 indicates moderate effect; *V* $\gt$ 0.5 indicates large or strong effect.]. 
#' 
#' Association plot ([Figure \@ref(fig:figure3-majukan-combined-assocplot)](#figure3-majukan-combined-assocplot)).
#' 
#' 
## ----figure3-majukan-combined-assocplot, fig.cap="Association plot between metaphoric senses of *majukan* with AV and PASS.", fig.asp = .8----
## FIGURE 3 ===========
# Uncomment (i.e. delete the hashtags in) from the following three lines to activate the code to save the plot in computer
# png("figs/figure-3.jpeg", width = 7, height = 6, units = "in", res = 600)
# vcd::assoc(voice~sense, mutate(majukan_combined_av_pass, voice = toupper(voice)), shade = TRUE, legend = legend_resbased(fontsize = 10), gp_labels = gpar(fontsize = 9), labeling_args = list(set_varnames = c(voice = "Voice", sense = "Senses"), set_labels = list(sense = c("advancing", "cause to happen earlier", "proposing"))))
# dev.off()
vcd::assoc(voice~sense, mutate(majukan_combined_av_pass, voice = toupper(voice)), shade = TRUE, legend = legend_resbased(fontsize = 10), gp_labels = gpar(fontsize = 9), labeling_args = list(set_varnames = c(voice = "Voice", sense = "Senses"), set_labels = list(sense = c("advancing", "cause to happen earlier", "proposing"))))

#' 
#' ## Experimental data for *majukan*
#' 
#' Codes for experimental data for *majukan*.
#' 
## ----majukan-exp-meta-lit-count-----------------------------------------------------

majukan_exp_meta_lit_sense <- majukan_exp1 %>% 
  count(sense, node, sort = T) %>% 
  mutate(perc = round(n/sum(n)*100, 2)) %>% 
  bind_rows(memajukan_exp1_sense_count %>% 
              mutate(perc = round(n/sum(n)*100, 2))) %>%
  bind_rows(dimajukan_exp1_sense_count %>% 
              mutate(perc = round(n/sum(n)*100, 2))) %>% 
  arrange(node) %>% 
  mutate(sense_type = if_else(str_detect(sense, "^phys"), "lit", "met"))

majukan_exp_meta_lit_root <- majukan_exp1 %>% 
  count(sense, clause_type, node, sort = T) %>% 
  group_by(sense) %>% 
  mutate(perc = round(n/sum(n)*100, 2)) %>% 
  arrange(sense) %>%
  ungroup()

majukan_exp_met_lit_by_word <- majukan_exp_meta_lit_sense %>% 
  group_by(node, sense_type) %>% 
  summarise(n=sum(n),perc=sum(perc), .groups = 'drop') %>% 
  group_by(node) %>% 
  mutate(total = sum(n), 
         pbin = map_dbl(pmap(list(x = n, n = total), binom.test), "p.value"), 
         dec = if_else(pbin >= 0.05, "ns", "***"), 
         dec = if_else(pbin < 0.05, "*", dec), 
         dec = if_else(pbin < 0.01, "**", dec), 
         dec = if_else(pbin < 0.001, "***", dec)) 

#' 
#' 
#' In experimental data, metaphoric senses are the predominant tokens for each *majukan* (N=`r pull(filter(majukan_exp_met_lit_by_word, node=='majukan', sense_type=='met'), n)`; `r pull(filter(majukan_exp_met_lit_by_word, node=='majukan', sense_type=='met'), perc)`%), *memajukan* (N=`r pull(filter(majukan_exp_met_lit_by_word, node=='memajukan', sense_type=='met'), n)`; `r pull(filter(majukan_exp_met_lit_by_word, node=='memajukan', sense_type=='met'), perc)`%), and *dimajukan* (N=`r pull(filter(majukan_exp_met_lit_by_word, node=='dimajukan', sense_type=='met'), n)`; `r pull(filter(majukan_exp_met_lit_by_word, node=='dimajukan', sense_type=='met'), perc)`%) over their literal, physical motion sense. For *majukan*, `r majukan_exp_meta_lit_root %>% filter(sense!="phys_motion") %>% group_by(clause_type) %>% summarise(n=sum(n), .groups='drop') %>% mutate(perc = round(n/sum(n)*100, 2)) %>% filter(clause_type=='imperative') %>% pull(perc)`% (N=`r majukan_exp_meta_lit_root %>% filter(sense!="phys_motion") %>% group_by(clause_type) %>% summarise(n=sum(n), .groups='drop') %>% mutate(perc = round(n/sum(n)*100, 2)) %>% filter(clause_type=='imperative') %>% pull(n)`) of its `r pull(filter(majukan_exp_met_lit_by_word, node=='majukan', sense_type=='met'), n)` metaphoric tokens are in imperative clause, and `r majukan_exp_meta_lit_root %>% filter(sense!="phys_motion") %>% group_by(clause_type) %>% summarise(n=sum(n), .groups='drop') %>% mutate(perc = round(n/sum(n)*100, 2)) %>% filter(clause_type=='declarative') %>% pull(n)` tokens are in declarative clause, of which only `r majukan_exp1_declarative_sense_count_by_voice %>% filter(sense!='phys_motion', voice=='uv') %>% pull(n)` is in UV. The literal sense of *majukan* is significantly greater in PASS *di-* (N=`r filter(majukan_exp_met_lit_by_word, sense_type=='lit', node != "majukan", node=='dimajukan')$n`) compared to AV *meN-* (N=`r filter(majukan_exp_met_lit_by_word, sense_type=='lit', node != "majukan", node=='memajukan')$n`) (*X*^2^~goodness-of-fit~=`r chisq.test(filter(majukan_exp_met_lit_by_word, sense_type=='lit', node != "majukan")$n)$statistic`; *df*=`r chisq.test(filter(majukan_exp_met_lit_by_word, sense_type=='lit', node != "majukan")$n)$parameter`; *p*~two-tailed~`r if(chisq.test(filter(majukan_exp_met_lit_by_word, sense_type=='lit', node != "majukan")$n)$p.value < 0.05) paste(" < 0.05") else if(chisq.test(filter(majukan_exp_met_lit_by_word, sense_type=='lit', node != "majukan")$n)$p.value < 0.01) paste(" < 0.01") else if(chisq.test(filter(majukan_exp_met_lit_by_word, sense_type=='lit', node != "majukan")$n)$p.value < 0.001) paste(" < 0.001")`).
#' 
#' 
#' [Figure \@ref(fig:figure4-majukan-exp-metaphoric-lit-sense-plot)](#figure4-majukan-exp-metaphoric-lit-sense-plot) visualises the distribution of metaphoric and literal senses across voice for EXPERIMENTAL DATA.
#' 
#' 
## ----figure4-majukan-exp-metaphoric-lit-sense-plot, fig.cap="Distribution of metaphoric and literal senses of *majukan* in AV and PASS (sentence-production)."----

majukan_exp_sense_av_pass <- bind_rows(memajukan_exp1_sense_count,
                                       dimajukan_exp1_sense_count,
                                       select(filter(majukan_exp1_declarative_sense_count_by_voice, 
                                                     voice == "av"), -voice)) %>% 
  mutate(voice = "pass", 
         voice = if_else(str_detect(node, "^di", negate = TRUE), "av", voice))

majukan_exp_sense_av_pass_plotdf <- majukan_exp_sense_av_pass %>% 
  filter(node != "majukan") %>% 
  group_by(voice, sense) %>%
  summarise(n = sum(n), .groups = 'drop') %>% 
  group_by(voice) %>% 
  mutate(perc = round(n/sum(n)*100, 2)) %>% 
  ungroup() %>% 
  mutate(sense = replace(sense, sense=="phys_motion", "LIT. caused forward motion"),
         voice = fct_relevel(as.factor(voice), c("av", "pass")),
         sense = fct_relevel(as.factor(sense), c("advancing", "temporal", "LIT. caused forward motion")))

majukan_exp_sense_av_pass_chisq <- majukan_exp_sense_av_pass_plotdf %>% 
  pivot_wider(-perc, names_from = voice, values_from = n) %>% 
  column_to_rownames('sense') %>% 
  as.matrix() %>% 
  chisq.test()

majukan_exp_sense_av_pass_cramer <- round(assocstats(majukan_exp_sense_av_pass_chisq$observed)$cramer, 3)

## FIGURE 4 =========
fig4 <- majukan_exp_sense_av_pass_plotdf %>% 
    mutate(sense = as.character(sense), 
           sense = replace(sense, sense=='temporal', 'cause to happen earlier'), 
           sense = as.factor(sense), 
           sense = fct_relevel(sense)) %>% 
  ggplot(aes(x = toupper(voice), y = perc, fill = sense)) + 
  geom_col(position = position_dodge(.9)) + 
  # scale_fill_brewer(palette = "PuBu", type = "qual", direction = -1) + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(9, "YlGnBu")[7],
                               RColorBrewer::brewer.pal(9, "YlGnBu")[4],
                               RColorBrewer::brewer.pal(9, "YlGnBu")[1])) +
  theme_bw() + 
  geom_text(aes(label = paste("N=", n, sep = "")), position = position_dodge(.9), vjust = -.35, size = 3.5) + 
  theme(legend.position = "top") +
  labs(x = "Voice", y = "Percentages", fill = "Senses", caption = bquote(paste(italic(X)["independence"]^2, "=", .(round(majukan_exp_sense_av_pass_chisq$statistic, 2)), "; ", italic(df), "=", .(majukan_exp_sense_av_pass_chisq$parameter), "; ", italic(p)["two-tailed"], .(if(majukan_exp_sense_av_pass_chisq$p.value < 0.001) " < 0.001" else if(majukan_exp_sense_av_pass_chisq$p.value < 0.01) " < 0.01" else if(majukan_exp_sense_av_pass_chisq$p.value < 0.05) " < 0.05" else if(majukan_exp_sense_av_pass_chisq$p.value >= 0.05) paste("=", round(majukan_exp_sense_av_pass_chisq$p.value, 3))), "; Cramér's ", italic(V), "=", .(majukan_exp_sense_av_pass_cramer))))

# Uncomment (i.e. delete the hashtag) the following line to activate the code line to save the plot to computer
# fig4 + ggsave("figs/figure-4.jpeg", width = 7, height = 5.5, dpi = 600)

fig4

#' 
#' There is converging trend between the metaphoric senses in the corpus and experimental data, with the exception of the absense of 'proposing' sense in experimental data.
#' 
#' 
## ----figure5-majukan-exp-met-lit-combined-assocplot, fig.cap="Association plot between metaphoric and literal senses of *majukan* in AV and PASS from sentence-production experiment.", fig.asp=.85----
## FIGURE 5 ===========
names(dimnames(majukan_exp_sense_av_pass_chisq$observed)) <- c("sense", "voice")
# Uncomment (i.e. delete the hashtags in) the following three lines to activate the code to save the plot in computer
# png("figs/figure-5.jpeg", width = 7, height = 6, units = "in", res = 600)
# vcd::assoc(majukan_exp_sense_av_pass_chisq$observed[c(1, 3, 2), ], shade = TRUE, legend = legend_resbased(fontsize = 10), gp_labels = gpar(fontsize = 7.75), labeling_args = list(set_varnames = c(voice = "Voice", sense = "Senses")), set_labels = list(voice = c("AV", "PASS"), sense = c("advancing", "cause to happen earlier", "LIT. caused forward motion")))
# dev.off()
vcd::assoc(majukan_exp_sense_av_pass_chisq$observed[c(1, 3, 2), ], shade = TRUE, legend = legend_resbased(fontsize = 10), gp_labels = gpar(fontsize = 7.75), labeling_args = list(set_varnames = c(voice = "Voice", sense = "Senses")), set_labels = list(voice = c("AV", "PASS"), sense = c("advancing", "cause to happen earlier", "LIT. caused forward motion")))

#' 
#' 
#' # Analysis for *mundurkan*, *memundurkan*, *dimundurkan* {#mundurkan-all}
#' 
#' ## Corpus data for *mundurkan*
#' 
## ----mundurkan-load-sample, message = FALSE, warning = FALSE------------------------
mundurkan <- as_tibble(readRDS("mundurkan_BARE_all_data.rds")) %>% 
  filter(sense != "duplicate")

memundurkan <- as_tibble(readRDS("mundurkan_AV_all_data.rds")) %>% 
  filter(sense != "duplicate") %>% 
  select(-sense) %>% 
  rename(sense = sense_generic)

dimundurkan <- as_tibble(readRDS("mundurkan_PASS_all_data.rds"))

#' 
## ----mundurkan-met-lit-sense-count--------------------------------------------------
mundurkan_met_lit_sense <- mundurkan %>% 
  count(sense, voice) %>% 
  mutate(node = "mundurkan") %>% 
  bind_rows(memundurkan %>% 
              count(sense) %>% 
              mutate(voice = "av", node = "memundurkan")) %>% 
  bind_rows(dimundurkan %>% 
              count(sense) %>% 
              mutate(voice = "pass", node = "dimundurkan")) %>% 
  mutate(sense_type = if_else(str_detect(sense, "^phys"), "lit", "met"))

mundurkan_met_lit_by_word <- mundurkan_met_lit_sense %>% 
  group_by(sense_type, node) %>% 
  tally(n) %>% 
  arrange(node) %>% 
  group_by(node) %>% 
  mutate(perc = round(n/sum(n) * 100, 2),
         total = sum(n),
         pbin = map_dbl(pmap(list(x = n, n = total), binom.test), "p.value"), 
         dec = if_else(pbin >= 0.05, "ns", "***"), 
         dec = if_else(pbin < 0.05, "*", dec), 
         dec = if_else(pbin < 0.01, "**", dec), 
         dec = if_else(pbin < 0.001, "***", dec))

#' 
#' 
## ----mundurkan-metaphoric-lit-sense-count-1-----------------------------------------
mundurkan_met_lit <- mundurkan %>% 
  filter(voice == "av") %>% 
  select(node, sense, voice) %>% 
  mutate(node = tolower(node))

memundurkan_met_lit <- memundurkan %>% 
  select(node, sense) %>% 
  mutate(node = tolower(node), voice = "av")

dimundurkan_met_lit <- dimundurkan %>% 
  select(node, sense) %>% 
  mutate(node = tolower(node), voice = "pass")

mundurkan_met_lit_all <- bind_rows(mundurkan_met_lit, 
                               memundurkan_met_lit, 
                               dimundurkan_met_lit) %>% 
  mutate(sense = replace(sense, sense == "retreat; retrospect", "change one's mind"),
         sense = replace(sense, sense == "phys_move back", "LIT. caused backward motion"),
         sense = as_factor(sense), 
         sense = fct_relevel(sense, 
                             "temporal_postpone", 
                             "withdraw s.o.", 
                             "change one's mind", 
                             "LIT. caused backward motion")) %>% 
  filter(node != "mundurkan")

mundurkan_met_lit_all_count <- mundurkan_met_lit_all %>% 
  count(sense) %>% 
  mutate(perc=n/sum(n)*100)

mundurkan_met_lit_all_count_by_voice <- mundurkan_met_lit_all %>% 
  count(sense, voice) %>% 
  group_by(voice) %>% 
  mutate(perc=n/sum(n)*100)

#' 
## ----mundurkan-met-lit-stats, warning = FALSE, message = FALSE----------------------
mundurkan_met_lit_all_count_by_voice_mtx <- mundurkan_met_lit_all_count_by_voice %>% 
  ungroup() %>% 
  select(-perc) %>% 
  pivot_wider(names_from = voice, 
              values_from = n, 
              values_fill = 0L) %>% 
  data.frame(row.names = 1) %>% 
  as.matrix()

mundurkan_met_lit_all_chisq <- chisq.test(mundurkan_met_lit_all_count_by_voice_mtx)

mundurkan_met_lit_all_fye <- fisher.test(mundurkan_met_lit_all_count_by_voice_mtx)

mundurkan_met_lit_assocstats <- assocstats(mundurkan_met_lit_all_count_by_voice_mtx)

#' 
#' Codes for [Figure \@ref(fig:figure6-mundurkan-met-lit-sense-plot)](#figure6-mundurkan-met-lit-sense-plot)
#' 
## ----figure6-mundurkan-met-lit-sense-plot, fig.cap="Distribution of metaphoric and literal senses of *mundurkan* across AV and PASS"----

# PERCENTAGES-----
## FIGURE 6 ===========
fig6 <- mundurkan_met_lit_all_count_by_voice %>% 
  mutate(voice = toupper(voice),
         sense = as.character(sense), 
         sense = replace(sense, sense=="temporal_postpone", 'postpone'), 
         sense = factor(sense, levels = c("postpone", 
                                          "withdraw s.o.", "change one's mind", 
                                          "LIT. caused backward motion"))) %>% 
  ggplot(aes(x = voice, y = perc, fill = sense)) +
  geom_col(position = position_dodge(.9)) +
  # scale_fill_brewer(palette = "PuBu", type = "qual", direction = -1) + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(9, "YlGnBu")[7], 
                               RColorBrewer::brewer.pal(9, "YlGnBu")[4], 
                               RColorBrewer::brewer.pal(9, "YlGnBu")[2],
                               RColorBrewer::brewer.pal(9, "YlGnBu")[1])) +
  theme_bw() + 
  theme(legend.position = "top") +
  geom_text(aes(label = paste("N=", n, sep = "")), 
            position = position_dodge(.9), 
            vjust = -.35, size = 3) + 
  labs(x = "Voice", y = "Percentages", fill = "Senses", caption = bquote(paste(italic(p)["Fisher Exacts; two-tailed"], .(if(mundurkan_met_lit_all_fye $p.value < 0.001) " < 0.001" else if(mundurkan_met_lit_all_fye$p.value < 0.01) " < 0.01" else if(mundurkan_met_lit_all_fye $p.value < 0.05) " < 0.05" else if(mundurkan_met_lit_all_fye $p.value >= 0.05) paste("=", round(mundurkan_met_lit_all_fye $p.value, 3))), "; Cramér's ", italic(V), "=", .(round(mundurkan_met_lit_assocstats$cramer, 3)))))

# Uncomment the following line to activate the code to save the image to the computer
# fig6 + ggsave("figs/figure-6.jpeg", width = 7, height = 5.5, units = "in", dpi = 600)
fig6

#' 
#' 
#' 
#' 
#' 
#' 
## ----figure7-mundurkan-met-lit-assocplot, fig.cap="Association plot between metaphoric and literal senses of *mundurkan* with AV and PASS.", fig.asp=.8----
## FIGURE 7 ============
mtx_tb <- mundurkan_met_lit_all_count_by_voice_mtx
rownames(mtx_tb)[c(1, 3)] <- c("postpone", "change one's mind")
names(dimnames(mtx_tb)) <- c("Sense", "Voice")
colnames(mtx_tb) <- toupper(colnames(mtx_tb))
# Uncomment the following three lines to activate the code to save the plot in computer
# png("figs/figure-7.jpeg", width = 7, height = 6, units = "in", res = 600)
# vcd::assoc(mtx_tb, shade = TRUE, gp_labels = gpar(fontsize = 7.5), legend = legend_resbased(fontsize = 10.5), labeling_args = list(set_varnames = c(Sense = "Senses")))
# dev.off()
vcd::assoc(mtx_tb, shade = TRUE, gp_labels = gpar(fontsize = 7.5), legend = legend_resbased(fontsize = 10.5), labeling_args = list(set_varnames = c(Sense = "Senses")))

#' 
#' 
#' 
#' ## Experimental data for *mundurkan*
#' 
#' 
## ----mundurkan-exp-meta-lit-count---------------------------------------------------
mundurkan_exp_meta_lit_sense <- mundurkan_exp1 %>% 
  filter(clause_type=="declarative") %>%
  count(sense, node, sort = T) %>% mutate(perc = round(n/sum(n)*100, 2)) %>%
  bind_rows(memundurkan_exp1_sense_count %>% mutate(perc = round(n/sum(n)*100, 2))) %>%
  bind_rows(dimundurkan_exp1_sense_count %>% mutate(perc = round(n/sum(n)*100, 2))) %>% arrange(node) %>%
  mutate(sense_type = if_else(str_detect(sense, "^phys"), "lit", "met"))

mundurkan_exp_meta_lit_root <- mundurkan_exp1 %>% 
  # filter(clause_type=="declarative") %>% 
  count(sense, clause_type, node, sort = T) %>% 
  group_by(sense) %>% 
  mutate(perc = round(n/sum(n)*100, 2)) %>% 
  arrange(sense) %>%
  ungroup()

mundurkan_exp_met_lit_by_word <- mundurkan_exp_meta_lit_sense %>% 
  group_by(node, sense_type) %>% 
  summarise(n=sum(n),perc=sum(perc), .groups = 'drop') %>% 
  group_by(node) %>% 
  mutate(total = sum(n), 
         pbin = map_dbl(pmap(list(x = n, n = total), binom.test), "p.value"), 
         dec = if_else(pbin >= 0.05, "ns", "***"), 
         dec = if_else(pbin < 0.05, "*", dec), 
         dec = if_else(pbin < 0.01, "**", dec), 
         dec = if_else(pbin < 0.001, "***", dec))

#' 
#' 
#' 
## ----figure8-mundurkan-exp-metaphoric-lit-sense-plot, fig.cap="Distribution of metaphoric and literal senses of *mundurkan* in AV and PASS (sentence-production)", warning=FALSE----

## FIGURE 8 =========
mundurkan_exp_sense_av_pass <- bind_rows(memundurkan_exp1_sense_count,
                                       dimundurkan_exp1_sense_count, 
                                       select(filter(mundurkan_exp1_declarative_sense_count_by_voice, voice == "av"), -voice)) %>% 
  mutate(voice = "pass", 
         voice = if_else(str_detect(node, "^di", negate = TRUE), "av", voice),
         sense = replace(sense, sense == "temporal", "postpone")) %>% 
  filter(node != "mundurkan")

mundurkan_exp_met_lit_sense_av_pass_plotdf <- mundurkan_exp_sense_av_pass %>% 
  # filter(sense != "phys_motion") %>% 
  group_by(voice, sense) %>%
  summarise(n = sum(n), .groups = 'drop') %>% 
  group_by(voice) %>% 
  mutate(perc = round(n/sum(n)*100, 2)) %>% 
  ungroup() %>% 
  mutate(sense = replace(sense, sense == "phys_motion", "LIT. caused backward motion"),
         voice = fct_relevel(as.factor(voice), c("av", "pass")),
         sense = fct_relevel(as.factor(sense), c("postpone", "withdraw s.o.", "deteriorate", "LIT. caused backward motion")))

mundurkan_exp_met_lit_sense_av_pass_chisq <- mundurkan_exp_met_lit_sense_av_pass_plotdf %>% 
  pivot_wider(-perc, names_from = voice, values_from = n, values_fill = 0) %>% 
  column_to_rownames('sense') %>% 
  as.matrix() %>% 
  chisq.test(correct = TRUE)
mundurkan_exp_met_lit_sense_av_pass_fisher <- fisher.test(mundurkan_exp_met_lit_sense_av_pass_chisq$observed)
mundurkan_exp_met_lit_sense_av_pass_cramer <- round(assocstats(mundurkan_exp_met_lit_sense_av_pass_chisq$observed)$cramer, 3)

fig8 <- mundurkan_exp_met_lit_sense_av_pass_plotdf %>% 
  ggplot(aes(x = toupper(voice), y = perc, fill = sense)) + 
  geom_col(position = position_dodge(.9)) + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(9, "YlGnBu")[7], 
                               RColorBrewer::brewer.pal(9, "YlGnBu")[4],
                               RColorBrewer::brewer.pal(9, "YlGnBu")[2],
                               RColorBrewer::brewer.pal(9, "YlGnBu")[1])) +
  theme_bw() + 
  geom_text(aes(label = paste("N=", n, sep = "")), position = position_dodge(.9), vjust = -.35, size = 3) + 
  theme(legend.position = "top") +
  labs(x = "Voice", y = "Percentages", fill = "Senses", caption = bquote(paste(italic(p)["Fisher Exacts; two-tailed"], .(if(mundurkan_exp_met_lit_sense_av_pass_fisher$p.value < 0.001) " < 0.001" else if(mundurkan_exp_met_lit_sense_av_pass_fisher$p.value < 0.01) " < 0.01" else if(mundurkan_exp_met_lit_sense_av_pass_fisher$p.value < 0.05) " < 0.05" else paste("=", round(mundurkan_exp_met_lit_sense_av_pass_fisher$p.value, 3))), "; Cramér's ", italic(V), "=", .(mundurkan_exp_met_lit_sense_av_pass_cramer))))

# Uncomment the following line to activate the code to save the image to computer
# fig8 + ggsave("figs/figure-8.jpeg", width = 7, height = 5.5, units = "in", dpi = 600)
fig8

#' 
#' 
## ----figure9-mundurkan-exp-met-lit-combined-assocplot, fig.cap="Association plot between metaphoric senses of *mundurkan* in AV and PASS from sentence-production experiment.", fig.asp=.775----
## FIGURE 9 ==========
mundurkan_exp_met_lit_assocplot_tb <- mundurkan_exp_met_lit_sense_av_pass_plotdf %>% 
  arrange(sense) %>% 
  pivot_wider(-perc, names_from = voice, values_from = n, values_fill = 0) %>% 
  data.frame(row.names = 1) %>% 
  as.matrix()
names(dimnames(mundurkan_exp_met_lit_assocplot_tb)) <- c("sense", "voice")

# Uncomment the following three lines to activate the code for saving the plot in computer:

# png("figs/figure-9.jpeg", width = 7, height = 6, units = "in", res = 600)
# vcd::assoc(mundurkan_exp_met_lit_assocplot_tb, shade = TRUE, legend = legend_resbased(fontsize = 10), gp_labels = gpar(fontsize = 7), labeling_args = list(set_varnames = c(voice = "Voice", sense = "Senses")), set_labels = list(voice = c("AV", "PASS")))
# dev.off()

vcd::assoc(mundurkan_exp_met_lit_assocplot_tb, shade = TRUE, legend = legend_resbased(fontsize = 10), gp_labels = gpar(fontsize = 7), labeling_args = list(set_varnames = c(voice = "Voice", sense = "Senses")), set_labels = list(voice = c("AV", "PASS")))

#' 
#' # Analysis for *ajukan*, *mengajukan*, and *diajukan*
#' 
## ----aju-data-load, message = FALSE, warning = FALSE--------------------------------
aju_root <- readRDS("aju_ROOT_all_data.rds")
aju_root_sense_count <- count(aju_root, sense, sort = TRUE)
aju_av <- readRDS("aju_AV_all_data.rds")

#' 
#' In the corpus, the root *aju*^[The bound root *aju* appears to be a reflex of an old Austronesian form \**-a-tu* that expresses physical sense of 'forward, onwards; towards the hearer' [@blust_austronesian_2010]; see [this link](https://www.trussel2.com/ACD/acd-s_c2.htm#2170). We thank the anonymous reviewer for pointing this out to us.] occurs in total `r sum(aju_root_sense_count$n)` times: `r pull(filter(aju_root_sense_count, sense == "PROPER_NAME"), n)` of these are proper name, `r pull(filter(aju_root_sense_count, sense == "typo_mengajukan"), n)` is typo for the suffixed AV form *mengajukan*, and only the remaining `r pull(filter(aju_root_sense_count, sense == "advanced"), n)` tokens can be considered relevant. In all these relevant tokens, *aju* is used as modifier in a noun phrase meaning 'advanced' (e.g., *tim __aju__* '*advanced* team'), rather than as head-predicate that can have voice prefixes. Then, the AV form *mengaju* also occurred in the corpus, but they are all typos for *mengaku* 'to admit something' and *mengacu* 'to refer to something'. The search for the potential passive form *diaju* returned no hits in the corpus. These search results for the AV and PASS with *aju* converge with the zero results from the entry in KBBI. The conventional verbal morphological constructions for *aju* is the suffixed one with -*kan* that we used in the analyses.
#' 
## ----ajukan-load-sample, message = FALSE, warning = FALSE---------------------------
ajukan <- tibble::as_tibble(readRDS("ajukan_BARE_sample_data.rds")) %>% 
  rename(voice = voice_type)
ajukan_cases <- nrow(ajukan)
mengajukan <- tibble::as_tibble(readRDS("ajukan_AV_sample_data.rds"))
diajukan <- tibble::as_tibble(readRDS("ajukan_PASS_sample_data.rds"))

#' 
#' 
## ----ajukan-voice-count-------------------------------------------------------------
ajukan_unclear_voice_id <- which(ajukan$voice == "blur")
ajukan_typo_voice_id <- which(ajukan$voice == "typo_di")
ajukan_typo_voice_df <- ajukan[ajukan_typo_voice_id, ]
ajukan <- ajukan %>% 
  filter(voice != "typo_di") %>% 
  mutate(voice = replace(voice, voice == "blur", "unclear")) 
ajukan_voice_tb <- ajukan %>% 
  count(voice, sort = TRUE) %>% 
  mutate(perc = round((n/sum(n) * 100), 2))

#' 
#' 
## ----figure10-ajukan-voice-plot, fig.cap="Distribution of *ajukan* in different voice."----
## FIGURE 10 ============
aju_voice_count <- c(av = nrow(filter(mengajukan, subsense1 != "unclear")), 
                     pass = nrow(diajukan), 
                     uv = nrow(filter(ajukan, voice != "unclear", voice != "av")))
aju_voice_chisq <- chisq.test(aju_voice_count)
fig10 <- data.frame(voice = toupper(names(aju_voice_count)), 
           n = aju_voice_count, 
           row.names = NULL) %>% 
  ggplot(aes(x = voice, y = n, fill = voice)) +
  geom_col() +
  geom_text(aes(label = paste("N=", n, sep = "")), vjust = -.35, size = 3) +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(11, "RdYlBu")[1], 
                               RColorBrewer::brewer.pal(11, "RdYlBu")[8], 
                               RColorBrewer::brewer.pal(11, "RdYlBu")[5])) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(y = "Raw frequency", x = "Voice", caption = bquote(paste(italic(X)["goodness-of-fit"]^2, "=", .(round(aju_voice_chisq$statistic, 2)), "; ", italic(df), "=", .(aju_voice_chisq$parameter), "; ", italic(p)["two-tailed"], .(if(aju_voice_chisq$p.value < 0.001) " < 0.001"))))

# Uncomment the following line to activate the code to save the plot in computer
# fig10 + ggsave("figs/figure-10.jpeg", width = 7, height = 5.5, units = "in", dpi = 600)

fig10

#' 
#' 
## ----ajukan-core-sense-statistics---------------------------------------------------
# Chi-Square goodness-of-fit test of senses for *ajukan* in AV and PASS from the corpus
ajukan_av_pass_core_chisq <- chisq.test(aju_voice_count[names(aju_voice_count)!="uv"]) 

# Chi-Square goodness-of-fit test of senses for *ajukan* in AV and PASS from the experiment
ajukan_exp_av_pass_core_chisq <- chisq.test(c(nrow(mengajukan_exp1), nrow(diajukan_exp1)))

## Even if we run the statistics for the subsenses (put in the `sense` column) for the passive and active in the experimental data, there is no clear and more importantly strong preference for each subsense to each voice for *ajukan*. The core, generic sense of these subsenses is 'propose; put forward'. Run the following codes.

# join the AV and PASS data for *ajukan* from the experiment
ajukan_exp_av_pass_subsense_mtx <- matrix(c(mengajukan_exp1_subsense_count$n, diajukan_exp1_subsense_count$n), ncol = 2, byrow = FALSE, dimnames = list(subsense = c("file; submit", "propose", "nominate", "apply for"), voice = c("av", "pass")))

# run the fisher.test
ajukan_exp_av_pass_subsense_fye <- fisher.test(ajukan_exp_av_pass_subsense_mtx)
# the p-value is larger than the standard level of p < 0.05. Uncomment the following code to verify that the p-value is indeed NOT smaller than 0.05
# fisher.test(ajukan_exp_av_pass_subsense_mtx)$p.value < 0.05

# the Cramér's V is also small (i.e., the effect of different distribution is small). Uncomment the following code to see the results
# vcd::assocstats(ajukan_exp_av_pass_subsense_mtx)$cramer

# Uncomment the following code to see the association plot for the subsenses of *ajukan* across voice. They are all in grey shading.
# vcd::assoc(ajukan_exp_av_pass_subsense_mtx, shade = TRUE, legend = legend_resbased(fontsize = 10), gp_labels = gpar(fontsize = 7), labeling_args = list(set_varnames = c(voice = "Voice", subsense = "Sub-senses")), set_labels = list(voice = c("AV", "PASS")))

#' 
#' 
#' # Analysis for *undur*, *undurkan*, *mengundurkan*, *diundurkan*
#' 
## ----undur-load-data, message=FALSE, warning=FALSE----------------------------------
# UNDUR------
undur <- readRDS("undur_ROOT_all_data.rds") %>% 
  select(-duplicate) %>% 
  mutate(function_of_node = replace(function_of_node, is.na(function_of_node),
                                    "v-tr"),
         function_of_node = replace(function_of_node, 
                                    function_of_node == "intransitive", 
                                    "v-intr"),
         sense = replace(sense, sense == "temporal_postpone", "postpone"),
         node = tolower(node)) %>%
  filter(!typo, !sense %in% c("duplicate", "kind_of_animal"), 
         function_of_node != "nominal") %>% 
  as_tibble()
mengundur <- readRDS("undur_AV_all_data.rds") %>% 
  filter(!typo, sense != "duplicate") %>% 
  mutate(voice = "av", 
         sense = replace(sense, sense == "temporal_postpone", "postpone"),
         node = tolower(node)) %>% 
  as_tibble() # all temporal 'postpone'
diundur <- readRDS("undur_PASS_sample_data.rds") %>% 
  filter(sense != "duplicate") %>% 
  mutate(voice = "pass", 
         sense = replace(sense, sense == "temporal_postpone", "postpone"),
         node = tolower(node)) %>% 
  as_tibble() # all temporal 'postpone'

# UNDURKAN------
undurkan <- readRDS("undurkan_BARE_all_data.rds") %>% 
  as_tibble() %>% 
  mutate(sense = replace(sense, sense == "temporal_postpone", "postpone"),
         node = tolower(node)) # low frequency; all AV
mengundurkan <- readRDS("undurkan_AV_sample_data.rds") %>%
  mutate(voice = "av", sense = replace(sense, sense == "temporal_postpone", "postpone"),
         node = tolower(node)) %>% 
  as_tibble() # vastly 'step down; bow out' in fixed verb phrase "mengundurkan diri"
diundurkan <- readRDS("undurkan_PASS_all_data.rds") %>% 
  filter(sense != "duplicate") %>% 
  mutate(voice = "pass", 
         sense = replace(sense, sense == "temporal_postpone", "postpone"),
         node = tolower(node)) %>% 
  as_tibble() # all temporal


#' 
## ----undur-undurkan-forms-count-----------------------------------------------------
undur_sense_allvoice <- undur %>% 
  filter(voice == "uv") %>% 
  select(node, voice, sense) %>% 
  bind_rows(mengundur %>% select(node, voice, sense)) %>% 
  bind_rows(diundur %>% select(node, voice, sense)) %>% 
  mutate(node = as.factor(node),
         node = fct_relevel(node, "undur", "mengundur", "diundur"),
         base = "undur")
undurkan_sense_allvoice <- undurkan %>% 
  filter(voice == "uv") %>% 
  select(node, voice, sense) %>% 
  bind_rows(mengundurkan %>% select(node, voice, sense)) %>% 
  bind_rows(diundurkan %>% select(node, voice, sense)) %>% 
  mutate(node = as.factor(node),
         node = fct_relevel(node, "mengundurkan", "diundurkan"),
         base = "undurkan")
undur_undurkan_forms <- bind_rows(undur_sense_allvoice, undurkan_sense_allvoice)
undur_undurkan_forms_count <- undur_undurkan_forms %>% 
  count(node, voice, sort = TRUE)
undur_undurkan_forms_chisq <- undur_undurkan_forms_count %>%
  pivot_wider(names_from = voice,
              values_from = n,
              values_fill = 0) %>%
  arrange(node) %>%
  data.frame(row.names = 1) %>%
  rowSums() %>% 
  chisq.test()

#' 
## ----figure11-undur-undurkan-forms-plot, fig.cap="Distribution of voice for the metaphoric usages of the base *undur* and *undurkan*"----
# SPLIT BY BASE-----
## FIGURE 11 ============
undur_undurkan_forms_count_with_base <- undur_undurkan_forms_count %>%
    mutate(voice = toupper(voice),
           base = c("undurkan", "undur", "undur", "undurkan", "undur")) %>% 
  group_by(base) %>% 
  mutate(perc = round(n/sum(n)*100, 2))
fig11 <- undur_undurkan_forms_count %>%
    mutate(voice = toupper(voice),
           base = c("undurkan", "undur", "undur", "undurkan", "undur")) %>%
  ggplot(aes(x = node, y = n, fill = voice)) +
  geom_col(position = position_dodge(.9)) +
  geom_text(aes(label = paste("N=", n, sep = "")), vjust = -.35, size = 3) +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(11, "RdYlBu")[1], RColorBrewer::brewer.pal(11, "RdYlBu")[8], RColorBrewer::brewer.pal(11, "RdYlBu")[5])) +
  theme_bw() +
  facet_wrap(~base, scales = "free_x", labeller = labeller(base = c(undur = "base/root: UNDUR", undurkan = "base/root: UNDURKAN"))) +
  theme(legend.position = "top", strip.text.x = element_text(face = "bold")) +
  labs(fill = "Voice", x = "Verbs", y = "Raw frequency")

# Uncomment the following line to activate the code to save the plot in computer
# fig11 + ggsave("figs/figure-11.jpeg", width = 8, height = 5.5, units = "in", dpi = 600)
fig11

#' 
#' 
## ----undur-sense-allvoice-count-----------------------------------------------------
undur_sense_allvoice <- undur %>% 
  filter(voice == "uv") %>%
  select(node, voice, sense) %>% 
  bind_rows(mengundur %>% select(node, voice, sense)) %>% 
  bind_rows(diundur %>% select(node, voice, sense))
undur_sense_allvoice_count <- undur_sense_allvoice %>% 
  count(voice, sense, sort = T)
undur_allvoice_count <- undur_sense_allvoice_count %>% group_by(voice) %>%
  summarise(n=sum(n), .groups='drop') %>% 
  data.frame(row.names = 1)

# code to run the Chi-Square goodness-of-fit for distribution of voice types of *undur* (at the end of Section 5.4, i.e., in the paragraph below Figure 11).
undur_allvoice_chisq <- chisq.test(undur_allvoice_count)

#' 
#' 
## ----undur-postpone-sense-allvoice-chisq-goodness-of-fit----------------------------
# Code to run the Chi-Square goodness-of-fit for *undur* in AV and PASS. (Section 5.4.1 in the paper)
names(undur_sense_allvoice_count$n) <- undur_sense_allvoice_count$voice
undur_senses_allvoice_goodness_of_fit <- chisq.test(undur_sense_allvoice_count$n[names(undur_sense_allvoice_count$n)!="uv"])

#' 
#' 
#' [Figure \@ref(fig:figure11-undur-undurkan-forms-plot) above](#figure11-undur-undurkan-forms-plot) shows asymmetry in the frequency of occurrence of the words in certain voice morphologies. The suffixed AV *mengundurkan* (`r pull(filter(undur_undurkan_forms_count_with_base, node=='mengundurkan'), perc)`% of the total `r pull(filter(tally(undur_undurkan_forms_count_with_base, wt=n), base=='undurkan'), n)` tokens of the base *undurkan*) is much more frequent than the unsuffixed AV *mengundur* (`r pull(filter(undur_undurkan_forms_count_with_base, node=='mengundur'), perc)`% of the total `r pull(filter(tally(undur_undurkan_forms_count_with_base, wt=n), base=='undur'), n)` tokens of the base *undur*). The unsuffixed PASS *diundur* (`r pull(filter(undur_undurkan_forms_count_with_base, node=='diundur'), perc)`% of the total `r pull(filter(tally(undur_undurkan_forms_count_with_base, wt=n), base=='undur'), n)` tokens of the base *undur*) is much more frequent than the suffixed PASS *diundurkan* (`r pull(filter(undur_undurkan_forms_count_with_base, node=='diundurkan'), perc)`% of the total `r pull(filter(tally(undur_undurkan_forms_count_with_base, wt=n), base=='undurkan'), n)` tokens of the base *undurkan*). The UV form is significantly lower than expected by chance compared to other voice types for unsuffixed verbs based on the root *undur* (*X*^2^~goodness-of-fit~=`r round(undur_allvoice_chisq$statistic, 2)`; *df*=`r undur_allvoice_chisq$parameter`; *p*~two-tailed~`r if(undur_allvoice_chisq$p.value < 0.001) " < 0.001"`). For this reason, we only analysed the AV and PASS data for *undur* root (i.e. *mengundur* and *diundur*) and *undurkan* (*mengundurkan* and *diundurkan*). Similar stems are analysed in the experimental data.
#' 
#' 
#' ## *undur*
#' 
#' The root *undur* in AV *mengundur* (N=`r pull(filter(undur_sense_allvoice_count, voice=='av'), n)`), UV *undur* (N=`r pull(filter(undur_sense_allvoice_count, voice=='uv'), n)`), and PASS *diundur* (N=`r pull(filter(undur_sense_allvoice_count, voice=='pass'), n)`) all conveys the 'postpone' sense. No variation of senses found across voice. *Undur* is thus meaning-preserving in the sample since its AV and PASS alternations preserve the same meaning. However, comparing the distribution of this sense in AV and PASS suggests that 'postpone' is significantly more frequent in PASS than in AV (*X*^2^~goodness-of-fit~=`r round(undur_senses_allvoice_goodness_of_fit$statistic, 2)`; *df*=`r undur_senses_allvoice_goodness_of_fit$parameter`; *p*~two-tailed~`r if(undur_senses_allvoice_goodness_of_fit$p.value < 0.001) " < 0.001"`), simply because the token frequency of the PASS is greater.
#' 
## ----undur-exp-postpone-sense-allvoice-chisq-goodness-of-fit------------------------
# code for chi-square goodness-of-fit test for the EXPERIMENTAL data of *undur* in AV and PASS (Section 5.4.1)
undur_sense_allvoice_exp_goodness_of_fit <- chisq.test(c(pass = diundur_exp1_sense_count$n, 
                                                         av = nrow(filter(mengundur_exp1, P_lu !="INTR", sense=="temporal"))))

# uncomment the code below to print the results
# undur_sense_allvoice_exp_goodness_of_fit

#' 
#' 
#' If we turn to the experimental data, all transitive occurrences of the AV *mengundur* (N=`r mengundur_exp1_sense_count$n[mengundur_exp1_sense_count$sense=='temporal']`)^[Note that `r nrow(filter(mengundur_exp1, P_lu =="INTR", sense=="phys_motion"))` out of `r nrow(mengundur_exp1)` tokens of *mengundur* in the elicitation data are intransitive, meaning 'step back', where the SUBJ is the mover/Actor.] and all the occurrences of the PASS *diundur* (N=`r diundur_exp1_sense_count$n`) express the 'postpone' sense; however, its distribution across these two voice types do not differ significantly (*X*^2^~goodness-of-fit~=`r round(chisq.test(c(diundur_exp1_sense_count$n, nrow(filter(mengundur_exp1, P_lu !="INTR", sense=="temporal"))))$statistic, 2)`; *df*=`r chisq.test(c(diundur_exp1_sense_count$n, nrow(filter(mengundur_exp1, P_lu !="INTR", sense=="temporal"))))$parameter`; *p*~two-tailed~=`r round(chisq.test(c(diundur_exp1_sense_count$n, nrow(filter(mengundur_exp1, P_lu !="INTR", sense=="temporal"))))$p.value, 3)`). This is finding that supports the meaning-preserving hypothesis in voice alternation. 
#' 
#' While predominantly in corpus sample and sentence-production *mengundur* and *diundur* express metaphoric meaning of 'postpone', remnant of the archaic physical meaning 'backward motion' is still produced in the elicitation experiment. There are `r nrow(filter(mengundur_exp1 , P_lu=='INTR', sense=="phys_motion"))` tokens and all are intransitive with *meN-* prefix (unlike the transitive metaphoric use of *undur* in AV and PASS):
#' 
#' (@mengundur_intr1) `r pull(filter(mengundur_exp1 , P_lu=='INTR', sense=="phys_motion"), node_sentences)[4]`
#' 
#' (@mengundur_intr1) `r pull(filter(mengundur_exp1 , P_lu=='INTR', sense=="phys_motion"), node_sentences)[1]`
#' 
#' The remaining `r nrow(filter(mengundur_exp1, str_detect(sense, "^others")))` metaphoric uses of intransitive *mengundur* are mixture of 'back off (of a competition)' (N=`r nrow(filter(mengundur_exp1, str_detect(sense, "^others_back")))`) (see @mengundur_backoff), 'deteriorate' (N=`r nrow(filter(mengundur_exp1, str_detect(sense, "^others_deter")))`) (@mengundur_deteriorate), and 'reduce' (N=`r nrow(filter(mengundur_exp1, str_detect(sense, "^others_red")))`) (@mengundur_reduce).
#' 
#' (@mengundur_backoff) `r pull(filter(mengundur_exp1 , sense=="others_back off"), node_sentences)[2]`
#' (@mengundur_deteriorate) `r pull(filter(mengundur_exp1 , sense=="others_deteriorate"), node_sentences)[1]`
#' (@mengundur_reduce) `r pull(filter(mengundur_exp1 , sense=="others_reduce"), node_sentences)[1]`
#' 
#' 
#' ## *undurkan*
#' 
#' ### Corpus data for *undurkan*
#' 
## ----undurkan-sense-count-----------------------------------------------------------

undurkan_sense_allvoice <- bind_rows(mengundurkan %>% select(node, voice, sense)) %>%
  bind_rows(diundurkan %>% select(node, voice, sense))

undurkan_sense_allvoice_count <- undurkan_sense_allvoice %>% 
  count(voice, sense, sort = T) %>% 
  group_by(voice) %>% 
  mutate(perc = round(n/sum(n) * 100, 2)) %>% 
  ungroup()

undurkan_sense_all_count <- undurkan_sense_allvoice_count %>% 
  group_by(sense) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  mutate(perc = round(n/sum(n) * 100, 2))


#' 
#' 
#' In contrast to the unsuffixed *undur*, the suffixed form *undurkan* predominantly convey the sense of 'cause someone to step down or resign' (`r pull(filter(undurkan_sense_all_count, sense=="step down; resign; bow out"), perc)`%; N=`r pull(filter(undurkan_sense_all_count, sense=="step down; resign; bow out"), n)`), followed by the 'postpone' sense (`r pull(filter(undurkan_sense_all_count, sense=="postpone"), perc)`%; N=`r pull(filter(undurkan_sense_all_count, sense=="postpone"), n)`). No literal, physical motion sense is found for *undurkan*. [Figure \@ref(fig:figure12-undurkan-sense-allvoice-plot)](#figure12-undurkan-sense-allvoice-plot) shows the distribution of these senses across voice.
#' 
## ----undurkan-sense-voice-chisq-fye, warning = FALSE, message = FALSE---------------
undurkan_sense_voice_chisq <- undurkan_sense_allvoice_count %>% 
  filter(voice != "uv") %>% 
  select(-perc) %>% 
  pivot_wider(values_from = n, names_from = voice, values_fill = 0) %>%
  data.frame(row.names = 1) %>% 
  as.matrix() %>% 
  chisq.test()
undurkan_sense_cramer <- round(vcd::assocstats(undurkan_sense_voice_chisq$observed)$cramer, 3)

# Fisher Exact Test due to distributional assumption for Chi-Square test is not met
undurkan_sense_voice_fye <- undurkan_sense_allvoice_count %>% 
  filter(voice != "uv") %>% 
  select(-perc) %>% 
  pivot_wider(values_from = n, names_from = voice, values_fill = 0) %>%
  data.frame(row.names = 1) %>% 
  fisher.test()

#' 
## ----figure12-undurkan-sense-allvoice-plot, fig.cap = "Distribution of metaphoric senses of *undurkan* across AV and PASS."----
# PERCENTAGES-------
## FIGURE 12 =================
fig12 <- undurkan_sense_allvoice_count %>% 
  mutate(sense = replace(sense, sense != "postpone", "cause to step down/resign"),
         voice = toupper(voice)) %>% 
  ggplot(aes(x = voice, y = perc, fill = sense)) +
  geom_col(position = position_dodge(.9)) +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(9, "YlGnBu")[7],
                               RColorBrewer::brewer.pal(9, "YlGnBu")[4])) +
  theme_bw() +
  geom_text(aes(label = paste("N=", n, sep = "")), 
            position = position_dodge(.9), 
            vjust = -.35, size = 3) +
  theme(legend.position = "top") +
  labs(x = "Voice", fill = "Metaphoric senses", y = "Percentages", caption = bquote(paste(italic(p)["Fisher Exacts; two-tailed"], .(if(undurkan_sense_voice_fye$p.value < 0.001) " < 0.001"), "; Cramér's ", italic(V), "=", .(undurkan_sense_cramer))))

# Uncomment the following line to active the code to save the plot in computer
# fig12 + ggsave("figs/figure-12.jpeg", width = 7, height = 5.5, units = "in", dpi = 600)
fig12

#' 
#' Comparing the observed with the expected frequencies of the senses across voice, 'postpone' occurs more frequently than expected in PASS and is relatively less frequent in AV over 'step down', which is more frequent than expected. This asymmetric distribution is highly significant (*p*~Fisher~ ~Exact;~ ~two-tailed~ `r if(undurkan_sense_voice_fye$p.value < 0.001) " < 0.001" else if(undurkan_sense_voice_fye$p.value < 0.01) " < 0.01" else if(undurkan_sense_voice_fye$p.value < 0.05) " < 0.05" else if(undurkan_sense_voice_fye$p.value >= 0.05) paste("=", round(undurkan_sense_voice_fye$p.value, 3))`) and demonstrates a highly strong effect (Cramér's *V*=`r undurkan_sense_cramer`). The fixed phrase *mengundurkan diri* 'to cause one's self to step back; resign' accounts for the high frequency of 'step down' in AV and this phrase has no PASS counterpart. The association plot in [Figure \@ref(fig:figure13-undurkan-sense-assocplot)](#figure13-undurkan-sense-assocplot) further emphasises the preference of 'postpone' in PASS and the dispreferences of 'postpone' in AV and 'step down' in PASS.  
#' 
#' 
## ----figure13-undurkan-sense-assocplot, fig.cap = "Association plot between metaphoric senses of *undurkan* with AV and PASS."----
## FIGURE 13 ==========
names(dimnames(undurkan_sense_voice_chisq$observed)) <- c("Sense", "Voice")
colnames(undurkan_sense_voice_chisq$observed) <- toupper(colnames(undurkan_sense_voice_chisq$observed))

# Uncomment the following three lines to activate the codes to save the plot to computer
# png("figs/figure-13.jpeg", width = 7, height = 6, units = "in", res = 600)
# assoc(undurkan_sense_voice_chisq$observed, shade = TRUE, gp_labels = gpar(fontsize = 10), legend = legend_resbased(fontsize = 10.5))
# dev.off()

assoc(undurkan_sense_voice_chisq$observed, shade = TRUE, gp_labels = gpar(fontsize = 10), legend = legend_resbased(fontsize = 10.5))

#' 
#' ### Experimental data for *undurkan*
#' 
#' 
## ----figure14-mengundurkan-diundurkan-exp-sense-plot, warning = FALSE, message = FALSE, fig.cap="Distribution of metaphoric senses of *undurkan* across AV and PASS from sentence-production experiment"----

undurkan_exp_sense_av_pass <- bind_rows(mengundurkan_exp1_sense_count,
                                       diundurkan_exp1_sense_count)
undurkan_exp_sense_av_pass_plotdf <- undurkan_exp_sense_av_pass %>% 
  mutate(voice = "pass", voice = if_else(str_detect(node, "^me"), "av", voice),
         sense = replace(sense, sense %in% c("step down; resign; bow out", "sacked from a position"), "cause to step down/resign")) %>%
  group_by(voice, sense) %>%
  summarise(n = sum(n), .groups = 'drop') %>% 
  group_by(voice) %>% 
  mutate(perc = round(n/sum(n)*100, 2)) %>% 
  ungroup() %>% 
  mutate(voice = fct_relevel(as.factor(voice), c("av", "pass")),
         sense = replace(sense, sense == "temporal", "postpone"),
         sense = fct_relevel(as.factor(sense), c("cause to step down/resign", "postpone")))
undurkan_exp_sense_av_pass_chisq <- undurkan_exp_sense_av_pass_plotdf %>% 
  pivot_wider(-perc, names_from = voice, values_from = n, values_fill = 0L) %>% 
  column_to_rownames('sense') %>% 
  as.matrix() %>% 
  chisq.test()
undurkan_exp_sense_av_pass_fye <- fisher.test(undurkan_exp_sense_av_pass_chisq$observed)
undurkan_exp_sense_av_pass_cramer <- round(assocstats(undurkan_exp_sense_av_pass_chisq$observed)$cramer, 3)

## FIGURE 14 ========

fig14 <- undurkan_exp_sense_av_pass_plotdf %>% 
  ggplot(aes(x = toupper(voice), y = perc, fill = sense)) + 
  geom_col(position = position_dodge(.9)) + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(9, "YlGnBu")[7],
                               RColorBrewer::brewer.pal(9, "YlGnBu")[4])) +
  theme_bw() + 
  geom_text(aes(label = paste("N=", n, sep = "")), position = position_dodge(.9), vjust = -.35, size = 2.75) + 
  theme(legend.position = "top") +
  labs(x = "Voice", y = "Percentages", fill = "Metaphoric senses", caption = bquote(paste(italic(X)["independence"]^2, "=", .(round(undurkan_exp_sense_av_pass_chisq$statistic, 2)), "; ", italic(df), "=", .(undurkan_exp_sense_av_pass_chisq$parameter), "; ", italic(p)["two-tailed"], .(if(undurkan_exp_sense_av_pass_chisq$p.value < 0.001) " < 0.001" else if(undurkan_exp_sense_av_pass_chisq$p.value < 0.01) " < 0.01" else if(undurkan_exp_sense_av_pass_chisq$p.value < 0.05) " < 0.05" else if(undurkan_exp_sense_av_pass_chisq$p.value >= 0.05) paste("=", round(undurkan_exp_sense_av_pass_chisq$p.value, 3))), "; Cramér's ", italic(V), "=", .(undurkan_exp_sense_av_pass_cramer))))

# Uncomment the following line to activate the code to save the plot in computer
# fig14 + ggsave("figs/figure-14.jpeg", width = 7, height = 5.5, units = "in", dpi = 600)

fig14

#' 
#' The physical, cause something to move backwards sense for the stem *undurkan* only occurs in the unprefixed *undurkan* (i.e. in `r undurkan_exp1 %>% count(sense) %>% filter(sense=='phys_motion') %>% pull(n)` out of `r undurkan_exp1 %>% count(sense) %>% .$n %>% sum()` tokens produced by the participants), and never occurs in the AV *mengundurkan* and PASS *diundurkan* as they all express metaphoric senses. There are only `r pull(filter(undurkan_exp1_declarative_voice_count, voice=='uv'), n)` tokens of UV from `r sum(undurkan_exp1_declarative_voice_count$n)` declarative use of *undurkan*; the rest are AV.
#' 
#' 
#' Distribution of senses across voice in experimental data mirrors that in the corpus and the distribution is statistically highly significant (*X*^2^=`r round(undurkan_exp_sense_av_pass_chisq$statistic, 2)`; *df*=`r undurkan_exp_sense_av_pass_chisq$parameter`; *p*~two-tailed~`r if(undurkan_exp_sense_av_pass_chisq$p.value < 0.001) " < 0.001" else if(undurkan_exp_sense_av_pass_chisq$p.value < 0.01) " < 0.01" else if(undurkan_exp_sense_av_pass_chisq$p.value < 0.05) " < 0.05" else if(undurkan_exp_sense_av_pass_chisq$p.value >= 0.05) paste("=", round(undurkan_exp_sense_av_pass_chisq$p.value, 3))`)^[For this experimental data, we use Chi-Square test since the distributional assumption is met (i.e. all expected frequencies are larger than five)] with a highly strong effect size (Cramér's *V*=`r undurkan_exp_sense_av_pass_cramer`). Sentence production data clearly show that speakers have strong representation of association between 'postpone' and PASS. It is argued that this is the entrenched form-meaning pairing speakers predominantly have for *undurkan*.
#' 
#' 
## ----figure15-undurkan-exp-sense-assocplot, fig.cap = "Association plot between metaphoric senses of *undurkan* with AV and PASS from sentence-production experiment."----
## FIGURE 15 =====
names(dimnames(undurkan_exp_sense_av_pass_chisq$observed)) <- c("Sense", "Voice")
colnames(undurkan_exp_sense_av_pass_chisq$observed) <- toupper(colnames(undurkan_exp_sense_av_pass_chisq$observed))

# Uncomment the following three lines to activate the code to save the plot in computer
# png("figs/figure-15.jpeg", width = 7, height = 6, units = "in", res = 600)
# assoc(undurkan_exp_sense_av_pass_chisq$observed, shade = TRUE, gp_labels = gpar(fontsize = 10), legend = legend_resbased(fontsize = 10.5))
# dev.off()

assoc(undurkan_exp_sense_av_pass_chisq$observed, shade = TRUE, gp_labels = gpar(fontsize = 10), legend = legend_resbased(fontsize = 10.5))

#' 
#' 
## ----table3-parallelism-across-temporal-sense---------------------------------------
## TABLE 3 ==========
obs_exp <- c(if_else(majukan_combined_av_pass_chisq0$observed["temporal", "pass"] > 
                       majukan_combined_av_pass_chisq0$expected["temporal", "pass"], 
                    "PASS > AV", "PASS < AV"), 
             if_else(majukan_exp_sense_av_pass_chisq$observed["temporal", "pass"] > 
                       majukan_exp_sense_av_pass_chisq$expected["temporal", "pass"], 
                    "PASS > AV", "PASS < AV"),
             if_else(mundurkan_met_lit_all_chisq$observed["temporal_postpone", "pass"] > 
                       mundurkan_met_lit_all_chisq$expected["temporal_postpone", "pass"],
                    "PASS > AV", "PASS < AV"),
             if_else(mundurkan_exp_met_lit_sense_av_pass_chisq$observed["postpone", "pass"] > 
                       mundurkan_exp_met_lit_sense_av_pass_chisq$expected["postpone", "pass"],
                     "PASS > AV", "PASS < AV"),
             if_else(undur_senses_allvoice_goodness_of_fit$observed[names(undur_senses_allvoice_goodness_of_fit$observed) == "pass"] > 
                       undur_senses_allvoice_goodness_of_fit$expected[names(undur_senses_allvoice_goodness_of_fit$expected) == "pass"],
                     "PASS > AV", "PASS < AV"),
             if_else(undur_sense_allvoice_exp_goodness_of_fit$observed[names(undur_sense_allvoice_exp_goodness_of_fit$observed) == "pass"] > 
                       undur_sense_allvoice_exp_goodness_of_fit$expected[names(undur_sense_allvoice_exp_goodness_of_fit$expected) == "pass"],
                     "PASS > AV", "PASS < AV"),
             if_else(undurkan_sense_voice_chisq$observed["postpone", "PASS"] > 
                       undurkan_sense_voice_chisq$expected["postpone", "pass"],
                     "PASS > AV", "PASS < AV"),
             if_else(undurkan_exp_sense_av_pass_chisq$observed["postpone", "PASS"] > 
                       undurkan_exp_sense_av_pass_chisq$expected["postpone", "pass"],
                     "PASS > AV", "PASS < AV")
             )
effsize <- c(round(majukan_combined_av_pass_assocstats$cramer, 3),
             majukan_exp_sense_av_pass_cramer,
             round(mundurkan_met_lit_assocstats$cramer, 3),
             mundurkan_exp_met_lit_sense_av_pass_cramer,
             NA,
             NA,
             undurkan_sense_cramer,
             undurkan_exp_sense_av_pass_cramer
             )
signif <- c(majukan_combined_av_pass_chisq0$p.value,
            majukan_exp_sense_av_pass_chisq$p.value,
            mundurkan_met_lit_all_fye$p.value,
            mundurkan_exp_met_lit_sense_av_pass_fisher$p.value,
            undur_senses_allvoice_goodness_of_fit$p.value,
            undur_sense_allvoice_exp_goodness_of_fit$p.value,
            undurkan_sense_voice_fye$p.value,
            undurkan_exp_sense_av_pass_chisq$p.value
            )
datatype <- rep(c("corpus", "elicitation"), 4)
bases <- rep(c("majukan", "mundurkan", "undur", "undurkan"), each = 2)
temporal_sense <- rep(c("happen earlier", "postpone"), c(2, 6))
parallelism_df <- tibble(temporal_sense, bases, datatype, obs_exp, signif, effsize) %>% 
  mutate(signif_code = if_else(signif < 0.05, "*", "ns"),
         signif_code = if_else(signif < 0.01, "**", signif_code),
         signif_code = if_else(signif < 0.001, "***", signif_code),
         pval_short = if_else(signif < 0.05, "*p* < 0.05", "*p* > 0.05"),
         pval_short = if_else(signif < 0.01, "*p* < 0.01", pval_short),
         pval_short = if_else(signif < 0.001, "*p* < 0.001", pval_short),
         pval_short = if_else(signif > 0.1, "*p* > 0.1", pval_short),
         signif_test = if_else(is.na(effsize), "ChiSquare Goodness-of-Fit", "ChiSquare"),
         signif_test = replace(signif_test, bases == "undurkan" & datatype == "corpus", "FisherExact"),
         signif_test = replace(signif_test, bases == "mundurkan", "FisherExact"),
         bases = paste("*", bases, "*", sep = ""))
parallelism_df %>% 
  knitr::kable(caption = "Parallelism across words for the interconnection of PASS and the 'temporal' senses")

#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' # Conclusion
#' 
#' # References {-}
