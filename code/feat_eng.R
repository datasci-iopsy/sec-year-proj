#keep env cln
rm(list = ls())

#script containing useful functions!
source("funs.R")

#load libs
library(tidyverse)
library(sjmisc) #masks purrr::is_empty; tidyr::replace_na; tibble::add_case
library(patchwork)
# library(psych) #masks ggplot::alpha

#script opts
theme_set(theme_minimal())
# option(tibble.width = Inf)

#specify col_names
names(read_csv("../data/proj-data_cln.csv", n_max = 0)) -> col_names

#read in data
read_csv("../data/proj-data_cln.csv", #dir w/ data file
         col_types = cols(Age = "d"), #parse "Age" col as num
         skip = 2, #skip first 2 rows
         col_names = col_names) -> #specify col names
    raw

#list containing processed dfs
list() -> dat_ls

#demographics items
raw %>%
    #extract cols of interest
    select(Age:Race, Edu, Industry) %>%
    #shorten "High school..." for ease of reading
    mutate(Edu = if_else(grepl("High school", Edu), "High school/GED", Edu)) %>%
    #shorted opt for Edu
    mutate(Edu = str_remove_all(Edu, "in college ")) ->
    dat_ls$demo

#extract respective scales
raw %>%
    #extract only scales
    select(!Age:Industry, !starts_with("Att_Chk")) %>%
    #apply custom func across cols - convert chrs to nums
    map_df(., function(.x) {
        unfactorise(.x)
        }) %>%
    #handle reverse coded items
    mutate(
        across(
            ends_with("_R"), ~ 6 - .x
            )
        ) ->
    dat_ls$scales

# Demographic Review ------------------------------------------------------

# #demograph tbls - sjmisc pkg
# frq(dat_ls$demo, Sex:Industry) %>%
#     map(., ~ tibble(.x) %>%
#             select(val, frq, pct = raw.prc) %>%
#             slice(1:10))

#calc proportions across demographic items
dat_ls$demo[-1] %>%
    #apply `count` function across df
    map(., function(.x) {
        count(tibble(x = .x), x) %>%
            mutate(pct = (n / sum(n) * 100)) %>%  #calc props
            slice(1:10) #extract top 10
        }) %>%
    #apply custom plot function across cols
    map(., function(.x) {
        ggplot(.x,
               aes(y = reorder(.data[["x"]], pct),  x = .data[["pct"]] )) +
            #lollipop plot
            geom_point(aes(color = .data[["x"]])) +
            geom_segment(
                aes(y = .data[["x"]],
                    yend = .data[["x"]],
                    x = 0,
                    xend = .data[["pct"]],
                    color = .data[["x"]]),
                size = .75) +
            # #bar graph version - comment out lines 70 - 78
            # geom_bar(aes(fill = .data[["x"]] ),
            #          stat = "identity") +
            #paste "%" symbol across axis ticks
            scale_x_continuous(labels = function(.x) { paste0(.x, "%") }) +
            #rm titles from x & y axes
            labs(x = "",
                 y = "") +
            theme(legend.position = "none") #rm legend
        }) ->
    demo_plot_ls

#extract plots
# attach(demo_plot_ls) #to call plots directly from list
demo_plot_ls$Sex + labs(subtitle = "Sex") +
    demo_plot_ls$Race + labs(subtitle = "Race") +
    demo_plot_ls$Edu + labs(subtitle = "Edu") +
    demo_plot_ls$Industry + labs(subtitle = "Industry - Top 10") +
    plot_annotation(title = "Demographics")
# detach(demo_plot_ls) #undo attach; run line 93-95 to plot post detach

#save plot
ggsave("../figs/demo_prop_plot.png",
       plot = last_plot(),
       width = 10,
       height = 6)

# End ----


# Likert Review -----------------------------------------------------------

#list of scales
list() -> scales_ls

#extract respective scales
select(raw, starts_with("BFI")) -> scales_ls$bfi
select(raw, starts_with("HOS")) -> scales_ls$hos
select(raw, starts_with("EQ")) -> scales_ls$eq
select(raw, starts_with("PA")) -> scales_ls$pa
select(raw, starts_with("NA")) -> scales_ls$na
select(raw, starts_with("JUS")) -> scales_ls$jus
select(raw, starts_with("CWB")) -> scales_ls$cwb
select(raw, starts_with("SAT")) -> scales_ls$sat

#update
find_var(dat_ls$scales, pattern = "BFI_E", out = "df") %>% plot_likert()
plot_likert(test)

#list of demographic variable counts
lapply(dat_ls$demo[-1],
                  function(x) {

                      my_lst = data.frame(table(x))
                      names(my_lst) = c("level", "n")

                      return(my_lst)
                        }
                    )
demoList

#convert fcts to num across survey items; "unfactorised" is in funs.R script
dat = as.data.frame(sapply(dat[, -c(1:6)], unfactorise)) %>%
    #convert reverse coded items by subtracting 6
    mutate_at(
        vars(ends_with("_R")), list(~ 6 - .)
    )

#list of construct subscales
datList = list(
    bfi = select(dat, starts_with("BFI")),
    hos = select(dat, starts_with("HOS")),
    eq = select(dat, starts_with("EQ")),
    pan = select(dat, matches("PA|NA")),
    jus = select(dat, starts_with("JUS"))
    )

#Confirmatory Factor Analysis
shhh(MVN) #sROC 0.1-2 loaded
shhh(lavaan) #masks psych::cor2cov
shhh(GPArotation)

#check assumptions for bfi items
mvn(select(datList[["bfi"]], contains("_A")), univariatePlot = "histogram")
mvn(select(datList[["bfi"]], contains("_N")), univariatePlot = "histogram")

#paste('full',paste(paste0('var',1:50),collapse='+'),sep='=~') #useful syntax!

#CFA of agreeableness and neuroticism; most relevant to study
bfi_mod = ' agree =~ BFI_A1_R + BFI_A2 + BFI_A3_R + BFI_A4 + BFI_A5 +
            BFI_A6_R + BFI_A7 + BFI_A8_R

            neuro =~ BFI_N1 + BFI_N2_R + BFI_N3 + BFI_N4 + BFI_N5_R +
            BFI_N6 + BFI_N7_R + BFI_N8'

bfi_modNest = ' full =~ BFI_A1_R + BFI_A2 + BFI_A3_R + BFI_A4 + BFI_A5 +
                BFI_A6_R + BFI_A7 + BFI_A8_R + BFI_N1 + BFI_N2_R + BFI_N3 +
                BFI_N4 + BFI_N5_R + BFI_N6 + BFI_N7_R + BFI_N8 '

#fit the models
bfi_fit <- cfa(bfi_mod, data = datList[["bfi"]], estimator = "DWLS")
bfiNest_fit = cfa(bfi_modNest, data = datList[["bfi"]], estimator = "DWLS")

#fit statistics
summary(bfi_fit, fit.measures = TRUE, standardized = TRUE)
summary(bfiNest_fit, fit.measures = TRUE, standardized = TRUE)

#compare complex vs nested models - nested wins!
anova(bfi_fit, bfiNest_fit)

#CFA of hostility items
#check assumptions for hostility items
mvn(select(datList[["hos"]], contains("_R")), univariatePlot = "histogram")
mvn(select(datList[["hos"]], contains("_S")), univariatePlot = "histogram")

#CFA of resentment and suspicion
hos_mod = ' resent =~ HOS_R1 + HOS_R2 + HOS_R3 + HOS_R4 + HOS_R5 + HOS_R6_R +
            HOS_R7 + HOS_R8
            suspi =~ HOS_S1 + HOS_S2 + HOS_S3 + HOS_S4 + HOS_S5 + HOS_S6 +
            HOS_S7 + HOS_S8 + HOS_S9_R + HOS_S10_R '

hos_modNest = ' full =~ HOS_R1 + HOS_R2 + HOS_R3 + HOS_R4 + HOS_R5 + HOS_R6_R +
                HOS_R7 + HOS_R8 + HOS_S1 + HOS_S2 + HOS_S3 + HOS_S4 + HOS_S5 +
                HOS_S6 + HOS_S7 + HOS_S8 + HOS_S9_R + HOS_S10_R '

#fit the models
hos_fit = cfa(hos_mod, data = datList[["hos"]], estimator = "DWLS")
hosNest_fit = cfa(hos_modNest, data = datList[["hos"]], estimator = "DWLS")

#fit statistics
summary(hos_fit, fit.measures = TRUE, standardized = TRUE)
summary(hosNest_fit, fit.measures = TRUE, standardized = TRUE)

#compare complex vs nested models - nested wins!
anova(hos_fit, hosNest_fit)

#CFA of equity sensitivity
#check assumptions
mvn(datList[["eq"]], univariatePlot = "histogram")

#CFA of equity sensitivity items
eq_mod = ' eq =~ EQ1_R + EQ2_R + EQ3_R + EQ4_R + EQ5_R + EQ6_R + EQ7_R + EQ8 +
           EQ9 + EQ10_R + EQ11 + EQ12 + EQ13 + EQ14 + EQ15 + EQ16 '

#fit the model
eq_fit = cfa(eq_mod, data = datList[["eq"]], estimator = "DWLS")

#fit statistics
summary(eq_fit, fit.measures = TRUE, standardized = TRUE)

#CFA of PANAS
#check assumptions
mvn(select(datList[["pan"]], contains("PA")), univariatePlot = "histogram")
mvn(select(datList[["pan"]], contains("NA")), univariatePlot = "histogram")

#paste('full',paste(paste0('var',1:50),collapse='+'),sep='=~') #useful syntax!
panas_mod = ' pa =~ PA1 + PA2 + PA3 + PA4 + PA5 + PA6 + PA7 + PA8 + PA9 + PA10
              na =~ NA1 + NA2 + NA3 + NA4 + NA5 + NA6 + NA7 + NA8 + NA9 + NA10 '

panas_modNest = ' full =~ PA1 + PA2 + PA3 + PA4 + PA5 + PA6 + PA7 + PA8 + PA9 +
                  PA10 + NA1 + NA2 + NA3 + NA4 + NA5 + NA6 + NA7 + NA8 + NA9 +
                  NA10 '

#fit the model
panas_fit = cfa(panas_mod, data = datList[["pan"]], estimator = "DWLS")
panasNest_fit = cfa(panas_modNest, data = datList[["pan"]], estimator = "DWLS")

#summary statistics
summary(panas_fit, fit.measure = TRUE, standardized = TRUE)
summary(panasNest_fit, fit.measure = TRUE, standardized = TRUE)

#compare complex model with nested
anova(panas_fit, panasNest_fit) #nested model wins!

#CFA of org justice
#check assumptions
mvn(select(datList[["jus"]], contains("_P")), univariatePlot = "histogram")
mvn(select(datList[["jus"]], contains("_D")), univariatePlot = "histogram")
mvn(select(datList[["jus"]], contains("_INT")), univariatePlot = "histogram")
mvn(select(datList[["jus"]], contains("_INF")), univariatePlot = "histogram")

#build models
jus_mod = ' proc =~ JUS_P1 + JUS_P2 + JUS_P3 + JUS_P4 + JUS_P5 + JUS_P6 + JUS_P7
            dist =~ JUS_D1 + JUS_D2 + JUS_D3 + JUS_D4
            int =~ JUS_INT1 + JUS_INT2 + JUS_INT3
            inf =~ JUS_INF1 + JUS_INF2 + JUS_INF3 + JUS_INF4 + JUS_INF5 '

jus_modNest = ' full =~ JUS_P1 + JUS_P2 + JUS_P3 + JUS_P4 + JUS_P5 + JUS_P6 +
                JUS_P7 + JUS_D1 + JUS_D2 + JUS_D3 + JUS_D4 + JUS_INT1 +
                JUS_INT2 + JUS_INT3 + JUS_INF1 + JUS_INF2 + JUS_INF3 +
                JUS_INF4 + JUS_INF5 '

#fit models
jus_fit = cfa(jus_mod, data = datList[["jus"]], estimator = "DWLS")
jusNest_fit = cfa(jus_modNest, data = datList[["jus"]], estimator = "DWLS")

#summary statistics
summary(jus_fit, fit.measures = TRUE, standardized = TRUE)
summary(jusNest_fit, fit.measures = TRUE, standardized = TRUE)

#model comparison
anova(jus_fit, jusNest_fit) #nested fits better!









