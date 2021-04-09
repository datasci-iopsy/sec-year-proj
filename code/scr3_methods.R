#keep env cln
rm(list = ls())

#load libs
library(tidyverse) #see output for masks
library(psych) #masks ggplot2::%+%, alpha
# library(GGally) #S3 methods overwritten
library(ggcorrplot)
library(lavaan) #masks psych::cor2cov
library(semPlot) #S3 methods overwritten by lme4
library(tictoc)
library(tidyLPA)
library(mclust) #masks psych::sim; purrr::map
library(patchwork)
# library(sjmisc) #masks purrr::is_empty; tidyr::replace_na; tibble::add_case

#script opts
purrr::map -> map #unmask and set default map fun
theme_set(theme_minimal())
options(tibble.width = Inf)

#import data -- load from feat_eng script
load("../data/r_objs/feat_eng_cln.rda")

# #list to hold feats
# list() -> vars_ls

#select feats of interest
constr_ls$agg %>%
    #rm suffix; KEEP IN MIND these are scale scores (mean aggregation)
    rename_with(~ str_remove(.x, "_ss")) %>%
    select(
        bfi_a:bfi_n,
        starts_with("hos"),
        eq,
        pa:na,
        starts_with("jus")
        ) ->
    # mutate(
    #     across(
    #         everything(), function(x, na.rm = FALSE) {
    #             (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
    #             },
    #         .names = "c_{.col}"
    #         )
    #     ) ->
    vars

#quick overview of data
glimpse(vars)
summary(vars)

# Correlation Analysis ----------------------------------------------------

# #pairs plot
# GGally::ggpairs(
#     data = vars_ls$all,
#     # upper = "blank",
#     lower = list(continuous = wrap(ggally_smooth_lm))
#     )

#list to hold corr info
list() -> corrs_ls

psych::corr.test(
    x = vars,
    use = "pairwise", #no missings in data
    method = "pearson",
    alpha = .05
    ) ->
    corrs_ls$res

#using ggcorrplot package!
ggcorrplot::ggcorrplot(
    corr = corrs_ls$res$r,
    method = "square",
    type = "lower",
    title = "Zero-order Correlation Coefficients",
    colors = c("red", "white", "skyblue"),
    lab = TRUE,
    digits = 2
    ) ->
    corrs_ls$plot

# #save corr plot
# ggsave("../figs/corr_plot.png", corrs_ls$plot)

# End ----

# CFA - BFI ---------------------------------------------------------------

#list to hold models and fits
list() -> cfa_mods
list() -> cfa_fits

#CFA of agreeableness, conscientiousness, & neuroticism
cfa_mods$bfi_f3 =
'
    agree =~ bfi_a1_r + bfi_a2 + bfi_a3_r + bfi_a4 + bfi_a5 + bfi_a6_r +
    bfi_a7 + bfi_a8_r + bfi_a9

    consc =~ bfi_c1 + bfi_c2_r + bfi_c3 + bfi_c4_r + bfi_c5_r + bfi_c6 +
    bfi_c7 + bfi_c8 + bfi_c9_r

    neuro =~ bfi_n1 + bfi_n2_r + bfi_n3 + bfi_n4 + bfi_n5_r + bfi_n6 +
    bfi_n7_r + bfi_n8
'

cfa_mods$bfi_f1 =
'
    one_f =~ bfi_a1_r + bfi_a2 + bfi_a3_r + bfi_a4 + bfi_a5 + bfi_a6_r +
    bfi_a7 + bfi_a8_r + bfi_a9 + bfi_c1 + bfi_c2_r + bfi_c3 + bfi_c4_r +
    bfi_c5_r + bfi_c6 + bfi_c7 + bfi_c8 + bfi_c9_r +  bfi_n1 + bfi_n2_r +
    bfi_n3 + bfi_n4 + bfi_n5_r + bfi_n6 + bfi_n7_r + bfi_n8
'

#fit the models
cfa(cfa_mods$bfi_f3, data = constr_ls$full, estimator = "WLSMV")  ->
    cfa_fits$bfi_f3

cfa(cfa_mods$bfi_f1, data = constr_ls$full, estimator = "WLSMV")  ->
    cfa_fits$bfi_f1

#fit statistics
summary(cfa_fits$bfi_f3, fit.measures = TRUE, standardized = TRUE)
summary(cfa_fits$bfi_f1, fit.measures = TRUE, standardized = TRUE)

# #compare complex vs nested models - nested wins!
# anova(cfa_fits$bfi_f3, cfa_fits$bfi_f1)

# #visualize model
# semPaths(cfa_fits$bfi_f3, "std", layout = "circle2")

# End ----

# CFA - Hostility ---------------------------------------------------------

#CFA of resentment & suspicion
cfa_mods$hos_f2 =
'
    resent =~ hos_r1 + hos_r2 + hos_r3 + hos_r4 + hos_r5 + hos_r6_r +
    hos_r7 + hos_r8

    sus =~ hos_s1 + hos_s2 + hos_s3 + hos_s4 + hos_s5 + hos_s6 + hos_s7 +
    hos_s8 + hos_s9_r + hos_s10_r
'

cfa_mods$hos_f1 =
'
    one_f =~ hos_r1 + hos_r2 + hos_r3 + hos_r4 + hos_r5 + hos_r6_r +
    hos_r7 + hos_r8 + hos_s1 + hos_s2 + hos_s3 + hos_s4 + hos_s5 + hos_s6 +
    hos_s7 + hos_s8 + hos_s9_r + hos_s10_r
'

#fit the models
cfa(cfa_mods$hos_f2, data = constr_ls$full, estimator = "WLSMV")  ->
    cfa_fits$hos_f2

cfa(cfa_mods$hos_f1, data = constr_ls$full, estimator = "WLSMV")  ->
    cfa_fits$hos_f1

#fit statistics
summary(cfa_fits$hos_f2, fit.measures = TRUE, standardized = TRUE)
summary(cfa_fits$hos_f1, fit.measures = TRUE, standardized = TRUE)

# #compare complex vs nested models - nested wins!
# anova(cfa_fits$hos_f2, cfa_fits$hos_f1)

# #visualize model
# semPaths(cfa_fits$hos_f2, "std", layout = "circle2")

# End ----

# CFA - Equity Sensitivity ------------------------------------------------

cfa_mods$eq_f1 =
'
    eq =~ eq1_r + eq2_r + eq3_r + eq4_r + eq5_r + eq6_r + eq7_r + eq8 +
    eq9 + eq10_r + eq11 + eq12 + eq13 + eq14 + eq15 + eq16
'

#fit the models
cfa(cfa_mods$eq_f1, data = constr_ls$full, estimator = "WLSMV")  ->
    cfa_fits$eq_f1

#fit statistics
summary(cfa_fits$eq_f1, fit.measures = TRUE, standardized = TRUE)

# #compare complex vs nested models - nested wins!
# anova(cfa_fits$eq_f1, cfa_fits$eq_...)

# #visualize model
# semPaths(cfa_fits$eq_f1, "std", layout = "circle2")

# End ----

# CFA - Affectivity -------------------------------------------------------

cfa_mods$aff_f2 =
    '
    pa =~ pa1 + pa2 + pa3 + pa4 + pa5 + pa6 + pa7 + pa8 + pa9 + pa10

    na =~ na1 + na2 + na3 + na4 + na5 + na6 + na7 + na8 + na9 + na10
'

cfa_mods$aff_f1 =
'
    one_f =~ pa1 + pa2 + pa3 + pa4 + pa5 + pa6 + pa7 + pa8 + pa9 + pa10 +
    na1 + na2 + na3 + na4 + na5 + na6 + na7 + na8 + na9 + na10
'

#fit the models
cfa(cfa_mods$aff_f2, data = constr_ls$full, estimator = "WLSMV")  ->
    cfa_fits$aff_f2

cfa(cfa_mods$aff_f1, data = constr_ls$full, estimator = "WLSMV")  ->
    cfa_fits$aff_f1

#fit statistics
summary(cfa_fits$aff_f2, fit.measures = TRUE, standardized = TRUE)
summary(cfa_fits$aff_f1, fit.measures = TRUE, standardized = TRUE)

# #compare complex vs nested models - nested wins!
# anova(cfa_fits$aff_f2, cfa_fits$aff_f1)

# #visualize model
# semPaths(cfa_fits$aff_f2, "std", layout = "circle2")

# End ----

# CFA - Organizational Justice --------------------------------------------

cfa_mods$jus_f4 =
    '
    proc =~ jus_p1 + jus_p2 + jus_p3 + jus_p4 + jus_p5 + jus_p6 + jus_p7

    distr =~ jus_d1 + jus_d2 + jus_d3 + jus_d4

    int =~ jus_int1 + jus_int2 + jus_int3 + jus_int4

    info =~ jus_inf1 + jus_inf2 + jus_inf3 + jus_inf4 + jus_inf5
'

# cfa_mods$bfi_f1 =
#     '
#     one_f =~ bfi_a1_r + bfi_a2 + bfi_a3_r + bfi_a4 + bfi_a5 + bfi_a6_r +
#     bfi_a7 + bfi_a8_r + bfi_a9 + bfi_c1 + bfi_c2_r + bfi_c3 + bfi_c4_r +
#     bfi_c5_r + bfi_c6 + bfi_c7 + bfi_c8 + bfi_c9_r +  bfi_n1 + bfi_n2_r +
#     bfi_n3 + bfi_n4 + bfi_n5_r + bfi_n6 + bfi_n7_r + bfi_n8
# '

#fit the models
cfa(cfa_mods$jus_f4, data = constr_ls$full, estimator = "WLSMV")  ->
    cfa_fits$jus_f4

# cfa(cfa_mods$bfi_f1, data = constr_ls$full, estimator = "DWLS")  ->
#     cfa_fits$bfi_f1

#fit statistics
summary(cfa_fits$jus_f4, fit.measures = TRUE, standardized = TRUE)
# summary(cfa_fits$bfi_f1, fit.measures = TRUE, standardized = TRUE)

# #compare complex vs nested models - nested wins!
# anova(cfa_fits$bfi_f3, cfa_fits$bfi_f1)

# # #visualize model
# semPaths(cfa_fits$jus_f4, "std", layout = "circle2")

# End ----

# LPA - tidyLPA -----------------------------------------------------------

# #explore multiple lpa models
# tic()
# set.seed(919)
# vars %>%
#     select(bfi_a:na) %>%
#     estimate_profiles(n_profiles = 2:6, models = c(1, 2, 3, 6)) ->
#     tidyLPA_res
# toc() # ~2,100 secs (35 mins)
#
# #save model - takes a while to run!
# save(tidyLPA_res, file = "mods/tidyLPA_res.rda")

#load lpa models
load("mods/tidyLPA_res.rda")

#quick answers - all fit indices
compare_solutions(
    tidyLPA_res,
    c("LogLik", "AIC", "AWE", "BIC", "CAIC", "CLC", "KIC", "SABIC", "ICL")
    ) #AHP states model 6 w/ 2 classes as optimal!

# ?calc_lrt

#extrac fits
# map_df(lpa_res, ~ .x$fit)
get_fit(tidyLPA_res) %>%
    rename_with(tolower) %>%
    mutate(
        model = case_when(
            model == 1L     ~ "EEI",
            model == 2L     ~ "VVI",
            model == 3L     ~ "EEE",
            model == 6L     ~ "VVV",
            ),
        profiles = as.integer(classes),
        across(c(entropy:blrt_p), round, 3)
        ) %>%
    select(
        model,
        profiles,
        loglik:entropy
        ) %>%
    pivot_longer(
        -c(model, profiles),
        names_to = "fit_stat",
        values_to = "val"
        ) ->
    lpa_fit_df

#plot
lpa_fit_df %>%
    filter(!fit_stat %in% "loglik") %>%
    ggplot(aes(x = profiles, y = val)) +
    geom_point(aes(color = model, shape = model), size = 2) +
    geom_line(aes(group = model, color = model, linetype = model)) +
    facet_wrap(vars(
        factor(
            fit_stat,
            levels = c("aic", "awe", "bic", "caic",
                       "clc", "kic", "sabic", "icl", "entropy"),
            labels = c("AIC", "AWE", "BIC", "CAIC",
                       "CLC", "KIC", "SABIC", "ICL", "Entropy")
            )
        ), scales = "free_y") +
    theme(legend.title = element_blank()) +
    labs(
        title = "tidyLPA Fit Statistics",
        x = "",
        y = ""
        )

# #save plot
# ggsave(last_plot(), filename = "../figs/tidyLPA_fit_plots.png",
#        width = 1400, height = 750)

tidyLPA_res$model_6_class_2$model$classification %>%
    bind_cols("grp" = ., vars) %>%
    mutate(
        grp = factor(grp),
        across(
            where(is.double), function(x, na.rm = FALSE) {
                (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
                }
            # .names = "c_{.col}"
            )
        ) %>%
    group_by(grp) %>%
    summarise(across(c(bfi_a:na), ~ mean(.x))) %>%
    pivot_longer(!grp, names_to = "var", values_to = "val") %>%
    ggplot(aes(x = var, y = val)) +
    geom_point(aes(color = grp, shape = grp), size = 2) +
    geom_line(aes(group = grp, color = grp, linetype = grp)) +
    labs(x = "", y = "", title = "tidyLPA") +
    coord_cartesian(ylim = c(-2, 2))
# g1

grp_brkdwn_fun = function(mod, pkg = c("tidyLPA", "mclust")) {
    match.arg(pkg) -> pkg
    pkg

    if (is.na(pkg)) {
        stop("invalid 'pkg' argument")
    }

    if (class(mod)[1] == "tidyProfile.mclust" && pkg == "tidyLPA") {
        mod$model$classification ->
            mod
    } else if (class(mod) == "Mclust" && pkg == "mclust") {
        mod$classification ->
            mod
    } else {
        stop("mod must be matched class 'tidyProfile.mclust' or 'Mclust'")
    }

    #df already provided for this script! (vars must be in global env)
    mod %>%
        bind_cols("grp" = ., vars) %>%
        mutate(
            grp = factor(grp),
            across(
                where(is.double), function(x, na.rm = FALSE) {
                    (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
                }
                # .names = "c_{.col}"
            )
        ) %>%
        group_by(grp) %>%
        summarise(across(c(bfi_a:na), ~ mean(.x))) %>%
        pivot_longer(!grp, names_to = "var", values_to = "val") %>%
        ggplot(aes(x = var, y = val)) +
        geom_point(aes(color = grp, shape = grp), size = 2) +
        geom_line(aes(group = grp, color = grp, linetype = grp)) +
        labs(x = "", y = "", title = paste0(pkg, "Cluster Solution")) +
        coord_cartesian(ylim = c(-2, 2))
}


grp_brkdwn_fun(mod = tidyLPA_res, pkg = "today")
tryCatch(
    grp_brkdwn_fun("orange"),
    error = identity
)
# End ----

# mclust LPA --------------------------------------------------------------

# #pairs plot
# clPairs(select(vars, bfi_a:na), colors = "red")

#explore multiple lpa models
tic()
set.seed(984)
vars %>%
    select(bfi_a:na) %>%
    Mclust(
        data = .,
        G = 2:6,
        # initialization = list(hcPairs = hcRandomPairs(.))
        ) ->
    mclust_res
toc()

#summary
plot(mclust_res, "BIC")

12:6 -> k #number of clusters to initialise
sprintf("mclust_%d", 2:6) -> mod_names
# mclust.options(subset = 150)

#loop over k
tic()
set.seed(919)
map(k, function(.k) {
    vars %>%
        select(bfi_a:na) %>%
        Mclust(
            data = .,
            G = .k,
            initialization = list(hcPairs = hcRandomPairs(.))
        )
    }) ->
    mclust_res_ls
toc() # ~7.2 secs

#rename elements
mod_names -> names(mclust_res)


#review
summary(mclust_res)

mclust_res$mclust_4$classification %>%
    bind_cols("grp" = ., vars)
    mutate(
        grp = factor(grp),
        across(
            where(is.double), function(x, na.rm = FALSE) {
                (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
                }
            # .names = "c_{.col}"
            )
        ) %>%
    group_by(grp) %>%
    summarise(across(c(bfi_a:na), ~ mean(.x))) %>%
    pivot_longer(!grp, names_to = "var", values_to = "val") %>%
    ggplot(aes(x = var, y = val)) +
    geom_point(aes(color = grp)) +
    geom_line(aes(group = grp, color = grp)) +
    coord_cartesian(ylim = c(-2, 2)) ->
    # scale_y_continuous(breaks = seq(-2, 2, .5)) ->
    g2

(g1 + labs(title = "tidyLPA")) / g2 + labs(title = "mclust") +
    plot_layout(guides = "collect")

plot(mclust_lpa_res,
     what = "BIC",
     ylim = range(mclust_lpa_res$BIC[,-(1:2)], na.rm = T),
     legendArgs = list(x = "bottomleft"))

table(Class, mclust_lpa_res$classification)
