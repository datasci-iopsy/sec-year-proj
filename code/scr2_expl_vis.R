#keep env cln
rm(list = ls())

#load libs
library(tidyverse) #see output for masks
library(sjmisc) #masks purrr::is_empty; tidyr::replace_na; tibble::add_case
library(likert)
library(patchwork)

#script opts
theme_set(theme_minimal())
options(tibble.width = Inf)

#import data -- load from feat_eng script
load("../data/r_objs/feat_eng_cln.rda")

# Demographic Review ------------------------------------------------------

# #demograph tbls - sjmisc pkg
# frq(dat_ls$demo, sex:industry) %>%
#     #edit frq tbls - convert col into df/tibble
#     map(., function(.x) { tibble(.x) } %>%
#             #extract cols of interest
#             select(val, frq, pct = raw.prc) %>%
#             #descending order
#             arrange(desc(pct)) %>%
#             #extact top 10 rows
#             slice(1:10))

#counts across demographic items - bar plots...
dat_ls$demo[-1] %>% #rm Age
    map(., function(.x) {
        ggplot(tibble(.x), aes(y = fct_rev(fct_infreq(.x)))) +
            geom_bar(aes(fill = .x), stat = "count", size = 10) +
            labs(x = "",
                 y = "") +
            theme(legend.position = "none")
    }) %>%
    wrap_plots()

#lollipop version!
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
            scale_x_continuous(labels = function(.x) { paste0(.x, "%") }) +
            #rm titles from x & y axes
            labs(x = "",
                 y = "") +
            theme(legend.position = "none") #rm legend
    }) ->
    demo_plot_ls

#print & arrange plots
# attach(demo_plot_ls) #to call plots directly from list
demo_plot_ls$sex + labs(subtitle = "Sex") +
    demo_plot_ls$race + labs(subtitle = "Race") +
    demo_plot_ls$edu + labs(subtitle = "Edu") +
    demo_plot_ls$industry + labs(subtitle = "Industry - Top 10") +
    plot_annotation(title = "Demographics")

# #save plot - last saved 21-03-22
# ggsave("../figs/demo_prop_plot.png",
#        plot = last_plot(),
#        width = 10,
#        height = 6)

# detach(demo_plot_ls) #undo attach; run line 93-95 to plot post detach

# End ----

# scales_ls$full %>%
#     select(matches("_E")) %>%
#     mutate(across(everything(),
#                   ~ factor(.x,
#                            labels = c("Strongly disagree",
#                                       "Somewhat disagree",
#                                       "Neither agree nor disagree",
#                                       "Somewhat agree",
#                                       "Strongly agree")))) %>%
#     map_df(., ~ count(tibble(x = .x), x), .id = "df") %>%
#     ggplot(aes(y = x, x = n, fill = x)) +
#     geom_bar(stat = "identity") +
#     facet_wrap(vars(df)) +
#     theme(legend.position = "none")

# Item Anchors ------------------------------------------------------------

#list comprising scale anchors
list() -> anchor_ls

c("Strongly disagree", "Somewhat disagree", "Neutral",
  "Somewhat agree", "Strongly agree") ->
    anchor_ls$agree

c("Very slightly", "A little", "Moderately", "Quite a bit", "Extremely") ->
    anchor_ls$amt

c("Very small extent", "Small extent", "Moderate extent",
  "Large extent", "Very large extent") ->
    anchor_ls$ext

c("Never", "Sometimes", "Half the time", "Most of the time", "Always") ->
    anchor_ls$freq

c("Very dissatisfied", "Somewhat dissatisfied", "Neurtral",
  "Somewhat satisfied", "Very satisfied") ->
    anchor_ls$sat

# End ----

# Likert Plots ------------------------------------------------------------

#review anchors
anchor_ls

scales_ls$full %>%
    names() %>%
    str_extract("[^[0-9]]+") %>%
    unique() %>%
    set_names() ->
    prefixes

#custom function for likert plots
lkrt_plot_fun = function(.df, item_stem, anchor, ord = TRUE, title = NULL,
                         legend_pos = "none", ...)
    {
    anchor_ls -> anc

    #TO DO: use `match.args` & `switch` to specify scales!
    if (!anchor %in% names(anc)) {
        stop("anchor must be 'agree', 'amt', 'ext', 'freq', or 'sat'")
    }


    # .df %>%
    #     names() %>%
    #     str_extract("[^[0-9]]+") %>%
    #     unique() %>%
    #     set_names() ->
    #     prefixes

    #error check...learn tryCatch to bolster error handling...
    suppressWarnings(
        if (!item_stem %in% names(prefixes)) {
            stop("item_stem not found")
            }
        )

    # map(prefixes[item_stem], function(.x) {
        .df %>%
            select(matches(item_stem)) %>%
            mutate(
                across(
                    everything(), function(.fct) {
                        factor(.fct, labels = anc[[anchor]], ordered = TRUE)
                    }
                )
            ) %>%
            as.data.frame() %>%
            likert() %>%
            plot(
                ordered = ord,
                legend = "",
                legend.position = legend_pos,
                centered = FALSE
                ) +
            labs(title = title, y = "")
            # facet_wrap(vars(item_stem))
    # })
}

#Store plots
list() -> lkrt_plot_ls

#assign to list
lkrt_plot_fun(.df = scales_ls$full,
              item_stem = c("bfi_c", "bfi_n", "bfi_a"),
              anchor = "agree",
              legend_pos = "right"
              ) %>%
    wrap_plots() %>%
    plot_annotation(
        title = "Personality",
        subtitle = "Conscientiousness | Neuroticism | Agreeableness"
        ) ->
    lkrt_plot_ls$bfi

#exe plots
lkrt_plot_fun(.df = scales_ls$full,
              item_stem = c("hos_r", "hos_s"),
              anchor = "agree",
              legend_pos = "right"
              ) %>%
    wrap_plots() %>%
    plot_annotation(
        title = "Hostility",
        subtitle = "Resentment | Suspicion"
        ) ->
    lkrt_plot_ls$hos

lkrt_plot_fun(.df = scales_ls$full,
              item_stem = "eq",
              anchor = "agree",
              legend_pos = "right"
              ) %>%
    wrap_plots() %>%
    plot_annotation(title = "Equity Sensitivity") ->
    lkrt_plot_ls$eq

lkrt_plot_fun(.df = scales_ls$full,
              item_stem = c("pa", "na"),
              anchor = "amt",
              legend_pos = "right"
              ) %>%
    wrap_plots() %>%
    plot_annotation(
        title = "Affectivity",
        subtitle = "Positive Affect | Negative Affect"
        ) ->
    lkrt_plot_ls$panas

lkrt_plot_fun(.df = scales_ls$full,
              item_stem = c("jus_p", "jus_d", "jus_int", "jus_inf"),
              anchor = "ext",
              legend_pos = "right"
              ) %>%
    wrap_plots() %>%
    plot_annotation(
        title = "Justice",
        subtitle = "Procedural | Distributive | Interactional | Informational"
        ) ->
    lkrt_plot_ls$jus

lkrt_plot_fun(.df = scales_ls$full,
              item_stem = c("cwb_s", "cwb_pd", "cwb_w", "cwb_t"),
              anchor = "freq",
              legend_pos = "right"
              ) %>%
    wrap_plots() %>%
    plot_annotation(
        title = "Counterproductive Work Behavior",
        subtitle = "Sabotage | Product Deviance | Withdrawal | Theft"
    ) ->
    lkrt_plot_ls$cwb

lkrt_plot_fun(.df = scales_ls$full,
              item_stem = c("sat"),
              anchor = "sat",
              legend_pos = "right"
              ) %>%
    wrap_plots() %>%
    plot_annotation(title = "Satisfacton") ->
    lkrt_plot_ls$sat

# End ----


