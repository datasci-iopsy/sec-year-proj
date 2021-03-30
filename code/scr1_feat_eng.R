#keep env cln
rm(list = ls())

#load libs
library(tidyverse) #see output for masks

#script opts
# option(tibble.width = Inf)

# Data Import & Quick Updates ---------------------------------------------

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
    #convert col names to lowercase
    rename_with(tolower) %>%
    #extract cols of interest
    select(age:race, edu, industry) %>%
    #shorten "High school..." for ease of reading
    mutate(edu = if_else(grepl("High school", edu), "High school/GED", edu)) %>%
    #shorted opt for Edu
    mutate(edu = str_remove_all(edu, "in college ")) ->
    dat_ls$demo

#custom function to convert anchors from chr to nums
unfactorise = function(x) {
    case_when(
        x %in% c(
            "Strongly disagree",
            "Very Slightly",
            "To a very small extent",
            "Never",
            "Very dissatisfied"
        ) ~ 1,
        x %in% c(
            "Somewhat disagree",
            "A Little",
            "To a small extent",
            "Sometimes",
            "Somewhat dissatisfied"
        ) ~ 2,
        x %in% c(
            "Neither agree nor disagree",
            "Moderately",
            "To a moderate extent",
            "About half the time",
            "Neither satisfied nor dissatisfied"
        ) ~ 3,
        x %in% c(
            "Somewhat agree",
            "Quite a Bit",
            "To a large extent",
            "Most of the time",
            "Somewhat satisfied"
        ) ~ 4,
        x %in% c(
            "Strongly agree",
            "Extremely",
            "To a very large extent",
            "Always",
            "Very satisfied"
        ) ~ 5
    )
}

#extract scales
raw %>%
    #convert col names to lowercase
    rename_with(tolower) %>%
    #extract only items used in scales; attention checks previously processed!
    select(!age:industry & !starts_with("att_chk")) %>%
    # #alphabetize col names; chrs don't read double digit nums in order...
    # select(order(colnames(.))) %>%
    #apply custom func across cols - convert chrs to nums
    map_df(., function(.x) {
        unfactorise(.x)
        }) %>%
    #handle reverse coded items
    mutate(
        across(
            ends_with("_r"), ~ 6 - .x
            )
        ) ->
    dat_ls$survey

# End ----

# Scale & Construct Extraction --------------------------------------------

#list to hold separated scales
list() -> cnstr_ls

#extract relevant prefixes
dat_ls$survey %>%
    names() %>%
    str_extract("[^[0-9]]+") %>%
    unique() %>%
    set_names() ->
    prefixes

#print
prefixes

#custom function
scl_pull_fun = function(prfx = NULL) {

    if (!is.character(prfx)) {
        stop("prefix must be chr class")
    } else if (!prfx %in% prefixes) {
        stop("prfx not found")
    }

    #use updated df to extract scales
    dat_ls$survey %>%
        select(matches(prfx)) %>%
        mutate(ss = apply(., 1, mean, na.rm = TRUE)) %>%
        rename_with(~ paste0(prfx, "_ss"), ss) ->
        scl_df

    return(scl_df)
}

#bfi - personality
map(prefixes[c("bfi_e", "bfi_a", "bfi_c", "bfi_n", "bfi_o")],
    ~ scl_pull_fun(prfx = .x)) ->
    cnstr_ls$bfi

#hostility
map(prefixes[c("hos_r", "hos_s")],
    ~ scl_pull_fun(prfx = .x)) ->
    cnstr_ls$hos

#equity sensitivity
scl_pull_fun(prfx = "eq") ->
    cnstr_ls$eq

#affectivity - positive & negative
map(prefixes[c("pa", "na")],
    ~ scl_pull_fun(prfx = .x)) ->
    cnstr_ls$affect

#organizational justice
map(prefixes[c("jus_p", "jus_d", "jus_int", "jus_inf")],
    ~ scl_pull_fun(prfx = .x)) ->
    cnstr_ls$jus

#counterproductive work behavior
map(prefixes[c("cwb_s", "cwb_pd", "cwb_w", "cwb_t", "cwb_a")],
    ~ scl_pull_fun(prfx = .x)) ->
    cnstr_ls$cwb

#satisfaction -- general, intrinsic, & extrinsic
scl_pull_fun(prfx = "sat") ->
    cnstr_ls$sat

#combine cnstr into sorted df
reduce(cnstr_ls$bfi, bind_cols) %>%
    bind_cols(reduce(cnstr_ls$hos, bind_cols)) %>%
    bind_cols(cnstr_ls$eq) %>%
    bind_cols(reduce(cnstr_ls$affect, bind_cols)) %>%
    bind_cols(reduce(cnstr_ls$jus, bind_cols)) %>%
    bind_cols(reduce(cnstr_ls$cwb, bind_cols)) %>%
    bind_cols(cnstr_ls$sat) ->
    cnstr_ls$full

# End ----

# Data Aggregation --------------------------------------------------------
cnstr_ls$full %>%
    select(ends_with("_ss")) ->
    cnstr_ls$agg

#save relevant dfs as R obj
save(dat_ls, cnstr_ls, prefixes, file = "../data/r_objs/feat_eng_cln.rda")
