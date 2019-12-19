#keep env cln
rm(list = ls())

shhh = function(...) {
    suppressWarnings(
        suppressPackageStartupMessages(base::library(...))
        )
}

shhh(tidyverse)
shhh(psych)

#read in file (skip the second line)
all_content = readLines("./data/proj-data_cln.csv")
     skip_second = all_content[-2]
     dat = read.csv(textConnection(skip_second), header = T, stringsAsFactors = T)

dat = dat %>%
    mutate(
        Att_Chk1 = NULL,
        Att_Chk2 = NULL,
        Employed = NULL
    )

