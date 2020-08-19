
# Setup ---------------

library(tidyverse) #< Load tidyverse to get the pipe.
library(readxl)


# Change this to reflect the location of YOUR file.
data.file <- "data/ebola-cases-and-deaths-who-gar-sitrep.xlsx"
# It might look like this
data.file <- "C:/Users/u0092104/Downloads/ebola-cases-and-deaths-who-gar-sitrep.xlsx"
# if you didn't move it from the downloads
# (and you happened to have the same user name as me, which would be really weird)

data.file <- choose.files()
(data.file <- "C:\\Users\\u0092104\\Box\\R Bootcamp\\R Bootcamp 2020\\data\\ebola-cases-and-deaths-who-gar-sitrep.xlsx")

library(readxl)
ebola_cases_and_deaths_who_gar_sitrep <- read_excel("data/ebola-cases-and-deaths-who-gar-sitrep.xlsx")
View(ebola_cases_and_deaths_who_gar_sitrep)

# Work -------------------

# Read the first sheet.
ebola.raw <- read_excel(data.file)

# Inspect with these
str(ebola)
glimpse(ebola)
head(ebola, 3)
tail(ebola, 3)
summary(ebola)

# Get the names of the worksheets in the excel workbook
sheets <-
sheet.names <- excel_sheets(data.file)
length(sheet.names)

# Read all the sheets
# For each sheet name, read that worksheet from the workbook.
all.ebola.raw.data <-
    map(sheet.names, readxl::read_excel, path=data.file)


## Understanding the Pipe ---------------------

# this
data.file %>% readxl::excel_sheets()
# is the same as this
readxl::excel_sheets(data.file)

a %>% b(c=TRUE)
b(a, c=TRUE)


# Understanding map -----------------------------

raw.data <- map(head(sheets, 5), readxl::read_excel, path=data.file)
# Is equivalent to
raw.data <- list()
raw.data[[1]] <- read_xlsx(path=data.file, sheets[[1]])
raw.data[[2]] <- read_xlsx(path=data.file, sheets[[2]])
raw.data[[3]] <- read_xlsx(path=data.file, sheets[[3]])
raw.data[[4]] <- read_xlsx(path=data.file, sheets[[4]])
raw.data[[5]] <- read_xlsx(path=data.file, sheets[[5]])
# Which is also equivalent to
raw.data <- list()
for (i in 1:length(sheets)) {
    raw.data[[i]] <- read_xlsx(path=data.file, sheets[[i]])
}


# All together as one command ----------------------

all.ebola.raw.data <-
    "data/ebola-cases-and-deaths-who-gar-sitrep.xlsx" %>%
                                #< Take data.file, ie. the file name of the xlsx file
                                #< and pass that to excel_sheets
    readxl::excel_sheets() %>%  #< to get a vector of sheet names
                                #< and pass that to map where
    map(readxl::read_excel,     #< for each sheet name, read that sheet's worth of data
        path="data/ebola-cases-and-deaths-who-gar-sitrep.xlsx")


# Error
all.ebola.raw.data %>% bind_rows()

# Fix 1  - Fix Total Deaths
tmp <- list()
for( i in seq_along(all.ebola.raw.data)){
    tmp[[i]] <-
        mutate( all.ebola.raw.data[[i]]
              , 'Total deaths' = as.integer(`Total deaths`)
              , 'Total cases' = as.integer(`Total cases`)
              )
}
tmp %>% bind_rows()

# Fix 2 select common variables
tmp2 <- list()
for( i in seq_along(all.ebola.raw.data)){
    tmp2[[i]] <-
        select( tmp[[i]], Country, `Case def.`, `Total cases`, `Total deaths`, `Country Report Date`)
}

bind_rows(tmp2)

all.ebola.raw.data %>% keep(~is.character(.$`Total deaths`))

ebola.data.raw <-
    "data/ebola-cases-and-deaths-who-gar-sitrep.xlsx" %>%
                                #< Take data.file, ie. the file name of the xlsx file
                                #< and pass that to excel_sheets
    readxl::excel_sheets() %>%  #< to get a vector of sheet names
                                #< and pass that to map where
    map_dfr(readxl::read_excel,     #< for each sheet name, read that sheet's worth of data
            path="data/ebola-cases-and-deaths-who-gar-sitrep.xlsx"
            , na = '..'
            , .id = "SheetNum")


common.vars <- (var.names <- all.ebola.raw.data %>%
    map(tbl_vars)
    ) %>%
    reduce(intersect)


for( i in seq_along(all.ebola.raw.data)){
    all.ebola.raw.data[[i]] <-
        mutate_at( all.ebola.raw.data[[i]]
                 , c('Total cases', 'Total deaths')
                 , as.integer
                 )

    all.ebola.raw.data[[i]] <-
        select(all.ebola.raw.data[[i]], Country, `Case def.`, `Total cases`, `Total deaths`, `Country Report Date`)

    all.ebola.raw.data[[i]]$SheetNum <- i
}

t(sapply(all.ebola.raw.data, tbl_vars))
t(sapply(all.ebola.raw.data, map, class))


reduce(all.ebola.raw.data, bind_rows)


all.ebola.raw.data %>%
    map( mutate)


tmp <- list()
for( i in seq_along(all.ebola.raw.data)){
    tmp[[i]] <-
        all.ebola.raw.data[[i]] %>%
        mutate_at( .
                 , c('Total cases', 'Total deaths')
                 , as.integer
                 ) %>%
        select(., Country, `Case def.`, `Total cases`, `Total deaths`, `Country Report Date`) %>%
        mutate(SheetNum = i)
}


# fixer <- function(data){
#     data %>%
#         mutate_at( .
#                  , c('Total cases', 'Total deaths')
#                  , as.integer
#                  ) %>%
#         select(., Country, `Case def.`, `Total cases`, `Total deaths`, `Country Report Date`)
# }


all.ebola.raw.data.df <- all.ebola.raw.data %>%
    map_dfr( . %>%
                mutate_at( .
                         , c('Total cases', 'Total deaths')
                         , as.integer
                         ) %>%
                select(., Country, `Case def.`, `Total cases`, `Total deaths`, `Country Report Date`)
           , .id = "SheetNum")


var.names[[1]]
last(var.names)

intersect(var.names[[1]], last(var.names))

save(all.ebola.raw.data.df, file="data/all.ebola.raw.data.df.RData")
saveRDS(all.ebola.raw.data.df, file="data/all.ebola.raw.data.df.rds")

ebola.data <- readRDS("C:/Users/u0092104/Box/R Bootcamp/R Bootcamp 2020/data/all.ebola.raw.data.df.rds")

library(magrittr)
ebola.data %<>% mutate_at('Country Report Date', zoo::na.locf)

filter( ebola.data
      , Country == 'Nigeria'
      , `Case def.` == 'Confirmed'
      )
filter( ebola.data
      , (Country == 'Nigeria') | (Country == 'Sierra Leone')
      , `Case def.` == 'Confirmed'
      )


`Confirmed Cases for Sierra Leone` <-
filter( ebola.data
      , Country == 'Sierra Leone'
      , `Case def.` == 'Confirmed'
      ) %>%
    select(last_col(), Country, `Case def.`, starts_with('Total')) %>%
    distinct()
