
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
        # select(all.ebola.raw.data[[i]], Country, `Case def.`, `Total cases`, `Total deaths`, `Country Report Date`)
        select(all.ebola.raw.data[[i]], !!!common.vars)

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

# ebola.data <- readRDS("C:/Users/u0092104/Box/R Bootcamp/R Bootcamp 2020/data/all.ebola.raw.data.df.rds")
#
# library(magrittr)
# ebola.data %<>% mutate_at('Country Report Date', zoo::na.locf)
# ebola.data %<>% mutate_at('Total deaths', coalesce, 0L)
# saveRDS(ebola.data, 'data/ebola.data.rds')

library(magrittr)

ebola.data <- all.ebola.raw.data.df %>%
    mutate_at('Country Report Date', zoo::na.locf) %>%
    mutate_at(c('Total deaths', 'Total cases'), coalesce, 0L)

saveRDS(ebola.data, 'ebola.data.rds')

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


library(tidyverse)

# Make subsets
confirmed <- ebola.data %>% filter(`Case def.` == 'Confirmed') %>%
    select(-`Case def.`) %>% distinct() %>%
    rename_at(vars(starts_with("total")), ~paste("Confirmed", .))
probable  <- ebola.data %>% filter(`Case def.` == 'Probable') %>%
    select(-`Case def.`) %>% distinct() %>%
    rename_at(vars(starts_with("total")), ~paste("Probable", .))
suspected <- ebola.data %>% filter(`Case def.` == 'Suspected') %>%
    select(-`Case def.`) %>%  distinct() %>%
    rename_at(vars(starts_with("total")), ~paste("Suspected", .))
all.cases <- ebola.data %>% filter(`Case def.` == 'All') %>%
    select(-`Case def.`) %>%  distinct() %>%
    rename_at(vars(starts_with("total")), ~paste("All", .))

ebola.option1 <-
confirmed %>%
    full_join(probable) %>%
    full_join(suspected) %>%
    full_join(all.cases)

glimpse(ebola.option1)



a <- ebola.data %>% filter(`Case def.` == 'Confirmed') # %>% select(-`Case def.`)
b  <- ebola.data %>% filter(`Case def.` == 'Probable') #%>% select(-`Case def.`)

full_join(a,b, )

inner_join(a,b, by = c("SheetName", "Country", "Country Report Date"), suffix = c(' Confirmed', ' Probable'))

ebola.data %>% filter(Country == 'Liberia2', `Country Report Date` == lubridate::ymd('2015-01-03'))

ebola.option2 <-
    ebola.data %>%
    select(SheetNum, Country, `Case def.`, `Total cases`, `Country Report Date`) %>%
    tidyr::spread('Case def.', 'Total cases', fill=0L)
glimpse(ebola.option2)

cases <- ebola.data %>% select(-`Total deaths`) %>%
    tidyr::spread('Case def.', 'Total cases')
deaths <-  ebola.data %>% select(-`Total cases`) %>%
    tidyr::spread('Case def.', 'Total deaths')
ebola.option3 <-
    full_join( cases, deaths
             , c('SheetNum', 'Country', 'Country Report Date')
             , suffix = c(".cases", ".deaths"))
glimpse(ebola.option3)


# Summarization -----------------------------------------------------------

summarize( ebola.option3
         , 'Observations' = n()
         , 'Number of countries' = n_distinct(Country)
         , "# of Reporting dates" = n_distinct(`Country Report Date`)
         , max.cases = max(All.cases, na.rm=TRUE)
         , max.deaths = max(All.deaths, na.rm=TRUE)
         )
summarize( ebola.option3 %>% group_by(Country)
         , 'Observations' = n()
         , 'Number of countries' = n_distinct(Country)
         , "# of Reporting dates" = n_distinct(`Country Report Date`)
         , max.cases = max(All.cases, na.rm=TRUE)
         , max.deaths = max(All.deaths, na.rm=TRUE)
         )
summarize( ebola.option3 %>% group_by(Country,Year = lubridate::year(`Country Report Date`))
         , 'Observations' = n()
         , 'Number of countries' = n_distinct(Country)
         , "# of Reporting dates" = n_distinct(`Country Report Date`)
         , max.cases = max(All.cases, na.rm=TRUE)
         , max.deaths = max(All.deaths, na.rm=TRUE)
         )

ebola.option3 %>% group_by(Country) %>%
  summarise( "# of Reporting dates" = n_distinct(`Country Report Date`)
           , max.cases = max(All.cases, na.rm=TRUE)
           , max.deaths = max(All.deaths, na.rm=TRUE)
           )



# Plots -------------------------------------------------------------------

ebola.plot1 <-
ggplot(data=ebola.data) +               #< Our 'data' component
    geom_point(                         #< a point geometry layer
        aes( x = `Country Report Date`  #< our mappings
           , y = `Total cases`
           , col = Country
           )
      , stat = 'unique'                 #< our statistic
      , position = 'identity'           #< how to position data
    )
ebola.plot1
print(ebola.plot1)


ggplot(data= (ebola.data)
        #< Move mappings here because inheritance.
      , aes( x = `Country Report Date`
           , y = `Total cases`
           , col = Country
           , group = `Case def.`
           )
      ) +
    geom_point()+
    geom_line()

ggplot(data= (ebola.data)
        #< Move mappings here because inheritance.
      , aes( x = `Country Report Date`
           , y = `Total cases`
           , col = Country
           )
      ) +
    geom_point()+
    geom_line()


ggplot(data= ebola.data %>% select(2:4, 6) %>% distinct()
        #< Move mappings here because inheritance.
      , aes( x = `Country Report Date`
           , y = `Total cases`
           , col = Country
           )
      ) +
    geom_point( stat = 'unique'
              , size = 1         #< An explicit aesthetic.
              ) +
    geom_smooth(method='gam', formula=y ~ s(x, bs = "cs")) +   #< new 'smooth' layer
    facet_wrap(~`Case def.`, 4, 1)  #< new 2x2 faceting for Case def.



ggplot(data=ebola.data, aes(x = `Case def.`, y = `Total deaths`)) +
    geom_boxplot()



