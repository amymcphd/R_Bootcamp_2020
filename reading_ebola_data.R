
# Setup ---------------

library(tidyverse) #< Load tidyverse to get the pipe.
library(readxl)


# Change this to reflect the location of YOUR file.
data.file <- "data/ebola-cases-and-deaths-who-gar-sitrep.xlsx"
# It might look like this
data.file <- "C:/Users/u0092104/Downloads/ebola-cases-and-deaths-who-gar-sitrep.xlsx"
# if you didn't move it from the downloads
# (and you happened to have the same user name as me, which would be really weird)

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
sheets <- excel_sheets(data.file)
length(sheets)

# Read all the sheets
# For each sheet name, read that worksheet from the workbook.
all.ebola.raw.data <-
    map(sheets, readxl::read_excel, path=data.file)


## Understanding the Pipe ---------------------

# this
data.file %>% readxl::excel_sheets()
# is the same as this
readxl::excel_sheets(data.file)


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
for (i in 1:5) {
    raw.data[[i]] <- read_xlsx(path=data.file, sheets[[i]])
}


# All together as one command ----------------------

all.ebola.raw.data <-
    data.file %>%               #< Take data.file, ie. the file name of the xlsx file
                                #< and pass that to excel_sheets
    readxl::excel_sheets() %>%  #< to get a vector of sheet names
                                #< and pass that to map where
    map(readxl::read_excel,     #< for each sheet name, read that sheet's worth of data
        path=data.file) %>%
    set_names(sheets)           #< then set the names of the list items to the sheet names.
