# Data wrangling
# Based on our meeting @2024.12.06 wrong sheetws were used to exctract data, despite our correspondence at 2024.12.02.

fil <- here::here("inst","extdata","2024.11.25_prev_data",
                  "PCT teljes adatbázis_anonim.xlsx")


descriptor <-
  here::here("inst","extdata","2024.11.25_prev_data","description.xlsx") %>%
  file.path() %>%
  readxl::read_excel(skip = 0)

# make a list to be used as labels for 'labelled::'
labs <-
  # take a 1xN matrix
  matrix( descriptor$description,
          ncol = length(descriptor$description)
  ) %>%
  as.list() %>%
  # add column names as 'names'
  `names<-`(descriptor$name_new)

sheets <- readxl::excel_sheets(fil) %>%
  .[3:6] # !!!!!!!!!!!!!!!!!!!! HARD CODED INTERESTING SHEETS

for (dataset in sheets) {

  datachunk <- fil |>
    readxl::read_xlsx(sheet = dataset
                      ,skip = 0 # !!!!!!!!!!!!!!!!!! HARD CODED
                      ) |> # or read_xls() as appropriate
    # handle duplicate colnames
    janitor::clean_names() |>
    mutate( across( .cols = which( descriptor$trf == "factor"),
                    .fns = as.factor
    ),
    across( .cols = which( descriptor$trf == "numeric"),
            .fns = as.numeric # removing potential '?', 'NA', '.' etc.
    ),
    across( .cols = which( descriptor$trf == "date"),
            .fns = lubridate::as_datetime
    )
    # ,across( .cols = which( descriptor$trf == "text"),
    #         .fns = as.character
    # )
    ) %>%
    `colnames<-`( descriptor$name_new) %>%
    #labelled::`var_label<-`(   labs  ) %>%
    # select only columns whose name isnt NA due to duplicates or whatnot
    select( descriptor$name_new[!is.na(descriptor$name_new)] %>% unique() ) %>%
    mutate(dataset = dataset)

  # binding the actual dataset to a 'master list'
  if ( dataset == sheets[1]) {
    data <- list()
    data[[dataset]] <- datachunk
  } else {
    data[[dataset]] <- datachunk
  }
}

dat_full <- bind_rows(data) %>%
  filter(is.na(rowno) == FALSE)


save(dat_full, file = here::here("data","dat_full.rds"))

