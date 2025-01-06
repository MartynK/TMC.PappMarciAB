# Data wrangling

fil <- here::here("inst","extdata","2024.12.07_prev_data2",
                  "PCT t-1 t0 teljes adatbázis_MZs.xls")


descriptor <-
  here::here("inst","extdata","2024.12.07_prev_data2","description.xlsx") %>%
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
  .[1] # !!!!!!!!!!!!!!!!!!!! HARD CODED INTERESTING SHEETS

for (dataset in sheets) {

  datachunk <- fil |>
    readxl::read_xls(sheet = dataset
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

dat_full_new <- bind_rows(data) %>%
  filter(is.na(id) == FALSE)



exp_join <- left_join(
  dat_full %>%
    mutate(dept = as.character(dept),
           sex = as.character(sex)
           ),
  dat_full_new %>% select(
    "dept","sex","admission","enrollment","age","saps","pm_perc",
    "pct_t0"),
  by = c(
    #"dept","sex",
    "admission"
    ,"enrollment"
    ,"age"
    ,"saps"
    #,"pm_perc"
    ),
  keep = FALSE
)

plot(exp_join$pct_t0.x,exp_join$pct_t0.y)
table(exp_join$pct_t0.x - exp_join$pct_t0.y == 0)

# I'll SUPPOSE the t_neg1 PCT values are for pat.1-120 which are the same for
# which are in the last database.
# Only this way is the problem solvable
pct_tn1 <- dat_full$pct_tn1 %>% as.character() %>% as.numeric()

save(pct_tn1, file = here::here("data","pct_tn1.rds"))

