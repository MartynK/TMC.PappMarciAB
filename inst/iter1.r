# Load required libraries
library(gtsummary)
library(flextable)
library(dplyr)

# Example tables etc.

fun_generate_arm <- function(ab_prop = 0.5,
                             inf_prop_abpoz = 0.1,
                             inf_prop_abneg = 0.9,
                             prop_spurious = 0.4,
                             n = 100) {

  n_ab <- round(n * ab_prop)
  n_abneg <- n - n_ab
  n_inf_abpoz <- round(n_ab * inf_prop_abpoz)
  n_inf_abneg <- round(n_abneg * inf_prop_abneg)
  n_spur <- round((n_ab - n_inf_abpoz) * prop_spurious)

  # Generate the table-like results
  dat <- data.frame(
    id = 1:n,
    ab   = c( rep("TRUE",n_ab),rep("FALSE",n_abneg)),
    spur = c( rep(NA,n_inf_abpoz),
                rep(TRUE,n_spur), rep(FALSE,n_ab-n_spur-n_inf_abpoz),
              rep(NA,n_abneg)),
    inf  = c( rep("TRUE",n_inf_abpoz), rep("FALSE", n_ab - n_inf_abpoz),
              rep("TRUE",n_inf_abneg), rep("FALSE", n_abneg - n_inf_abneg)))
}

fun_generate_tables <- function(dat) {
  # Create a cross-tabulation summary
  tbl_summary <- dat %>%
    count(ab, inf) %>%
    mutate(percent = n / sum(n))

  tbl_cross <- data.frame(
    inf_poz = c(dat %>% filter(inf == TRUE  & ab == TRUE) %>% nrow(),
                dat %>% filter(inf == TRUE  & ab == FALSE) %>% nrow()),
    inf_neg = c(dat %>% filter(inf == FALSE & ab == TRUE) %>% nrow(),
                dat %>% filter(inf == FALSE & ab == FALSE) %>% nrow())
  ) %>%
    `rownames<-`(c("ab_poz", "ab_neg"))

  # Create a more intuitive color scale function for a numeric vector
  get_colors <- function(values) {
    colors <- colorRampPalette(c("#D3D3D3", "#4D4D4D"))(100)
    minv <- min(values, na.rm=TRUE)
    rangeval <- max(values, na.rm=TRUE) - minv
    cols <- colors[1 + as.integer(99 * ((values - minv) / rangeval))]
    return(cols)
  }

  colors. <- get_colors(tbl_cross %>% {c(.$inf_poz, .$inf_neg)})

  # Apply the color scale to the table (assign colors per cell) and add row names
  tbl_cross_humread <- tbl_cross %>%
    tibble::rownames_to_column("exmple_arm") %>%
    flextable() %>%
    flextable::bg(i = 1, j = 2, bg = colors.[1]) %>%
    flextable::bg(i = 1, j = 3, bg = colors.[3]) %>%
    flextable::bg(i = 2, j = 2, bg = colors.[2]) %>%
    flextable::bg(i = 2, j = 3, bg = colors.[4])


  # Display the table
  tbl_cross_humread


  # make a 2x1 table for spuriousity
  tbl_spur <- dat %>%
    filter(!is.na(spur)) %>%
    count(spur) %>%
    mutate(percent = n / sum(n))

  # print humanreadable spuriosity table
  tbl_spur_humread <- tbl_spur %>%
    .[c(2,1),] %>%
    `colnames<-`(c("Spurious AB therapy?","No.cases","Proportion")) %>%
    flextable()

  return(list(
    tbl_summary = tbl_summary,
    tbl_cross = tbl_cross_humread,
    tbl_spur = tbl_spur_humread
  ))

}

dat <-  fun_generate_arm(inf_prop_abpoz = 0.3)


tables_control <- fun_generate_arm() %>% fun_generate_tables()
tables_allab    <- fun_generate_arm(ab_prop = 0.9) %>% fun_generate_tables()


