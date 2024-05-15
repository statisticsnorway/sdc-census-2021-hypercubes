
#' Hypercube formulas
#'
#' Formulas for the hypercubes in the 32 total population groups
#'
#' @param id Vector of hypercube IDs or a single integer representing a group.
#'           When NULL, a formula for all hypercubes is returned.
#' @param full When TRUE, all coarser aggregates are included.
#' @param operator When "*", a  formula with all marginals/totals.  These can be omitted with ":" instead.
#' @param remove_geo_n Whether to remove "geo_n". When there is only a single "geo_n" category, "geo_n" may be removed.
#'
#' @return
#' @export
#'
#' @examples
#' hyper_formula(1)
#' hyper_formula(2, full = FALSE, operator = ":")
#' hyper_formula(2)
#' hyper_formula(2, remove_geo_n = TRUE)
#' hyper_formula(c("1.4", "2.1", "2.2", "2.3", "31.3", "32.1", "32.2", "32.3"))
hyper_formula <- function(id = NULL, full = TRUE, operator = "*", remove_geo_n = FALSE) {
  id <- h_id(id)
  if (full) {
    k <- hyper_cubes_32_full()
  } else {
    k <- hyper_cubes_32()
  }
  if (!is.null(id)) {
    k <- k[names(k) %in% id]
  }
  if (remove_geo_n) {
    k <- lapply(k, function(x) {
      gsub("geo_n + ", "", x, fixed = TRUE)
    })
    k <- lapply(k, function(x) {
      x[x != "geo_n"]
    })
  }
  f <- as.formula(SmallCountRounding:::AddSim(SmallCountRounding:::List2string(k, sepWithin = operator)))
  environment(f) <- globalenv()
  f
}


# ex: h_id(1)
#     h_id(32)
h_id <- function(id) {
  hid <- names(hyper_cubes_32())
  if (length(id) == 1)
    if (as.integer(id) == as.numeric(id)) {
      return(hid[as.integer(hid) == as.integer(id)])
    }
  id
}


hyper_cubes_32 <- function() {
  list(`1.1` = c("geo_n", "sex", "age_h", "lms_h"), `1.2` = c("geo_n",
  "sex", "age_h", "hst_h"), `1.3` = c("geo_n", "sex", "age_h",
  "fst_h"), `1.4` = c("geo_n", "sex", "lms_h", "hst_h"), `2.1` = c("geo_m",
  "sex", "age_l", "lms_l", "fst_h"), `2.2` = c("geo_m", "sex",
  "age_l", "hst_h", "har"), `2.3` = c("geo_m", "sex", "age_m",
  "har", "loc"), `3.1` = c("geo_h", "sex", "age_m"), `3.2` = c("geo_h",
  "sex", "hst_m"), `3.3` = c("geo_h", "sex", "lms_l"), `4.1` = c("geo_l",
  "sex", "age_h", "cas_h"), `4.2` = c("geo_l", "sex", "age_h",
  "occ"), `4.3` = c("geo_l", "sex", "age_h", "edu"), `5.1` = c("geo_l",
  "sex", "age_m", "occ", "ind_l"), `5.2` = c("geo_l", "sex", "age_m",
  "occ", "sie"), `5.3` = c("geo_l", "sex", "age_m", "occ", "edu"
  ), `5.4` = c("geo_l", "sex", "age_l", "sie", "edu"), `5.5` = c("geo_n",
  "sex", "occ", "ind_l", "edu"), `5.6` = c("geo_l", "sex", "age_m",
  "ind_l", "sie"), `5.7` = c("geo_l", "sex", "age_l", "ind_l",
  "edu"), `6.1` = c("geo_l", "sex", "age_m", "lpw_n", "occ"), `6.2` = c("geo_l",
  "sex", "age_m", "lpw_n", "edu"), `6.3` = c("geo_l", "sex", "lpw_n",
  "ind_l", "sie"), `7.1` = c("geo_n", "sex", "age_m", "lpw_l",
  "ind_l"), `7.2` = c("geo_n", "sex", "age_m", "lpw_l", "sie"),
    `8.1` = c("geo_h", "sex", "coc_l"), `8.2` = c("geo_h", "sex",
    "pob_l"), `9.1` = c("geo_n", "sex", "age_m", "coc_l", "pob_h"
    ), `9.2` = c("geo_m", "sex", "age_m", "yae_h"), `9.3` = c("geo_m",
    "sex", "age_m", "pob_h"), `9.4` = c("geo_m", "sex", "pob_h",
    "yae_h"), `10.1` = c("geo_m", "sex", "age_m", "coc_l", "yat"
    ), `10.2` = c("geo_m", "sex", "age_m", "pob_l", "yat"), `10.3` = c("geo_l",
    "sex", "age_m", "cas_l", "coc_l", "yat"), `11.1` = c("geo_m",
    "sex", "age_m", "coc_h"), `11.2` = c("geo_m", "sex", "coc_h",
    "yae_l"), `12.1` = c("geo_m", "sex", "age_m", "yae_l", "roy"
    ), `12.2` = c("geo_m", "sex", "age_m", "pob_m", "roy"), `12.3` = c("geo_l",
    "sex", "coc_m", "pob_m", "roy"), `12.4` = c("geo_l", "sex",
    "age_m", "sie", "roy"), `13.1` = c("geo_l", "sex", "pob_m",
    "yae_h", "har"), `13.2` = c("geo_m", "sex", "age_m", "roy",
    "har"), `13.3` = c("geo_m", "age_m", "pob_m", "har"), `13.4` = c("geo_m",
    "age_m", "coc_m", "har"), `13.5` = c("geo_l", "coc_m", "pob_m",
    "yae_h"), `14.1` = c("geo_l", "sex", "age_m", "cas_h", "coc_l"
    ), `14.2` = c("geo_l", "sex", "age_m", "cas_h", "pob_l"),
    `14.3` = c("geo_l", "sex", "age_m", "cas_h", "yae_l"), `14.4` = c("geo_l",
    "sex", "age_m", "cas_h", "roy"), `14.5` = c("geo_l", "sex",
    "age_l", "cas_l", "roy", "har"), `15.1` = c("geo_l", "sex",
    "age_l", "cas_l", "edu", "pob_l"), `15.2` = c("geo_l", "sex",
    "cas_l", "edu", "yae_h"), `15.3` = c("geo_l", "sex", "cas_l",
    "coc_l", "yae_h"), `15.4` = c("geo_l", "sex", "age_m", "cas_l",
    "coc_l", "pob_l"), `16.1` = c("geo_l", "sex", "age_m", "occ",
    "coc_l"), `16.2` = c("geo_l", "sex", "age_m", "occ", "pob_l"
    ), `16.3` = c("geo_l", "sex", "age_m", "occ", "yae_l"), `16.4` = c("geo_l",
    "sex", "age_m", "occ", "roy"), `16.5` = c("geo_l", "sex",
    "occ", "pob_l", "yae_l"), `17.1` = c("geo_l", "sex", "age_m",
    "ind_h", "coc_l"), `17.2` = c("geo_n", "sex", "age_m", "ind_h",
    "yae_l"), `17.3` = c("geo_l", "sex", "age_m", "ind_h", "roy"
    ), `18.1` = c("geo_l", "sex", "ind_h", "sie", "pob_l"), `18.2` = c("geo_l",
    "sex", "ind_h", "edu", "pob_l"), `18.3` = c("geo_l", "sex",
    "ind_l", "coc_l", "pob_l"), `19.1` = c("geo_l", "sex", "age_m",
    "edu", "pob_l"), `19.2` = c("geo_l", "sex", "age_m", "edu",
    "yae_l"), `19.3` = c("geo_l", "sex", "edu", "pob_l", "yae_h"
    ), `20.1` = c("geo_l", "sex", "age_m", "lpw_n", "coc_l"),
    `20.2` = c("geo_l", "sex", "age_m", "lpw_n", "pob_l"), `21.1` = c("geo_l",
    "sex", "age_m", "lms_l", "cas_h"), `21.2` = c("geo_l", "sex",
    "age_m", "lms_l", "edu"), `21.3` = c("geo_l", "sex", "age_m",
    "fst_m", "cas_h"), `21.4` = c("geo_l", "sex", "age_m", "fst_m",
    "edu"), `21.5` = c("geo_l", "sex", "age_m", "hst_h", "cas_h"
    ), `22.1` = c("geo_l", "sex", "age_m", "hst_h", "edu"), `22.2` = c("geo_l",
    "sex", "age_m", "hst_h", "sie"), `23.1` = c("geo_n", "sex",
    "age_m", "hst_l", "cas_l", "edu"), `23.2` = c("geo_l", "sex",
    "age_m", "fst_l", "cas_l", "edu"), `24.1` = c("geo_n", "sex",
    "age_m", "lms_l", "fst_l", "cas_l"), `24.2` = c("geo_l",
    "sex", "age_m", "lms_l", "hst_m", "cas_l"), `25.1` = c("geo_m",
    "sex", "age_m", "lms_l", "pob_l"), `25.2` = c("geo_l", "sex",
    "age_m", "lms_l", "hst_m", "coc_l"), `25.3` = c("geo_l",
    "sex", "age_m", "lms_l", "hst_m", "pob_l"), `26.1` = c("geo_m",
    "sex", "age_m", "fst_l", "coc_l"), `26.2` = c("geo_m", "sex",
    "age_m", "hst_m", "pob_l"), `26.3` = c("geo_l", "sex", "age_m",
    "hst_m", "coc_l", "pob_l"), `27.1` = c("geo_m", "sex", "age_l",
    "fst_m", "yae_l"), `27.2` = c("geo_m", "sex", "age_l", "hst_m",
    "yae_l"), `28.1` = c("geo_l", "sex", "age_m", "fst_m", "roy"
    ), `28.2` = c("geo_l", "sex", "age_m", "hst_m", "roy"), `29.1` = c("geo_l",
    "sex", "age_m", "lms_l", "cas_l", "pob_l"), `29.2` = c("geo_l",
    "sex", "age_m", "fst_l", "cas_l", "pob_l"), `29.3` = c("geo_l",
    "sex", "age_m", "hst_m", "cas_l", "pob_l"), `30.1` = c("geo_l",
    "sex", "age_m", "lms_l", "cas_l", "coc_l"), `30.2` = c("geo_l",
    "sex", "age_m", "fst_l", "cas_l", "coc_l"), `30.3` = c("geo_l",
    "sex", "age_m", "hst_m", "cas_l", "coc_l"), `31.1` = c("geo_l",
    "sex", "age_m", "hst_m", "sie", "pob_l"), `31.2` = c("geo_l",
    "sex", "age_m", "fst_l", "sie", "pob_l"), `31.3` = c("geo_l",
    "sex", "hst_m", "edu", "pob_l"), `32.1` = c("geo_l", "sex",
    "age_m", "hst_m", "sie", "coc_l"), `32.2` = c("geo_l", "sex",
    "age_m", "fst_l", "sie", "coc_l"), `32.3` = c("geo_l", "sex",
    "hst_m", "edu", "coc_l"))
}


hyper_cubes_32_full <- function() {
  list(`1.1` = c("geo_n", "sex", "(age_l + age_m + age_h)", "(lms_l + lms_h)"
  ), `1.2` = c("geo_n", "sex", "(age_l + age_m + age_h)", "(hst_l + hst_m + hst_h)"
  ), `1.3` = c("geo_n", "sex", "(age_l + age_m + age_h)", "(fst_l + fst_m + fst_h)"
  ), `1.4` = c("geo_n", "sex", "(lms_l + lms_h)", "(hst_l + hst_m + hst_h)"
  ), `2.1` = c("(geo_n + geo_l + geo_m)", "sex", "age_l", "lms_l",
  "(fst_l + fst_m + fst_h)"), `2.2` = c("(geo_n + geo_l + geo_m)",
  "sex", "age_l", "(hst_l + hst_m + hst_h)", "har"), `2.3` = c("(geo_n + geo_l + geo_m)",
  "sex", "(age_l + age_m)", "har", "loc"), `3.1` = c("(geo_n + geo_l + geo_m + geo_h)",
  "sex", "(age_l + age_m)"), `3.2` = c("(geo_n + geo_l + geo_m + geo_h)",
  "sex", "(hst_l + hst_m)"), `3.3` = c("(geo_n + geo_l + geo_m + geo_h)",
  "sex", "lms_l"), `4.1` = c("(geo_n + geo_l)", "sex", "(age_l + age_m + age_h)",
  "(cas_l + cas_h)"), `4.2` = c("(geo_n + geo_l)", "sex", "(age_l + age_m + age_h)",
  "occ"), `4.3` = c("(geo_n + geo_l)", "sex", "(age_l + age_m + age_h)",
  "edu"), `5.1` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)",
  "occ", "ind_l"), `5.2` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)",
  "occ", "sie"), `5.3` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)",
  "occ", "edu"), `5.4` = c("(geo_n + geo_l)", "sex", "age_l", "sie",
  "edu"), `5.5` = c("geo_n", "sex", "occ", "ind_l", "edu"), `5.6` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "ind_l", "sie"), `5.7` = c("(geo_n + geo_l)",
  "sex", "age_l", "ind_l", "edu"), `6.1` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "lpw_n", "occ"), `6.2` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "lpw_n", "edu"), `6.3` = c("(geo_n + geo_l)",
  "sex", "lpw_n", "ind_l", "sie"), `7.1` = c("geo_n", "sex", "(age_l + age_m)",
  "(lpw_n + lpw_l)", "ind_l"), `7.2` = c("geo_n", "sex", "(age_l + age_m)",
  "(lpw_n + lpw_l)", "sie"), `8.1` = c("(geo_n + geo_l + geo_m + geo_h)",
  "sex", "coc_l"), `8.2` = c("(geo_n + geo_l + geo_m + geo_h)",
  "sex", "pob_l"), `9.1` = c("geo_n", "sex", "(age_l + age_m)",
  "coc_l", "(pob_l + pob_m + pob_h)"), `9.2` = c("(geo_n + geo_l + geo_m)",
  "sex", "(age_l + age_m)", "(yae_l + yae_h)"), `9.3` = c("(geo_n + geo_l + geo_m)",
  "sex", "(age_l + age_m)", "(pob_l + pob_m + pob_h)"), `9.4` = c("(geo_n + geo_l + geo_m)",
  "sex", "(pob_l + pob_m + pob_h)", "(yae_l + yae_h)"), `10.1` = c("(geo_n + geo_l + geo_m)",
  "sex", "(age_l + age_m)", "coc_l", "yat"), `10.2` = c("(geo_n + geo_l + geo_m)",
  "sex", "(age_l + age_m)", "pob_l", "yat"), `10.3` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "cas_l", "coc_l", "yat"), `11.1` = c("(geo_n + geo_l + geo_m)",
  "sex", "(age_l + age_m)", "(coc_l + coc_m + coc_h)"), `11.2` = c("(geo_n + geo_l + geo_m)",
  "sex", "(coc_l + coc_m + coc_h)", "yae_l"), `12.1` = c("(geo_n + geo_l + geo_m)",
  "sex", "(age_l + age_m)", "yae_l", "roy"), `12.2` = c("(geo_n + geo_l + geo_m)",
  "sex", "(age_l + age_m)", "(pob_l + pob_m)", "roy"), `12.3` = c("(geo_n + geo_l)",
  "sex", "(coc_l + coc_m)", "(pob_l + pob_m)", "roy"), `12.4` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "sie", "roy"), `13.1` = c("(geo_n + geo_l)",
  "sex", "(pob_l + pob_m)", "(yae_l + yae_h)", "har"), `13.2` = c("(geo_n + geo_l + geo_m)",
  "sex", "(age_l + age_m)", "roy", "har"), `13.3` = c("(geo_n + geo_l + geo_m)",
  "(age_l + age_m)", "(pob_l + pob_m)", "har"), `13.4` = c("(geo_n + geo_l + geo_m)",
  "(age_l + age_m)", "(coc_l + coc_m)", "har"), `13.5` = c("(geo_n + geo_l)",
  "(coc_l + coc_m)", "(pob_l + pob_m)", "(yae_l + yae_h)"), `14.1` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "(cas_l + cas_h)", "coc_l"), `14.2` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "(cas_l + cas_h)", "pob_l"), `14.3` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "(cas_l + cas_h)", "yae_l"), `14.4` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "(cas_l + cas_h)", "roy"), `14.5` = c("(geo_n + geo_l)",
  "sex", "age_l", "cas_l", "roy", "har"), `15.1` = c("(geo_n + geo_l)",
  "sex", "age_l", "cas_l", "edu", "pob_l"), `15.2` = c("(geo_n + geo_l)",
  "sex", "cas_l", "edu", "(yae_l + yae_h)"), `15.3` = c("(geo_n + geo_l)",
  "sex", "cas_l", "coc_l", "(yae_l + yae_h)"), `15.4` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "cas_l", "coc_l", "pob_l"), `16.1` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "occ", "coc_l"), `16.2` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "occ", "pob_l"), `16.3` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "occ", "yae_l"), `16.4` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "occ", "roy"), `16.5` = c("(geo_n + geo_l)",
  "sex", "occ", "pob_l", "yae_l"), `17.1` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "(ind_l + ind_h)", "coc_l"), `17.2` = c("geo_n",
  "sex", "(age_l + age_m)", "(ind_l + ind_h)", "yae_l"), `17.3` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "(ind_l + ind_h)", "roy"), `18.1` = c("(geo_n + geo_l)",
  "sex", "(ind_l + ind_h)", "sie", "pob_l"), `18.2` = c("(geo_n + geo_l)",
  "sex", "(ind_l + ind_h)", "edu", "pob_l"), `18.3` = c("(geo_n + geo_l)",
  "sex", "ind_l", "coc_l", "pob_l"), `19.1` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "edu", "pob_l"), `19.2` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "edu", "yae_l"), `19.3` = c("(geo_n + geo_l)",
  "sex", "edu", "pob_l", "(yae_l + yae_h)"), `20.1` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "lpw_n", "coc_l"), `20.2` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "lpw_n", "pob_l"), `21.1` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "lms_l", "(cas_l + cas_h)"), `21.2` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "lms_l", "edu"), `21.3` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "(fst_l + fst_m)", "(cas_l + cas_h)"
  ), `21.4` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)", "(fst_l + fst_m)",
  "edu"), `21.5` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)",
  "(hst_l + hst_m + hst_h)", "(cas_l + cas_h)"), `22.1` = c("(geo_n + geo_l)",
  "sex", "(age_l + age_m)", "(hst_l + hst_m + hst_h)", "edu"),
    `22.2` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)", "(hst_l + hst_m + hst_h)",
    "sie"), `23.1` = c("geo_n", "sex", "(age_l + age_m)", "hst_l",
    "cas_l", "edu"), `23.2` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)",
    "fst_l", "cas_l", "edu"), `24.1` = c("geo_n", "sex", "(age_l + age_m)",
    "lms_l", "fst_l", "cas_l"), `24.2` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "lms_l", "(hst_l + hst_m)", "cas_l"
    ), `25.1` = c("(geo_n + geo_l + geo_m)", "sex", "(age_l + age_m)",
    "lms_l", "pob_l"), `25.2` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)",
    "lms_l", "(hst_l + hst_m)", "coc_l"), `25.3` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "lms_l", "(hst_l + hst_m)", "pob_l"
    ), `26.1` = c("(geo_n + geo_l + geo_m)", "sex", "(age_l + age_m)",
    "fst_l", "coc_l"), `26.2` = c("(geo_n + geo_l + geo_m)",
    "sex", "(age_l + age_m)", "(hst_l + hst_m)", "pob_l"), `26.3` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "(hst_l + hst_m)", "coc_l", "pob_l"
    ), `27.1` = c("(geo_n + geo_l + geo_m)", "sex", "age_l",
    "(fst_l + fst_m)", "yae_l"), `27.2` = c("(geo_n + geo_l + geo_m)",
    "sex", "age_l", "(hst_l + hst_m)", "yae_l"), `28.1` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "(fst_l + fst_m)", "roy"), `28.2` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "(hst_l + hst_m)", "roy"), `29.1` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "lms_l", "cas_l", "pob_l"), `29.2` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "fst_l", "cas_l", "pob_l"), `29.3` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "(hst_l + hst_m)", "cas_l", "pob_l"
    ), `30.1` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)",
    "lms_l", "cas_l", "coc_l"), `30.2` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "fst_l", "cas_l", "coc_l"), `30.3` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "(hst_l + hst_m)", "cas_l", "coc_l"
    ), `31.1` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)",
    "(hst_l + hst_m)", "sie", "pob_l"), `31.2` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "fst_l", "sie", "pob_l"), `31.3` = c("(geo_n + geo_l)",
    "sex", "(hst_l + hst_m)", "edu", "pob_l"), `32.1` = c("(geo_n + geo_l)",
    "sex", "(age_l + age_m)", "(hst_l + hst_m)", "sie", "coc_l"
    ), `32.2` = c("(geo_n + geo_l)", "sex", "(age_l + age_m)",
    "fst_l", "sie", "coc_l"), `32.3` = c("(geo_n + geo_l)", "sex",
    "(hst_l + hst_m)", "edu", "coc_l"))
}

