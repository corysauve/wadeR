#' mIBI Calculator
#'
#' Calculates the mIBI per the Indiana Department of Environmental Management - IDEM - methodology
#'
#' @param df Dataframe containing taxa information
#' @param TotalIndividuals Numeric value of total sample size of macros
#'
#' @keywords mIBI
#' @export
#' @examples
#'
#' Variables required in 'df' parameter:
#'
#' date: Date of sampling
#' samp_id: Unique number of individual sample
#' collect_meth: Indicates if sample if from a kick-net or Hester-Dendy sampler
#' order: Order of taxa
#' family: Family of taxa
#' genus: Genus of taxa
#' ind_num: Total number of individuals in each taxonomic group
#' tot_val: Tolerance value associated with taxa according to the US EPA Rapid Bioassessment Protocols for Use in Wadeable Streams and Rivers
#' feed_grp: Feeding group classification according to US EPA Rapid Bioassessment Protocols for Use in Wadeable Streams and Rivers
#' hab_bhv: Habitat behavior classification according to US EPA Rapid Bioassessment Protocols for Use in Wadeable Streams and Rivers

mIBI_calc <- function(df, TotalIndividuals) {

  MetricValues <- df %>%
    mutate(
      ept = if_else(
        order == "Plecoptera" | order == "Ephemeroptera" | order == "Tricoptera",
        1,
        0
      ),
      dipt = if_else(
        order == "Diptera",
        1,
        0
      ),
      ortho_tany = if_else(
        genus == "Orthocladiinae" | genus == " Tanytarsini",
        ind_num,
        0
      ),
      non_insect = if_else(
        order == "Isopoda" | order == " Amphipoda" | order == "Oligochaeta" | order == "Nematoda" | order == "Nematomorpha)",
        ind_num,
        0
      ),
      intol = if_else(
        tol_val <= 3,
        ind_num,
        0
      ),
      tol = if_else(
        tol_val >= 3,
        ind_num,
        0
      ),
      pred = if_else(
        feed_grp == "pr",
        ind_num,
        0
      ),
      shr_scr = if_else(
        feed_grp == "sh" | feed_grp == "sc",
        ind_num,
        0
      ),
      col_fil = if_else(
        feed_grp == "gc",
        ind_num,
        0
      ),
      spraw = if_else(
        hab_bhv == "sp",
        ind_num,
        0
      )
    )

  mIBIvalues <- MetricValues %>%
    group_by(
      samp_id
    ) %>%
    mutate(
      m1_val = n()
    ) %>%
    summarize(
      m1_val = n(),
      m2_val = TotalIndividuals,
      m3_val = sum(ept, na.rm = TRUE),
      m4_val = sum(dipt, na.rm = TRUE),
      m5_val = ((sum(ortho_tany) / sum(ind_num)) * 100),
      m6_val = ((sum(non_insect) / sum(ind_num)) * 100),
      m7_val = ((sum(intol) / sum(ind_num)) * 100),
      m8_val = ((sum(tol) / sum(ind_num)) * 100),
      m9_val = ((sum(pred) / sum(ind_num)) * 100),
      m10_val = ((sum(shr_scr) / sum(ind_num)) * 100),
      m11_val = ((sum(col_fil) / sum(ind_num)) * 100),
      m12_val = ((sum(spraw) / sum(ind_num)) * 100)
    )

  mIBIscores <- mIBIvalues %>%
    group_by(
      samp_id
    )%>%
    summarize(m1_score = (if_else(m1_val < 21, 1,
                                  if_else(m1_val >= 21 & m1_val < 41, 3,
                                          if_else(m1_val >= 41, 5, NULL)))),
              m2_score = (if_else(m2_val < 129, 1,
                                  if_else(m2_val >= 129 & m2_val < 258, 3,
                                          if_else(m2_val >= 258, 5, NULL)))),
              m3_score = (if_else(m3_val < 2, 1,
                                  if_else(m3_val >= 2 & m3_val < 4, 3,
                                          if_else(m3_val >= 4, 5, NULL)))),
              m4_score = (if_else(m4_val < 7, 1,
                                  if_else(m4_val >= 7 & m4_val < 14, 3,
                                          if_else(m4_val >= 14, 5, NULL)))),
              m5_score = (if_else(m5_val >= 47, 1,
                                  if_else(m5_val >= 24 & m5_val < 47, 3,
                                          if_else(m5_val < 24, 5, NULL)))),
              m6_score = (if_else(m6_val >= 35, 1,
                                  if_else(m6_val >= 18 & m6_val < 35, 3,
                                          if_else(m6_val < 18, 5, NULL)))),
              m7_score = (if_else(m7_val < 15.9, 1,
                                  if_else(m7_val >= 15.9 & m7_val < 31.8, 3,
                                          if_else(m7_val >= 31.8, 5, NULL)))),
              m8_score = (if_else(m8_val >= 25.3, 1,
                                  if_else(m8_val >= 12.6 & m8_val < 25.3, 3,
                                          if_else(m8_val < 12.6, 5, NULL)))),
              m9_score = (if_else(m9_val < 18, 1,
                                  if_else(m9_val >= 18 & m9_val < 36, 3,
                                          if_else(m9_val >= 36, 5, NULL)))),
              m10_score = (if_else(m10_val < 10, 1,
                                   if_else(m10_val >= 10 & m10_val < 20, 3,
                                           if_else(m10_val >= 20, 5, NULL)))),
              m11_score = (if_else(m11_val >= 20, 1,
                                   if_else(m11_val >= 10 & m11_val < 20, 3,
                                           if_else(m11_val < 10, 5, NULL)))),
              m12_score = (if_else(m12_val < 3, 1,
                                   if_else(m12_val >= 3 & m12_val < 6, 3,
                                           if_else(m12_val >= 6, 5, NULL))))
    )

  mIBI <- mIBIscores %>%
    mutate(
      mIBI = rowSums(
        .[2:13])
    ) %>%
    select(samp_id, mIBI)

  return(mIBI)

}
