# This script was originally written by Peter Bjødstrup Jensen, Clinical Pharmacology, Pharmacy and Environmental Medicine, Department of Public Health, University of Southern Denmark.
# It was shared privately, and it has not been publicly released. Uploaded here with acknowledgment of his contribution.

timeslicer <- function(dt, col_id, col_wstart, col_wend, col_epstart, col_epend, col_dim, col_state) {

  DT <- data.table::copy(dt)

  # Rename to fixed column names
  old_names <- c(col_id, col_wstart, col_wend, col_epstart, col_epend, col_dim, col_state)
  newnames <- c("ID", "WFROM", "WTO", "EFROM", "ETO", "DIM", "STATE")

  setnames(DT, old_names, newnames)

  # dates as integers
  DT[, WFROM := as.integer(WFROM)]
  DT[, WTO := as.integer(WTO)]
  DT[, EFROM := as.integer(EFROM)]
  DT[, ETO := as.integer(ETO)]

  # create a separate datatable with just the unique obs-window per person
  DT_master <- unique(DT[,.(ID, WFROM, WTO)])

  # keep only episodes within or overlapping obs window
  DT <-  DT[ETO >= WFROM & EFROM <= WTO]

  # truncate episodes ends that overlap obs window range
  DT[, EFROM := ifelse(EFROM < WFROM, WFROM, EFROM)]
  DT[, ETO   := ifelse(ETO > WTO, WTO, ETO)]

  setorder(DT, ID, DIM, EFROM)
  #print(DT)
  #create records for leading, intermittent and trailing state 0's
  DT[, T1 := ETO + 1]
  DT[, T2 := shift(EFROM, type = "lead") - 1, by= c("ID", "DIM")]
  # if T2 is NA and smaller than right side of obswindow
  DT[is.na(T2) & T1 < WTO, T2 := WTO]
  # identify any 'leading' state 0's from start to first episode
  leading_zero_states <- DT[, .SD[1], by = c("ID", "DIM")][WFROM < EFROM, .(ID, DIM, EFROM = WFROM, ETO = EFROM - 1, STATE = 0)]
  # identify intermittent and trailing state 0's
  intermit_trail_zero_states <- DT[!is.na(T2) & T2 >= T1, .(ID, DIM, EFROM = T1, ETO = T2, STATE = 0)]
  # bind leading, intermittent and trailing state 0 periods to non-state 0 episodes
  DT <- DT[,.(ID, DIM, EFROM, ETO, STATE)]
  #print(str(DT))
  #print(str(leading_zero_states))
  #print(str(intermit_trail_zero_states))

  DT <- rbindlist(list(DT, leading_zero_states, intermit_trail_zero_states))
  setorder(DT, ID, DIM, EFROM, ETO)

  ## End of part one. For each ID, the full observation periods is now spanned by
  ## segments of time with state >= 1 or state 0 for every dimension

  ## PART TWO - For each dimension, iteratively join time segments together to build
  ## smaller and smaller overlapping segments.

  dims <- unique(DT[, DIM])
  setnames(DT, c("EFROM", "ETO"), c("T1", "T2"))
  setnames(DT_master, c("WFROM", "WTO"), c("T1_master", "T2_master"))

  for (dim in dims) {
    print(paste0("timeslicer: dim - ",dim))
    # subset with dim record
    DT_dim <- DT[DIM == dim]
    # right outer join to current "master" datatable
    DT_master <- DT_dim[DT_master, on = "ID", allow.cartesian = TRUE]

    # add state 0 segment for ID's with no episode for this dim
    DT_master[is.na(DIM),  `:=` (T1 = T1_master,
                                 T2 = T2_master,
                                 STATE = 0)]

    # keep only records where joined records has segments overlapping (Allen overlaps)
    DT_master <- DT_master[!(T1_master > T2 | T1 > T2_master)]

    # update start and end times of overlapping segments
    DT_master[, FROM := pmax(T1, T1_master)]
    DT_master[, TO   := pmin(T2, T2_master)]

    # drop stuff
    DT_master[, c("T1","T2","T1_master", "T2_master") := NULL]
    setnames(DT_master, c("FROM", "TO"), c("T1_master", "T2_master"))
    setorder(DT_master, ID,T1_master, T2_master)
    setnames(DT_master, "STATE", dim)
    DT_master[, DIM := NULL]
  }

  # finalize returned DT
  setnames(DT_master, c("ID", "T1_master", "T2_master"), c(col_id, "From", "To"))

  # integers as dates
  # DT_master[, From := as.Date(From)]
  # DT_master[, To := as.Date(To)]

  return(DT_master)
}
