format_table_modal_maturity <- function(tab, fmt = "%i", extra_rows = "Total", matur_unique = modal_matur_unique) {
  # tab[] <- lapply(tab, function(x) ifelse(is.na(x), "-", sprintf(fmt, x)))
  cbind(`Modal maturity` = c(matur_unique, extra_rows), tab)
}

format_table_modal_sex <- function(tab, fmt = "%i", extra_rows = "Total", sex_unique = modal_sex_unique) {
  # tab[] <- lapply(tab, function(x) ifelse(is.na(x), "-", sprintf(fmt, x)))
  cbind(`Modal sex` = c(sex_unique, extra_rows), tab)
}

format_table_matur_stage <- function(tab, fmt = "%i", extra_rows = "Total", matur_unique = modal_matur_unique) {
  # tab[] <- lapply(tab, function(x) ifelse(is.na(x), "-", sprintf(fmt, x)))
  cbind(`Maturity stage` = c(matur_unique, extra_rows), tab)
}

format_table_sex_stage <- function(tab, fmt = "%i", extra_rows = "Total", sex_unique = modal_sex_unique) {
  # tab[] <- lapply(tab, function(x) ifelse(is.na(x), "-", sprintf(fmt, x)))
  cbind(`Sex category` = c(sex_unique, extra_rows), tab)
}



sample_data_overview_table <- function(dat, strata) {
  #debug
  #dat<-ad_long_all
  #strata<-config$strata

  dat$MaturityTemp=dat$Maturity

  maturation_sample_data_overview <- dat %>%
  select_at(unique(c("FishID", "SampleID", "length", "modal_maturity", "year", "ices_area", "qtr", strata, "reader", "Maturity", "MaturityTemp"))) %>%
  spread(key = reader, value = MaturityTemp) %>%
    group_by_at(unique(c("year", "ices_area", "qtr", strata))) %>%
    summarise(
      min_len = 5*round(min(length, na.rm = TRUE)/5),
      max_len = 5*round(max(length, na.rm = TRUE)/5),
      num = length(unique(SampleID)),
      mat_stages = do.call(paste, c(as.list(sort(unique(as.character(Maturity)))), sep = ";")),
      modal_mat_stages = do.call(paste, c(as.list(sort(unique(as.character(modal_maturity)))), sep = ";"))
    ) %>%
    as.data.frame

  dat$SexTemp=dat$Sex

  sex_sample_data_overview <- dat %>%
   select_at(unique(c("FishID", "SampleID", "length", "modal_sex", "year", "ices_area", "qtr", strata, "reader", "Sex", "SexTemp"))) %>%
   spread(key = reader, value = SexTemp) %>%
    group_by_at(unique(c("year", "ices_area", "qtr", strata))) %>%
    summarise(
      min_len = 5*round(min(length, na.rm = TRUE)/5),
      max_len = 5*round(max(length, na.rm = TRUE)/5),
      num = sum(!is.na(SampleID)),
      sex_cat = do.call(paste, c(as.list(sort(unique(as.character(Sex)))), sep = ";")),
      modal_sex_cat = do.call(paste, c(as.list(sort(unique(as.character(modal_sex)))), sep = ";"))
    ) %>%
    as.data.frame


  sample_data_overview=maturation_sample_data_overview
  sample_data_overview$sex_cat=sex_sample_data_overview$sex_cat
  sample_data_overview$modal_sex_cat=sex_sample_data_overview$modal_sex_cat

  total=data.frame(year="Total",
                   ices_area=do.call(paste, c(as.list(unique(sample_data_overview$ices_area)), sep = ";")),
                   qtr=do.call(paste, c(as.list(sort(unique(sample_data_overview$qtr))), sep = ";")),
                   strata,
                   min_len=min(sample_data_overview$min_len),
                   max_len=max(sample_data_overview$max_len),
                   num=sum(sample_data_overview$num),
                   mat_stages=do.call(paste, c(as.list(sort(unique(as.character(dat$Maturity)))), sep = ";")),
                   modal_mat_stages=do.call(paste, c(as.list(sort(unique(as.character(dat$modal_maturity)))), sep = ";")),
                   sex_cat=do.call(paste, c(as.list(sort(unique(as.character(dat$Sex)))), sep = ";")),
                   modal_sex_cat=do.call(paste, c(as.list(sort(unique(as.character(dat$modal_sex)))), sep = ";")))

  colnames(total)[which(names(total) == 'strata')] <- strata


  sample_data_overview=rbind(sample_data_overview, total)

  sample_data_overview
}


# Participants table
reader_data_table <- function(dat, strata){
  dat %>%
    select_at(unique(c("reader", "expertise", "reader_number", strata))) %>%
    unique %>%
    arrange(reader_number) %>%
    dplyr::rename(`Reader code` = reader,
           Expertise = expertise,
           Experience_rank = reader_number)
}

read_maturity_reader <- function(dat, by = "reader") {
  table(dat$Maturity, dat[[by]]) %>%
    unclass %>%
    as.data.frame %>%
    mutate(Total = unname(rowSums(.)))
}

numbers_read_maturity <- function(dat, by = "reader") {
  table(dat$modal_maturity, dat[[by]]) %>%
  unclass %>%
  as.data.frame %>%
  mutate(Total = unname(rowSums(.)))
}

numbers_read_sex <- function(dat, by = "reader") {
  table(dat$modal_sex, dat[[by]]) %>%
    unclass %>%
    as.data.frame %>%
    mutate(Total = unname(rowSums(.)))
}

num_read_table_modal_maturity <- function(dat, by = "reader") {
  dat %>%
    numbers_read_maturity(by = by) %>%
    rbind(colSums(., na.rm = TRUE)) %>%
    format_table_modal_maturity
}

num_read_table_modal_sex <- function(dat, by = "reader") {
  dat %>%
    numbers_read_sex(by = by) %>%
    rbind(colSums(., na.rm = TRUE)) %>%
    format_table_modal_sex
}

# Prepare the table with the number of cases with multiple modes depending if the methodology used to define the mode is the traditional mode (no weighting of the readers), using a linear weighting for the readers, a negative exponential weighting or a multistage approach, where a combination of the different approaches is used.
multimode_cases_table_traditional<- function(dat, varmod) {

  MM_tab=tapply(eval(parse(text=paste0("dat$NModes_trad_", varmod))),dat$SampleID,max) %>% unclass %>% as.data.frame
  colnames(MM_tab)=c("NModes_trad")
  MM_tab$FishID=rownames(MM_tab)

  MM_tab=MM_tab[MM_tab$NModes_trad>1,]
  if(dim(MM_tab)[1]==0) {MM_tab=data.frame(FishID="----", NModes_trad="zero")}
  return(MM_tab)
}

multimode_cases_table_linear <- function(dat, varmod) {

  MM_tab=tapply(eval(parse(text=paste0("dat$NModes_linearweight_", varmod))),dat$SampleID,max) %>% unclass %>% as.data.frame
  colnames(MM_tab)=c("NModes_linear")
  MM_tab$FishID=rownames(MM_tab)

  MM_tab=MM_tab[MM_tab$NModes_linear>1,]
  if(dim(MM_tab)[1]==0) {MM_tab=data.frame(FishID="----", NModes_linear="zero")}
  return(MM_tab)
}

multimode_cases_table_negexp <- function(dat, varmod) {

  MM_tab=tapply(eval(parse(text=paste0("dat$NModes_negexpweight_", varmod))),dat$SampleID,max) %>% unclass %>% as.data.frame
  colnames(MM_tab)=c("NModes_negexp")
  MM_tab$FishID=rownames(MM_tab)

  MM_tab=MM_tab[MM_tab$NModes_negexp>1,]
  if(dim(MM_tab)[1]==0) {MM_tab=data.frame(FishID="----", NModes_negexp="zero")}
  return(MM_tab)
}

# in the case of the multistage approach, the number of multiple mode cases will be the same than the negative exponential weighting approach.
multimode_cases_table_multistage <- function(dat, varmod) {

  MM_tab=tapply(eval(parse(text=paste0("dat$NModes_negexpweight_", varmod))),dat$SampleID,max) %>% unclass %>% as.data.frame
  colnames(MM_tab)=c("NModes_multistage")
  MM_tab$FishID=rownames(MM_tab)

  MM_tab=MM_tab[MM_tab$NModes_multistage>1,]
  if(dim(MM_tab)[1]==0) {MM_tab=data.frame(FishID="----", NModes_multistage="zero")}
  return(MM_tab)
}


# Coefficient of unalikeability table (based in the statistics defined in Kader and Perry 2007)
cu_table <- function(dat, varmod, by = "reader") {

  cu_tab <-
    round(tapply(eval(parse(text=paste0("dat$", varmod))), list(eval(parse(text=paste0("dat$modal_", tolower(varmod)))), dat[[by]]), cu_II), digits=3) %>%
    unclass %>%
    as.data.frame %>%
    mutate(Total = round(tapply(eval(parse(text=paste0("dat$", varmod))), list(eval(parse(text=paste0("dat$modal_", tolower(varmod))))), cu_II), digits=3) %>% unclass %>% unname)
  cu_tab[modal_matur_unique_all == 0,] <- NA

  # Add weighted mean
  num_read <- eval(parse(text=paste0("numbers_read_", tolower(varmod))))(dat, by)
  cu_tab <- rbind(cu_tab,
                  round(colSums(cu_tab * num_read, na.rm = TRUE) / colSums(num_read * !is.na(cu_tab), na.rm = TRUE), digits=3))

  # # produce formatted version
  eval(parse(text=paste0("format_table_modal_", tolower(varmod))))(cu_tab, fmt = "%.0f %%", extra_rows = "Weighted Mean")
}

# Percentage of agreement table
pa_table <- function(ad_long, varmod, by = "reader") {

  pa_tab <-
    ad_long %>%
    filter(get(all_of(varmod)) == eval(parse(text=paste0("modal_", tolower(varmod))))) %>%
    with(., table(eval(parse(text=paste0("modal_", tolower(varmod)))), .[[by]])) %>%
    unclass %>%
    as.data.frame %>%
    mutate(
      Total = rowSums(.)
    )

  # Total numbers read
  num_read <- eval(parse(text=paste0("numbers_read_", tolower(varmod))))(ad_long, by)

  #It may be that some readers never agreed with the modal age/maturity stage. This would create differences with the dimensions of pa_tab and num_read. Next, this problem is solved by forcing pa_tab to have the same columns than num_read.
  A=colnames(pa_tab)
  B=colnames(num_read)
  diff=c(setdiff(A,B), setdiff(B,A))
  temp=as.data.frame(matrix(data=0,nrow=dim(pa_tab)[1],ncol=length(diff), dimnames = list(c(1:dim(pa_tab)[1]), diff)))
  pa_tab=cbind(pa_tab,temp)

  # # overall agreement per modal age
  pa_tab <- pa_tab / num_read * 100

  # # Add weighted mean
  pa_tab <- rbind(pa_tab,
                  colSums(pa_tab * num_read, na.rm = TRUE) / colSums(num_read * !is.na(pa_tab), na.rm = TRUE))
  pa_tab=round(pa_tab,digits=1)
  pa_tab[is.na(pa_tab)]="-"

  # produce formatted version
  eval(parse(text=paste0("format_table_modal_", tolower(varmod))))(pa_tab, fmt = "%.0f %%", extra_rows = "Weighted Mean")
}



reader_freq_table <- function(ad_long, varmod, by=NULL) {

  if(is.null(by)) {
    reader_freq_tab <-
      ad_long %>%
      ddply(.(eval(parse(text=paste0("modal_", tolower(varmod)))), get(all_of(varmod)), reader), summarise, count=length(get(all_of(varmod))))
    colnames(reader_freq_tab) <- c(paste0("modal_", tolower(varmod)), tolower(varmod), "reader","count")

    temp <-
      ad_long %>%
      ddply(.(eval(parse(text=paste0("modal_", tolower(varmod)))), reader), summarise, count=length(get(all_of(varmod))))
    colnames(temp)=c(paste0("modal_", tolower(varmod)), "reader","count")

    reader_freq_tab <- merge(reader_freq_tab, temp, by.x=c(paste0("modal_", tolower(varmod)), "reader"), by.y=c(paste0("modal_", tolower(varmod)), "reader"))

    reader_freq_tab$freqrelat <- round(reader_freq_tab$count.x/reader_freq_tab$count.y, digits=3)

    reader_freq_tab <- reader_freq_tab %>% select(-count.x, -count.y)

    reader_freq_tab <-
      reader_freq_tab %>%
      spread(key=reader, value=freqrelat)

    #temp <-
    #  expand.grid(
    #    unique(ad_long[, paste0("modal_", tolower(varmod))]),
    #    unique(ad_long[, varmod])
    #  )
    temp <- data.frame(expand.grid(unique(eval(parse(text=paste0("ad_long$modal_", tolower(all_of(varmod)))))), unique(eval(parse(text=paste0("ad_long$", all_of(varmod)))))))
    colnames(temp) <- c(paste0("modal_", tolower(all_of(varmod))), tolower(all_of(varmod)))

    reader_freq_tab <- merge(reader_freq_tab, temp, by.x=c(paste0("modal_", tolower(all_of(varmod))), tolower(all_of(varmod))),  by.y=c(paste0("modal_", tolower(all_of(varmod))), tolower(all_of(varmod))), all.y=TRUE)

    reader_freq_tab[,1] <- as.character(reader_freq_tab[,1])

    reader_freq_tab <- rbind(reader_freq_tab, c("all", "all", as.numeric(colMeans(reader_freq_tab[,3:dim(reader_freq_tab)[2]], na.rm=TRUE))))

    reader_freq_tab[is.na(reader_freq_tab)] <- 0

    reader_freq_tab$Total <- round(rowMeans(as.data.frame(lapply(reader_freq_tab[,3:dim(reader_freq_tab)[2]],as.numeric)), na.rm=TRUE), digits=3)

    reader_freq_tab[is.na(reader_freq_tab)] <- "-"
  } else {

    reader_freq_tab <-
      ad_long %>%
      ddply(.(eval(parse(text=paste0("modal_", tolower(varmod)))), get(varmod), reader, get(by)), summarise, count=length(get(all_of(varmod))))
    colnames(reader_freq_tab) <- c(paste0("modal_", tolower(varmod)), tolower(varmod), "reader", by, "count")

    temp <-
      ad_long %>%
      ddply(.(eval(parse(text=paste0("modal_", tolower(varmod)))), reader, get(all_of(by))), summarise, count=length(get(all_of(varmod))))
    colnames(temp) <- c(paste0("modal_", tolower(varmod)), "reader", by, "count")

    reader_freq_tab <- merge(reader_freq_tab, temp, by.x=c(paste0("modal_", tolower(varmod)), "reader", by), by.y=c(paste0("modal_", tolower(varmod)), "reader", by))

    reader_freq_tab$freqrelat=round(reader_freq_tab$count.x/reader_freq_tab$count.y, digits=3)

    reader_freq_tab=reader_freq_tab[,-c(5, 6)]

    reader_freq_tab=
      reader_freq_tab %>%
      spread(key=reader, value=freqrelat)

    temp=data.frame(expand.grid(unique(eval(parse(text=paste0("ad_long$modal_", tolower(all_of(varmod)))))), unique(eval(parse(text=paste0("ad_long$", all_of(varmod)))))))
    colnames(temp)=c(paste0("modal_", tolower(all_of(varmod))), tolower(all_of(varmod)))

    reader_freq_tab=merge(reader_freq_tab, temp, by.x=c(paste0("modal_", tolower(all_of(varmod))), tolower(all_of(varmod))),  by.y=c(paste0("modal_", tolower(all_of(varmod))), tolower(all_of(varmod))), all.y=T)

    reader_freq_tab[,1]=as.character(reader_freq_tab[,1])

    reader_freq_tab=rbind(reader_freq_tab, c("all", "all", as.numeric(colMeans(reader_freq_tab[,4:dim(reader_freq_tab)[2]], na.rm=TRUE))))

    reader_freq_tab[is.na(reader_freq_tab)]=0

    reader_freq_tab$Total=round(rowMeans(as.data.frame(lapply(reader_freq_tab[,4:dim(reader_freq_tab)[2]],as.numeric)), na.rm=TRUE), digits=3)

    reader_freq_tab[is.na(reader_freq_tab)]="-"

    }
  return(reader_freq_tab)

}



general_freq_table <- function(ad_long, varmod, by=NULL) {
  if(is.null(by)){
    general_freq_tab <-
      tapply(eval(parse(text=paste0("ad_long$", varmod))), list(eval(parse(text=paste0("ad_long$modal_", tolower(varmod)))), eval(parse(text=paste0("ad_long$", varmod)))),length) %>%
      unclass %>%
      as.data.frame

    general_freq_tab=round(general_freq_tab/rowSums(general_freq_tab, na.rm=TRUE), digits=3)

    general_freq_tab$modes=rownames(general_freq_tab)

    temp=data.frame(expand.grid(unique(eval(parse(text=paste0("ad_long$", varmod))))))
    colnames(temp)=c("modal_maturity")

    general_freq_tab=merge(general_freq_tab, temp, by.x=c("modes"),  by.y=c("modal_maturity"), all.y=T)
    general_freq_tab$modes=as.character(general_freq_tab$modes)

    general_freq_tab=general_freq_tab[with(general_freq_tab, order(modes)),]
    general_freq_tab[is.na(general_freq_tab)]="-"

    colnames(general_freq_tab)=c(paste0("modal_", tolower(varmod), "/", tolower(varmod), "_stages"), colnames(general_freq_tab)[-1])
  } else {
    general_freq_tab <-
      ad_long %>%
      ddply(.(get(all_of(paste0("modal_", tolower(varmod)))), get(all_of(varmod)), .[[by]]), summarise, length(get(all_of(varmod))))
    colnames(general_freq_tab)=c(paste0("modal_", tolower(varmod)), varmod, by, "frequency")
    general_freq_tab <-
      general_freq_tab %>%
      spread(key=get(all_of(varmod)), value=frequency)
    general_freq_tab=general_freq_tab[with(general_freq_tab, order(get(all_of(by)))),]
    general_freq_tab[is.na(general_freq_tab)]="-"
  }
  return(general_freq_tab)
}



data_overview_table <- function(dat, varmod, report_token) {

   if (any(dat$TypeAnnotation == "eventOrganizer" & dat$DoesSampleHaveHistologyImage == "Yes")) {
    hist <-
     dat[dat$TypeAnnotation == "eventOrganizer" & dat$DoesSampleHaveHistologyImage == "Yes", ] %>%
     ddply(.(FishID), summarise, Histology = "yes") %>%
     select(., c("FishID", "Histology"))
     dat <- merge(dat, hist, by.x = "FishID", by.y = "FishID", all.x = TRUE)
     dat$Histology[is.na(dat$Histology)] <- "no"
   } else {
    dat$Histology <- "no"
  }

  # Select only columns of maturity staging
  ad_wide <-
    dat %>%
  # select(FishID, SampleID, length, ices_area, stock, prep_method, reader, all_of(varmod), Histology) %>%
  # select(FishID, length, ices_area, stock, prep_method, reader, all_of(varmod), Histology) %>%
  select(FishID, length, stock, prep_method, reader, all_of(varmod), Histology) %>%
   spread(key = reader, value = all_of(varmod))

  # Calculate, modal maturity, percentage agreement and cu
  readings <-
    ad_wide %>%
    select(matches("R[0-9][0-9]*"))

  complete <- complete.cases(readings)

  ad_wide[c("Mode", "PA %", "CU %")] <- NA

  ad_wide$Mode[complete] <- apply(readings[complete,], 1, Mode_II)
  ad_wide$`PA %`[complete] <- round(rowMeans(readings[complete, ] == ad_wide$Mode[complete], na.rm = TRUE) * 100)
  ad_wide$`CU %`[complete] <- round(apply(readings[complete,], 1, cu_II), 3)
  ad_wide$`CU %`[is.nan(ad_wide$`CU %`)] <- NA
  ad_wide <- dplyr::rename(ad_wide, `ICES area` = ices_area)
  ad_wide[is.na(ad_wide)] <- "-"

  # add hyper link for tables
 # if(is.null(dat$SampleID)) {
   if(is.null(dat$FishID)) {
    dat %>%
      group_by(FishID, EventID) %>%
      summarise(
        `Image ID` = sprintf("[%s](http://smartdots.ices.dk/viewImage?tblEventID=%i&SmartImageID=%s&token=%s)",FishID, EventID, FishID, report_token) %>%
          unique %>%
          paste(collapse = "-")
      ) %>%
      right_join(ad_wide, by = c("FishID")) %>%
      rename(
        `Fish ID` = FishID,
        `Event ID` = EventID
      ) %>%
      as.data.frame
  } else {
    dat %>%
  #   group_by(SampleID, FishID, EventID) %>%
     group_by(FishID, EventID) %>%
      summarise(
        `Image ID` = sprintf("[%s](http://smartdots.ices.dk/viewImage?tblEventID=%i&SmartImageID=%s&token=%s)", SampleID, EventID, SampleID, report_token) %>%
          unique %>%
          paste(collapse = "-")
      ) %>%
     # right_join(ad_wide, by = c("FishID", "SampleID")) %>%
     right_join(ad_wide, by = "FishID") %>%
      rename(
       # `Sample ID` = SampleID,
        `Fish ID` = FishID,
        `Event ID` = EventID
      ) %>%
      as.data.frame
    }
}


maturity_composition <- function(dat, by = "reader") {
  # Number of gonads staged per reader and maturity stage
  dat %>%
    with(., table(Maturity, .[[by]])) %>%
    unclass %>%
    as.data.frame
}


sex_composition <- function(dat, by = "reader") {
  # Number of gonads staged per reader and maturity stage
  dat %>%
    with(., table(Sex, .[[by]])) %>%
    unclass %>%
    as.data.frame
}

maturity_composition_table <- function(ad_long, by = "reader") {
  if (nrow(ad_long) == 0) return(data.frame("Maturity stage" = numeric(0)))
  # Number of gonads staged per reader and maturity state
  ad_long %>%
    maturity_composition(by = by)  %>%
    rbind(colSums(., na.rm = TRUE)) %>%
  format_table_matur_stage(matur_unique = sort(unique(ad_long$Maturity)))
}

sex_composition_table <- function(ad_long, by = "reader") {
 if (nrow(ad_long) == 0) return(data.frame("Sex category" = numeric(0)))
  # Number of gonads staged per reader and sex category
  ad_long %>%
    sex_composition(by = by)  %>%
    rbind(colSums(., na.rm = TRUE)) %>%
  format_table_sex_stage(sex_unique = sort(unique(ad_long$Sex)))
}



# Maturity Stage Error Matrix MSEM  ###########################################################
# The MSEM calculates the relative contribution of each maturity stage to the modal maturity stage. It shows the proportion of each modal maturity stage mis-staged as other stages.
# The MSEM is calculated per area and only including the readings
# of the advanced stagers.
# AGE ERROR MATRIX  ###########################################################

# The AEM calculates the relative contribution of each age to the modal age.
# The AEM is calculated per area and only including the readings
# of the advanced readers.

mat_stage_error_matrix <- function(dat, by = NULL) {
  # Relative contribution of each maturity stage per modal maturity stage (long format)
  # msem=ad_long %>%
  dat %>%
    group_by_at(c("modal_maturity", "Maturity", by)) %>%
    summarise(
      matur_per_modal = length(!is.na(Maturity))
    ) %>%
    ungroup %>%
    group_by_at(c("modal_maturity", by)) %>%
    mutate(
      total_per_modal = sum(matur_per_modal)
    ) %>%
    ungroup %>%
    mutate(
      rel_matur = round(matur_per_modal / total_per_modal, digits=3)
    ) %>%
    select_at(c(by, "Maturity", "modal_maturity", "rel_matur")) %>%
    spread(modal_maturity, rel_matur) %>%
    mutate(Maturity = Maturity) %>%
    rename(`Maturity/Modal maturity` = Maturity) %>%
    as.data.frame %>%
    # split into several data.frames?
    by(apply(.[by], 1, paste, collapse = ", "), function(x) {rownames(x) <- NULL; x}) %>%
    unclass %>%
    (function(x) {
      attr(x, "call") <- NULL
      x
    })
}



sex_stage_error_matrix <- function(dat, by = NULL) {
  # Relative contribution of each maturity stage per modal maturity stage (long format)
  # msem=ad_long %>%
  dat %>%
    group_by_at(c("modal_sex", "Sex", by)) %>%
    summarise(
      sex_per_modal = length(!is.na(Sex))
    ) %>%
    ungroup %>%
    group_by_at(c("modal_sex", by)) %>%
    mutate(
      total_per_modal = sum(sex_per_modal)
    ) %>%
    ungroup %>%
    mutate(
      rel_sex = round(sex_per_modal / total_per_modal, digits=3)
    ) %>%
    select_at(c(by, "Sex", "modal_sex", "rel_sex")) %>%
    spread(modal_sex, rel_sex) %>%
    mutate(Sex = Sex) %>%
    rename(`Sex/Modal sex` = Sex) %>%
    as.data.frame %>%
    # split into several data.frames?
    by(apply(.[by], 1, paste, collapse = ", "), function(x) {rownames(x) <- NULL; x}) %>%
    unclass %>%
    (function(x) {
      attr(x, "call") <- NULL
      x
    })
}
