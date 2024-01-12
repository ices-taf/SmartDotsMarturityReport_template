
# Determine modal maturity and CV ##################################################

# For each sample the cv and modal maturity are calculated.
# If one maturity is more frequent than others, that maturity is chosen as modal maturity.
# If no maturity is more frequent, then the average of all ages are chosen or
# if two (or more) ages are equally frequent then the age read by the most
# expericed reader will be chosen as modal age.
# WHich method to use is set in the ma_method variable.
# If the modal age is 0 the CV is set to 0 as well.

add_modal_trad <- function(ad, varmod, ma_method) {
  
  # ages by fish
  out <-
    ad %>%
    select(FishID, SampleID, reader, all_of(varmod)) %>%
    ddply(.(FishID, SampleID, get(all_of(varmod))), summarise, count=length(reader)) %>%
    spread(key = "get(all_of(varmod))", value = count)
  
  out[is.na(out)]=0
  
  datsel <- out %>% select(-c(FishID, SampleID))
  
  # Determine modal maturity stage depending on ma_method
  out$modal_trad <-
    if (ma_method == "Mean") {
      stop ("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(datsel, 1,
            function(x) {
              if (!is.null(Mode_I(x))) {
                Mode_I(x)
              } else {
                trunc(mean(x, na.rm = TRUE) + 0.5)
              }
            })
    }

  
  countcases=vector(length=dim(out)[1])
  for(e in 1:dim(out)[1])
  {
    sel=out[e,]
    df=sel[,-c(1,2,dim(out)[2])]
    max=max(df)
    countcases[e]=length(df[which(df==max & df!=0)])
  }
  
  out$NModes_trad=countcases
  
  # calculate CU (coefficient of unalikeability)
  out$cu <- apply(datsel, 1, cu_I)
  
  colnames(out)=c(names(out)[-c((length(names(out))-2):length(names(out)))], paste0("modal_trad_", varmod), paste0("NModes_trad_", varmod), paste0("cu_", varmod))
  # merge CV and modal age to data
  right_join(ad, out, by = c("FishID", "SampleID"))
}



add_modal_linearweight <- function(ad, varmod, ma_method) {
  
  # ages by fish
  out <-
    ad %>%
    select(FishID, SampleID, weight_I, all_of(varmod)) %>%
    ddply(.(FishID, SampleID, get(all_of(varmod))), summarise, readerweight=sum(weight_I)) %>%
    spread(key = "get(all_of(varmod))", value = readerweight)
  
  out[is.na(out)]=0
  
  datsel <- out %>% select(-c(FishID, SampleID))
  
  # Determine modal maturity stage depending on ma_method
  out$modal_linearweight <-
    if (ma_method == "Mean") {
      stop ("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(datsel, 1,
            function(x) {
              if (!is.null(Mode_I(x))) {
                Mode_I(x)
              } else {
                trunc(mean(x, na.rm = TRUE) + 0.5)
              }
            })
    }
  
  
  countcases=vector(length=dim(out)[1])
  for(e in 1:dim(out)[1])
  {
    sel=out[e,]
    df=sel[,-c(1, 2, dim(out)[2])]
    max=max(df)
    countcases[e]=length(df[which(df==max & df!=0)])
  }
  
  out$NModes_linear=countcases
  
  colnames(out)=c(names(out)[-c((length(names(out))-1):length(names(out)))], paste0("modal_linearweight_", varmod), paste0("NModes_linearweight_", varmod))
  # merge CV and modal age to data
  right_join(ad, out, by = c("FishID", "SampleID"))
}



add_modal_negexpweight <- function(ad, varmod, ma_method) {
  
  # ages by fish
  out <-
    ad %>%
    select(FishID, SampleID, weight_II, all_of(varmod)) %>%
    ddply(.(FishID, SampleID, get(all_of(varmod))), summarise, readerweight=sum(weight_II)) %>%
    spread(key = "get(all_of(varmod))", value = readerweight)
  
  out[is.na(out)]=0
  
  datsel <- out %>% select(-c(FishID, SampleID))
  
  # Determine modal maturity stage depending on ma_method
  out$modal_negexpweight <-
    if (ma_method == "Mean") {
      stop ("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(datsel, 1,
            function(x) {
              if (!is.null(Mode_I(x))) {
                Mode_I(x)
              } else {
                trunc(mean(x, na.rm = TRUE) + 0.5)
              }
            })
    }
  
  
  countcases=vector(length=dim(out)[1])
  for(e in 1:dim(out)[1])
  {
    sel=out[e,]
    df=sel[,-c(1,2,dim(out)[2])]
    max=max(df)
    countcases[e]=length(df[which(df==max & df!=0)])
  }
  
  out$NModes_negexp=countcases
  
  colnames(out)=c(names(out)[-c((length(names(out))-1):length(names(out)))], paste0("modal_negexpweight_", varmod), paste0("NModes_negexpweight_", varmod))
  # merge CV and modal age to data
  right_join(ad, out, by = c("FishID", "SampleID"))
}




select_mode=function(ad, ma_method, mode_definition){

  dat1=ad[ad$TypeAnnotation=="eventOrganizer"& ad$DoesSampleHaveHistologyImage=="Yes",] %>%
    mutate(modal_maturity=Maturity, modal_sex=Sex) %>%
    select(FishID, SampleID, modal_maturity, modal_sex) %>%
    distinct()
  
  #selad=ad[ad$SampleID %in% setdiff(ad$SampleID, dat1$SampleID), ]
  selad=ad[ad$FishID %in% setdiff(ad$FishID, dat1$FishID), ]

  if(mode_definition=="multistage")
    {
  dat2=selad[selad$TypeAnnotation=="reader",] %>%
      select(FishID, SampleID, modal_trad_Maturity, NModes_trad_Maturity, modal_linearweight_Maturity, NModes_linearweight_Maturity, modal_negexpweight_Maturity, NModes_negexpweight_Maturity) %>%
      distinct()
  dat2$modal_maturity <-
      if (ma_method == "Mean") {
        stop ("mean not implemented yet")
      } else if (ma_method == "Mode") {
        apply(dat2, 1,
              function(x) {
                if (!is.null(Mode_I(x))) {
                  ifelse(x[4]==1, x[3], ifelse(x[6]==1, x[5], ifelse(x[8]==1, x[7],"Multimode")))
                } else {
                  trunc(mean(x, na.rm = TRUE) + 0.5)
                }
              })
      }
  dat2=dat2 %>%
    select(FishID, SampleID, modal_maturity)
  
  dat3=selad[selad$TypeAnnotation=="reader",] %>%
    select(FishID, SampleID, modal_trad_Sex, NModes_trad_Sex, modal_linearweight_Sex, NModes_linearweight_Sex, modal_negexpweight_Sex, NModes_negexpweight_Sex) %>%
    distinct()
  dat3$modal_sex <-
    if (ma_method == "Mean") {
      stop ("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(dat3, 1,
            function(x) {
              if (!is.null(Mode_I(x))) {
                ifelse(x[4]==1, x[3], ifelse(x[6]==1, x[5], ifelse(x[8]==1, x[7],"Multimode")))
              } else {
                trunc(mean(x, na.rm = TRUE) + 0.5)
              }
            })
    }
  dat3=dat3 %>%
  select(FishID, SampleID, modal_sex)

  dat4=merge(dat2, dat3, by.x=c("FishID", "SampleID"), by.y=c("FishID", "SampleID"))
  
  } else {
    
    dat2=selad[selad$TypeAnnotation=="reader",] %>%
      select(FishID, SampleID, modal_trad_Maturity, NModes_trad_Maturity) %>%
      distinct()
    dat2$modal_maturity <-
      if (ma_method == "Mean") {
        stop ("mean not implemented yet")
      } else if (ma_method == "Mode") {
        apply(dat2, 1,
              function(x) {
                if (!is.null(Mode_I(x))) {
                  x[3]
                } else {
                  trunc(mean(x, na.rm = TRUE) + 0.5)
                }
              })
      }
    dat2=dat2 %>%
      select(FishID, SampleID, modal_maturity)
    
    
    
    dat3=selad[selad$TypeAnnotation=="reader",] %>%
      select(FishID, SampleID, modal_trad_Sex, NModes_trad_Sex) %>%
      distinct()
    dat3$modal_sex <-
      if (ma_method == "Mean") {
        stop ("mean not implemented yet")
      } else if (ma_method == "Mode") {
        apply(dat3, 1,
              function(x) {
                if (!is.null(Mode_I(x))) {
                  x[3]
                } else {
                  trunc(mean(x, na.rm = TRUE) + 0.5)
                }
              })
      }
    dat3=dat3 %>%
      select(FishID, SampleID, modal_sex)
    
    dat4=merge(dat2, dat3, by.x=c("FishID", "SampleID"), by.y=c("FishID", "SampleID"))
  }

  dat=rbind(dat1, dat4)
  
  ad=distinct(right_join(ad, dat, by = c("FishID", "SampleID")))
  
  # ad=distinct(merge(ad, dat, by.x="FishID", by.y="FishID"))

  return(ad)

}
