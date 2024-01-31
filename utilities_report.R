

# Style output tables #########################################################

# These four functions are used to change the style of the output tables.
# Depending on the form of the table different styels are chiosen.

# Style 0
style_table0 <- function(tab) {

  # Capitalize first letter of column and make header boldface
  names(tab) <- pandoc.strong.return(names(tab))

  return(tab)
}

# Style 1
style_table1 <- function(tab) {

  # Capitalize first letter of column, make header, last column and second
  # last row in boldface and make last row italic
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(ncol(tab))
  emphasize.strong.rows(nrow(tab))

  return(tab)
}

# Style 2
style_table2 <- function(tab) {

  # Capitalize first letter of column, make header, last column and
  # last row in boldface
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(ncol(tab))
  emphasize.strong.rows(nrow(tab))

  return(tab)
}

# Style 3
style_table3 <- function(tab) {

  # Capitalize first letter of column, make header and first column boldface
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(1)

  return(tab)
}




# Here the maturity bias are plotted for all readers combined.

plot_general_freq_matur <- function(ad_long, strata=NULL){

  # There are conflicts between plyr and ddply, and, it seems that it is necesary unload plyr and load it again for the function ddply and other working properly
  unloadNamespace("plyr")
  library("plyr")

    freq_tab <-
    ad_long %>%
    ddply(.(modal_maturity, Maturity), summarize, count=length(Maturity))

    total_tab <-
      ad_long %>%
      ddply(.(modal_maturity), summarize, count=length(Maturity))
    
    freq_tab=merge(freq_tab, total_tab, by.x="modal_maturity", by.y="modal_maturity")
    freq_tab$count=round(freq_tab$count.x/freq_tab$count.y, digits=3)
    freq_tab=freq_tab[,-c(3,4)]
    
    diffmodal=setdiff(sort(unique(ad_long$Maturity)), sort(unique(freq_tab$modal_maturity)))
    mat=sort(unique(ad_long$Maturity))
    completedat=as.data.frame(expand.grid(diffmodal,mat))
    completedat$count=rep(0,dim(completedat)[1])
    colnames(completedat)=c("Maturity", "modal_maturity", "count")
    
    freq_tab=rbind(freq_tab, completedat)
    freq_tab=as.data.frame(ddply(freq_tab,.(modal_maturity, Maturity), summarize, count=if(sum(count)==0){NA} else {sum(count, na.rm=TRUE)}))
    
    ggplot(freq_tab, aes(x = modal_maturity, y = Maturity, size = count, label = count)) +
      geom_point(shape = 21, fill = "white") +
      scale_size(range = c(2, 15), guide = "legend") +
      geom_text(aes(label=as.character(count),hjust=0.5,vjust=-1), size=3) +
      geom_abline(mapping = NULL, data = freq_tab, slope=1, 0, na.rm = TRUE, show.legend = NA) +
      ggtitle("Relative frequency plot") +
      xlab("Modal maturity") + ylab("Maturity stage") +
      theme(
        plot.title = element_text(color="black", size=20, face="bold", hjust=0.5),
        axis.title.x = element_text(color="black", size=15, face="bold"),
        axis.title.y = element_text(color="black", size=15, face="bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
      ) +
      theme(legend.position = "none") +
      theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"))
    
    }




# Here the sex categorization bias are plotted for all readers combined.

plot_general_freq_sex <- function(ad_long, strata=NULL){
  
  # There are conflicts between plyr and ddply, and, it seems that it is necesary unload plyr and load it again for the function ddply and other working properly
  unloadNamespace("plyr")
  library("plyr")
  
  freq_tab <-
    ad_long %>%
    ddply(.(modal_sex, Sex), summarize, count=length(Sex))
  
  total_tab <-
    ad_long %>%
    ddply(.(modal_sex), summarize, count=length(Sex))
  
  freq_tab=merge(freq_tab, total_tab, by.x="modal_sex", by.y="modal_sex")
  freq_tab$count=round(freq_tab$count.x/freq_tab$count.y, digits=3)
  freq_tab=freq_tab[,-c(3,4)]
  
  diffmodal=setdiff(sort(unique(ad_long$Sex)), sort(unique(freq_tab$modal_sex)))
  sex=sort(unique(ad_long$Sex))
  completedat=as.data.frame(expand.grid(diffmodal,sex))
  completedat$count=rep(0,dim(completedat)[1])
  colnames(completedat)=c("modal_sex", "Sex", "count")

  freq_tab=rbind(freq_tab, completedat)
  freq_tab=as.data.frame(ddply(freq_tab,.(modal_sex, Sex), summarize, count=if(sum(count)==0){NA} else {sum(count, na.rm=TRUE)}))
  
  ggplot(freq_tab, aes(x = modal_sex, y = Sex, size = count, label = count)) +
    geom_point(shape = 21, fill = "white") +
    scale_size(range = c(2, 25), guide = "legend") +
    geom_text(aes(label=as.character(count),hjust=0.5,vjust=-1), size=3) +
    geom_abline(mapping = NULL, data = freq_tab, slope=1, 0, na.rm = TRUE, show.legend = NA) +
    ggtitle("Relative frequency plot") +
    xlab("Modal sex") + ylab("Sex category") +
    theme(
      plot.title = element_text(color="black", size=20, face="bold", hjust=0.5),
      axis.title.x = element_text(color="black", size=15, face="bold"),
      axis.title.y = element_text(color="black", size=15, face="bold"),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15)
    ) +
    theme(legend.position = "none") +
    theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"))
  
  
}



# Sex and maturity bias plots  #############################################################


# The maturity bias plots are made per reader.
# For each reader is the mean age and 2 times standard deviation
# (as error bars) plotted against the modal age.

plot_bias_matur <- function(ad_long) {
  
  # There are conflicts between plyr and ddply, and, it seems that it is necesary unload plyr and load it again for the function ddply and other working properly
  unloadNamespace("plyr")
  library("plyr")
  
  ad_long$reader=as.character(ad_long$reader)
  # return list of plots
  lapply(sort(unique(ad_long$reader)), function(ireader) {
    # Plot data and make settings/design
    freq_tab <-
      ad_long %>%
      filter(reader == ireader) %>%
      ddply(.(modal_maturity, Maturity, reader), summarize, count=length(Maturity))
    
    diffmodal=setdiff(sort(unique(ad_long$Maturity)), sort(unique(freq_tab$modal_maturity)))
    mat=sort(unique(ad_long$Maturity))
    completedat=as.data.frame(expand.grid(diffmodal, mat))
    completedat$reader=rep(unique(freq_tab$reader),dim(completedat)[1])
    completedat$count=rep(NA,dim(completedat)[1])
    colnames(completedat)=c( "modal_maturity", "Maturity", "reader", "count")
    
    freq_tab=rbind(freq_tab, completedat)
    freq_tab=as.data.frame(ddply(freq_tab,.(modal_maturity, Maturity, reader), summarize, count=sum(count)))
    
    total_tab <-
      ad_long %>%
      filter(reader == ireader) %>%
      ddply(.(modal_maturity), summarize, count=length(Maturity))
    
    freq_tab=merge(freq_tab, total_tab, by.x="modal_maturity", by.y="modal_maturity", all.x=TRUE)
    freq_tab$count=round(freq_tab$count.x/freq_tab$count.y, digits=3)
    freq_tab=freq_tab[,-c(4,5)]
    
    ggplot(freq_tab, aes(x = modal_maturity, y = Maturity, size = count, label = count, group = reader)) +
      geom_point(shape = 21, fill = "white") +
      geom_text(aes(label=as.character(count),hjust=0.5,vjust=-1), size=3) +
      scale_size(range = c(2, 15), guide = "legend") +
      geom_abline(mapping = NULL, data = freq_tab, slope=1, 0, na.rm = TRUE, show.legend = NA) +
      facet_wrap(~ reader, ncol = 2) +
      theme(strip.text.x=element_text(size=25, hjust=0.5, vjust=0.5)) +
      xlab("Modal maturity") + ylab("Maturity stage") +
      theme(
        plot.title = element_text(color="black", size=20, face="bold", hjust=0.5),
        axis.title.x = element_text(color="black", size=15, face="bold"),
        axis.title.y = element_text(color="black", size=15, face="bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
      ) +
      theme(legend.position = "none") +
      theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"))
    
   })
}


plot_bias_sex <- function(ad_long) {
  
  # There are conflicts between plyr and ddply, and, it seems that it is necesary unload plyr and load it again for the function ddply and other working properly
  unloadNamespace("plyr")
  library("plyr")
  
  ad_long$reader=as.character(ad_long$reader)
  # return list of plots
  lapply(sort(unique(ad_long$reader)), function(ireader) {
    # Plot data and make settings/design
    freq_tab <-
      ad_long %>%
      filter(reader == ireader) %>%
      ddply(.(modal_sex, Sex, reader), summarize, count=length(Sex))
    
    diffmodal=setdiff(sort(unique(ad_long$Sex)), sort(unique(freq_tab$modal_sex)))
    mat=sort(unique(ad_long$Sex))
    completedat=as.data.frame(expand.grid(diffmodal, mat))
    completedat$reader=rep(unique(freq_tab$reader),dim(completedat)[1])
    completedat$count=rep(NA,dim(completedat)[1])
    colnames(completedat)=c( "modal_sex", "Sex", "reader", "count")
    
    freq_tab=rbind(freq_tab, completedat)
    freq_tab=as.data.frame(ddply(freq_tab,.(modal_sex, Sex, reader), summarize, count=sum(count)))

    total_tab <-
      ad_long %>%
      filter(reader == ireader) %>%
      ddply(.(modal_sex), summarize, count=length(Sex))
    
    freq_tab=merge(freq_tab, total_tab, by.x="modal_sex", by.y="modal_sex", all.x=TRUE)
    freq_tab$count=round(freq_tab$count.x/freq_tab$count.y, digits=3)
    freq_tab=freq_tab[,-c(4,5)]

    ggplot(freq_tab, aes(x = modal_sex, y = Sex, size = count, label = count, group = reader)) +
      geom_point(shape = 21, fill = "white") +
      geom_text(aes(label=as.character(count),hjust=0.5,vjust=-1), size=3) +
      scale_size(range = c(2, 15), guide = "legend") +
      geom_abline(mapping = NULL, data = freq_tab, slope=1, 0, na.rm = TRUE, show.legend = NA) +
      facet_wrap(~ reader, ncol = 2) +
      theme(strip.text.x=element_text(size=25, hjust=0.5, vjust=0.5)) +
      xlab("Modal sex") + ylab("Sex stage") +
      theme(
        plot.title = element_text(color="black", size=20, face="bold", hjust=0.5),
        axis.title.x = element_text(color="black", size=15, face="bold"),
        axis.title.y = element_text(color="black", size=15, face="bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)
      ) +
      theme(legend.position = "none") +
      theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"))
    
  })
}



# Plot std, ca and pa #########################################################

# Plot overall std, CA and PA per modal maturity stage in same plot
plot_stat_matur <- function(ad_long) {

  
  # Again, beacuse of conflicts between plyr and dplyr it is necesary to unload the library plyr
  unloadNamespace("plyr")
  
  # Combine the three data sets
  dat_in <-
    ad_long %>%
      group_by(modal_maturity) %>%
      summarise(
        cu = cu_II(Maturity),
        pa = mean(Maturity == modal_maturity, na.rm = TRUE) * 100)

  # Limit to use for axis
  cu_lim <- ceiling(max(dat_in$cu, na.rm = T))

  p <-
    dat_in %>%
    filter(!is.na(cu)) %>%
    ggplot() +
    theme_bw() +
    # Coefficient of unalikeability
    geom_line(aes(x = modal_maturity, y = cu, colour = "cu")) +
    geom_point(aes(x = modal_maturity, y = cu, colour = "cu",
                   shape = "cu"), size = 3) +
    # PA
    geom_line(aes(x = modal_maturity, y = pa*cu_lim/100, colour = "pa")) +
    geom_point(aes(x = modal_maturity, y = pa*cu_lim/100, colour = "pa",
                   shape = "pa"), size = 3) +
    # Make left side y-axis
    scale_y_continuous(name = expression("Coefficient of unalikability CU (years)"),
                       limits = c(0, cu_lim))  +
    # Make right side y-axis
    scale_y_continuous(name = expression("Coefficient of unalikability (years)"),
                       sec.axis = sec_axis(~ . * 100/cu_lim,
                                           name = "Percentage of Agreement PA (%)"),
                       limits = c(0, cu_lim))
   print(p)
 
  # # Colors and labels
  # p +
  #   theme(axis.text.y = element_text(color = "#80B1D3"),
  #         axis.text.y.right = element_text(color = "#FB8072")
  #         #text = element_text(family="Calibri")
  #         ) +
  #   scale_colour_manual(name = "Measure",
  #                       values = c("#80B1D3", "#FB8072"),
  #                       labels = c("CU", "PA")) +
  #   scale_shape_manual(name = "Measure", values = c(16, 8, 17),
  #                      labels = c("CU", "PA")) +
  #   labs(x = "Modal maturity", colour = "")
  # 
  # print(p)
  # Now, I load again plyr and dplyr in this order:
  library(plyr); library(dplyr)
  
}






plot_stat_sex <- function(ad_long) {
  
  
  # Again, beacuse of conflicts between plyr and dplyr it is necesary to unload the library plyr
  unloadNamespace("plyr")
  
  # Combine the three data sets
  dat_in <-
    ad_long %>%
    group_by(modal_sex) %>%
    summarise(
      cu = cu_II(Sex),
      pa = mean(Sex == modal_sex, na.rm = TRUE) * 100)
  
  # Limit to use for axis
  cu_lim <- ceiling(max(dat_in$cu, na.rm = T))
  
  p <-
    dat_in %>%
    filter(!is.na(cu)) %>%
    ggplot() +
    theme_bw() +
    # Coefficient of unalikeability
    geom_line(aes(x = modal_sex, y = cu, colour = "cu")) +
    geom_point(aes(x = modal_sex, y = cu, colour = "cu",
                   shape = "cu"), size = 3) +
    # PA
    geom_line(aes(x = modal_sex, y = pa*cu_lim/100, colour = "pa")) +
    geom_point(aes(x = modal_sex, y = pa*cu_lim/100, colour = "pa",
                   shape = "pa"), size = 3) +
    # Make left side y-axis
    scale_y_continuous(name = expression("Coefficient of unalikability CU (years)"),
                       limits = c(0, cu_lim))  +
    # Make right side y-axis
    scale_y_continuous(name = expression("Coefficient of unalikability (years)"),
                       sec.axis = sec_axis(~ . * 100/cu_lim,
                                           name = "Percentage of Agreement PA (%)"),
                       limits = c(0, cu_lim))
  print(p)
  
  # # Colors and labels
  # p +
  #   theme(axis.text.y = element_text(color = "#80B1D3"),
  #         axis.text.y.right = element_text(color = "#FB8072")
  #         #text = element_text(family="Calibri")
  #   ) +
  #   scale_colour_manual(name = "Measure",
  #                       values = c("#80B1D3", "#FB8072"),
  #                       labels = c("CU", "PA")) +
  #   scale_shape_manual(name = "Measure", values = c(16, 8, 17),
  #                      labels = c("CU", "PA")) +
  #   labs(x = "Modal sex", colour = "")
  # 
  # print(p)
  # Now, I load again plyr and dplyr in this order:
  library(plyr); library(dplyr)
  
}

