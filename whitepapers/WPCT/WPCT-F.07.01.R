# HEADER
#' PhUSE Figure 7.1 Box plot - Measurements by Analysis Timepoint, Visit and Treatment
#  White paper: PhUSE Central Tendency
#  Specs:       https://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/specification/
#  Output:      https://github.com/phuse-org/phuse-scripts/blob/master/whitepapers/WPCT/outputs_r/
#  Contributors: Jeno Pizarro, Suzie Perigaud, Alan Hopkins
#
#
#  TESTING and QUALIFICATION:
#  12-JUL-2015, Jeno  - Initial scripting
#  30-AUG-2015, Suzie - add out of limit red dots and horizontal lines
#  09-NOV-2015, Jeno  - edited to use ADaM only
#  10-JAN-2016, Jeno  - allow user to select treatment arm variable, population flag
#                     - turn on or off horizontal lines, red outliers, select TIFF, JPEG, or PNG and 
#                       choose file size
#                     - rename arms (if they are too long), automatically read in CSV or XPT
#                     - logic to split visits across pages automatically .
#  24-May-2017, Alan  - Created an R package using the code developed above
#
#' @data  Data frame
#' @param treatmentname Which treatment arm variable? e.g."TRTA" #TRTA, TRTP, etc
#' @param useshortnames  #Rename Treatment Arms? (if you wish to display shorter names).TRUE OR FALSE
#' @param oldnames Treatment Arms old names .e.g "Xanomeline Low Dose","Xanomeline High Dose"
#' @param newnames Treatment Arms new names e.g., "X-low", "X-High"
#' @param usepopflag Subset on a population flag. TRUE OR FALSE
#' @param popflag Value "SAFFL"
#' @param testname Test or parameter to be analyzed e.g."DIABP"
#' @param yaxislabel Labels for y axis
#' @param selectedvisits Visit numbers to be analyzed e.g 0,2,4,6,8,12,16,20,24
#' @param perpage Number of visits to display per page
#' @param dignum Number of digits in table, standard deviation = dignum +1
#' @param redoutliers True or False. Configure Upper and Lower Limits to highlight outliers in red and display horizontal lines
#'     Highlight outliers above/below limits?
#' @param horizontallines True or False.
#' @param enterlimits True or False. Input lower / upper limits or use ANRLO, ANRHI values in data.
#'    Input lower / upper limits or use ANRLO, ANRHI values in data.
#'    If using values in data, they should be uniform if displaying horizontal lines.
#'    TRUE = enter own limits, FALSE = use ANRLO/ANRHI in data
#' @param ANRLO Lower limit(s)
#' @param ANRHI Upper limit(s)
#' @param inputdirectory Set input file directory
#' @param outputdirectory Set output file directory
#' @param testfilename Accepts CSV or XPT files
#' @param filetype Output file type - TIFF or JPEG or PNG
#' @param pixelwidth Choose output file size: pixel width
#' @param pixelheight Choose output file size: pixel height
#' @param outputfontsize Choose output font size
#' @param charttitle Title for the chart
#' @return PhUSE Figure 7.1 Box plot - Measurements by Analysis Timepoint, Visit and Treatment
#' @import Hmisc
#' @import ggplot2
#' @import tools
#' @import gridExtra
#' @import data.table
#' @export
boxplotfunc<-function(data, treatmentname, useshortnames = c(TRUE,FALSE), oldnames, newnames,
                      usepopflag = c(TRUE,FALSE), popflag, testname, yaxislabel, selectedvisits,
                      perpage, dignum, redoutliers = c(TRUE, FALSE),
                      horizontallines = c(TRUE,FALSE), enterlimits= c(TRUE,FALSE), ANRLO, ANRHI,
                      inputdirectory, outputdirectory, testfilename, filetype = c("PNG","TIFF","JPEG"),
                      pixelwidth, pixelheight, outputfontsize, charttitle){
  
  # buildtable function to be called later, summarize data to enable creation of accompanying datatable
  buildtable <- function(avalue, dfname, by1, by2, dignum){
                         byvarslist <- c(by1,by2)
                         summary <- eval(dfname)[,list(n = .N,
                         mean = round(mean(eval(avalue), na.rm = TRUE), digits=dignum),
                         sd = round(sd(eval(avalue), na.rm = TRUE), digits=dignum+1),
                         min = round(min(eval(avalue), na.rm = TRUE), digits=dignum),
                         q1 = round(quantile(eval(avalue), .25, na.rm = TRUE), digits=dignum),
                         mediam = round(median(eval(avalue), na.rm = TRUE), digits=dignum),
                         q3 = round(quantile(eval(avalue), .75, na.rm = TRUE), digits = dignum),
                         max = round(max(eval(avalue), na.rm = TRUE), digits = dignum)
                       ),
               by = byvarslist]
    return(summary)
  }
  
  #SELECT VARIABLES (examples in parenthesis): TREATMENT (TRTP, TRTA), PARAMCD (LBTESTCD)
  #colnames(testresults)[names(testresults) == "OLD VARIABLE"] <- "NEW VARIABLE"
  colnames(testresultsread)[names(testresultsread) == treatmentname] <- "TREATMENT"
  colnames(testresultsread)[names(testresultsread) == popflag] <- "FLAG" #select population flag to subset on such as SAFFL or ITTFL
  
  if (useshortnames == TRUE){
    for(i in 1:length(oldnames)) {
      testresultsread$TREATMENT <- ifelse(testresultsread$TREATMENT == oldnames[i], as.character(newnames[i]), as.character(testresultsread$TREATMENT))
    }
  }
  
  #determine number of pages needed
  initial <- 1
  visitsplits <- ceiling((length(selectedvisits)/perpage))
  #for each needed page, subset selected visits by number per page
  for(i in 1:visitsplits) {
    #subset on test, visits, population to be analyzed
    if (usepopflag == TRUE){
      testresults <- subset(testresultsread, PARAMCD == testname & AVISITN %in% selectedvisits[(initial):
                           (ifelse(perpage*i>length(selectedvisits),length(selectedvisits),perpage*i))]
                            & FLAG == "Y")
    } else {
      testresults <- subset(testresultsread, PARAMCD == testname & AVISITN %in% selectedvisits[(initial):(perpage*i)])
    }
    initial <- initial + perpage
    testresults<- data.table(testresults)
    
    #setkey for speed gains when summarizing
    setkey(testresults, USUBJID, TREATMENT, AVISITN)
    
    #create a variable for the out of limits data
    if (enterlimits == TRUE){
      testresults$OUT <- ifelse(testresults$AVAL < ANRLO | testresults$AVAL > ANRHI, testresults$AVAL, NA)
    } else if (enterlimits == FALSE){
      testresults$OUT <- ifelse(testresults$AVAL < testresults$ANRLO | testresults$AVAL > testresults$ANRHI, testresults$AVAL, NA)
    } else {print("WARNING - Manual entry of limits or automatic usage of limits in data not defined")}
    #specify plot
    p <- ggplot(testresults, aes(factor(AVISITN), fill = TREATMENT, AVAL))
    # add notch, axis labels, legend, text size
    p1 <- p + geom_boxplot(notch = TRUE) + xlab("Visit Number") + ylab(yaxislabel) + 
                  theme(legend.position="bottom", legend.title=element_blank(), 
                  text = element_text(size = outputfontsize), 
                  axis.text.x  = element_text(size=outputfontsize), 
                  axis.text.y = element_text(size=outputfontsize)) +
                  ggtitle(charttitle)
    # add mean points
    p2 <- p1 + stat_summary(fun.y=mean, colour="dark red", geom="point", position=position_dodge(width=0.75))
    # out of limits jittered red points
    p3 <- p2 + geom_jitter(data = testresults, aes(factor(AVISITN), testresults$OUT), colour = "dark red", 
                           position = position_dodge(width=0.75))
    # horizontal limit lines
    if(enterlimits == TRUE){
      p4 <- p2 + geom_hline(yintercept = c(ANRLO,ANRHI), colour = "red")
      pall <- p3 + geom_hline(yintercept = c(ANRLO,ANRHI), colour = "red")
    } else if (enterlimits == FALSE) {
      p4 <- p2 + geom_hline(yintercept = c(testresults$ANRLO,testresults$ANRHI), colour = "red")
      pall <- p3 + geom_hline(yintercept = c(testresults$ANRLO,testresults$ANRHI), colour = "red")
    }
    #call summary table function
    summary <- buildtable(avalue = quote(AVAL), dfname= quote(testresults), by1 = "AVISITN", by2 = "TREATMENT", dignum)[order(AVISITN, TREATMENT)]
    table_summary <- data.frame(t(summary))
    
    t1theme <- ttheme_default(core = list(fg_params = list (fontsize = outputfontsize)))
    t1 <- tableGrob(table_summary, theme = t1theme, cols = NULL)
    
    if (filetype == "TIFF"){
      #Output to TIFF
      tiff(file.path(outputdirectory,paste("plot",i,".TIFF",sep = "" )), width = pixelwidth, 
           height = pixelheight, units = "px", pointsize = 12)
      if (redoutliers == TRUE & horizontallines == TRUE) {
        grid.arrange(pall, t1, ncol = 1)
      } else if (redoutliers == TRUE & horizontallines == FALSE) {
        grid.arrange(p3, t1, ncol = 1)
      } else if (redoutliers == FALSE & horizontallines == TRUE) {
        grid.arrange(p4, t1, ncol = 1)
      } else {
        grid.arrange(p2, t1, ncol = 1)
      }
      dev.off()
    }
    if (filetype == "JPEG") {
      # Optionally, use JPEG
      jpeg(file.path(outputdirectory,paste("plot",i,".JPEG",sep = "" )), width = pixelwidth, 
           height = pixelheight, units = "px", pointsize = 12)
      if (redoutliers == TRUE & horizontallines == TRUE) {
        grid.arrange(pall, t1, ncol = 1)
      } else if (redoutliers == TRUE & horizontallines == FALSE) {
        grid.arrange(p3, t1, ncol = 1)
      } else if (redoutliers == FALSE & horizontallines == TRUE) {
        grid.arrange(p4, t1, ncol = 1)
      } else {
        grid.arrange(p2, t1, ncol = 1)
      }
      dev.off()
    }
    if (filetype == "PNG") {
      # Optionally, use PNG
      png(file.path(outputdirectory,paste("plot",i,".PNG",sep = "" )), width = pixelwidth, 
          height = pixelheight, units = "px", pointsize = 12)
      if (redoutliers == TRUE & horizontallines == TRUE) {
        grid.arrange(pall, t1, ncol = 1)
      } else if (redoutliers == TRUE & horizontallines == FALSE) {
        grid.arrange(p3, t1, ncol = 1)
      } else if (redoutliers == FALSE & horizontallines == TRUE) {
        grid.arrange(p4, t1, ncol = 1)
      } else {
        grid.arrange(p2, t1, ncol = 1)
      }
      dev.off()
    }
  }
}
