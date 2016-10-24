#Probability Plots for each country Black and White Friendly
require(readstata13); require(ggplot2); require(plotROC); require(tidyr)
require(lubridate); require(grid); require(gridExtra); require(scales)
require(gtable)
#"grid" serves -unit.pmax-, "gridExtra" serves -grid.arrange-, "scales" serives -date_breaks-
#"gtable" used to plot second axis

all_files <- list.files()[grep("With.*dta$",list.files())]
graph_title_additions <- " - Equity Crisis"
crisisName <- "crisisST"   #There is one instance below that uses MIPRCrisis as string
desti_path_s <- "RGBProbPlots/"

models_list <- paste0("L", 4:8, "_")

controldf_s = read.dta13("equityNoSenti.dta")

source("ggplotDualAxis functions.R")
#The following function emulates ggplot2's default color scheme
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
color_schem1 = c(gg_color_hue(2),"grey50")
shape_schem = c(8,1,16) #asterisk, circle, filled circle
line_schem = c(1,2)
#*************************************************************

for (curr_file in all_files) {
  dta = read.dta13(curr_file)
  print(paste("Working on", curr_file))
  dta = dta[, -grep("^ccode", colnames(dta))]
  dta$date = NA
  for (i in seq(nrow(dta))) {
    if (substr(dta$quarter2[i],5,6)=="q1") dta$date[i] = paste0(substr(dta$quarter2[i],1,4),"-03-31")
    if (substr(dta$quarter2[i],5,6)=="q2") dta$date[i] = paste0(substr(dta$quarter2[i],1,4),"-06-30")
    if (substr(dta$quarter2[i],5,6)=="q3") dta$date[i] = paste0(substr(dta$quarter2[i],1,4),"-09-30")
    if (substr(dta$quarter2[i],5,6)=="q4") dta$date[i] = paste0(substr(dta$quarter2[i],1,4),"-12-31")
  }
  dta$date2=dta$date; dta$date=as.Date(ymd(dta$date)-days(90)) ; dta$date2=as.Date(ymd(dta$date2))
  
  countries_list = unique(dta$country)
  desti_path = desti_path_s
  
  for (cntry in countries_list) {
    print(paste0("Printing ", cntry, "..."))
    crisisBands = data.frame(end=subset(subset(dta, country==cntry), crisisST==1)$date2)
    crisisBands$end = as.Date(crisisBands$end)
    if (nrow(crisisBands)==0) {
      crisisBands = data.frame(end=as.Date("1995-01-31"), start=as.Date("1995-01-30"))
    } else {
      crisisBands$start = crisisBands$end - 91
    }
    
    for (count in 4:5) {

      if (count==4) {
        model = grep("L4_dlrgdp", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,model[1]])
        colnames(controldf) = paste0(model[1], "_ctrl")
      } else {
        model = grep("L[5-8]_dlrgdp", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,gsub("[5-8]","8",model[1])])
        colnames(controldf) = paste0(model[1], "_ctrl")
      }
      sentiDF = subset(dta, country==cntry)[,grep("^date$|^dlloansr$|^dlrgdp$", names(dta))]
      tempColNums = grep("^dlloansr$|^dlrgdp$", names(sentiDF))
      sentiDF[,tempColNums] = lapply(sentiDF[,tempColNums], function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T))
      sentiDF = gather(sentiDF, senti, value, -date)
      sentiDF$senti = factor(sentiDF$senti, levels=c(grep("^dlloansr$",names(dta),value=T),grep("^dlrgdp$",names(dta),value=T)))
      
      plotdata = gather(data.frame(subset(dta,country==cntry)[,c("date",model)], controldf), Key, Value, -date)
      plotdata$Key = factor(plotdata$Key, levels = c(grep("xbar|zero",model,invert=T,value=T),grep("xbar",model,value=T),names(controldf)[1]))
      gplot1 = ggplot(data=plotdata) +
        geom_jitter(aes(x=date, y=Value, shape=Key, color=Key), width=0, height=0, size=2) +
        coord_cartesian(ylim=c(0,1)) + scale_x_date(breaks = date_breaks("2 year")) +
        scale_shape_manual(name="Real GDP\nGrowth",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values = shape_schem) +
        scale_color_manual(name="Real GDP\nGrowth",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values = color_schem1) +
        theme_bw() + labs(list(title = paste(cntry,graph_title_additions), x = "", y="Predicted Probability", colour=model[1])) +
        theme(plot.title = element_text(size = 13, face = "bold", vjust = 1), axis.ticks.x=element_blank(),
              axis.text.x=element_blank(), plot.margin=unit(c(0.25,0.1,-0.5,0.1), "lines"))
      gplot2 = ggplot() + geom_rect(data=crisisBands, mapping=aes(xmin=start,xmax=end,ymin=-Inf,ymax=+Inf), fill="grey40", alpha=0.3) +
        geom_line(data=sentiDF, aes(x=date,y=value,linetype=senti)) + geom_hline(yintercept=0) + ylab("Standardized Values") +
        scale_linetype_manual(name="",labels=list(bquote(Delta~"Loans"),bquote("Real GDP\nGrowth")),values=line_schem) + theme_bw() +
        theme(panel.background = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      if (count==4) {
        model = grep("L4_dlpc", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,model[1]])
        colnames(controldf) = paste0(model[1], "_ctrl")
      } else {
        model = grep("L[5-8]_dlpc", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,gsub("[5-8]","8",model[1])])
        colnames(controldf) = paste0(model[1], "_ctrl")
      }
      sentiDF = subset(dta, country==cntry)[,grep("^date$|^dlpc$|dlloansr$", names(dta))]
      tempColNums = grep("^dlpc$|dlloansr$", names(sentiDF))
      sentiDF[,tempColNums] = lapply(sentiDF[,tempColNums], function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T))
      sentiDF = gather(sentiDF, senti, value, -date)
      sentiDF$senti = factor(sentiDF$senti, levels=c(grep("^dlloansr$",names(dta),value=T),grep("^dlpc$",names(dta),value=T)))
      
      plotdata = gather(data.frame(subset(dta,country==cntry)[,c("date",model)], controldf), Key, Value, -date)
      plotdata$Key = factor(plotdata$Key, levels = c(grep("xbar|zero",model,invert=T,value=T),grep("xbar",model,value=T),names(controldf)[1]))
      hplot1 = ggplot(data=plotdata) +
        geom_jitter(aes(x=date, y=Value, shape=Key, color=Key), width=0, height=0, size=2) +
        coord_cartesian(ylim=c(0,1)) + scale_x_date(breaks = date_breaks("2 year")) +
        scale_shape_manual(name="Inflation",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values=shape_schem) +
        scale_color_manual(name="Inflation",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values = color_schem1) +
        theme_bw() + labs(list(x="",y="Predicted Probability",color=model[1])) +
        theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"))
      hplot2 = ggplot() + geom_rect(data=crisisBands, mapping=aes(xmin=start,xmax=end,ymin=-Inf,ymax=+Inf), fill="grey40", alpha=0.3) +
        geom_line(aes(x=date, y=value, linetype=senti), data=sentiDF) + geom_hline(yintercept=0) + ylab("Standardized Values") +
        scale_linetype_manual(name="",labels=list(bquote(Delta~"Loans"),bquote("Inflation")),values=line_schem) + theme_bw() +
        theme(panel.background = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      if (count==4) {
        model = grep("L4_stir", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,model[1]])
        colnames(controldf) = paste0(model[1], "_ctrl")
      } else {
        model = grep("L[5-8]_stir", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,gsub("[5-8]","8",model[1])])
        colnames(controldf) = paste0(model[1], "_ctrl")
      }
      sentiDF = subset(dta, country==cntry)[,grep("^date$|^stir$|dlloansr$", names(dta))]
      tempColNums = grep("^stir$|dlloansr$", names(sentiDF))
      sentiDF[,tempColNums] = lapply(sentiDF[,tempColNums], function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T))
      sentiDF = gather(sentiDF, senti, value, -date)
      sentiDF$senti = factor(sentiDF$senti, levels=c(grep("^dlloansr$",names(dta),value=T),grep("^stir$",names(dta),value=T)))
      
      plotdata = gather(data.frame(subset(dta,country==cntry)[,c("date",model)], controldf), Key, Value, -date)
      plotdata$Key = factor(plotdata$Key, levels = c(grep("xbar|zero",model,invert=T,value=T),grep("xbar",model,value=T),names(controldf)[1]))
      kplot1 = ggplot(data=plotdata) +
        geom_jitter(aes(x=date, y=Value, shape=Key, color=Key), width=0, height=0, size=2) +
        coord_cartesian(ylim=c(0,1)) + scale_x_date(breaks = date_breaks("2 year")) +
        scale_shape_manual(name="Policy Rate",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values=shape_schem) +
        scale_color_manual(name="Policy Rate",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values = color_schem1) +
        theme_bw() + labs(list(x="",y="Predicted Probability",color=model[1])) +
        theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"))
      kplot2 = ggplot() + geom_rect(data=crisisBands, mapping=aes(xmin=start,xmax=end,ymin=-Inf,ymax=+Inf), fill="grey40", alpha=0.3) +
        geom_line(aes(x=date, y=value, linetype=senti), data=sentiDF) + geom_hline(yintercept=0) + ylab("Standardized Values") +
        scale_linetype_manual(name="",labels=list(bquote(Delta~"Loans"),bquote("Policy Rate")),values=line_schem) + theme_bw() +
        theme(panel.background = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      if (count==4) {
        model = grep("L4_rstir", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,model[1]])
        colnames(controldf) = paste0(model[1], "_ctrl")
      } else {
        model = grep("L[5-8]_rstir", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,gsub("[5-8]","8",model[1])])
        colnames(controldf) = paste0(model[1], "_ctrl")
      }
      sentiDF = subset(dta, country==cntry)[,grep("^date$|^rstir$|dlloansr$", names(dta))]
      tempColNums = grep("^rstir$|dlloansr$", names(sentiDF))
      sentiDF[,tempColNums] = lapply(sentiDF[,tempColNums], function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T))
      sentiDF = gather(sentiDF, senti, value, -date)
      sentiDF$senti = factor(sentiDF$senti, levels=c(grep("^dlloansr$",names(dta),value=T),grep("^rstir$",names(dta),value=T)))
      
      plotdata = gather(data.frame(subset(dta,country==cntry)[,c("date",model)], controldf), Key, Value, -date)
      plotdata$Key = factor(plotdata$Key, levels = c(grep("xbar|zero",model,invert=T,value=T),grep("xbar",model,value=T),names(controldf)[1]))
      iplot1 = ggplot(data=plotdata) +
        geom_jitter(aes(x=date, y=Value, shape=Key, color=Key), width=0, height=0, size=2) +
        coord_cartesian(ylim=c(0,1)) + scale_x_date(breaks = date_breaks("2 year")) +
        scale_shape_manual(name="Real Policy Rate",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values=shape_schem) +
        scale_color_manual(name="Real Policy Rate",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values = color_schem1) +
        theme_bw() + labs(list(x="",y="Predicted Probability",color=model[1])) +
        theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), plot.margin=unit(c(-0.5,0.1,-0.5,0.1), "lines"))
      iplot2 = ggplot() + geom_rect(data=crisisBands, mapping=aes(xmin=start,xmax=end,ymin=-Inf,ymax=+Inf), fill="grey40", alpha=0.3) +
        geom_line(aes(x=date, y=value, linetype=senti), data=sentiDF) + geom_hline(yintercept=0) + ylab("Standardized Values") +
        scale_linetype_manual(name="",labels=list(bquote(Delta~"Loans"),bquote("Real Policy Rate")),values=line_schem) + theme_bw() +
        theme(panel.background = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      if (count==4) {
        model = grep("L4_loansgdp", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,model[1]])
        colnames(controldf) = paste0(model[1], "_ctrl")
      } else {
        model = grep("L[5-8]_loansgdp", colnames(dta), value = T)
        model = grep("zero", model, value=T, invert=T)
        controldf = data.frame(subset(controldf_s, country==cntry)[,gsub("[5-8]","8",model[1])])
        colnames(controldf) = paste0(model[1], "_ctrl")
      }
      sentiDF = subset(dta, country==cntry)[,grep("^date$|^loansgdp$|dlloansr$|^loans1$|^gdp$|^cpi$", names(dta))]
      sentiDF$lloansr = log(sentiDF$loans1*100/sentiDF$cpi); sentiDF$lgdpr = log(sentiDF$gdp*100/sentiDF$cpi)
      tempColNums = grep("^loansgdp$|dlloansr$|^lloansr$|^lgdpr$", names(sentiDF))
      sentiDF[,tempColNums] = lapply(sentiDF[,tempColNums], function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T))
      sentiDF2 = gather(sentiDF[,grep("^gdp$|^cpi$|^loans1$",names(sentiDF),invert=T)], senti, value, -date)
      sentiDF2$senti = factor(sentiDF2$senti, levels=c(grep("^dlloansr$",names(sentiDF),value=T),grep("^loansgdp$",names(sentiDF),value=T),
                                                     grep("^lloansr$",names(sentiDF),value=T),grep("^lgdpr$",names(sentiDF),value=T)))
      sentiDF = sentiDF2
      
      plotdata = gather(data.frame(subset(dta,country==cntry)[,c("date",model)], controldf), Key, Value, -date)
      plotdata$Key = factor(plotdata$Key, levels = c(grep("xbar|zero",model,invert=T,value=T),grep("xbar",model,value=T),names(controldf)[1]))
      jplot1 = ggplot(data=plotdata) +
        geom_jitter(aes(x=date, y=Value, shape=Key, color=Key), width=0, height=0, size=2) +
        coord_cartesian(ylim=c(0,1)) + scale_x_date(breaks = date_breaks("2 year"), labels=date_format("%Y")) +
        scale_shape_manual(name="Loans-to-GDP",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values=shape_schem) +
        scale_color_manual(name="Loans-to-GDP",labels=c("Senti: Raw", "Senti: Mean", "No Senti"),values = color_schem1) +
        theme_bw() + labs(list(y="Predicted Probability", color=model[1]))
        theme(plot.margin=unit(c(-0.5,0.1,0.25,0.1), "lines"), axis.text.x=element_text(angle = 45, hjust=1))
      jplot2 = ggplot() + geom_rect(data=crisisBands, mapping=aes(xmin=start,xmax=end,ymin=-Inf,ymax=+Inf), fill="grey40", alpha=0.3) +
          geom_line(aes(x=date, y=value, linetype=senti), data=sentiDF) + geom_hline(yintercept=0) + ylab("Standardized Values") +
          scale_linetype_manual(name="",labels=list(bquote(Delta~"Loans"),bquote("Loans-to-GDP"),bquote("ln(rLoans)"),bquote("ln(rGDP)"))
                             ,values=1:4) + theme_bw() +
          theme(panel.background = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
      ggrob = (SuperDualAxes(gplot1,gplot2))#,combineLegends=F))
      #ggrob[["grobs"]][[which(ggrob$layout$name=="guide-box")]][["grobs"]] <- NULL
      hgrob = (SuperDualAxes(hplot1,hplot2))#,combineLegends=F))
      #hgrob[["grobs"]][[which(hgrob$layout$name=="guide-box")]][["grobs"]] <- NULL
      kgrob = (SuperDualAxes(kplot1,kplot2))
      igrob = (SuperDualAxes(iplot1,iplot2))#,combineLegends=F))
      #igrob[["grobs"]][[which(igrob$layout$name=="guide-box")]][["grobs"]] <- NULL
      jgrob = (SuperDualAxes(jplot1,jplot2))#,combineLegends=F))
      #jgrob[["grobs"]][[which(jgrob$layout$name=="guide-box")]][["grobs"]] <- NULL
      
      maxWidth = grid::unit.pmax(ggrob$widths[2:5], hgrob$widths[2:5], kgrob$widths[2:5],
                                 igrob$widths[2:5], jgrob$widths[2:5])
      
      ggrob$widths[2:5] = as.list(maxWidth); hgrob$widths[2:5] = as.list(maxWidth)
      kgrob$widths[2:5] = as.list(maxWidth); igrob$widths[2:5] = as.list(maxWidth)
      jgrob$widths[2:5] = as.list(maxWidth)
      
      if (count==4) {
        png(filename = paste0(desti_path,gsub( "[^[:alnum:] ]", "", cntry), "L4", ".png"),width = 1000, height = 1000)
      } else {
        png(filename = paste0(desti_path,gsub( "[^[:alnum:] ]", "", cntry), "L4Plus", ".png"),width = 1000, height = 1000)
      }
      grid.arrange(ggrob, hgrob, kgrob, igrob, jgrob, ncol=1, nrow=5,
                   heights= c(0.22,0.19,0.19,0.19,0.21))
      grid.arrange(ggrob,hgrob,kgrob,jgrob,ncol=1,nrow=4,
                   heights = c(0.27,0.23,0.23,0.27))
      dev.off()
    } 
  }
}

