#!/usr/bin/env Rscript
#
library("colorRamps")
library("R2HTML")
#
selectTopSets <- function(t,ms) {

  #print("ms:"); print(ms)

  # sort by total sum of rows and get the names
  rs <- names(sort(apply(t,1,sum),decreasing=TRUE))
  #print("rs:");  print(rs)
  #print("rs length:"); print(length(rs))
  # select only 7 sets
  if (ms <= length(rs)) {
      rsa <- rs[1:ms]
  } else {
      rsa <- rs
  }
  #print("rsa:"); print(rsa)

  # summarize all the remaining sets
  if (ms < length(rs)) {
      rsb <- rs[(ms+1):length(rs)]
      #print("rsb:"); print(rsb)
      #print(dim(rsb))
      if(! is.null(dim(rsb))) {
        cs <- colSums(t[rsb,])
        r <- rbind(t[rsa,],"rest"=cs)
      } else {
        r <- t[rsa,]
      }
  } else {
      r <- t[rsa,]
  }

  # and add them to the table
  #print("r:"); print(r)
  return(r)
}
  
plotit <- function(ylab) {

  layout(matrix(c(1,2), nrow = 1), widths = c(0.7, 0.3))
  par(mar = c(5, 4, 4, 2) + 0.1)
  print(ts)
  print(str(ts))
  print(rownames(ts))
  print(rank(rownames(ts)))
  print(cols[rank(rownames(ts))])
  cols.new <- cols[rank(rownames(ts))]
  print(cols.new)
  barplot(ts,las=1,legend.text=NULL,
          col=cols.new,xlab="tracks",ylab=ylab,
          main=paste("top",ms,"overlap sets"))
  par(mar = c(5, 0, 4, 2) + 0.1)
  plot(ts,type = "n", axes = FALSE, ann = FALSE)
  legend("top",rownames(ts),fill=cols.new)

}


#
t <- read.table("multovl.strict.focus.anc.txt",header=TRUE,
colClasses=c("character","character","character",
"integer","integer","integer","integer","integer","integer",
"character","character",
"integer","integer",
"character","character",
"integer",
"factor"))
attach(t)
#
ms <- 7
#
#cols <- primary.colors(nlevels(code),steps = 4, no.white = TRUE)
cols <- rev(primary.colors(15,steps = 4, no.white = TRUE))
#
tt <- t(table(tracknum,code))
tt.html <- rbind(tt,apply(tt,2,sum))
HTML(tt.html,file="multovl.strict.focus.anc.num.html",append=FALSE)
#
if ( dim(tt)[1] == "1") {
  ts <- tt
} else {
  ts <- selectTopSets(tt,ms)
}
#
jpeg("multovl.strict.focus.anc.num.jpeg")
plotit("overlaps")
dev.off()
print("plotted multovl.strict.focus.anc.num.jpeg")
pdf("multovl.strict.focus.anc.num.pdf")
plotit("overlaps")
dev.off()
print("plotted multovl.strict.focus.anc.num.pdf")

#
cs <- colSums(tt)/100
tp <- t(apply(tt,1,'/',cs))
tp.html <- rbind(tp,apply(tp,2,sum))
HTML(tp.html,file="multovl.strict.focus.anc.perc.html",append=FALSE)
if (dim(tt)[1] == "1") {
  ts <- tp
} else {
  ts <- selectTopSets(tp,ms)
}
#
jpeg("multovl.strict.focus.anc.perc.jpeg")
plotit("percentages")
dev.off()
print("plotted multovl.strict.focus.anc.perc.jpeg")
pdf("multovl.strict.focus.anc.perc.pdf")
plotit("percentages")
dev.off()
print("plotted multovl.strict.focus.anc.perc.pdf")
