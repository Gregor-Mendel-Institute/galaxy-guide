#!/usr/bin/env Rscript
#
t <- read.table("multovl.strict.anc.txt",header=TRUE,
colClasses=c("character","character","character",
"integer","integer","integer","integer","integer","integer",
"character","character",
"integer","integer",
"character","character",
"integer",
"factor"))
attach(t)
#
# we aggregate by max multiplicity
agg.mult <- aggregate(t[,7],list("tracknum"=tracknum,"id_orig"=id_orig),max)
colnames(agg.mult) <- c("tracknum","id_orig","multiplicity")
#
# we merge and thus select only those with max multiplicity
mrg.mult <- merge(agg.mult,t,by.x=c("tracknum","id_orig","multiplicity"),by.y=c("tracknum","id_orig","multiplicity"))
#
# we aggregate by max length
agg.len <- aggregate(mrg.mult[,9],list("tracknum"=mrg.mult[,1],"id_orig"=mrg.mult[,2]),max)
colnames(agg.len) <- c("tracknum","id_orig","length")
#
# we merge and thus get those with max mult and length (default keeps only matched rows)
mrg <- merge(agg.len,mrg.mult,by.x=c("tracknum","id_orig","length"),by.y=c("tracknum","id_orig","length"))
#
# reorder columns
t.new <- mrg[,c(5:9,3,4,1,10:15,2,16:17)]
#
write.table(t.new,file="multovl.strictAgg.anc.txt",sep="\t",quote=FALSE,row.names=FALSE)
