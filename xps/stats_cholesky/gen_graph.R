library(ggplot2)
library(plyr)

datafile = commandArgs(TRUE)[1]
wholeframe = read.table(datafile, header=TRUE)

pd = position_dodge(width=.1)



pdf("graph_distrib.pdf", width = 10, height=4)

#print(framsum)

subframe = subset(wholeframe, threads == "192")


myplot = ggplot(subframe, aes(x=duration, fill=task_id))
myplot = myplot + geom_density()
#myplot = myplot + theme(legend.position="right", legend.title=element_text(size=14), legend.text=element_text(size=13), axis.text=element_text(size=12))
#myplot = myplot + scale_x_discrete(name="Matrix sizes (512 BS)")
myplot = myplot + scale_fill_discrete(name="Kernel: ")
myplot = myplot + ggtitle("Distribution of Cholesky's kernels duration")
myplot = myplot + xlab("Duration (cycles)")
myplot = myplot + xlim(10000, 25000000)
#myplot = myplot + scale_fill_discrete(name="", labels=c("sRand\npLoc", "sHierarchy\npAffinityLoc", "sRandNuma\npAffinity"))
#myplot = myplot + guides(fill=guide_legend(nrow=3, byrow=TRUE, keyheight=3), color=FALSE)



print(myplot)
dev.off()



pdf("graph_distrib_overview.pdf", width = 10, height=6)

#print(framsum)
subframe = subset(wholeframe, threads != "8")



myplot = ggplot(subframe, aes(x=duration, fill=as.factor(threads)))
myplot = myplot + geom_density()
#myplot = myplot + theme(legend.position="right", legend.title=element_text(size=14), legend.text=element_text(size=13), axis.text=element_text(size=12))
#myplot = myplot + scale_x_discrete(name="Matrix sizes (512 BS)")
myplot = myplot + scale_fill_discrete(name="Number of threads: ")
myplot = myplot + facet_wrap(~task_id, ncol=2)
myplot = myplot + ggtitle("Distribution of Cholesky's kernels duration")
myplot = myplot + xlab("Duration (cycles)")
myplot = myplot + xlim(10000, 8500000)



#myplot = myplot + scale_fill_discrete(name="", labels=c("sRand\npLoc", "sHierarchy\npAffinityLoc", "sRandNuma\npAffinity"))
#myplot = myplot + guides(fill=guide_legend(nrow=3, byrow=TRUE, keyheight=3), color=FALSE)



print(myplot)
dev.off()



