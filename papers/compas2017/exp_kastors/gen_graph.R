library(ggplot2)
library(plyr)
library(lattice)
library(reshape)

datafile = commandArgs(TRUE)[1]
wholeframe = read.table(datafile, header=TRUE)
#wholeframe = subset(wholeframe, Size == "16384")
#wholeframe = subset(wholeframe, Blocksize == "512")
#wholeframe = subset(wholeframe, Placement == "contiguous")
#wholeframe = subset(wholeframe, Numactl == "all")
#print(wholeframe)
framsum = ddply(wholeframe, c("Runtime","WSselect", "Progname", "WSpush","Size","Blocksize","Threads", "Taskprio"), summarize, GFlops = mean(Gflops), Std = sd(Gflops), Nxp=length(Runtime))
#framsum = ddply(wholeframe, c("Runtime","WSselect", "Progname", "WSpush","Size","Blocksize","Threads"), summarize, GFlops = mean(Gflops))

pd = position_dodge(width=.1)
#melted = melt(framsum, id.vars=c("GFlops", "Progname", "WSselect", "WSpush", "Runtime", "Threads", "Std", "Nxp"))
#print(framsum)




pdf(paste("graph_", datafile, ".pdf", sep = ''), width = 10, height=6)
#myplot = ggplot(data=melted, aes(x=variable, y=GFlops, fill=factor(value)))
#myplot = myplot + geom_bar(stat="identity", position="dodge")
#myplot = myplot + facet_grid(~Progname)






myplot = ggplot(framsum, aes(x=factor(Progname), y = GFlops, fill=interaction(WSselect, WSpush, Taskprio)))
myplot = myplot + geom_bar(stat="identity", position=position_dodge())
myplot = myplot + geom_errorbar(position=position_dodge(0.9), aes(color=interaction(WSselect, WSpush, Taskprio), ymin=GFlops-(2*Std/Nxp), ymax=GFlops+(2*Std/Nxp), width=.1))
myplot = myplot + facet_grid(Size~Blocksize)



#myplot = myplot + geom_line(aes(x=Threads, y=GFlops, group=interaction(Progname, Runtime,Size,Blocksize),color=interaction(Progname, Runtime,Size,Blocksize)))
#myplot = myplot + facet_grid(Numactl~Placement)
#myplot = myplot + guides(col = guide_legend(ncol=3))
#myplot = myplot + theme(legend.text = element_text(size=8), legend.title = element_text(size=8), legend.position="bottom")
#myplot = myplot + ggtitle("Performance of OpenMP Runtimes")
print(myplot)
dev.off()

