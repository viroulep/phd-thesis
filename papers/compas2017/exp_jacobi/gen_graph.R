library(ggplot2)
library(grid)
library(plyr)
library(lattice)
#library(reshape)

datafile = commandArgs(TRUE)[1]
wholeframe = read.table(datafile, header=TRUE)

pd = position_dodge(width=.1)
#melted = melt(framsum, id.vars=c("GFlops", "Progname", "WSselect", "WSpush", "Runtime", "Threads", "Std", "Nxp"))
#print(framsum)

frameiompkomp = subset(wholeframe, Progname == "jacobi_block_for" | Progname == "jacobi_block_taskdep" | Progname == "jacobi_block_taskdep_affinity")
framekomp = subset(wholeframe, Runtime == "komp")
frameiomp = subset(wholeframe, Runtime == "iomp" & Blocksize == "2048")


#Graph for distribution

pdf("jacobi_scale_iomp.pdf", width = 10, height=4)

myplot = ggplot(frameiomp, aes(x=Threads, y = Time, fill=interaction(Progname, Runtime)))
myplot = myplot + geom_errorbar(show.legend=FALSE, position=position_dodge(0.9), aes(color=interaction(Progname, Runtime), ymin=Time-(2*Stddev/Iterations), ymax=Time+(2*Stddev/Iterations), width=.1))
myplot = myplot + geom_point(aes(color=interaction(Progname, Runtime), shape=interaction(Progname, Runtime), group=interaction(Progname, Runtime)))
myplot = myplot + scale_colour_discrete(name="Runtime and Program: ", labels=c("Clang/LLVM: For", "Clang/LLVM: Dep. Tasks"))
myplot = myplot + scale_shape_discrete(name="Runtime and Program: ", labels=c("Clang/LLVM: For", "Clang/LLVM: Dep. Tasks"))
#myplot = myplot + facet_wrap(Size~Blocksize)
myplot = myplot + guides(fill=FALSE)
myplot = myplot + theme(legend.position="right")
myplot = myplot + geom_line(aes(color=interaction(Progname, Runtime), shape=interaction(Progname, Runtime)))
#myplot = myplot + scale_color_grey()
myplot = myplot + ylab("Time (s)")
myplot = myplot + theme(legend.position=c(.88,.65))
myplot = myplot + ggtitle("Jacobi's performances, Matrix size: 49152, Blocksize: 2048")


print(myplot)
dev.off()

pdf("jacobi_scale_iomp_komp.pdf", width = 10, height=4)

myplot = ggplot(frameiompkomp, aes(x=Threads, y = Time, fill=interaction(Progname, Runtime)))
myplot = myplot + geom_errorbar(show.legend=FALSE, position=position_dodge(0.9), aes(color=interaction(Progname, Runtime), ymin=Time-(2*Stddev/Iterations), ymax=Time+(2*Stddev/Iterations), width=.1))
myplot = myplot + geom_point(aes(color=interaction(Progname, Runtime), shape=interaction(Progname, Runtime), group=interaction(Progname, Runtime)))
myplot = myplot + scale_colour_discrete(name="Runtime and Program: ", labels=c("Clang / For", "Clang / Dep. Tasks", "Komp / For", "Komp / Dep. Tasks", "Komp / Dep. Tasks + Affinity"))
myplot = myplot + scale_shape_discrete(name="Runtime and Program: ", labels=c("Clang / For", "Clang / Dep. Tasks", "Komp / For", "Komp / Dep. Tasks", "Komp / Dep. Tasks + Affinity"))
myplot = myplot + facet_wrap(Size~Blocksize)
myplot = myplot + guides(fill=FALSE)
myplot = myplot + theme(legend.position="right")
myplot = myplot + geom_line(aes(color=interaction(Progname, Runtime), shape=interaction(Progname, Runtime)))
#myplot = myplot + scale_color_grey()
myplot = myplot + ylab("Time (s)")


print(myplot)
dev.off()

pdf("jacobi_scale.pdf", width = 10, height=4)
myplot = ggplot(framekomp, aes(x=Threads, y = Time, fill=Progname, shape=Progname, group=Progname))
#myplot = myplot + geom_line(aes(color=Progname, shape=Progname, group=Progname))
myplot = myplot + geom_line(aes(color=Progname, shape=Progname, group=Progname)) + geom_point(aes(color=Progname, shape=Progname, group=Progname))
myplot = myplot + geom_errorbar(show.legend=FALSE, position=position_dodge(0.9), aes(shape=Progname, color=Progname, ymin=Time-(2*Stddev/Iterations), ymax=Time+(2*Stddev/Iterations), width=.1))
myplot = myplot + facet_wrap(Size~Blocksize)
#myplot = myplot + scale_colour_discrete(name="Program version: ", labels=c("For", "For + Affinity", "Dep. Tasks", "Dep. Tasks + strict Affinity", "Dep. Tasks + non-strict Affinity"))
#myplot = myplot + scale_shape_discrete(name="Program version: ", labels=c("For", "For + Affinity", "Dep. Tasks", "Dep. Tasks + strict Affinity", "Dep. Tasks + non-strict Affinity"))
myplot = myplot + guides(fill=FALSE)
myplot = myplot + theme(legend.position="right")
#myplot = myplot + scale_color_grey()
myplot = myplot + ylab("Time (s)")
myplot = myplot + theme(legend.position=c(.87,.7))
myplot = myplot + ggtitle("Jacobi's performances using libKOMP\n")


print(myplot)
dev.off()

#Generate the "Strict" graph, we'd better just make an explanation about it

#framestrict = subset(wholeframe, Progname == "dpotrf_taskdep" & WSpush_init != "defaultnuma" & WSpush_init != "numactl" & ((Size=="16384" & Blocksize == "256") | (Size=="32768" & Blocksize == "512")) & ((WSselect == "hws_N_P" & WSpush == "Whws") | (WSselect == "hws_N" & WSpush == "Whws")))
#framsum = ddply(framestrict, c("Runtime","WSselect", "Progname", "WSpush","WSpush_init", "Strict_Push", "Size","Blocksize","Threads"), summarize, GFlops = mean(Gflops), Std = sd(Gflops), Nxp=length(Runtime))
#pdf("graph_strict.pdf", width = 10, height=6)


#myplot = ggplot(framsum, aes(x=factor(WSpush_init), y = GFlops, fill=interaction(Strict_Push, WSselect, WSpush)))
#myplot = myplot + geom_bar(stat="identity", position=position_dodge())
#myplot = myplot + geom_errorbar(position=position_dodge(0.9), aes(color=interaction(Strict_Push, WSselect, WSpush), ymin=GFlops-(2*Std/Nxp), ymax=GFlops+(2*Std/Nxp), width=.1))
#myplot = myplot + facet_wrap(Size~Blocksize, ncol=4)
#myplot = myplot + theme(legend.position="bottom")
#myplot = myplot + scale_fill_discrete(name="Strategy (WSselect + WSpush) : ", labels=c("Loose (sNuma + pNumaWLoc)", "Strict (sNuma + pNumaWLoc)", "Loose (sNumaProc + pNumaWLoc)", "Strict (sNumaProc + pNumaWLoc)"))
#myplot = myplot + scale_x_discrete(name="Data distribution")
#myplot = myplot + guides(fill=guide_legend(nrow=2, byrow=TRUE), color=FALSE)
#myplot = myplot + ggtitle("Cholesky's performances using 16K and 32K matrices for strict or loose strategy\n")


#print(myplot)
#dev.off()


