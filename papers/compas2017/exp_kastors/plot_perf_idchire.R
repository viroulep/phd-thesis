source("./utils_scripts/utils.R")

library(ggplot2)
library(RColorBrewer)
library(dplyr)

files1 <- dir("./", pattern = "^kastors-*", full.names = TRUE, ignore.case = TRUE)
#files1 <- dir("./", pattern = "^test-*", full.names = TRUE, ignore.case = TRUE)
input_files <- c(files1)


# read results
results <- data.frame()
for (i in 1:length(input_files)){
  dfs <- readcsv_kastors(input_files[i])
  results <- rbindlist(list(results, dfs))
}

 #Routine Threads  Seconds   GFlops           T1           T2 Numactl Compiler          Runtime    Type    Host   Places     N  NB Origin
#results_stat <- ddply(results, c("Routine", "Threads", "Numactl", "Compiler",
                            #"Runtime", "Type", "Host", "N", "NB"),

results <- results[!(results$Runtime == "libkomp-noaff"), ]
#results
results_stat <- ddply(results, c("Routine", "Threads", "Numactl", "Compiler", "Runtime", "Type", "Host", "N", "NB"),
                      summarize,
                      n=length(GFlops), Gflops=mean(GFlops), sd=sd(GFlops))
#print(results_stat)

pdf("graph_dpotrf.pdf", width = 10, height=4)
results_stat %>%
    filter((Routine == "dpotrf" | Routine == "dgeqrf")& N == 32768) %>%
    ggplot(aes(x=Threads, y=Gflops, color=Runtime))+
    theme_bw()+
    coord_cartesian(xlim=c(0,200)) +
    geom_point(size=2,aes(shape=Runtime))+
    geom_line(size=0.5)+
    scale_colour_discrete(name="Runtime: ", labels=c("LibKomp + Affinity", "Clang", "Gcc"))+
    scale_shape_discrete(name="Runtime: ", labels=c("LibKomp + Affinity", "Clang", "Gcc"))+
    geom_errorbar(position=position_dodge(0.9), aes(color=Runtime, ymin=Gflops-(2*sd/n), ymax=Gflops+(2*sd/n), width=.1))+
    facet_grid(~ Routine, scales="free_y")

dev.off()

