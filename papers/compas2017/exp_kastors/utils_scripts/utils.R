library(data.table)
library(rjson)
library(plyr)
library(dplyr)
printf <- function(...) invisible(print(sprintf(...)))

readcsv_kastors <- function(file) {
  df <- read.csv(file, header=FALSE, strip.white=TRUE)
  head(df)
  if (ncol(df) == 15){
    # kastors input
    names(df) <- c("Routine", "Threads", "Seconds", "GFlops", "T1", "T2", "Numactl", "Compiler", "Runtime", "Type", "Host", "Governor", "Places", "N", "NB")
  } else if (ncol(df) == 16) {
    names(df) <- c("Routine", "Threads", "Seconds", "GFlops", "T1", "T2", "Numactl", "Compiler", "Runtime", "Type", "Host", "Governor", "Places", "N", "NB", "IB")
  } else {
    if (grepl("dpotrf", file)){
      names(df) <- c("Routine", "Threads", "Seconds", "GFlops", "T1", "T2", "Numactl", "Compiler", "Runtime", "Type", "Host", "Governor", "Places", "Uplo", "N", "PadA", "NB", "ZeroCol")
    } else if (grepl("dgetrf", file)) {
      names(df) <- c("Routine", "Threads", "Seconds", "GFlops", "T1", "T2", "Numactl", "Compiler", "Runtime", "Type", "Host", "Governor", "Places",  "M", "N", "PadA", "NB", "IB", "NTPF", "ZeroCol")
    } else if (grepl("dgeqrf", file)) {
      names(df) <- c("Routine", "Threads", "Seconds", "GFlops", "T1", "T2", "Numactl", "Compiler", "Runtime", "Type", "Host", "Governor", "Places",  "M", "N", "PadA", "NB", "IB", "Hous", "Ortho")
    }
  }
  df = df[(names(df) %in% c("Routine", "Threads", "Seconds", "GFlops", "T1", "T2", "Numactl", "Compiler",
                            "Runtime", "Type", "Host", "Gflops", "N", "NB", "Places"))]

  df$Routine <- as.factor(as.character(df$Routine))
  df$Numactl <- as.factor(as.character(df$Numactl))
  df$Compiler <- as.factor(as.character(df$Compiler))
  df$Runtime <- as.factor(as.character(df$Runtime))
  df$Type <- as.factor(as.character(df$Type))
  df$Origin <- as.factor(as.character(file))

  # Return data frame
  df
}



readcsv_likwid_idchire <- function(file) {
  df <- read.csv(file, header=TRUE, strip.white=TRUE, comment.char = "#", sep=",")
  df < df %>% as.data.frame()
  names(df) <- c("Start", "Stop", "Duration", "Socket", "CPU", "PKG", "PP0", "DRAM", "TEMP")
#  Tmin <- min(df$Start)
#  df$Start <- df$Start*1e-9
#  df$Stop <- df$Stop*1e-9
#  df$Duration <- df$Duration*1e-9

  df0 <- df %>% filter(Socket == 0)
  df1 <- df %>% filter(Socket == 1)
  df2 <- df %>% filter(Socket == 2)
  df3 <- df %>% filter(Socket == 3)
  df4 <- df %>% filter(Socket == 4)
  df5 <- df %>% filter(Socket == 5)
  df6 <- df %>% filter(Socket == 6)
  df7 <- df %>% filter(Socket == 7)
  df8 <- df %>% filter(Socket == 8)
  df9 <- df %>% filter(Socket == 9)
  df10 <- df %>% filter(Socket == 10)
  df11 <- df %>% filter(Socket == 11)
  df12 <- df %>% filter(Socket == 12)
  df13 <- df %>% filter(Socket == 13)
  df14 <- df %>% filter(Socket == 14)
  df15 <- df %>% filter(Socket == 15)
  df16 <- df %>% filter(Socket == 16)
  df17 <- df %>% filter(Socket == 17)
  df18 <- df %>% filter(Socket == 18)
  df19 <- df %>% filter(Socket == 19)
  df20 <- df %>% filter(Socket == 20)
  df21 <- df %>% filter(Socket == 21)
  df22 <- df %>% filter(Socket == 22)
  df23 <- df %>% filter(Socket == 23)


  # Joules ?
  dfa <- data.frame(Start=df0$Start,
                    Stop=df0$Stop,
                    Duration = df0$Duration,
                    PKG  = df0$PKG+df1$PKG+df2$PKG+df3$PKG+df4$PKG+df5$PKG+df6$PKG+df7$PKG+df8$PKG+df9$PKG+df10$PKG+df11$PKG+df12$PKG+df13$PKG+df14$PKG+df15$PKG+df16$PKG+df17$PKG+df18$PKG+df19$PKG+df20$PKG+df21$PKG+df22$PKG+df23$PKG,
                    EnergyCore  = df0$PKG+df1$PKG+df2$PKG+df3$PKG+df4$PKG+df5$PKG+df6$PKG+df7$PKG+df8$PKG+df9$PKG+df10$PKG+df11$PKG+df12$PKG+df13$PKG+df14$PKG+df15$PKG+df16$PKG+df17$PKG+df18$PKG+df19$PKG+df20$PKG+df21$PKG+df22$PKG+df23$PKG,
                    PP0 = df0$PP0+df1$PP0+df2$PP0+df3$PP0+df4$PP0+df5$PP0+df6$PP0+df7$PP0+df8$PP0+df9$PP0+df10$PP0+df11$PP0+df12$PP0+df13$PP0+df14$PP0+df15$PP0+df16$PP0+df17$PP0+df18$PP0+df19$PP0+df20$PP0+df21$PP0+df22$PP0+df23$PP0,
                    EnergyDram = df0$DRAM + df1$DRAM+ df2$DRAM+ df3$DRAM+ df4$DRAM+ df5$DRAM+ df6$DRAM+ df7$DRAM+ df8$DRAM+ df9$DRAM+ df10$DRAM+ df11$DRAM+ df12$DRAM+ df13$DRAM+ df14$DRAM+ df15$DRAM+ df16$DRAM+ df17$DRAM+ df18$DRAM+ df19$DRAM+ df20$DRAM+ df21$DRAM+ df22$DRAM+ df23$DRAM,
                    DRAM = df0$DRAM + df1$DRAM+ df2$DRAM+ df3$DRAM+ df4$DRAM+ df5$DRAM+ df6$DRAM+ df7$DRAM+ df8$DRAM+ df9$DRAM+ df10$DRAM+ df11$DRAM+ df12$DRAM+ df13$DRAM+ df14$DRAM+ df15$DRAM+ df16$DRAM+ df17$DRAM+ df18$DRAM+ df19$DRAM+ df20$DRAM+ df21$DRAM+ df22$DRAM+ df23$DRAM,
                    ENERGY = df0$PKG + df0$DRAM + df1$PKG + df1$DRAM+ df2$PKG + df2$DRAM+ df3$PKG + df3$DRAM+ df4$PKG + df4$DRAM+ df5$PKG + df5$DRAM+ df6$PKG + df6$DRAM+ df7$PKG + df7$DRAM+ df8$PKG + df8$DRAM+ df9$PKG + df9$DRAM+ df10$PKG + df10$DRAM+ df11$PKG + df11$DRAM+ df12$PKG + df12$DRAM+ df13$PKG + df13$DRAM+ df14$PKG + df14$DRAM+ df15$PKG + df15$DRAM+ df16$PKG + df16$DRAM+ df17$PKG + df17$DRAM+ df18$PKG + df18$DRAM+ df19$PKG + df19$DRAM+ df20$PKG + df20$DRAM+ df21$PKG + df21$DRAM+ df22$PKG + df22$DRAM+ df23$PKG + df23$DRAM,
                    Watts = (df0$PKG + df0$DRAM + df1$PKG + df1$DRAM+ df2$PKG + df2$DRAM+ df3$PKG + df3$DRAM+ df4$PKG + df4$DRAM+ df5$PKG + df5$DRAM+ df6$PKG + df6$DRAM+ df7$PKG + df7$DRAM+ df8$PKG + df8$DRAM+ df9$PKG + df9$DRAM+ df10$PKG + df10$DRAM+ df11$PKG + df11$DRAM+ df12$PKG + df12$DRAM+ df13$PKG + df13$DRAM+ df14$PKG + df14$DRAM+ df15$PKG + df15$DRAM+ df16$PKG + df16$DRAM+ df17$PKG + df17$DRAM+ df18$PKG + df18$DRAM+ df19$PKG + df19$DRAM+ df20$PKG + df20$DRAM+ df21$PKG + df21$DRAM+ df22$PKG + df22$DRAM+ df23$PKG + df23$DRAM)/(df0$Duration*1e-9),
                    TEMP0 = df0$TEMP,
                    TEMP1 = df1$TEMP,
                    TEMP2 = df2$TEMP,
                    TEMP3 = df3$TEMP,
                    TEMP4 = df4$TEMP,
                    TEMP5 = df5$TEMP,
                    TEMP6 = df6$TEMP,
                    TEMP7 = df7$TEMP,
                    TEMP8 = df8$TEMP,
                    TEMP9 = df9$TEMP,
                    TEMP10 = df10$TEMP,
                    TEMP11 = df11$TEMP,
                    TEMP12 = df12$TEMP,
                    TEMP13 = df13$TEMP,
                    TEMP14 = df14$TEMP,
                    TEMP15 = df15$TEMP,
                    TEMP16 = df16$TEMP,
                    TEMP17 = df17$TEMP,
                    TEMP18 = df18$TEMP,
                    TEMP19 = df19$TEMP,
                    TEMP20 = df20$TEMP,
                    TEMP21 = df21$TEMP,
                    TEMP22 = df22$TEMP,
                    TEMP23 = df23$TEMP
                    )
  # Return data frame
  dfa
}

readcsv_likwid_idkat <- function(file) {
  df <- read.csv(file, header=TRUE, strip.white=TRUE, comment.char = "#", sep=",")
  df < df %>% as.data.frame()
  names(df) <- c("Start", "Stop", "Duration", "Socket", "CPU", "PKG", "PP0", "DRAM", "TEMP")
#  Tmin <- min(df$Start)
#  df$Start <- df$Start*1e-9
#  df$Stop <- df$Stop*1e-9
#  df$Duration <- df$Duration*1e-9

  df0 <- df %>% filter(Socket == 0)
  df1 <- df %>% filter(Socket == 1)
  df2 <- df %>% filter(Socket == 2)
  df3 <- df %>% filter(Socket == 3)

  # Joules ?
  dfa <- data.frame(Start=df0$Start,
                    Stop=df0$Stop,
                    Duration = df0$Duration,
                    PKG  = df0$PKG+df1$PKG+df2$PKG+df3$PKG,
                    EnergyCore  = df0$PKG+df1$PKG+df2$PKG+df3$PKG,
                    PP0 = df0$PP0+df1$PP0+df2$PP0+df3$PP0,
                    DRAM = df0$DRAM + df1$DRAM+ df2$DRAM+ df3$DRAM,
                    EnergyDram = df0$DRAM + df1$DRAM+ df2$DRAM+ df3$DRAM,
                    ENERGY = df0$PKG + df0$DRAM + df1$PKG + df1$DRAM+ df2$PKG + df2$DRAM+ df3$PKG + df3$DRAM,
                    Watts = (df0$PKG + df0$DRAM + df1$PKG + df1$DRAM+ df2$PKG + df2$DRAM+ df3$PKG + df3$DRAM)/(df0$Duration*1e-9),
                    TEMP0 = df0$TEMP,
                    TEMP1 = df1$TEMP,
                    TEMP2 = df2$TEMP,
                    TEMP3 = df3$TEMP
                    )
  # Return data frame
  dfa
}

makeGpLikwidCompare <- function(iomp, gomp) {
  Tbeg <- min(iomp$Start)
  iomp$Start <- iomp$Start-Tbeg
  Tbeg <- min(gomp$Start)
  gomp$Start <- gomp$Start-Tbeg
  gp <-  ggplot()+
    theme_bw(base_size=8)+
    ylab("Watts")+
    geom_line(data=iomp, size=0.5, aes(x=Start, y=PKG/(Duration*1e-9), colour="PKG iomp"))+
    geom_line(data=iomp, size=0.5, aes(x=Start, y=DRAM/(Duration*1e-9), colour="DRAM iomp"))+
    geom_line(data=gomp, size=0.5, aes(x=Start, y=PKG/(Duration*1e-9), colour="PKG gomp"))+
    geom_line(data=gomp, size=0.5, aes(x=Start, y=DRAM/(Duration*1e-9), colour="DRAM gomp"))+
    theme (
      legend.position = "bottom"
  ) 
  gp
}

makeGpLikwidCompare2 <- function(iomp, gomp, kompaffinity) {
  Tbeg <- min(iomp$Start)
  iomp$Start <- iomp$Start-Tbeg
  Tbeg <- min(gomp$Start)
  gomp$Start <- gomp$Start-Tbeg
  Tbeg <- min(kompaffinity$Start)
  kompaffinity$Start <- kompaffinity$Start-Tbeg
  gp <-  ggplot()+
    theme_bw(base_size=8)+
    ylab("Watts")+
    geom_line(data=iomp, size=0.5, aes(x=Start, y=PKG/(Duration*1e-9), colour="PKG iomp"))+
    geom_line(data=iomp, size=0.5, aes(x=Start, y=DRAM/(Duration*1e-9), colour="DRAM iomp"))+
    geom_line(data=gomp, size=0.5, aes(x=Start, y=PKG/(Duration*1e-9), colour="PKG gomp"))+
    geom_line(data=gomp, size=0.5, aes(x=Start, y=DRAM/(Duration*1e-9), colour="DRAM gomp"))+
    geom_line(data=kompaffinity, size=0.5, aes(x=Start, y=PKG/(Duration*1e-9), colour="PKG kompaffinity"))+
    geom_line(data=kompaffinity, size=0.5, aes(x=Start, y=DRAM/(Duration*1e-9), colour="DRAM kompaffinity"))+
    #geom_line(data=iomp, size=0.5, aes(x=Start, y=ENERGY, colour="PKG iomp"))+
    #geom_line(data=gomp, size=0.5, aes(x=Start, y=ENERGY, colour="PKG gomp"))+
    #geom_line(data=kompaffinity, size=0.5, aes(x=Start, y=Watts, colour="PKG kompaffinity"))+
    #geom_line(data=iomp, size=0.5, aes(x=Start, y=Watts, colour="PKG iomp"))+
    #geom_line(data=gomp, size=0.5, aes(x=Start, y=Watts, colour="PKG gomp"))+
    #geom_line(data=kompaffinity, size=0.5, aes(x=Start, y=Watts, colour="PKG kompaffinity"))+
    theme (
      legend.position = "bottom",
      legend.title = element_blank()
  )
  gp
}

makeGpLikwidDRAM <- function(iomp, gomp, kompaffinity) {
  Tbeg <- min(iomp$Start)
  iomp$Start <- iomp$Start-Tbeg
  Tbeg <- min(gomp$Start)
  gomp$Start <- gomp$Start-Tbeg
  Tbeg <- min(kompaffinity$Start)
  kompaffinity$Start <- kompaffinity$Start-Tbeg
  gp <-  ggplot()+
    theme_bw(base_size=8)+
    ylab("Watts")+
    geom_line(data=iomp, size=0.5, aes(x=Start, y=DRAM/(Duration*1e-9), colour="Clang"))+
    geom_line(data=gomp, size=0.5, aes(x=Start, y=DRAM/(Duration*1e-9), colour="Gcc"))+
    geom_line(data=kompaffinity, size=0.5, aes(x=Start, y=DRAM/(Duration*1e-9), colour="Komp + Affinity"))+
    theme (
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text=element_text(size=12)
  )
  gp
}

makeResult3 <- function(results, likwid, routine, size, ths, type, machine) {
  results_pot_iomp <- results %>%
  filter(Routine == routine) %>%
  filter(Runtime == "iomp") %>%
  filter(N == size) %>%
  filter(Threads == ths)

  Tmin <- results_pot_iomp[2,]$T1
  Tmax <- results_pot_iomp[2,]$T2
  likwid_pot_iomp <- likwid[likwid$Start >= Tmin & likwid$Stop <= Tmax,]

  results_pot_gomp <- results %>%
  filter(Routine == routine) %>%
  filter(Runtime == "gomp") %>%
  filter(N == size) %>%
  filter(Threads == ths)

  Tmin <- results_pot_gomp[2,]$T1
  Tmax <- results_pot_gomp[2,]$T2
  likwid_pot_gomp <- likwid[likwid$Start >= Tmin & likwid$Stop <= Tmax,]

  results_pot_kompaffinity <- results %>%
  filter(Routine == routine) %>%
  filter(Runtime == "libkomp-affinity") %>%
  filter(N == size) %>%
  filter(Threads == ths)

  Tmin <- results_pot_kompaffinity[2,]$T1
  Tmax <- results_pot_kompaffinity[2,]$T2
  likwid_pot_komp_affinity <- likwid[likwid$Start >= Tmin & likwid$Stop <= Tmax,]
  if (type == "DRAM") {
    gp1 <- makeGpLikwidDRAM(likwid_pot_iomp, likwid_pot_gomp, likwid_pot_komp_affinity)
  } else {
    gp1 <- makeGpLikwidCompare2(likwid_pot_iomp, likwid_pot_gomp, likwid_pot_komp_affinity)
  }
  gp1 <- gp1 + ggtitle(paste(routine, " matrix ", size, " ", ths, " threads ", machine))
  gp1
}


compute_energy <- function( T1, T2, energy )
{
  energy$CumulEnergy <- cumsum( energy$E );
  fapprox<-approxfun( energy$Stamp, energy$CumulEnergy );
  d<-fapprox(T2)-fapprox(T1);
  d;
}

# Return the cumulated energy between timestamp T1 and T2
compute_energy_rapl <- function( T1, T2, energy )
{
  energy$CumulEnergy <- cumsum( energy$EnergyCore );
  fapprox<-approxfun( energy$Start, energy$CumulEnergy );
  d<-fapprox(T2)-fapprox(T1);
  d;
}
compute_energy_rapl_dram <- function( T1, T2, energy )
{
  energy$CumulEnergy <- cumsum( energy$EnergyDram );
  fapprox<-approxfun( energy$Start, energy$CumulEnergy );
  d<-fapprox(T2)-fapprox(T1);
  d;
}

compute_power_rapl <- function( T1, T2, E )
{
    return (E / ((T2-T1)*1e9))
}

