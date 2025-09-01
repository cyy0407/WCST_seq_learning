library(plyr)


r1=modelData$allIndPars$r
p1=modelData$allIndPars$p
d1=modelData$allIndPars$d

r2=modelData$allIndPars$r
p2=modelData$allIndPars$p
d2=modelData$allIndPars$d

Group <- c(rep("CT+",129),rep("CT-",98)) #group
Group <- factor(Group) 

r=c(r1,r2)
p=c(p1,p2)
d=c(d1,d2)

Data <- data.frame(Group=Group,r=r,p=p,d=d) 



summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm)
                       )
                   },
                   measurevar
    )
    
    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
}

#依据分组对vale进行统计
Data_summary <- summarySE(Data, measurevar="r", groupvars=c("Group"))
pal = c('#025259', '#007172', '#f4e2de', '#f29325', '#d94f04')


P1<- ggplot(Data, aes(x=Group, y=p,fill=Group)) + 
    geom_violin(trim=FALSE,color="white")+
    geom_boxplot(
        width=0.05, 
        position=position_dodge(0.9), 
        color="black",
        alpha = 0.7,
        linewidth=0.2)+
    scale_fill_manual(values = c("#56B4E9", "#E69F00"))+
    theme_bw()+ 
    theme(axis.text.x=element_text(hjust = 1,colour="black",size=10), 
          axis.text.y=element_text(size=8,), 
          axis.title.y=element_text(size = 10), #
          panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), 
          legend.text=element_text( colour="black",  
                                    size=8),
          legend.title=element_text( colour="black",
                                     size=10),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank())+  
    ylab("p")+xlab("Group") 

P1
ggsave(plot = P1, "_plots/p_violin.png", width = 4, height = 4, type = "cairo-png", units = "in")


P2<- ggplot(Data, aes(x=Group, y=r,fill=Group)) + 
    geom_violin(trim=FALSE,color="white")+
    geom_boxplot(
        width=0.05, 
        position=position_dodge(0.9), 
        color="black",
        alpha = 0.7,
        linewidth=0.2)+
    scale_fill_manual(values = c("#56B4E9", "#E69F00"))+
    theme_bw()+
    theme(axis.text.x=element_text(hjust = 1,colour="black",size=10), 
          axis.text.y=element_text(size=8,), 
          axis.title.y=element_text(size = 10), 
          panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), 
          legend.text=element_text( colour="black",  
                                    size=8),
          legend.title=element_text( colour="black",
                                     size=10),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank())+   
    ylab("r")+xlab("Group") 

P2
ggsave(plot = P2, "_plots/r_violin.png", width = 3, height = 4.5, type = "cairo-png", units = "in")
    

P3<- ggplot(Data, aes(x=Group, y=d,fill=Group)) + 
    geom_violin(trim=FALSE,color="white")+
    geom_boxplot(
        width=0.05, 
        position=position_dodge(0.9), 
        color="black",
        alpha = 0.7,
        linewidth=0.2)+
    scale_fill_manual(values = c("#56B4E9", "#E69F00"))+
    theme_bw()+
    theme(axis.text.x=element_text(hjust = 1,colour="black",size=10), 
          axis.text.y=element_text(size=8,), 
          axis.title.y=element_text(size = 10), #
          panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), 
          legend.text=element_text( colour="black",  
                                    size=8),
          legend.title=element_text( colour="black",
                                     size=10),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank())+  
    ylab("d")+xlab("Group") 

P3
ggsave(plot = P3, "_plots/d_violin.png", width = 4, height = 4, type = "cairo-png", units = "in")
