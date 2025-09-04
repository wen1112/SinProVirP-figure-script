# figure2 a-b,h-i
library(ggplot2)
library(ggsci)
library(ggpubr)
library(reshape2)
library(stringr)
library(vegan)


da <- read.csv("compare.res.for.plot.csv")
d <- da[,c(1:2,6:10)]
names(d) <- c("Method","Repeat","Precision","Recall","F1 score","Bray-Curtis distance","R squared")
p <- melt(d,id=c("Method","Repeat")) 
names(p) <- c("Method","Repeat","Variable","Values")
p$Repeat <- factor(p$Repeat,levels = 1:10)

lst <- list(c("VCP","WGVP"),c("Phanta","VCP"),c("WGVP","Phanta"))
##
p1 <- ggplot(p[p$Variable=="Recall",],aes(x=Method,y=Values,group=Method,color=Method))+theme_classic()+scale_color_nejm()+geom_boxplot()+facet_grid(.~Variable)+xlab("")+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ylim(0.3,1)+geom_signif(comparisons =lst,step_increase = 0.1,map_signif_level = T)
p2 <- ggplot(p[p$Variable=="Precision",],aes(x=Method,y=Values,group=Method,color=Method))+theme_classic()+scale_color_nejm()+geom_boxplot()+facet_grid(.~Variable)+xlab("")+ylab("")+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ylim(0.3,1)+geom_signif(comparisons =lst,step_increase = 0.1,map_signif_level = T)
p3 <- ggplot(p[p$Variable=="F1 score",],aes(x=Method,y=Values,group=Method,color=Method))+theme_classic()+scale_color_nejm()+geom_boxplot()+facet_grid(.~Variable)+xlab("")+ylab("")+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ylim(0.3,1)+geom_signif(comparisons =lst,step_increase = 0.1,map_signif_level = T)
p4 <- ggplot(p[p$Variable=="Bray-Curtis distance",],aes(x=Method,y=Values,group=Method,color=Method))+theme_classic()+scale_color_nejm()+geom_boxplot()+facet_grid(.~Variable)+xlab("")+ylab("")+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ylim(0,0.5)+geom_signif(comparisons =lst,step_increase = 0.1,map_signif_level = T)
p5 <- ggplot(p[p$Variable=="R squared",],aes(x=Method,y=Values,group=Method,color=Method))+theme_classic()+scale_color_nejm()+geom_boxplot()+facet_grid(.~Variable)+xlab("")+ylab("")+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ylim(0.3,1)+geom_signif(comparisons =lst,step_increase = 0.1,map_signif_level = T)
ggarrange(plotlist = list(p1,p2,p3,p4,p5),widths = 15,heights = 3,ncol = 5,nrow = 1,legend = "bottom",common.legend = T)


# figure2 e-g
a = paste0("Phanta.res.VCID.rep",1:10,".txt")
#a = paste0("SinProVirP.res.VCID.rep",1:10,".txt")
#a = paste0("WGVP.res.VCID.rep",1:10,".txt")
dir = paste("./",a,sep="")
n = length(dir)
outdatlist <- list()
i <- 1
for (i in 1:n){
  name <- a[i]
  sub1 <- str_split_fixed(name,"[.]",5)
  sub2 <- str_split_fixed(sub1[1],"[_]",6)
  mock = paste("mock",sub1[4],sep="_")
  new.data <- read.table(dir[i], header=T,check.names = F,sep="")
  new.data$Ground_truth <- as.numeric(new.data$Ground_truth)
  new.data$Ground_truth.norm <- new.data$Ground_truth/as.numeric(sum(new.data$Ground_truth))
  ##renorm
  rel_sum <- sum(new.data$Relative_abudannce)
  new.data$Relative_abudannce_norm <- new.data$Relative_abudannce/rel_sum
  new.data$Relative_abudannce <-  new.data$Relative_abudannce_norm
  outdatlist[[i]] <- new.data
}

for (i in 1:10) {
  d <- outdatlist[[i]]
  d$mock <- paste0("mock_",i)
  outdatlist[[i]] <- d
}

rbinddata <- as.data.frame(do.call(rbind,outdatlist))
# rbinddata$Type <- ifelse(rbinddata$Ground_truth==0,"Not in mock","Mock phage")
# rbinddata[which(rbinddata$Relative_abudannce==0),"Type"]<- "Not detected"
# rbinddata$Type <- factor(rbinddata$Type,levels = c("Mock phage","Not in mock","Not detected"))
subrbind <- rbinddata[which(rbinddata$Relative_abudannce>0&rbinddata$Ground_truth.norm>0),]
subrbind$Relative_abudannce_log_scale <-  scale(log10(subrbind$Relative_abudannce)) 
subrbind$Ground_truth.norm_log_scale <-  scale(log10(subrbind$Ground_truth.norm)) 
fit <- lm(formula = Ground_truth.norm_log_scale  ~ Relative_abudannce_log_scale,data=subrbind)
b <- summary(fit)
r2 <- round(b$r.squared,3)
s <- as.data.frame(t(subrbind[,c("Relative_abudannce","Ground_truth.norm")]))
bray <- round(as.numeric(vegdist(s,method = "bray")),3)
cor <- cor.test(subrbind$Relative_abudannce,subrbind$Ground_truth.norm,method = "s")
rho <- round(as.numeric(cor$estimate),3)
te <- paste0("lm:R squared=",r2,"\n","P value<2e-16")



subrbind <- as.data.frame(subrbind)
subrbind$Data <- "Phanta-based approach"
#subrbind$Data <- "SinProVirP approach"
#subrbind$Data <- "WGVP approach"
subrbind$Data <- as.factor(subrbind$Data)
subrbind$mock <- factor(subrbind$mock,levels = paste0("mock_",1:10))

subrbind$Relative_abudannce_log10 <- log10(subrbind$Relative_abudannce)
subrbind$Ground_truth_log10 <- log10(subrbind$Ground_truth)

lim <- min(subrbind$Relative_abudannce_log10,subrbind$Ground_truth_log10)

ggplot(subrbind,aes(Ground_truth_log10,Relative_abudannce_log10))+
  geom_point(size=1,aes(color=mock),shape=19)+
  geom_abline(slope=1, na.rm = FALSE, show.legend = NA,size=0.2,linetype="dashed")+
  annotate("text",label=te,x = -Inf, y = Inf, hjust = -.05,vjust = 1.5)+
  theme_classic()+xlim(lim,0)+ylim(lim,0)+xlab("Ground truth(log10)")+
  ylab("Relative abundance(log10)")+facet_wrap(Data~.)+
  scale_color_manual(values = c("#A4C9DD","#2572A9","#ADD487","#399938","#F19695","#D5231E","#F5BB6F","#EF7C1C","#C6B0D2","#653C90"))


# figure2 j-i
res <- read.csv("res.csv")
names(res)[1] <- "ratio"
precision <- aggregate(res$precision,by=list(res$method,res$`ratio`),mean)
recall <- aggregate(res$recall,by=list(res$method,res$`ratio`),mean)
f1 <- aggregate(res$F1.score,by=list(res$method,res$`ratio`),mean)
bray <- aggregate(res$bray.distance,by=list(res$method,res$`ratio`),mean)
R2 <- aggregate(res$R2,by=list(res$method,res$`ratio`),mean)


meandata <- cbind(precision,recall$x,f1$x,bray$x,R2$x,"ph-host100")
names(meandata) <- c("method","ratio","precision","recall","F1.score","bray.distance","R2","data")
#plot2
## need cbind bg & host to get 2 result, then could plot
#x1 <- ggplot(data=res,aes(x=`ratio`,y=precision))+geom_boxplot(aes(color=method),position=position_dodge(0),outlier.colour = NA)+scale_color_nejm()+ylim(0,1)+theme_classic()+geom_point(data=meandata,aes(x=`ratio`,y=precision,color=method,group=method))+geom_line(data=meandata,aes(color=method,group=method))+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+xlab("")
#x2 <- ggplot(data=res,aes(x=`ratio`,y=recall))+geom_boxplot(aes(color=method),position=position_dodge(0),outlier.colour = NA)+scale_color_nejm()+ylim(0,1)+theme_classic()+geom_point(data=meandata,aes(x=`ratio`,y=recall,color=method,group=method))+geom_line(data=meandata,aes(color=method,group=method))+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+xlab("")
x3 <- ggplot(data=res,aes(x=`ratio`,y=F1.score))+geom_boxplot(aes(color=method),position=position_dodge(0),outlier.colour = NA)+scale_color_nejm()+ylim(0,1)+theme_classic()+geom_point(data=meandata,aes(x=`ratio`,y=F1.score,color=method,group=method))+geom_line(data=meandata,aes(color=method,group=method))+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+xlab("")
x4 <- ggplot(data=res,aes(x=`ratio`,y=bray.distance))+geom_boxplot(aes(color=method),position=position_dodge(0),outlier.colour = NA)+scale_color_nejm()+ylim(0,1)+theme_classic()+geom_point(data=meandata,aes(x=`ratio`,y=bray.distance,color=method,group=method))+geom_line(data=meandata,aes(color=method,group=method))+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+xlab("")
x5 <- ggplot(data=res,aes(x=`ratio`,y=R2))+geom_boxplot(aes(color=method),position=position_dodge(0),outlier.colour = NA)+scale_color_nejm()+ylim(0,1)+theme_classic()+geom_point(data=meandata,aes(x=`ratio`,y=R2,color=method,group=method))+geom_line(data=meandata,aes(color=method,group=method))+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+xlab("")


ggarrange(plotlist = list(x3,x4,x5),widths = 15,heights = 3,ncol = 3,nrow=1,common.legend = T,legend = "right")
