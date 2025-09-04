# plot script for figure3 b-g


library(ggpubr)
library(ggplot2)
library(ggsignif)

mout <- read.table("./fig3bcd.txt",header=T,sep="\t",check.names = F)
mout$Method <- factor(mout$Method,levels = c("SinProVirP","Phanta")) 
precision <- mout[which(mout$variable=="Precision"),]
lst <- list(c("Novel species","DB species"))
pre <- ggplot(precision,aes(Type,value))+
  geom_boxplot(aes(color=Type),width=0.6,outlier.size = 0.1)+
  geom_jitter(aes(color=Type),size=1,alpha=0.8,shape=21)+
  facet_grid(~Method)+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  ylab("Precision")+
  geom_signif(comparisons = lst,test = "wilcox.test",step_increase = 0.1,map_signif_level = T)+
  scale_color_manual(values = c("#6FCEBC","#FFA07A"))+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ylim(0.5,1.05)
pre


recall <- mout[which(mout$variable=="Recall"),]
lst <- list(c("Novel species","DB species"))
re <- ggplot(recall,aes(Type,value))+
  geom_boxplot(aes(color=Type),width=0.6,outlier.size = 0.1)+
  geom_jitter(aes(color=Type),size=1,alpha=0.8,shape=21)+
  facet_grid(~Method)+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  ylab("Recall")+
  geom_signif(comparisons = lst,test = "wilcox.test",step_increase = 0.1,map_signif_level = T)+
  scale_color_manual(values = c("#6FCEBC","#FFA07A"))+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ylim(0.5,1.05)
re

f1 <- mout[which(mout$variable=="F1 score"),]
lst <- list(c("Novel species","DB species"))
fs <- ggplot(f1,aes(Type,value))+
  geom_boxplot(aes(color=Type),width=0.6,outlier.size = 0.1)+
  geom_jitter(aes(color=Type),size=1,alpha=0.8,shape=21)+
  facet_grid(~Method)+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  ylab("F1 score")+
  geom_signif(comparisons = lst,test = "wilcox.test",step_increase = 0.1,map_signif_level = T)+
  scale_color_manual(values = c("#6FCEBC","#FFA07A"))+theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ylim(0.5,1.05)
fs


mse <- read.csv("./fig3e.txt",sep="\t",header = T,check.names = F)
fig.mse <- ggplot(mse,aes(Dataset,MSE))+
  geom_boxplot(aes(color=Dataset),width = 0.5,outlier.size = 0)+
  geom_jitter(aes(color=Dataset),size=1,alpha=0.8,shape=21)+
  facet_grid(.~Tool)+
  scale_color_manual(values = c("#6FCEBC","#FFA07A"))+
  ylab("Mean Squared Error")+theme_classic()+theme(axis.title.x = element_blank())+
  geom_signif(comparisons = lst,test = "wilcox.test",step_increase = 0.1,map_signif_level = T)+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+ylim(0,6.9e-04)

fig.mse

comvc <- read.csv("./fig3fg.txt",header = T,sep="\t",check.names = F)
vcp <- comvc[which(comvc$Method=="SinProVirP"),]
phanta <- comvc[which(comvc$Method=="Phanta"),]

lst <- list(c("DB species","Novel species"))

h1 <- ggplot(vcp,aes(Mock_type,Ratio1))+
  facet_grid(.~check_same_VC)+
  geom_boxplot(aes(color=Mock_type),width = 0.5,outlier.size = 0)+
  geom_jitter(aes(color=Mock_type),size=1,alpha=0.8,shape=21)+
    scale_color_manual(values = c("#6FCEBC","#FFA07A","gray80"))+
  #scale_fill_manual(values = c("#5bb0c9","#a478b8","#f5ae52"))+
  geom_signif(comparisons = lst,test = "wilcox.test",step_increase = 0.1,map_signif_level = T)+
  theme_classic()+
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
  ylab("Aligned reads(%)")+
  scale_y_continuous(expand=expansion(mult=c(0,0.1)))+ylim(-2,105)


n1 <- ggplot(phanta,aes(Mock_type,Ratio1))+
  facet_wrap(.~check_same_VC)+
  geom_boxplot(aes(color=Mock_type),width = 0.5,outlier.size = 0)+
  geom_jitter(aes(color=Mock_type),size=1,alpha=0.8,shape=21)+
    scale_color_manual(values = c("#6FCEBC","#FFA07A","gray80"))+
  #scale_fill_manual(values = c("#5bb0c9","#a478b8","#f5ae52"))+
  geom_signif(comparisons = lst,test = "wilcox.test",step_increase = 0.1,map_signif_level = T)+
  theme_classic()+
  theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
  ylab("Aligned reads(%)")+
  scale_y_continuous(expand=expansion(mult=c(0,0.1)))+ylim(-2,105)

ggarrange(ggarrange(pre,re,fs,fig.mse,ncol=4,nrow=1,common.legend = T),ggarrange(h1,n1,common.legend = T,ncol=2),nrow=2,legend = "right",common.legend = T)



