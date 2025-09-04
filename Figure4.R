## figure4-b
library(VennDiagram)
niew <- read.csv("./01.profile/01.keep.UC.CD/NielsenHB2014.SinProVirP.genus.profile.txt",sep="\t",header=T,check.names = F)
sch <- read.csv("./01.profile/01.keep.UC.CD/Schimer2018.SinProVirP.genus.profile.txt",sep="\t",header=T,check.names = F)
heq <- read.csv("./01.profile/01.keep.UC.CD/HeQ2017.SinProVirP.genus.profile.txt",sep="\t",header=T,check.names = F)
prj <- read.csv("./01.profile/01.keep.UC.CD/PRJNA400072.SinProVirP.genus.profile.txt",sep="\t",header=T,check.names = F)


set1 <- as.character(colnames(niew ))
set2 <- as.character(colnames(prj))
set3 <- as.character(colnames(heq))
set4 <- as.character(colnames(sch))


# 绘制韦恩图
venn.plot <- draw.quad.venn(
  area1 = length(set1),
  area2 = length(set2),
  area3 = length(set3),
  area4 = length(set4),
  n12 = length(intersect(set1, set2)),
  n13 = length(intersect(set1, set3)),
  n14 = length(intersect(set1, set4)),
  n23 = length(intersect(set2, set3)),
  n24 = length(intersect(set2, set4)),
  n34 = length(intersect(set3, set4)),
  n123 = length(intersect(intersect(set1, set2), set3)),
  n124 = length(intersect(intersect(set1, set2), set4)),
  n134 = length(intersect(intersect(set1, set3), set4)),
  n234 = length(intersect(intersect(set2, set3), set4)),
  n1234 = length(intersect(intersect(set1, set2), intersect(set3, set4))),
  category = c("NielsenHB2014", "JiangS_2022","HeQ2017","Schimer2018" ),
  fill = c("#D6A2E8","#1ABC9C","#FFC93C","#FF7F50"),
  cat.cex = 0.8,
  margin = 0.05
)

venn.plot

# 显示韦恩图

## figure4-c
inte <- intersect(colnames(niew),colnames(sch))
inte1 <- intersect(inte,colnames(heq))
inte2 <- intersect(inte1,colnames(prj))

uni <- union(colnames(niew),colnames(sch))
uni1 <- union(uni,colnames(heq))
uni2 <- union(uni1,colnames(prj))


sub.niew <- niew[,inte2]
sub.sch <- sch[,inte2]
sub.heq <- heq[,inte2]
sub.prj <- prj[,inte2]


##shared.abundance
st1 <- as.data.frame(apply(sub.niew,1,sum))
m1 <- mean(st1$`apply(sub.niew, 1, sum)`) 
m1 #0.67

##shared.abundance
st2 <- as.data.frame(apply(sub.sch,1,sum))
m2 <- mean(st2$`apply(sub.sch, 1, sum)`) 
m2 #0.84

##shared.abundance
st3 <- as.data.frame(apply(sub.heq,1,sum))
m3 <- mean(st3$`apply(sub.heq, 1, sum)`) 
m3 #0.68

##shared.abundance
st4 <- as.data.frame(apply(sub.prj,1,sum))
m4 <- mean(st4$`apply(sub.prj, 1, sum)`) 
m4 #0.69

outm <- as.data.frame(matrix(nrow=4,ncol=2,""))
colnames(outm) <- c("Study","Abundance")
outm$Study <- c("NielsenHB_2014","HeQ_2017","Schirmer_2018","JiangS_2022")
outm$Abundance <- c(m1,m3,m2,m4)
outm$Study <- factor(outm$Study,levels = c("NielsenHB_2014","HeQ_2017","Schirmer_2018","JiangS_2022") )
outm$label <- paste0(round(outm$Abundance,4)*100,"%")

p2 <- ggplot(outm,aes(Study,Abundance))+
  geom_bar(aes(fill=Study),stat = "identity",width=0.4)+
  scale_fill_manual(values = c("#D6A2E8","#1ABC9C","#FFC93C","#FF7F50"))+
  theme_classic()+
  ylab("Total abundance of shared taxa")+
  geom_text(aes(label = label), vjust = -0.5, color = "black")+
  scale_y_continuous(expand=expansion(mult=c(0,0.1)))+theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45,hjust = 1))+ylim(0,1)+guides(fill=F)

p2

# figure4-d
library(reshape2)
library(vegan)
mcom.bray <- read.csv("./richness_diversity.txt",sep="\t",header=T,check.names = F)
mcom.bray$group <- as.factor(mcom.bray$group)
mcom.bray$group <- factor(mcom.bray$group,levels=c("NonIBD","UC","CD"))
list <- list(c("NonIBD","UC"),c("NonIBD","CD"),c("CD","UC"))

mcom.bray$Study <- factor(mcom.bray$Study,levels = c("NielsenHB_2014","HeQ_2017","Schirmer_2018","JiangS_2022"))


list <- list(c("NonIBD","CD"))
mcom.bray <- mcom.bray[which(mcom.bray$group!="UC"),]
p2 <- ggplot(mcom.bray,aes(group,value),aes(fill=Study))+
  geom_boxplot(aes(color=group),width=0.5,outlier.size = 0.1)+
  facet_grid(variable~Study,scales = "free")+
  scale_color_manual(values = c("#407a30","#F39C12"))+
  theme_bw()+
  ggsignif::geom_signif(stat="signif",test = "wilcox.test",comparisons = list,step_increase = 0.15)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1),axis.title.x=element_blank(),panel.grid = element_blank())+
  scale_y_continuous(expand=expansion(mult=c(0,0.2)))
p2 

# figure4-e
auc_inde1 <- read.csv("Independent_validation_AUC.txt",header=T,sep="\t",check.names = F)

lst <- list(
            c("SinProVirP","MetaPhlAn"),
            c("SinProVirP","MetaPhlAn+SinProVirP"),
            c("MetaPhlAn","MetaPhlAn+SinProVirP"))
pp4 <- ggplot(auc_inde1,aes(Method,AUC))+
  geom_boxplot(outlier.size = 0.1,aes(color=Method))+
  facet_grid(.~auc_inde1$Selected_times)+
   theme_classic()+
  ggsignif::geom_signif(comparisons = lst,test = "wilcox.test",step_increase = 0.1,map_signif_level = T)+
  scale_color_manual(values = c("#ea6921","#fbb461","#7fb2d5","#b5d66b","#a251ce"))+xlab("Features selected times in 100 repeats")
pp4

# figure4-f
vcp.spp <- c("VC_196_0","VC_1976_3","VC_2477_0","VC_6848_0","VC_42_0","VC_1956_0","VC_1211_0","VC_2017_0","VC_2441_0","VC_1505_0","VC_2024_0","VC_2462_0","VC_1079_0","VC_1940_0","VC_4614_0","VC_6424_0","VC_190_0","VC_2113_0","VC_2637_0","VC_3003_0","VC_3445_0","VC_3935_20","VC_4046_0","VC_4679_0","VC_6842_0","VC_971_0","VC_4267_0","VC_6877_0","VC_2310_40","VC_1662_10")

bacdat <- read.csv("./02.results/06.keep.same.number.feature.RF/heatmap.anno.sinprovirp.txt",sep="\t",header = T,check.names = F)
bacdat$Category <- factor(bacdat$Category,levels = c("CD","NONE","NonIBD","Siphoviridae","Myoviridae","Microviridae","Podoviridae","Peduoviridae","Unassigned"))
col <- c("#F39C12","gray80","#417640","#ecaf87","#9bbc59","#F1C40F","gray80","#20B2AA","#FF6347","#92A8D1")


ggplot(bacdat,aes(x=x,y=Feature,fill=Category))+
  geom_tile(color="gray95")+
  scale_fill_manual(values = col)+
  scale_x_discrete(expand = expansion(mult = c(0,0)))+
  scale_y_discrete(expand = expansion(mult = c(0,0)))+
  theme_bw()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 270,vjust = 0.5,hjust = 0,size=11),
    axis.text = element_text(color=1,size = 14),
    axis.title = element_text(size = 14,color=1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_blank()
  )

# figure4 g-h
od  <- read.csv("./02.results/06.keep.same.number.feature.RF/02.selected.features/Final_selected_feature_for_PhantaGenus.filterocc0.05.in.dis.ONLY_TimeCutoff_30",sep="\t",row.names = 1,check.names = F)
phanta_features  <- as.character(od$Var1)

dat <- read.csv("./02.results/06.keep.same.number.feature.RF/Phanta.phage-host.interaction.headpmap.data.txt",header = T,sep="\t",check.names = F)
dat$Species2 <- factor(dat$Species2,levels = spoder)
dat$Species1 <- factor(dat$Species1,levels = rev(phantaspp))
pp.d <- ggplot(dat,aes(Species2,Species1))+
  geom_tile(aes(fill=fill))+
  geom_point(data=dat[dat$`Phage-host` =="host",],shape=18,size=2)+
 # facet_grid(g~Group,scales = "free",space = "free")+
  scale_fill_manual(values = c("#93CC92","white","#F9C478"))+
  scale_x_discrete(expand = expansion(mult = c(0,0)))+
  scale_y_discrete(expand = expansion(mult = c(0,0)))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 1,hjust = 1,size=11),
    axis.text = element_text(color=1,size = 14),
    axis.title = element_text(size = 14,color=1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_blank()
  )
pp.d
##validation only
pp.v <- ggplot(dat,aes(Species2,Species1))+
  geom_tile(aes(fill=fill_vali))+
  geom_point(data=dat[dat$`Phage-host` =="host",],shape=18,size=2)+
 # facet_grid(g~Group,scales = "free",space = "free")+
  scale_fill_manual(values = c("#93CC92","white","#F9C478"))+
  scale_x_discrete(expand = expansion(mult = c(0,0)))+
  scale_y_discrete(expand = expansion(mult = c(0,0)))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90,vjust = 1,hjust = 1,size=11),
    axis.text = element_text(color=1,size = 14),
    axis.title = element_text(size = 14,color=1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_blank()
  )
library(ggpubr)
ggarrange(pp.d,pp.v,ncol=2,common.legend = T)


