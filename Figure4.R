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



