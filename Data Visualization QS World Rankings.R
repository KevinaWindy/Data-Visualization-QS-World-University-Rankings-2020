setwd("D:/A KULIAH KVN/3. AED C/FP EDA")

df<-read.csv("data topuni.csv",sep=",",quote="\"",check.names=F,dec=".")

library(ggplot2)
library(dplyr)

topuni=df%>%select(-c("Rank in 2019","Academic Reputation Rank",
                        "Employer Reputation Rank","Faculty to Student Ratio Rank",
                        "Citations per Faculty Rank","International Faculty Rank",
                        "International Student Rank"))

top100<-topuni[1:100,]
df2=top100 %>% 
  select(Country)%>%
  group_by(Country) %>% 
  summarise(Total = n())%>%
  arrange(desc(Total))%>%
  mutate(region=Country);
ggplot(df2, aes(x=Country, y = Total, fill=Country)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5),
        legend.position ="none") +
  labs(title = 'Jumlah Universitas Top 100',caption = 'Source : Kaggle',
         subtitle = "Berdasarkan Negara",
         x = 'Negara', y='Jumlah')+
  geom_text(aes(label=Total),vjust=0.5,hjust=-0.2,color="black",size=3.5)+coord_flip()

df4<-topuni[topuni$Country=="Indonesia"|topuni$Country=="Malaysia"|topuni$Country=="Philippines"|topuni$Country=="Singapore"|topuni$Country=="Thailand"|topuni$Country=="Brunei", ]
ggplot(df4,aes(x=Country,y=`Overall Score`,fill=Country))+
  geom_boxplot()+stat_summary(fun.y=mean,geom="point",shape=1,size=3,color="white")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = 'Boxplot Overall Score ',caption = 'Source : Kaggle',
       subtitle = "Berdasarkan Negara",
       x = 'Negara', y='Jumlah')

data<-topuni[topuni$Country=="Indonesia",]
data2<-data[data$`Institution Name`=="University of Indonesia"|data$`Institution Name`=="Universitas Gadjah Mada"|data$`Institution Name`=="Bandung Institute of Technology (ITB)", ]
ggplot(data2,aes(x=`Institution Name`,y=`Overall Score`,fill=`Institution Name`))+
  geom_bar(stat="identity",width=0.5)+
  geom_text(aes(label=`Overall Score`), vjust=1.6, color="black", size=3.5)+
  theme(legend.position = "none",axis.text.x = element_text(hjust = 0.5, vjust = 0.5))+
  labs(title = 'Barplot Overall Score ',caption = 'Source : Kaggle',
       subtitle = "Universitas di Indonesia",
       x = 'Negara', y='Jumlah')


topuni$`Employer Reputation`=as.numeric(topuni$`Employer Reputation`)
ggplot(topuni,aes(x=`Institute Age`,y=`Employer Reputation`, color=`Institute Age`)) + 
  geom_jitter()+
  labs(title = 'Jitter Plot Employer Reputation',caption = 'Source : Kaggle',
       subtitle = "Berdasarkan usia universitas",
       x = 'Usia Universitas', y='Employer Reputation', color="Usia Universitas")

dfbaru<-topuni[topuni$`Classification`=="S"|topuni$`Classification`=="M"|topuni$`Classification`=="L"|topuni$`Classification`=="XL", ]
topuni$`International Students`=as.numeric(topuni$`International Students`)
ggplot(dfbaru,aes(x=Classification,y=`International Students`,fill=`Classification`))+
  geom_boxplot()+
  stat_summary(fun=mean,geom="point",shape=1,size=3,color="white")+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5))+
  labs(title = 'Boxplot Ukuran Universitas',caption = 'Source : Kaggle',
       x = 'Ukuran Universitas', y='Jumlah Mahasiswa Internasional')+
  scale_x_discrete(limits=c("S", "M", "L","XL"))+
  scale_fill_discrete(name = "Klasifikasi Ukuran", labels = c("S :<5.000 students", "M: >= 5.000 students", "L: >= 12.000 students","XL: > 30.000 students"))

ggplot(topuni,aes(x=`Institute Age`,y=`Employer Reputation`, color=`Institute Age`))+ 
  geom_jitter()+
  labs(title = 'Employer Reputation',caption = 'Source : Kaggle',
       subtitle = "Berdasarkan usia universitas",
       x = 'Usia Universitas', y='Employer Reputation', color="Usia Universitas")+
  geom_hline(yintercept=47.5, color="orange", size=1) + 
  geom_vline(xintercept=5, color="orange", size=1)+
  annotate("segment", x = 5, xend = 2.5, y = 47.5, yend = 80, colour = "pink", size=3, alpha=1, arrow=arrow())+
  annotate("text", label = "Universitas Indonesia (5:17.5)", x = 2.5, y = 85, size = 3, colour = "red")

topuni$`Overall Score`=as.numeric(topuni$`Overall Score`)
top10<-topuni[1:10,]
win.graph()
ggplot(top10,aes(x=`Institution Name`,y=`Overall Score`,fill=`Overall Score`))+
  geom_bar(stat="identity", fill="steelblue",width=0.5)+
  geom_text(aes(label=`Overall Score`), vjust=-1.5, hjust=1,color="brown", size=3)+
  theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5))+
  labs(title = 'Overall Score',subtitle= 'Top 10 University',caption = 'Source : Kaggle',
       x = 'Universitas', y='Jumlah')+coord_flip()

topuni$`Academic Reputation`=as.numeric(topuni$`Academic Reputation`)
topuni$`Faculty Student`=as.numeric(topuni$`Faculty Student`)
topuni$`Citations per Faculty`=as.numeric(topuni$`Citations per Faculty`)
topuni$`Citations per Faculty`=as.numeric(topuni$`Citations per Faculty`)
topuni$`International Faculty`=as.numeric(topuni$`International Faculty`)
dfbaru<-topuni %>% select (`Institute Age`,`Academic Reputation`,`Employer Reputation`,`Faculty Student`,`Citations per Faculty`,`International Faculty`,`International Students`,`Overall Score`)
dfbaru<-na.omit(dfbaru)
M<-cor(dfbaru)
library(corrplot)
library(RColorBrewer)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
win.graph()
corrplot(M, method = "color", col = col(200),
         type = "lower", order = "hclust", number.cex = .7,
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 30,
        sig.level = 0.01, insig = "blank", 
         diag = FALSE)

plot(topuni$`Academic Reputation`, topuni$`Overall Score`,
     xlab = 'Academic Reputation',
     xaxt='n',
     ylab = 'Overall Score',
     yaxt='n',
     col = ifelse(topuni$Country=='Indonesia', 'red','green'),
     col.lab='#1e6091',
     col.axis= '#1e6091',
     cex.axis = 0.8,
     cex.lab = 0.8,
     font.lab = 2,
     pch = 19,
     frame.plot = F)
mtext('QS World University Rangkings', side=3, at=1, adj=0, cex=1.2,
      font = 2, col = '#1e6091')
mtext('Year 2020', side=3, at=1, adj=0, cex=0.75, line = -0.88,
      font = 2, col = '#1e6091')
text(40, 30, 'Indonesia', cex = 0.6, col = 'red')


axis(1, col = '#1e6091', col.axis= '#1e6091', cex.axis = 0.8, 
     at = seq(0, 100, 10),
     labels = c('0', '10', '20', '30', '40', '50','60','70','80','90','100'))
axis(2, col = '#1e6091', col.axis= '#1e6091', cex.axis = 0.8,at = seq(0, 100, 10),
     labels = c('0', '10', '20', '30', '40', '50','60','70','80','90','100'))



data=topuni[topuni$Country=="Indonesia",]
data9<-data %>% group_by(Classification) %>% summarise(Jumlah = n())%>%arrange(desc(Jumlah));
data9 = data9 %>%
  mutate(percent = round((Jumlah / sum(Jumlah)) * 100, 2))
data9
data9 <- data9 %>%
  arrange(desc(Classification)) %>%
  mutate(lab.ypos = cumsum(percent) - 0.5*percent)
data9
mycols <- c("#32CD32", "#9370DB")
ggplot(data9, aes(x = "", y = percent, fill = Classification)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = percent), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+labs(title="Pie chart Universitas di Indonesia", subtitle="berdasarkan Classification")
