library(readxl)
library(ggplot2)
library(dplyr)
data=read_excel("NY property 1 million.xlsx")

# 1. manipulate originla dataset type
str(data)

data$LTDEPTH=as.numeric(data$LTDEPTH)
data$LTFRONT=as.numeric(data$LTFRONT)
data$STORIES=as.numeric(data$STORIES)
data$BLDFRONT=as.numeric(data$BLDFRONT)
data$BLDDEPTH=as.numeric(data$BLDDEPTH)
data$BLOCK=as.character(data$BLOCK)
data$EXCD1=as.numeric(data$EXCD1)
data$EXCD2=as.numeric(data$EXCD2)
## batch manipulation 
t(data%>%
    select(everything())%>%
    summarize_all(funs(sum(is.na(.)/nrow(data))))
    )

nadetect=function(x){
  sum(is.na(x))/length(x)
}
sapply(data,nadetect)

# 2. split into categorical and numerical dataset
nu=data[,c("EXCD1","EXCD2","LTFRONT","LTDEPTH","STORIES","FULLVAL","AVLAND","AVTOT","EXLAND","EXTOT","BLDFRONT","BLDDEPTH","AVLAND2","AVTOT2","EXLAND2","EXTOT2")]
cate=data[,c("BLOCK","LOT","RECORD","BBLE","OWNER","EASEMENT","BLDGCL","TAXCLASS","STADDR","ZIP","EXMPTCL","PERIOD","YEAR","VALTYPE")]


# 3. summary statistics
sd(data[!is.na(data$STORIES),]$STORIES)  # filter NA for sd


# 4. numerical dataset
str(nu)

##EXCD1: discrete
summary(nu$EXCD1)
length(unique(data$EXCD1))
nrow(data[is.na(data$EXCD1),])
ggplot(numeric,aes(x=1,y=EXCD1))+
  geom_boxplot()+
  coord_flip()

ggplot(nu,aes(x=EXCD1))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  labs(y="Density",title="Distribution of EXCD1")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(EXCD1)) %>%
  group_by(EXCD1) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(EXCD1,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="EXCD1",title="Distribution of Top 20 EXCD1 Without NAs")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))

## EXCD2 
summary(data$EXCD2)
length(unique(data$EXCD2))
ggplot(nu,aes(x=EXCD2))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,10000))+
  labs(y="Density",title="Distribution of EXCD2")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(EXCD2)) %>%
  group_by(EXCD2) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x=reorder(EXCD2,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="EXCD2",title="Distribution of Top 10 EXCD2 Without NAs")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))

## LOT: continuous
summary(numeric$LOT)
length(unique(data$LOT))
ggplot(numeric,aes(x=1,y=LOT))+
  geom_boxplot()+
  coord_flip()

ggplot(numeric,aes(x=LOT))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,500))+
  labs(y="Density",title="Distribution of LOT")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(LOT)) %>%
  group_by(LOT) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(LOT,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="LOT",title="Distribution of Top 20 LOT")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))


## LTFRONT:con
summary(numeric$LTFRONT)
length(unique(data$LTFRONT))
ggplot(numeric,aes(x=LTFRONT))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,200))+
  labs(y="Density",title="Distribution of LTFRONT")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(LTFRONT)) %>%
  group_by(LTFRONT) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(LTFRONT,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="LTFRONT",title="Distribution of Top 20 LTFRONT")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))
## LTDEPTH:con
summary(data$LTDEPTH)
ggplot(numeric,aes(x=LTDEPTH))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,200))+
  labs(y="Density",title="Distribution of LTDEPTH")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(LTDEPTH)) %>%
  group_by(LTDEPTH) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(LTDEPTH,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="LTDEPTH",title="Distribution of Top 20 LTDEPTH")+
  theme(plot.title = element_text(hjust = 0.5))

## STORIES:??
summary(data$STORIES)

ggplot(nu,aes(x=STORIES))+
  geom_histogram(aes(y=..density..),bins=50,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,10))+
  labs(y="Density",title="Distribution of STORIES")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(STORIES)) %>%
  group_by(STORIES) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(STORIES,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="STORIES",title="Distribution of Top 20 STORIES Without NAs")+
  theme(plot.title = element_text(hjust = 0.5))

## FULLVAL:con
summary(data$FULLVAL)

ggplot(numeric,aes(x=FULLVAL))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,5e+06))+
  labs(y="Density",title="Distribution of FULLVAL")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(FULLVAL)) %>%
  group_by(FULLVAL) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(FULLVAL,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="FULLVAL",title="Distribution of Top 20 FULLVAL")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))

## AVLAND:con
summary(data$AVLAND)

ggplot(numeric,aes(x=AVLAND))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,1e+05))+
  labs(y="Density",title="Distribution of AVLAND")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(AVLAND)) %>%
  group_by(AVLAND) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(AVLAND,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="AVLAND",title="Distribution of Top 20 AVLAND")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))

##AVTOT:con
summary(data$AVTOT)

ggplot(numeric,aes(x=AVTOT))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,3e+05))+
  labs(y="Density",title="Distribution of AVTOT")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(AVTOT)) %>%
  group_by(AVTOT) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(AVTOT,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="AVTOT",title="Distribution of Top 20 AVTOT")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))
##EXLAND:??
summary(data$EXLAND)
length(unique(data$EXLAND))

ggplot(nu,aes(x=EXLAND))+
  geom_histogram(aes(y=..density..),bins=50,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,1e+04))+
  labs(y="Density",title="Distribution of EXLAND")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(EXLAND)) %>%
  group_by(EXLAND) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x=reorder(EXLAND,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="EXLAND",title="Distribution of Top 10 EXLAND")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))

##EXTOT:??
summary(data$EXTOT)

ggplot(numeric,aes(x=EXTOT))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,1e+04))+
  labs(y="Density",title="Distribution of EXTOT")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(EXTOT)) %>%
  group_by(EXTOT) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x=reorder(EXTOT,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="EXTOT",title="Distribution of Top 10 EXTOT")+
  theme(plot.title = element_text(hjust = 0.5))

##BLDFRONT
summary(data$BLDFRONT)

ggplot(numeric,aes(x=BLDFRONT))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,40))+
  labs(y="Density",title="Distribution of BLDFRONT")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(BLDFRONT)) %>%
  group_by(BLDFRONT) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(BLDFRONT,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="BLDFRONT",title="Distribution of Top 20 BLDFRONT")+
  theme(plot.title = element_text(hjust = 0.5))

##BLDDEPTH
summary(data$BLDDEPTH)

ggplot(nu,aes(x=BLDDEPTH))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,120))+
  labs(y="Density",title="Distribution of BLDDEPTH")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(BLDDEPTH)) %>%
  group_by(BLDDEPTH) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(BLDDEPTH,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="BLDDEPTH",title="Distribution of Top 20 BLDDEPTH")+
  theme(plot.title = element_text(hjust = 0.5))

##AVLAND2
summary(data$AVLAND2)

ggplot(numeric,aes(x=AVLAND2))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,3e+05))+
  labs(y="Density",title="Distribution of AVLAND2")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(AVLAND2)) %>%
  group_by(AVLAND2) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(AVLAND2,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="AVLAND2",title="Distribution of Top 20 AVLAND2 Without NAs")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))

##EXLAND2
summary(data$EXLAND2)

ggplot(numeric,aes(x=EXLAND2))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,5e+04))+
  labs(y="Density",title="Distribution of EXLAND2")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(EXLAND2)) %>%
  group_by(EXLAND2) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x=reorder(EXLAND2,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="EXLAND2",title="Distribution of Top 10 EXLAND2 Without NAs")+
  theme(plot.title = element_text(hjust = 0.5))

##EXTOT2
summary(data$EXTOT2)

ggplot(nu,aes(x=EXTOT2))+
  geom_histogram()+
  scale_x_continuous(limits=c(0,4e+05))+
  scale_y_continuous(limits = c(0,12000))

ggplot(numeric,aes(x=EXTOT2))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,4e+05))+
  labs(y="Density",title="Distribution of EXTOT2")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(EXTOT2)) %>%
  group_by(EXTOT2) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x=reorder(EXTOT2,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="EXTOT2",title="Distribution of Top 10 EXTOT2 Without NAs")+
  theme(plot.title = element_text(hjust = 0.5))

##AVTOT2
summary(data$AVTOT)

ggplot(nu,aes(x=AVTOT2))+
  geom_histogram(aes(y=..density..),bins = 30,fill="#999999")+
  geom_line(stat="density",col="#ef8a62",size=1)+
  scale_x_continuous(limits=c(0,2.5e+05))+
  labs(y="Density",title="Distribution of AVTOT2")+
  theme(plot.title = element_text(hjust = 0.5))

nu %>%
  filter(!is.na(AVTOT2)) %>%
  group_by(AVTOT2) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x=reorder(AVTOT2,-count),y=count))+
  geom_histogram(stat="identity",fill="#999999")+
  labs(x="AVTOT2",title="Distribution of Top 10 AVTOT2 Without NAs")+
  theme(axis.text.x = element_text(angle=45))+
  theme(plot.title = element_text(hjust = 0.5))


# 5. categorical dataset
## TAXCLASS
unique(cate$TAXCLASS)

cate %>%
  group_by(TAXCLASS) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=reorder(TAXCLASS,-count),y=count))+
           geom_bar(stat="identity")+
           xlab("Taxclass")+
           ggtitle("Distribution of Taxclass")
## BLOCK
summary(numeric$BLOCK)

ggplot(numeric,aes(x=BLOCK))+
  geom_histogram(aes(y=..density..),bins = 15,fill="#9ebcda")+
  geom_line(stat="density",col="#8856a7")+
  scale_x_continuous(breaks=seq(0,17500,2500))

numeric %>%
  group_by(BLOCK) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(BLOCK,-count),y=count))+
  geom_histogram(stat="identity")+
  theme(axis.text.x = element_text(angle=45))

## OWNER
length(unique(cate$OWNER))
colSums(is.na(cate[,"OWNER"]))/nrow(data)
cate %>%
  filter(!is.na(OWNER))%>%
  group_by(OWNER) %>%
  summarize(count=n()) %>%
  arrange(-count) %>%
  head(20) %>%
  ggplot(aes(x=reorder(OWNER,count),y=count))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=90))+
  coord_flip()+
  labs(x="OWNER",title="Distribution of Owner(NA excluded)")







