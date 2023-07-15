library(ggplot2)
library(grid)
library(lubridate)
library(gridExtra)
library(tidyverse) 
library(stringr) 
library(usmap) 


b <- choose.files(default="", caption="Select consumer complaints file",
                  multi=FALSE, filters=Filters,
                  index=nrow(Filters))

df <- read.csv(b)

names(df)[1] <- "Date"
df1 <- subset(df, Date < "2022-01-01")
df1 <- df1[order(df1$Date,decreasing = TRUE),]

df1$Year <- format(as.Date(df1$Date), "%Y")
df1$count <- ifelse(df1$Year==df1$Year,1,0)
df1$Company <- str_to_upper(df1$Company)


#Line graph

Trend <- df1[, c("Year","count")]
Year <- Trend %>% 
  group_by_if(is.numeric %>% Negate) %>%
  summarize_all(sum)
Year$count <- Year$count/1000

ggplot(data=Year, aes(x=Year, y=count,group=2, label=count)) +
  geom_line(color="red")+
  labs(x="Year", y="Complaints(in 1000)")+
  geom_text(nudge_y = 2)+
  ggtitle("Number of Complaints over the Years")+
  theme_bw(base_size = 9)


#type of complaints

Type <- df1[, c("Product","count")]
T <- Type %>% 
  group_by_if(is.numeric %>% Negate) %>%
  summarize_all(sum)

Types <- ggplot(data=T, aes(x=forcats::fct_reorder(Product,count),y=count)) +
  geom_bar(stat="identity",fill="red",position="dodge") + coord_flip()+
  labs(x="Product", y="Count")+
  ggtitle("Types of Complaints")+
  scale_y_continuous(labels=function(count) 
    format(count, big.mark = ",", scientific = FALSE))+
  geom_text(aes(x = Product, y = count, label = scales::comma(count)), 
            hjust = -0.01, size = 2,
            position = position_dodge(width = 0.5),inherit.aes = TRUE)+
  theme_bw(base_size=8)

Type1 <- df1[, c("Year","Product","count")]

T1 <- Type1 %>% 
  group_by_if(is.numeric %>% Negate) %>%
  summarize_all(sum)

T1$type <- ifelse(T1$Product=="Credit reporting, credit repair services, or other personal consumer reports"|
                    T1$Product=="Mortgage"|
                    T1$Product=="Debt collection","Yes","No")
Top3type <- subset(T1,type=="Yes")

Top3<- ggplot(Top3type,aes(Year,count,fill=Product))+
  geom_bar(stat="identity",position='dodge')+
  ggtitle("Top 3 complaints over the years")+
  scale_y_continuous(labels=function(count) 
    format(count, big.mark = ",", scientific = FALSE))+
  theme_bw(base_size=9)

graph <- grid.arrange(Types,Top3, ncol=1, 
                      top = textGrob("Type of Complaints over the Years",
                                     gp=gpar(fontsize=15,font=2)))

#mortgage
Mortgage <- subset(df1, Product == "Mortgage")
Mortgage <- Mortgage[,c("Issue","count")]
M <- Mortgage %>% 
  group_by_if(is.numeric %>% Negate) %>%
  summarize_all(sum)


ggplot(data=M, aes(x=forcats::fct_reorder(Issue,count),y=count)) +
  geom_bar(stat="identity",fill="blue",position="dodge") + coord_flip()+
  labs(x="Issue types", y="count")+
  ggtitle("Type of Mortgage Issue ")+
  scale_y_continuous(labels=function(count) 
    format(count, big.mark = ",", scientific = FALSE))+
  geom_text(aes(x = Issue, y = count, label = scales::comma(count)), 
            hjust = -0.01, size = 2,
            position = position_dodge(width = 0.5),inherit.aes = TRUE)+
  theme_bw(base_size=8)

#top ten companies

companies <- df1[, c("Company","count")]
C <- companies %>% 
  group_by_if(is.numeric %>% Negate) %>%
  summarize_all(sum)
C <- C[order(C$count,decreasing = TRUE),] 

TopTen<- head(C,10)
TopTen$count <- format(TopTen$count, big.mark = ",", scientific = FALSE)

mytheme <- ttheme_default(core = list(fg_params = list(fontsize=9)),
                          colhead = list(fg_params = list(fontsize=10, 
                                                          fontface="bold")))
Ten <- tableGrob(TopTen, theme=mytheme, rows=NULL)

C1 <- df1[, c("Year","Company","count")]
C2 <- C1 %>% 
  group_by_if(is.numeric %>% Negate) %>%
  summarize_all(sum)
C2 <- C2[order(C$count,decreasing = TRUE),] 

C2$top<-ifelse(C2$Company== "EQUIFAX, INC." | 
                 C2$Company== "TRANSUNION INTERMEDIATE HOLDINGS, INC."| 
                 C2$Company== "EXPERIAN INFORMATION SOLUTIONS INC."| 
                 C2$Company== "BANK OF AMERICA, NATIONAL ASSOCIATION"| 
                 C2$Company== "WELLS FARGO & COMPANY"| 
                 C2$Company== "JPMORGAN CHASE & CO."| 
                 C2$Company== "CITIBANK, N.A."| 
                 C2$Company== "CAPITAL ONE FINANCIAL CORPORATION"|
                 C2$Company== "NAVIENT SOLUTIONS, LLC."|
                 C2$Company== "SYNCHRONY FINANCIAL" ,"Yes","No") 

Top10 <- subset(C2,top=="Yes")

TT <- ggplot(Top10, aes(x = Year, y = count, colour = Company, group = Company)) +
  geom_line(position = position_dodge(0.2))+
  labs(title="Time Series of Top Ten Companies")+
  scale_y_continuous(labels=function(count) 
    format(count, big.mark = ",", scientific = FALSE))+
  theme_bw(base_size=9)

grid.arrange(
  arrangeGrob(Ten,top=textGrob("Top Ten Companies Recevied Most Complaints",
                               gp=gpar(fontsize=10,font=1))),
  arrangeGrob(TT),top ="Top Ten Most Received Companies Over the Years", nrow=2)


#map

state <- aggregate(df1$count~df1$State,data=df1,FUN=sum, na.rm=TRUE)
s <- state[-c(1,2,3,6,16,30,34,56), ]
names(s)[1] <- "state"
names(s)[2] <- "Count"

plot_usmap(data=s,values="Count",color="orange",labels=TRUE)+
  scale_fill_continuous(low="white",high="red",name="Complaint amount",
                        label=scales::comma)+
  labs(title="Number of Complaints by States",
       subtitle="Count by each state",caption="Source of data: Data.gov ")+
  theme(legend.position = "right")










