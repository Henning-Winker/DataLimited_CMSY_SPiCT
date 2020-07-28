library(reshape2)
library (ggplot2); theme_set(theme_bw())
library (ggrepel)

load("Data/Assessment Time Series Results.RData")
res <- res[res$Stock != "Cod7a", ]

test <- res[res$Year == 2017 & res$Stock !="Whg7a",]
test2 <- res[res$Year == 2015 & res$Stock =="Whg7a" ,]
test <- rbind (test, test2)
test_ICES <- test[test$Method== "ICES",]
test <- test[test$Method != "ICES",]

test3 <- merge(test, test_ICES, by = c("Stock", "Year", "variable"))
colnames(test3) <- c("Stock", "Year", "variable", "Method", "Est.Value", "ICES", "ICES.Value")

#######################################################
### Try a similar plot but relative to ICES rather than the refpoints

test3$Diff = test3$Est.Value - test3$ICES.Value

test4 <- dcast(test3, Stock + Year +  Method ~ variable)

# pdf("Kobe plot Est-ICES_July_annotations.pdf", width =  6.69291)
# ggplot (test4, aes(x= B_BMSY, y = F_FMSY, label = Stock, colour = Method))+
#   scale_color_manual(values=c("#D55E00", "#56B4E9"))+
#   geom_point(size = 2)+
#   geom_hline(yintercept = 0)+
#   geom_vline(xintercept = 0)+
#   xlab(expression(Assessed~B/B[MSY]~-~ICES~B/B[MSY]))+
#   ylab(expression(Assessed~F/F[MSY]~-~ICES~F/F[MSY]))+
#   geom_text_repel (aes(x= B_BMSY, y = F_FMSY, label = Stock), alpha = 0.5)+
#   theme(legend.position = c(0.9,0.65), legend.title = element_blank())+
#   annotate("text", label = expression (overestimates~F/F[MSY]~and), x = 2.38, y= 4.5, size = 3.5)+
#   annotate("text", label = expression (overestimates~B/B[MSY]), x = 2.38, y= 4, size = 3.5)+
#   annotate("text", label = expression (underestimates~F/F[MSY]~and), x = 2.38, y= -3.2, size = 3.5)+
#   annotate("text", label = expression (overestimates~B/B[MSY]), x = 2.38, y= -3.7, size = 3.5)+
#   annotate("text", label = expression (underestimates~F/F[MSY]~and), x = -0.9, y= -3.2, size = 3.5)+
#   annotate("text", label = expression (understimates~B/B[MSY]), x = -0.9, y= -3.7, size = 3.5)+
#   annotate("text", label = expression (overestimates~F/F[MSY]~and), x = -0.9, y= 4.5, size = 3.5)+
#   annotate("text", label = expression (understimates~B/B[MSY]), x = -0.9, y= 4, size = 3.5)
# dev.off()


annotations <- data.frame(
  xpos = c(-Inf,-Inf,-Inf,-Inf, Inf,Inf, Inf,Inf),
  ypos =  c(-Inf,-Inf, Inf,Inf, -Inf, -Inf, Inf, Inf),
  annotateText = c('Underestimates~F/F[MSY]', # BL
                   'Underestimates~B/B[MSY]', # BL
                   'Overestimates~F/F[MSY]',  # TL
                   'Underestimates~B/B[MSY]',  # TL
                   'Underestimates~F/F[MSY]', # BR
                   'Overestimates~B/B[MSY]', # BR
                   'Overestimates~F/F[MSY]', #TR 
                   'Overestimates~B/B[MSY]'), # TR
  hjustvar = c(-0.05, -0.05, -0.05, -0.05,  1.05, 1.05, 1.05, 1.05) ,
  vjustvar = c(-1.3, -0.2, 1.1, 2.2, -1.3, -0.2, 1.1, 2.2))


pdf("Kobe plot Est-ICES_July_annotations.pdf", width =  6.69291)
ggplot (test4)+
  scale_color_manual(values=c("#D55E00", "#56B4E9"))+
  geom_point(aes(x= B_BMSY, y = F_FMSY, colour = Method),size = 2)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlim (c(-1.2, 2.8))+
  ylim (c(-5.6, 4.3))+
  xlab(expression(Assessed~B/B[MSY]~-~ICES~B/B[MSY]))+
  ylab(expression(Assessed~F/F[MSY]~-~ICES~F/F[MSY]))+
  geom_text_repel (aes(x= B_BMSY, y = F_FMSY, label = Stock, colour = Method), alpha = 0.5)+
  theme(legend.position = c(0.92,0.65), legend.title = element_blank())+
  geom_text(data=annotations, aes(x=xpos,y=ypos,hjust=hjustvar,
                                    vjust=vjustvar,label= annotateText), parse = T)
dev.off()
