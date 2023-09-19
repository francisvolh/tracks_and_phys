data.mock <- data.frame(
  x = c(1,2,3,4,5,6,7,8,9,10 ),
  y = c(1,2,3,4,5,6,7,8,9,10 )
)


op.for.theor <- ggplot(data.mock, aes(x, y))+
  #geom_point()+
  geom_abline(intercept = 2, slope=.6, color = "blue")+
  geom_abline(intercept = 2.75, slope=.4, color = "red")+
  geom_vline(xintercept = 0, #linetype="dotted", 
             color = "black"#, size=1
             )+
  geom_curve(
    aes(x = 0, y = 0, xend = 11, yend = 5.25),
    #arrow = arrow(
     # length = unit(0.5, "cm")#, 
      #type="both" # Describes arrow head (open or closed)
    #),
    colour = "black",
    size = 0.6,
    angle = 90,
    curvature = -0.415# Anything other than 90 or 0 can look unusual
  )+
  xlim(c(-8,12))+
  #ylim(0,11)+
  scale_y_continuous(limits=c(0, 10), expand=expansion(mult=c(0, 0.05)))+
  geom_segment(aes(x = -6.65, y = 0.15, xend = 0, yend = 0.15),
               arrow = arrow(length = unit(.25, "cm"), ends = "both"))+
  geom_segment(aes(x = -3.25, y = 0.35, xend = 0, yend = 0.35),
               arrow = arrow(length = unit(.25, "cm"), ends = "both"))+
  geom_vline(xintercept = 3.75, linetype="dotted", 
             color = "blue"#, size=1
  )+
  geom_vline(xintercept = 5.25, linetype="dotted", 
             color = "red"#, size=1
  )+
  ylab("Energy intake")+
  xlab(NULL)+
  annotate("text", x = -1.65, y = .65, label = "short trip")+
  theme(plot.margin = unit(c(0, 0, 0.5, 0),
                           "inches"))+
  annotate("text", x = -4.75, y = .45, label = "long trip")+
  theme(plot.margin = unit(c(0, 0, 0.5, 0),
                           "inches"))+
  labs(tag = c("Trip time                                               Patch time")) +
  theme(plot.tag.position = c(0.45, -0.05))+
  annotate("text", x = 1.25, y = 5.45, 
           label =  expression("max. net gain for long trip" %->% ""),
           color = "blue")+
  annotate("text", x = 7.75, y = 9, 
           label = expression("" %<-% "max. net gain for long trip"), 
           color = "red")

op.for.theor

ggsave("plots/op.for.theor.png", op.for.theor,  units = 'cm', width = 25, height = 16)  

getwd()  
  






