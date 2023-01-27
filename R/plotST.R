ts.union(tsweeks[[30]],tsweeks[[40]])


autoplot(ts.union(tsweeks[[20]],tsweeks[[34]],tsweeks[[36]],tsweeks[[51]],tsweeks[[59]]), facets = FALSE)+ 
  scale_color_manual(labels = c(names(tsweeks[20]),names(tsweeks[34]),names(tsweeks[36]),names(tsweeks[51]),names(tsweeks[59])),
                     values=c("salmon", "aquamarine4","chartreuse3","deepskyblue3","hotpink1")) + xlab("setmana")+
  ylab("nombre de recàrregues")+
  labs (title = "sèrie de temps per codi municipi")



+
  aes(linetype = plot_group,
      size = plot_group) +
  scale_linetype_manual(labels = c("Actual", "Forecasted"),
                        values = c(1, 2)) +
  scale_size_manual(labels = c("Actual", "Forecasted"),
                    values = c(1, 2))



p <- autoplot(ts.union(tsweeks[[30]],tsweeks[[40]]), facets = FALSE)
p$data



autoplot(ts.union(tsweeks), facets = FALSE)
