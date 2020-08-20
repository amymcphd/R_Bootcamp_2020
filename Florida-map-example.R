


fl.data <- map_data("county", region="FL") %>%
    mutate( Rate = runif(n(), 0, .5)
          , cluster = sample(letters[1:4], n(), replace=TRUE)
          )


ggplot()+
    geom_polygon(data=fl.data, aes(x=long, y=lat, group=group, fill=Rate), col='black') +
    coord_map() +
    scale_fill_gradient(high = 'dodgerblue4', low='white')


ggplot()+
    geom_polygon(data=fl.data, aes(x=long, y=lat, group=group, fill=cluster), col='black') +
    coord_map() +
    scale_fill_manual(values = c('red', 'blue', 'gold', 'darkgreen') )
