df = read.csv('https://raw.githubusercontent.com/bcdunbar/datasets/master/meteorites_subset.csv')


fig <- df %>% plot_mapbox(lat = ~reclat, lon = ~reclong,
                          split = ~type, size=2,
                          mode = 'markers',
                          marker= list(size=5)) 

fig <- fig %>% layout(title = 'Meteorites by Class',
                      font = list(color='white'),
                      plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                      mapbox = list(style = 'dark'),
                      legend = list(orientation = 'h',
                                    font = list(size = 8)),
                                    margin = list(l = 25, r = 25,
                                                  b = 25, t = 25,
                                                  pad = 2)) 


fig


p <- plot_ly(diam_ech, x = ~carat, y = ~price, type = "scatter", mode = "markers",
             hoverinfo = 'text',
             text = ~paste('Carat: ', carat,
                           '\n Price: ', price,
                           '\n Clarity: ', diam_ech$clarity),
             color = ~carat)