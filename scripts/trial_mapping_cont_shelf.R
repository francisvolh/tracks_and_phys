df <- data.frame(x=c(2, 3, 4, 3, 4, 5, 6),
                 y=c(6, 3, 5, 2, 8, 11, 10))


ggplot(data = df, aes(x, y)) +
  geom_point() +
  geom_circle(aes(x0=5, y0=8, r=1), linetype='dashed', color='red',
              fill='blue', lwd=1.5, inherit.aes=FALSE) +
  coord_fixed()

hidrology <-sf::st_read("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/geocatmin Peru/Hidrogeologia.shp")

ggplot() +
  geom_sf(data = hidrology, aes())


conti.shelf<- sf::st_read("C:/Users/francis van oordt/OneDrive - McGill University/Documents/McGill/Field data/Global Margin shape/ContinentalMargins.shp")

ggplot(data = conti.shelf)+
  geom_sf()

conti.shelf.crop <- sf::st_crop(conti.shelf,  xmin = -135, xmax =  -123, ymin = 48 , ymax = 56)

world <-sf::st_read("C:/Users/Francis van Oordt/OneDrive - McGill University/Documents/McGill/00Res Prop v2/Chap 0 BC isotopes/gadm36_CAN_shp/gadm36_CAN_0.shp")
world_crop <- sf::st_crop(world,  xmin = -135, xmax =  -123, ymin = 48 , ymax = 56)

ggplot()+
  geom_sf(data = conti.shelf.crop, fill = "white")+
  geom_sf(data = world_crop)


