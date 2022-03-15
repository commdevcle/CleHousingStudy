data("penguins")
view(penguins)

view(diamonds)
#-- cut - market status, fill = housing condition
ggplot (data=diamonds) +
  geom_bar(mapping=aes(x=cut, fill=clarity))
