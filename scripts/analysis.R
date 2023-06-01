
#Run a prep script that includes necessary packages and some formatting thingies
source(here::here("prep/prep.R"))
source(here::here("prep/clean_data.R"))


#plot a map of Haiti with the participants' locations
#at each of the three cities
ggplot(hti) +
  geom_sf(fill = my_pal[[15]]) +
  geom_sf(data = nodes_sf
          , aes(geometry = st_jitter(geometry
                                     , amount = .05))
          , alpha = .8
          , color = my_pal[[1]]) +
  geom_sf_text(data = nodes_sf
               , aes(label = name
                     , st_jitter(geometry
                                 , amount = 1))
               , color = my_pal[[5]]
               , alpha = .7)+
  labs(title = "Haiti: Locations of Capacity Building \nParticipants"
       , subtitle = "Round 1 data collection, 2021") +
  theme_void() +
  theme(panel.background = ggplot2::element_blank()
        , plot.title.position = "plot" #Pushes the title to the very left of the plot window
        , plot.title = element_text(size = 26, family = "Corbel", color = "#000000")
        , plot.subtitle = element_text(size = 18, family = "Corbel", color = "#A9A9A9")
        , strip.background = ggplot2::element_rect(fill = "white") 
        , axis.text = element_blank () #ggplot2::element_text(size = 14, family = "Corbel", hjust = 0, color = "#000000")
        , plot.caption = ggplot2::element_text(size = 10, family = "Corbel", color = "#000000")
        , plot.background = element_rect("white"))

ggsave(filename = "viz/Haiti map.png"
       , device = "png"
       , height = 6
       , width = 8
       , unit = "in")

#Plot the counts of softwares used by participants
ggplot(data = sw_count
       , aes(reorder(factor(software), -n), n)) +
  geom_point(size = 14, color = my_pal[[2]]) +
  geom_segment(aes(x = factor(sw_count$software), xend = factor(sw_count$software) 
                   , y = 0, yend = sw_count$n)
               , linewidth = 2
               , color = my_pal[[2]]
               , alpha = .7) +
  geom_text(aes(x = software, y = n, label = n)
            , color = "white") +
  labs(title = "Most used analytic software packages"
       , subtitle = "Excel, R, and Stata are the most commonly used."
       , y = "Number of staff"
       , x = "Software") + 
  scale_y_continuous(limits = c(0, 10)
                     , breaks = c(0, 5, 10))+
  theme.plot() +
  theme(axis.text.x=element_text(hjust=0.5))

#save the plot to the viz folder
ggsave(filename = "viz/Software counts.png"
       , device = "png"
       , height = 6
       , width = 8
       , unit = "in")

#use the g_mentor graph object to visualize our network
ggraph(g_mentor, layout = "with_kk") +
  geom_edge_link(color = my_pal[[5]]
                 , alpha = .3) +
  geom_node_point(color = my_pal[[5]]
                  , size = 8) +
  geom_node_text(aes(label = name)
                 , color = "white")+
  labs(title = "Capacity building network participants") +
  theme(legend.position = "none")+
  theme.graph()

#save the plot to the viz folder
ggsave(filename = "viz/network.png"
       , device = "png"
       , height = 6
       , width = 8
       , unit = "in")

###Network statistics----

#Calculate some network statistics - density, mean distance,
#betweenness, and eigenvector centrality
#calculate the network density
density_ment <- edge_density(g_mentor)
#calculate the mean_distance
avg_dist_ment <- mean_distance(g_mentor, directed = FALSE)
#calculate the betweeness
b1 <- betweenness(g_mentor, directed = FALSE)
between_ment <- mean(b1)
#eigenvalues vector
e1 <- eigen_centrality(g_mentor, directed =FALSE)$vector
#calculate avg. eigenvalue of the network
eigen_ment <- mean(e1)
l <- c("Network", "Avg. Density", "Avg. Distance", "Avg. Betweenness", "Avg. Eigenvector")
m <- c("Capacity building network", density_ment, avg_dist_ment, between_ment, eigen_ment)
dat <- data.frame(l, m)
dat1 <- pivot_wider(data = dat
                    , names_from = l
                    , values_from = m) |>
  mutate(across(2:5, as.numeric))

#graph statistics table
set_flextable_defaults(background.color = "white")

network_table <- flextable(dat1) |>
  #set_header_labels(values = dat1) |> 
  align(align = "center", part = "all") |>
  colformat_double(digits = 2)

network_table

#save table
save_as_image(network_table
              , path="viz/network stats table.png")

#simulation of mentorship network 1000x
gl <- vector('list', 1000)

#a for loop to do this programmatically based off the g_mentor object
for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(
    n = gorder(g_mentor)
    , p.or.m = edge_density(g_mentor)
    , type = "gnp"
  )
}

#Then use the gl object to calculate averages for each of the stats presented above on the original two networks.

gl_ment_avg_density <- mean(unlist(lapply(gl, edge_density)))
gl_ment_avg_dist <- mean(unlist(lapply(gl, mean_distance, directed = FALSE)))
gl_ment_avg_between <- mean(unlist(lapply(gl, betweenness)))


#This one takes a few more steps 
gl_eigen_ment <- do.call(rbind, lapply(gl, eigen_centrality, directed = FALSE)) |>
  #do.call(rbind, .data) |>
  data.frame() 

gl_ment_avg_eigen <- mean(unlist(gl_eigen_ment$vector))


#make a flextable
names <- c("Network", "Avg. Density", "Avg. Distance", "Avg. Betweenness", "Avg. Eigenvector")

dat_ment <- c("Capacity building network", density_ment, avg_dist_ment,  between_ment, eigen_ment)

sim_ment <- c("Simulated networks", gl_ment_avg_density, gl_ment_avg_dist, gl_ment_avg_between, gl_ment_avg_eigen)


dat_all <- data.frame(names, dat_ment, sim_ment)

dat_all1 <- t(dat_all) |>
  data.frame()

names(dat_all1) <- dat_all1[1,]

dat_all2 <- dat_all1[2:5,] |>
  mutate(across(2:5, as.numeric))

set_flextable_defaults(background.color = "white")

network_table2 <- flextable(dat_all2) |>
  set_header_labels(values = names(dat_all2)) |>
  align(align = "center", part = "all") |>
  colformat_double(digits = 2) |>
  italic(i = c(2,4)) |>
  bold(i = c(1,3))

network_table2

save_as_image(network_table2, path = "viz/sim networks comp table.png")

#One example plotted to show what's happening
gl.apls = unlist(lapply(gl, mean_distance, directed=FALSE))

ggplot(data = as.data.frame(gl.apls)) +
  geom_histogram(aes(x = gl.apls), bins = 25
                 , fill = my_pal[[11]]
                 , alpha = .4) +
  geom_vline(xintercept = avg_dist_ment
             , linewidth = 1
             , linetype = "dashed"
             , color = my_pal[[14]]) +
  annotate(x = avg_dist_ment + 1.2, y = 135, label = glue::glue("{round(avg_dist_ment, digits = 2)}, is the average distance \nbetween connections \nin the mentoring network"), geom = "label", color = my_pal[[14]]) +
  geom_segment(aes(x = avg_dist_ment + .6, y = 145, xend = avg_dist_ment +.05, yend = 138),
               arrow = arrow(length = unit(0.25, "cm"))
               , linewidth = 1
               , color = my_pal[[14]])+
  labs(y = "# of simulations"
       , x = "Average distance between connections"
       , title = "How does the capacity building network \ncompare to similarly structured networks?"
       , subtitle = glue::glue("{mean(gl.apls < avg_dist_ment) * 100}", "% ", "of similar networks have a shorter average distance between \nconnections than the mentoring network"))+
  theme.plot()

ggsave("viz/sim comparison networks.png"
       , device = "png"
       , height = 6
       , width = 8)
