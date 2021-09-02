# Packages, data and options ----------------------------------------------
Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
library(sf)
library(tidygraph)
# remotes::install_github("luukvdmeer/sfnetworks", "develop")
library(sfnetworks)
library(mapview)
library(ggplot2)
library(tmap)
library(magrittr)

# Load the street segments that define the (main roads of the) road network of
# the Isle of Wight (an Island located in the south of England) according to a
# driving mode of transport
iow <- st_read("https://github.com/sfnetworks/OGH2021/raw/master/isle-of-wight.gpkg", quiet = TRUE)

# Let's see a plot
par(mar = rep(0, 4))
plot(iow)

# We can already notice that there are several groups of segments that look
# disconnected from the main network.

# From LINESTRING(s) to sfnetwork object ----------------------------------

# Now we can convert the "raw data" into a sfnetwork object.
sfn <- as_sfnetwork(iow, directed = FALSE)

# and let's see the printing method
sfn

# We can notice that:
# 1. The output reports a short description of the sfnetwork object summarising,
# among the other things, the number of nodes and edges, the CRS, the number of
# components.
# 2. We know that the sfnetwork class inherits the tbl_graph class (see
# ?tbl_graph) which means that we can apply all tidygraph and igraph methods to
# sfnetwork objects (we will see several examples later). Moreover, we can see
# that, by default, the nodes represent the active geometry (more details later
# on).
# 3. The connectivity among the road segments is summarised using two columns in
# the edge table named "from" and "to".

# Moreover:
# 1. The nodes of the sfnetwork object are created at the (unique) endpoints of
# the input lines. This has importance consequences (see below).
# 2. The argument `directed = FALSE` forces the construction of an undirected
# network. See the introductory vignette for examples on how to create an
# sfnetwork object mixing one-way and two-way roads.
# 3. The are several other ways to create an sfnetwork object. See ?sfnetwork
# and ?as_sfnetwork for more examples.
# 4. We can adjust the number of features printed for the active and inactive
# tables using the options `sfn_max_print_active` and `sfn_max_print_inactive`.
# For example:
options(sfn_max_print_active = 2L, sfn_max_print_inactive = 1L)
sfn

# Warning: this is still an experimental feature that exist only in the develop
# branch.

# Again, let's see a plot
plot(sfn)

# The sfnetwork method for the plot function is a wrapper around
# plot.sfc_GEOMETRY, which means that we can adjust several aspects of the plot.
plot(sfn, cex = 3, lty = 2, pch = 46)

# There is also an autoplot method
autoplot(sfn) + theme_minimal()

# We will see more complex examples later on.

# Active and inactive geometries ------------------------------------------

# A sfnetwork object is a multitable object composed by two sf tables (one for
# the nodes and one for the edges). There is always one of the two tables which
# is labeled as "active". That table is the target for the data manipulation
# verbs.

# For example
options(sfn_max_print_active = 0L, sfn_max_print_inactive = 0L)
sfn

# You can switch the active table using the function "activate" or the shortcuts
# "%N>% and %E>% (just for pipe chains in interactive coding sessions)
activate(sfn, edges)

# Pre-processing steps ----------------------------------------------------

# As already mentioned, real-world spatial networks may require a bunch of
# pre-processing steps before we can apply sf or igraph algorithms. Hence, we
# implemented a collection of morphers (see below for more details) to process
# the input data:

sfn_clean <- sfn %E>%
  convert(to_spatial_subdivision) %>%
  arrange(edge_length()) %>%
  convert(to_spatial_simple) %>%
  convert(to_spatial_smooth) %>%
  convert(to_components) %>%
  convert(to_spatial_simple, .clean = TRUE)

# Check the companion slides and the package's vignettes for several toy
# examples showing the importance of these morphers.

# Let's check the printing
sfn_clean

# and a plot
plot(sfn_clean)

# A few comments:
# 1. The spatial and graph morphers (i.e. to_spatial_subdivision,
# to_spatial_simple, to_spatial_smooth, and to_components) are used to create
# temporary alternate representation of the input network. On the other hand,
# the function "convert()" is used to extract this temporary representation (or
# its first graph in case of multiple representations) and convert it into an
# sfnetwork object. See the fifth package vignette for more details and check
# `?convert`.
# 2. The "clean" version of the network is much simpler than the original
# one since we extracted only the nodes and edges that belong to the first
# component of the network. We can clearly notice that from the printing and
# plot methods.

# Computing graph measures ------------------------------------------------

# We can use some dplyr verbs (which are loaded by tidygraph) to modify the
# edges and nodes tables. For example, the following code computes some simple
# graph measures (e.g. vertex degree) and adds them to the corresponding tables:
sfn_clean <- sfn_clean %N>%
  mutate(degree = centrality_degree())

# We can add a column in the edges table measuring the length of each LINESTRING
# object:
sfn_clean <- sfn_clean %E>%
  mutate(weight = as.numeric(edge_length()))

# and compute weighted graph measures:
sfn_clean <- sfn_clean %>%
  mutate(betw = centrality_edge_betweenness(weights = weight, directed = FALSE))

# Let's see a printing
options(sfn_max_print_active = 2L, sfn_max_print_inactive = 2L)
sfn_clean

# and some (more elaborate) plots:
ggplot() +
  geom_sf(data = st_as_sf(sfn_clean, "edges"), col = "grey", size = 1) +
  geom_sf(data = st_as_sf(sfn_clean, "nodes"), aes(col = degree, size = degree)) +
  theme_minimal() +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1.5, 3)) +
  guides(color = guide_legend(), size = guide_legend()) +
  labs(col = "Node degree", size = "Node degree") +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(3, "lines"),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 10)
  )

# or
ggplot() +
  geom_sf(data = st_as_sf(sfn_clean, "edges"), aes(col = betw, fill = betw), size = 1.2) +
  theme_minimal() +
  scale_color_viridis_c(trans = "sqrt") +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(col = "Edge centrality \nbetweenness", fill = "Edge centrality \nbetweenness") +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(4, "lines"),
    legend.text.align = 0
  )

# More comments:
# 1. A graph measure can only be calculated if the correct table is marked as
# active, i.e. the following code returns an error
sfn_clean %>%
  activate(nodes) %>%
  mutate(betw = centrality_edge_betweenness())
# 2. The tidygraph package implements tens of functions for computing graph
# measures. See https://tidygraph.data-imaginist.com/reference/index.html for
# more details.
# 3. Not all dplyr verbs can be directly applied to a sfnetwork since we cannot
# tamper with the relational data structure.
# 4. Some tidygraph functions require you to always specify the arguments
# "weights" and "directed" (which are not inferred from the sfnetwork).
# 5. The node and edges tables can be extracted using the function "st_as_sf"
# specifying the argument "nodes" or "edges". This is quite important for
# plotting routines. The same structure holds for st_as_sfc function.
# 6. The coordinates of the active geom can be extracted with st_coordinates:

sfn_clean %N>% st_coordinates() %>% head()

# or

sfn_clean %E>% st_coordinates() %>% head()
# 7. The estimation of graph measures is quite relevant for developing
# statistical models at the network level. See an example at the end of this
# presentation.

# Spatial filters (and other sf operations) -------------------------------

# A sfnetwork object is usually represented as two sf tables. Hence, we extended
# several sf functions defining a method for sfnetwork objects. For example, we
# can change the CRS of both tables:

sfn_clean <- st_transform(sfn_clean, 27700)

# This is quite relevant for all those packages (e.g. spatstat) that do not
# handle spherical coordinates.

# We can use st_filter to apply a binary spatial geometry predicate (such as
# st_intersects, st_contains, st_is_within_distance, and so on). For example, we
# can compute a 5km circular buffer around the centroid of the network
sfn_centroid <- st_centroid(st_as_sfc(st_network_bbox(sfn_clean)))
sfn_buffer <- st_buffer(sfn_centroid, 5000)

# and filter only the nodes that intersect that buffer
sfn_small <- sfn_clean %N>% st_filter(sfn_buffer)

# Let's see a plot:
par(mar = c(2.5, 2.5, 1, 1))
plot(sfn_clean, graticule = TRUE, axes = TRUE)
plot(sfn_buffer, add = TRUE, border = "orange", lwd = 3)
plot(sfn_small, add = TRUE, col = "red")

# More details:
# 1. The function st_network_bbox is used to compute the bounding box of edges'
# and nodes' bounding boxes (since they may be different, see ?st_network_bbox).
# 2. The function st_filter is always applied to the active element of the
# sfnetwork object.
# 3. The spatial functions that may change type, shape or position of the
# geometries (e.g. st_union or st_jitter) are not directly supported. You can
# apply them after extracting the spatial table.
# 4. Please note that when nodes are removed, their incident edges are removed
# as well. However, when edges are removed, the nodes at their endpoints remain,
# even if they don’t have any other incident edges. For example:

sfn_clean %E>% st_filter(sfn_buffer) %>% plot(lwd = 3)
plot(sfn_buffer, add = TRUE, border = "orange", lwd = 3)

# Tips

# The following code may be used to simulate an sfnetwork object where
# a) n nodes are randomly placed in a unit square;
# b) two nodes are connected if their are closer than parameter `radius`.

set.seed(123)
my_random_nodes <- play_geometry(200, radius = 0.1)
my_random_sfn <- as_sfnetwork(my_random_nodes, coords = c("x", "y"))

# Check the plot:
unit_square <- st_sfc(st_polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)))))
par(mar = rep(0, 4))
plot(unit_square, lwd = 2)
plot(my_random_sfn, add = TRUE)

# If you need to subset a large `sfnetwork` object, the following approach may
# be more efficient thant the usual `st_filter` (although a bit more verbose).
# First, simulate some data:

set.seed(123)
my_random_nodes <- play_geometry(1e6, radius = 5e-4)
my_random_sfn <- as_sfnetwork(my_random_nodes, coords = c("x", "y"))

# Define a polygon contained into the unit square:

small_square <- st_sfc(st_polygon(list(rbind(c(0.4, 0.4), c(0.4, 0.6), c(0.6, 0.6), c(0.6, 0.4), c(0.4, 0.4)))))

# and filter only the nodes that are contained into that small square:

system.time({
  idxs <- st_contains(small_square, my_random_sfn %>% activate(nodes))
  my_small_random_sfn_v1 <- my_random_sfn %N>% slice(unlist(idxs))
})

# The equivalent code with st_filter takes a little bit longer with equivalent results:

system.time({
  my_small_random_sfn_v2 <- my_random_sfn %N>% st_filter(small_square)
})

# As always, let's plot the result (but, first, we need to repeat the test with less points):

my_random_nodes <- play_geometry(5e4, radius = 5e-3)
my_random_sfn <- as_sfnetwork(my_random_nodes, coords = c("x", "y"))

idxs <- st_contains(small_square, my_random_sfn %>% activate(nodes))
my_small_random_sfn <- my_random_sfn %N>% slice(unlist(idxs))

plot(unit_square, lwd = 2)
plot(small_square, lwd = 2, border = "darkred", add = TRUE)
plot(my_random_sfn, add = TRUE, pch = 46, draw_lines = FALSE, cex = 0.01)
plot(my_small_random_sfn, add = TRUE, pch = 46, draw_lines = FALSE, cex = 0.01, col = "darkred")

rm(unit_square, small_square, my_random_nodes, my_random_sfn, my_small_random_sfn, my_small_random_sfn_v1, my_small_random_sfn_v2, idxs)

# Shortest paths ----------------------------------------------------------

# Calculating shortest paths between pairs of nodes is a core task in network
# analysis. The sfnetworks package offers wrappers around the path calculation
# functions of igraph, making it easier to use them when working with spatial
# data and tidyverse packages.

# Let's see an example. The following code can be used to estimate the path
# connecting the two random points in the Isle of Wight:
set.seed(1)
iow_poly <- st_as_sfc(st_network_bbox(sfn_clean))
start_point <- st_sample(iow_poly, 1)
end_point <- st_sample(iow_poly, 1)
sfn_path <- st_network_paths(
  x = sfn_clean,
  from = start_point,
  to = end_point
)
sfn_path

# The result is returned as an object of class tibble with one row per path. The
# columns node_paths and edge_paths the node and edge indices composing the
# path. We can plot the path as follows:
edges_path <- sfn_clean %>%
  st_geometry("edges") %>%
  extract(do.call(c, sfn_path$edge_paths))

par(mar = rep(0, 4))
plot(sfn_clean, col = "grey")
plot(edges_path, add = TRUE, col = "#8da0cb", lwd = 4)
plot(start_point, col = "#66c2a5", pch = 20, add = TRUE, cex = 2)
plot(end_point, col = "#fc8d62", pch = 20, add = TRUE, cex = 2)

# An analogous result can be obtained using the to_spatial_shortest_paths
# morpher:
sfn_shortest <- sfn_clean %>%
  convert(to_spatial_shortest_paths, from = start_point, to = end_point)

# Again, a plot
plot(sfn_clean, col = "grey")
plot(sfn_shortest, add = TRUE)
plot(start_point, col = "#66c2a5", pch = 20, add = TRUE, cex = 2)
plot(end_point, col = "#fc8d62", pch = 20, add = TRUE, cex = 2)

# The to_spatial_shortest_paths morpher internally calls st_network_paths and
# returns an sfnetwork object that contains only the nodes/edges that belong to
# the shortest path.

# More details:
# 1. If there is a column in the edges table named weight (such as in this
# case), the values in this column are automatically used as edge weights.
# 2. If there is no column named weight in the edges table, then
# st_network_paths() internally calculates the geographic lengths of the edges
# and uses those as weights.
# 3. Check the introductory vignettes for many many more examples (e.g.
# one-to-many routing, distance matrices, closest facility analysis, TSP,
# isochrones)

# Solution to the OGH challenge and relevant example ----------------------

# Now we will code one possible solution to the OGH 2021 challenge. First, we
# need to load the R package spatstat:
library(spatstat)

# and convert the sfn_clean object into linnet format:
my_linnet <- as.linnet(sfn_clean, sparse = TRUE)

# We can use the R package stars to rasterise one of the fields in the edges
# table. So, first extract the edges table
my_edges <- st_as_sf(sfn_clean, "edges")

# and then rasterise the betw column:
library(stars)
my_stars <- st_rasterize(
  my_edges["betw"],
  st_as_stars(st_network_bbox(sfn_clean), nx = 150, ny = 200)
)

# the output can be converted to im class
my_im <- as.im(my_stars)

# and then linim class
my_linim <- linim(my_linnet, my_im)

# Print the result
my_linim

# and plot it
plot(my_linim)

# As mentioned in the text of the challenge, this approach may be useful for
# statistical models at the (spatial) road network level. For example, we can
# simulate some data on the network:
set.seed(3)
my_lpp <- runiflpp(300, my_linnet)

# Plot the result. Please note that the red dots denote events on the linear network.
plot(my_lpp, pch = 16, cols = "red", col = "grey", cex = 0.8, main = "")

# And estimate its intensity as a function of the betw covariate:
par(mar = c(5, 4, 2, 2))
plot(rhohat(my_lpp, sqrt(my_linim)))

# Clearly this is just a toy example, check Prof. Rubak's presentation
# (http://spatstat.org/OGH2021/) for more details.
