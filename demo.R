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
iow <- st_read("https://github.com/sfnetworks/OGH2021/raw/master/isle-of-wight.gpkg")

# Let's see a plot
par(mar = rep(0, 4))
plot(iow)

# and an interactive plot
mapview(iow)

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
  convert(to_components, .clean = TRUE) %>%
  convert(to_spatial_simple)

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

sf.colors(2, categorical = TRUE)
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

# Extras (more advanced) --------------------------------------------------

# Solution to the OGH challenge and relevant example ----------------------
