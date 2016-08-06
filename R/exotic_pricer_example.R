

prod_time <- timeDate::timeSequence(from="2016-07-09", to="2017-07-09")
prod <- path_dependent_asian$new(prod_time)
r <- constant_parameters$new(0.01)
spot <- 100
vol <- constant_parameters$new(0.2)
div <- constant_parameters$new(0.0)
engine <- exotic_bs_engine$new(prod, r, spot, vol, div)

gatherer_mean <- statistics_mean$new()

system.time(engine$run_simulation(gatherer_mean, 1e5))

gatherer_mean$get_results()
