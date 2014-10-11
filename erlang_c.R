##
## R package "Erlang"
##
## Copyright (c) 2014 Yi Zhang
## Email: beingzy@gmail.com
##
##
## Modified from the "Erlang" Package: Patrick Hubers
## Repo ("https://github.com/phubers/erlang.git")
## A number of correction had been conducted
## And, new results had been validated with reliably calculator
##
## This packages contains functions related to (contact center) staffing,
## using the Erlang C function
##
## Introduction ofErlang C formula
## Purpose: predict the probability of call request could not be answered immediately
##    or, the call has to be on hold
## Effect:
##    Erlan C formula has the tendency of offering a higher number of required
##     agents to deliver a service of specificed service level
##     (e.g. 80/20, 80% of calls can be handled within 20 seconds)
##
## M/M/1 Queueing Model
## Assumption:
##  a. arrival occur at rate of lambda a Poisson process and move the process from state i to i +1
##  b. service time (handling time) have an exponential distribution with parameter 1/miu in the M/M/1 queue, where miu is the AHT
##  c. a single server serves customers one at a time from the front of the queue, first-come, first-served
##  d. the buffer is of infinite size, so there is no limit on the number of custoemrs it can contain
 
#' Calculate traffic intensity (a.k.a. workload, or, total traffic offered)
#'
#' @param arrival_rate: Number of arrivals per interval
#' @param avg_handle_time: Average handling time in seconds
#' @param interval_length: Length of interval in minutes (default = 60)
#' @return Traffic intensity in Erlangs
#' @export
intensity <- function(arrival_rate, avg_handle_time, interval_length = 60) {
  return (arrival_rate * avg_handle_time / interval_length)
}
 
#' Calculate agent occupancy
#'
#' @param number_of_agents: Number of available agents
#' @param intensity: Traffic intensity in Erlangs
#' @return Occupancy
#' @export
occupancy <- function(number_of_agents, intensity) {
  return (intensity / number_of_agents)
}
 
pow_fact <- function(a, b) {
  return (b^a/factorial(a))
}
 
#' Calculate the chance of a blocked call (Erlang C function)
#'
#' @param number_of_agents: Number of available agents
#' @param intensity: Traffic intensity in Erlangs
#' @return Chance of blocking
#' @export
erlang_c <- function(number_of_agents, intensity) {
  term <- pow_fact(number_of_agents, intensity)
  sum  <- 0
  for (k in 0:(number_of_agents-1)) {
    sum <- sum + pow_fact(k, intensity)
  }
  # ec <- term / (term + (1-occupancy(number_of_agents,intensity))*sum)
  comp <- term * number_of_agents / ( number_of_agents - intensity )
  ec   <- comp / ( sum + comp )
  return(ec)
}
 
#' Calculate the average waiting time
#'
#' @param number_of_agents: Number of available agents
#' @param arrival_rate: Number of arrivals per interval
#' @param avg_handling_time: Average handling time in seconds
#' @param interval_lengt: Length of an interval in minutes
#' @return Average waiting time in seconds
#' @export
avg_wait_time <- function(number_of_agents, arrival_rate, avg_handle_time, interval_length = 60) {
  int <- intensity(arrival_rate, avg_handle_time, interval_length)
  awt <-  (erlang_c(number_of_agents, int) * avg_handle_time ) / ( number_of_agents - int )
  return(awt)
}
 
#' Calculate the servicelevel
#'
#' Calculates the percentage of calls that are answered within the acceptable waiting time
#'
#' @param number_of_agents: Number of available agents
#' @param arrival_rate: Number of arrivals per interval
#' @param avg_handling_time: Average handling time in seconds
#' @param interval_length: Length of an interval in minutes
#' @param wait_time: Acceptable waiting time
#' @return Service level (% of calls answered within acceptable waiting time)
#' @export
service_level <- function(number_of_agents, arrival_rate, avg_handle_time, interval_length, wait_time) {
  a <- intensity(arrival_rate, avg_handle_time, interval_length)
  sl <- 1 - erlang_c(number_of_agents, a) * exp(-(number_of_agents - a)*(wait_time/avg_handle_time))
  return(sl)
}
 
#' Calculate the number of needed agents for SL goal
#'
#' Calculates the number of agents that are needed to achieve a required service level. Currently only
#' calculates a whole (integer) number of agents.
#'
#' @param arrival_rate: Number of arrivals per interval
#' @param avg_handle_time: Average handling time in seconds
#' @param interval_length: Length of an interval in minutes
#' @param wait_time: Acceptable waiting time (i.e. 20 seconds in a 80/20 SL goal)
#' @param service_level_goal: Service level goal, the percentage of calls answered within the acceptable waiting time
#' @return Number of agents needed to achieve service level
#' @export
number_of_agents_for_sl <- function(arrival_rate, avg_handle_time, interval_length, wait_time, service_level_goal) {
  int <- intensity(arrival_rate, avg_handle_time, interval_length)
  agents <- int
  while(service_level(agents, arrival_rate, avg_handle_time, interval_length, wait_time) < service_level_goal) {
    agents <- agents + 1
  }
  return(agents)
}
 
 
#' Calculate the number of needed agents for achieve an ASA goal
#'
#' Calculates the number of agents that are needed to achieve a required average speed of answer. Currently only
#' calculates a whole (integer) number of agents.
#'
#' @param arrival_rate: Number of arrivals per interval
#' @param avg_handle_time: Average handling time in seconds
#' @param interval_length: Length of an interval in minutes
#' @param wait_time: Waiting time goal in seconds
#' @return Number of agents needed to achieve ASA
#' @export
number_of_agents_for_asa <- function(arrival_rate, avg_handle_time, interval_length, wait_time) {
  int <- intensity(arrival_rate, avg_handle_time, interval_length)
  agents <- int
  while(avg_wait_time(agents, arrival_rate, avg_handle_time, interval_length) > wait_time) {
    agents <- agents + 1
  }
  return(agents)
}
