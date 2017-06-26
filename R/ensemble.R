#' Run an event occurrence simulation ensemble
#'
#' @param events the event occurrences as \code{POSIXct}
#' @param size the amount of ensemble members
#' @param tend the minimum forecast end time to simulate as \code{POSIXct}
#' @param tdiff_prng the pseudo-random number generator for the time differences
#'     between event beginnings. Defaults to \code{prng_from_sample(events)}.
#'
#' @return a \code{list} of ensemble members
#'
#' @export
event_simulation_ensemble <- function(
    events,
    tend,
    size = 100,
    tdiff_prng = prng_from_sample(
        diff(as.numeric(events)-as.numeric(min(events))))
    ) {
    member_generator <- ensemble_member_generator(
        tstart=max(events),tend=tend,tdiff_prng = tdiff_prng)
    ensemble <- sapply( X = paste("m",seq(size),sep=""), FUN = member_generator)
    return(ensemble)
    }

#' Create an ensemble member generator
#'
#' @param tstart the starting time as \code{POSIXct}
#' @param tend the minimum end time as \code{POSIXct}
#' @param tdiff_prng the pseudo-random number generator for the time differences
#'     between event beginnings.
#'
#' @return a numeric \code{vector} of simulated event beginning times
#'
#' @export
ensemble_member_generator <- function(
    tstart,
    tend,
    tdiff_prng
    ) {
    fun <- function(...) {
        t_cur = as.numeric(tstart)
        t_end = as.numeric(tend)
        m_events = c(t_cur)
        repeat{
            t_cur <- t_cur+tdiff_prng(1)
            m_events <- c(m_events,t_cur)
            if(t_cur > t_end)
                break
            }
        return(m_events)
        }
    return(fun)
    }

#' Get event occurrence probabilities from ensemble
#'
#' @param ensemble the ensemble
#' @param event_duration the duration of an event
#' @param times the times to get the probabilities for
#'
#' @export
ensemble_probabilities_times <- function(
    ensemble,
    event_duration,
    times
    ) {
    # is in this set of events an event at the given time?
    event_at_time <- function(events,time)
        any(events<time&time<events+event_duration)
    # what is the occurrence probability for this given time?
    occurrence_prob_at_time <- function(time) {
        length(which( # count
            sapply(ensemble, function(member) # what members
                    event_at_time(events=member,time=time)) # see an event here
            )) / length( ensemble) # norm it to get a percentage
        }
    times_prob <- sapply( as.numeric(times), occurrence_prob_at_time )
    res <- data.frame( time = times, prob = times_prob )
    return(res)
    }
