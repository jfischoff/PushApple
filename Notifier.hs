module Notifier where
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad.State
--import ProducerConsumer

--TODO
--refactor to not use the state monad    
    
type Time = POSIXTime
type Event = (Time, String)
type Events = [Event]
    
data Environment = Environment
    {
        events       :: Events,
        current_time :: PosixTime
    }

--type StateIO a = StateT Environment IO a

sort_events events = sortBy (\(x, _) (y, _) -> x `compare` y) events

events_to_fire time events = takeWhile (\(x, _) -> x < time) events

drop_old_events time events = dropWhile (\(x, _) -> x < time) events

--get_current_time = getPOSIXTime

--get_events = do
--    x <- get
--    return (events x)

--put_events :: Events -> StateIO ()
--put_events new_events = do
--    x <- get
--    put (x{events = new_events})
--    return  ()
    

--consumer new_event = do 
--    time <- liftIO $ getPOSIXTime
--    event_consumer new_event time

collect_events_to_process env new_event = (new_events_to_fire, result) where 
    new_combined_events = new_event:(events env)
    new_sorted_events = sort_events new_combined_events
    new_events_to_fire = events_to_fire (current_time env) new_sorted_events
    dropped_events = drop_old_events time events

    result =  dropped_events


      


