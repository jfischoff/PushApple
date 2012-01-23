module Exercise where

data Environment = Environment
    {
        schedules :: Events
    }

type Time = Int
    
type Events = [Event]    
    
type Event = (Time, EventType)

data EventType = Exercise
               | Meal Ingrediants
               | Sleep
               | Wakeup
             
           
type Weight = Double           
type Food   = String           
           
type Ingrediant = (Weight, Food)           
           
type Ingrediants = [Ingrediant]                 
           
data Exercise = ExerciseGoal ExerciseType
           
data ExerciseGoal = Timed Time
                  | Reps Int
                  | Distance Double
           
data ExerciseType = Run
                  | Lift LiftType Weight
                  | BodyWeight BodyWeightType       
                  
type LiftType = String
type BodyWeightType = String

--all of the crud
remove_item item_id = undefined 

add_meal time ingredients = undefined

add_sleep time = undefined
add_wakeup time = undefined

add_distance_run time distance = undefined
add_time_run event_time run_time = undefined 

add_rep_lift time lift weight rep = undefined
add_timed_lift time lift weight exercise_time = undefined

add_body_weight_rep time name rep = undefined
add_body_weight_time time name excerise_time = undefined

--I need to add something that sends notifications to the phone

--I have a thread that reads from the global state and
--there is a sorted list
--it keeps an index and increments it
--it has a sorted list that it updates when new events are added
--this is something that it handles by itself





