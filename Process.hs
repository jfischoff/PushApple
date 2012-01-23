module Process where
    
import Control.Concurrent
import Control.Concurrent.Chan

to_process f output input = do 
    input_value <- readChan input
    
    let output_value = f input_value
    
    writeChan output output_value
    
