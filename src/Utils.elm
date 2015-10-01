module Utils where


import Time
import Task


delayedTask: Time.Time -> Task.Task x a -> Task.Task x a
delayedTask delay task =
  Task.sleep delay `Task.andThen` always task 


sign : Float -> Float
sign value =
  if | value > 0 -> 1 
     | value < 0 -> -1 
     | otherwise -> 0

