module Helpers exposing (ifThenElse)


ifThenElse : Bool -> a -> a -> a
ifThenElse cond exp1 exp2 =
    if cond then
        exp1

    else
        exp2
