> module HistoryManagement where

> import ModelType

> manageHistory :: ModelChangedBy -> SystemState -> SystemState
> manageHistory chgBy (SystemState f t g ty h) =
>	SystemState f t g ty (addToHistory chgBy (SystemState f t g ty h))

> addToHistory :: ModelChangedBy -> SystemState -> ModelHistories
> addToHistory chgBy (SystemState f t g ty (ModelHistories h cPos)) =
>	ModelHistories ((takeHistoryTillCurPos h cPos)++(createNewhistory chgBy f t g ty (textStr t/=""))) (incrPos cPos (textStr t/=""))
>	where
>	incrPos pos True = pos + 1
>	incrPos pos False = pos 

> takeHistoryTillCurPos :: [ModelHistory] -> Int -> [ModelHistory]
> takeHistoryTillCurPos hList cPos =
>	take (cPos + 1) hList

> createNewhistory :: ModelChangedBy -> FileInfo -> TextualState -> VisualState -> TypeInfo -> Bool -> [ModelHistory]
> createNewhistory chgBy f t g ty True =
>	[ModelHistory f t g ty chgBy]
> createNewhistory chgBy f t g ty False = --Temporary fix the replace button bug, use of the Bool
>	[]



> --maybe we will keep only ten histories
