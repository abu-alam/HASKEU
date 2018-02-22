> module MVC where

> import Reactive.Banana
> import Reactive.Banana.Combinators
> import Reactive.Banana.WX

> type Model t a = Behavior t a
> type View t a = Moment t a
> type Controller t a 
>	= Moment t (Event t a)

> type BusinessRule e m 
>	= e -> m -> m

> type InterfaceLogic m a 
>	= m -> a

> class Frameworks t 
>	=> VCController w m e t where 
> 		controller	:: (w, (Event t e)) 
>				-> (BusinessRule e m) 
>				-> Controller t (m -> m) 
> 		controller (w, e) br 
>				= return (br <$> e)

> type AttrName = String

> class Frameworks t 
>	=> VCView w m a v t where 
>		view 	        :: (w, AttrName) 
>				-> Model t m 
>				-> InterfaceLogic m a 
>				-> View t v

> unionController 	:: Controller t (m -> m) 
>			-> Controller t (m -> m) 
>			-> Controller t (m -> m)
> unionController a b 
>	= (liftA2 union) a b

> mergeController 
>	:: [Controller t (m -> m)] 
>	-> Controller t (m -> m)
> mergeController lstEv 
>	= foldr1 (unionController) (lstEv)

> sequenceView 
>	:: [View t a] 
>	-> View t ()
> sequenceView lst = sequence_ lst

> mvc :: (Frameworks t)
>	=> m 
>	-> [Model t m -> View t v] 
>	-> [Controller t (m -> m)] 
>	-> View t ()
> mvc init lstView lstController = do  
>	c <- mergeController lstController
>	let
>		m = accumB init $ c
>	sequenceView [v m| v <- lstView]






