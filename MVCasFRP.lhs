Programmers brought up on object-oriented programming
languages may avoid functional ones because they cannot
see how to make use of their favourite design patterns.
This example helps them out by showing how the
famous Model-View-Controller (MVC) design pattern
corresponds to the Functional Reactive Programming (FRP)
framework. 


> {-# LANGUAGE ScopedTypeVariables #-}

> module Main where

> import Reactive.Banana
> import Reactive.Banana.Combinators
> import Reactive.Banana.WX
> import Graphics.UI.WX hiding (Event)
> import Graphics.UI.WXCore hiding (View, Event)
> import Dial

}

> type Model t m = Moment t (Behavior t m)

a view corresponds to a moment

> type View t d = Moment t d

and a controller corresponds to a event mapping function.

> type Controller t m 
>	= Moment t (Event t (m -> m))

\indent A business rule function maps from one model
to another and has the type

> type BusinessRule e m 
>	= e -> m -> m

where |e| is the event value and |m| is the 
model value.\\

An interface logic function maps from
a model to a widget attribute
and has the type

> type InterfaceLogic m a 
>	= m -> a

In this functional MVC implementation, the following two classes |ClsController| and |ClsView|
do the decoupling of a user-control and 
separate it into a controller and a view.
The |class ClsController| is a framework that is used to create a controller.
The function |controller| maps 
a business rule with a specific event
and returns a |Controller| type.
The |m| and |e|
in the |class| declaration are the types of
model value and event value respectively.

> class Frameworks t => ClsController m e t where 
> 		controller	 	::  Event t e
>				 	->  (BusinessRule e m) 
>				 	->  Controller t m 
> 		controller ev br 	=  return (br <$> ev)

The following function |unionController| 
can be used to concatenate two controllers. 

> unionController 	 ::  Controller t m 
>			 ->  Controller t m 
>			 ->  Controller t m
> unionController 	 =  liftA2 union

The |class ClsView| is a framework 
that is used to create a view.
The function |view| 
uses the interface logic 
to extract the value of a widget attribute from the model and then
sets the specific attribute. The |w|,|m|,|a| and |d| are the types of
widget, the model value, the widget attribute value and view value respectively.

> type AttrName = String
> class Frameworks t => ClsView w m a d t where 
>		view 	         ::  (w, AttrName) 
>				 ->  Model t m 
>				 ->  InterfaceLogic m a 
>				 ->  View t d

The reason for having separate classes for view and controller
is that there are some distinct type variables 
in the class declarations, and some of those cannot be used both in
the |view| and |controller| functions.
This also makes it possible
that a widget can be either a view or
a controller or both.\\

The |mvc| function below 
combines all views and controllers of a reactive system 
in a way so that the user does not have to pay any 
attention to the individual controller or view
to change the model value or to update a view respectively,
and it makes the whole MVC system operate successfully.
With the use of FRP combinator |accumB|,
the model value is updated everytime a controller
is in action.
More precisely, on the occurrence of any event  
the business rule associated with each event is applied to
the last value of the model, and it returns an updated model value.
As soon as the model value is changed, 
the |mergeView| function automatically updates the views in the system
by sequencing the view monads.

> mvc	 ::  Frameworks t 
>	 =>  m 
>	 ->  [Model t m -> View t d] 
>	 ->  [Controller t m] 
>	 ->  View t ()
> mvc init lstView lstController = 
>	do  
>	c 	<- mergeController lstController
>	let m 	= pure $ accumB init $ c
>	mergeView [v m| v <- lstView]

> mergeController :: [Controller t m] ->  Controller t m
> mergeController lstEv =
>	 foldr1 (unionController) (lstEv)

> mergeView :: [View t d] -> View t ()
> mergeView lst =
>	 sequence_ lst

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% An Example                                                      %% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{An Example}
\label{example}

As an example, consider an interface that allows
a \textit{volume level} to be set using a 
\textit{slider} or a \textit{dial}. 

A volume controller system has the volume level of any 
integer value between 0 to 11. So, we can use
the following type of |ModelType| for the type parameter |m| of the underlying |Model|:

> type ModelType = Int

The views of slider and dial widgets of the wxHaskell library perform some IO operations
to display the output on the 
screen and return null |()|.
So,|()| is used
as the |ViewType| for the type parameter |d| of the underlying |View| monads:

> type ViewType = ()

\indent Events in the
existing event-based framework
need to be adjusted in order to be used in 
this functional reactive programming based framework.
The events in wxHaskell library can be 
represented to FRP events using the functions, |event0| and |event1|, and 
wxHaskell widget views are updated via the function |sink|.
These three functions are defined in the {\em reactive-banana-wx\/}
library. The following coding of the
|eventSelect| function shows how a selection event of
any wxHaskell widget can be adjusted to
an FRP event.

< eventSelect 	:: 	(Frameworks t, Selecting w, Selection w) 
<		=> 	w -> Moment t (Event t Int)
< eventSelect w =
<	do
<	eSelect <- event0 w select
<	b <- (behavior w selection)
<	return (b <@ eSelect)

The following function can be used to update a wxHaskell widget attribute:

< viewWidget	:: 	(WxAttributes w a, Frameworks t) 	
<		=> 	(w, AttrName) 
<		-> 	Model t m 
<		-> 	InterfaceLogic m a 
<		-> 	View t ()
< viewWidget (w, a) m il =		
<	m >>= (\m1 -> (sink w [(getWxAttr a) :== (il <$> m1)]))

\indent Only one |ClsController| instance is needed for both the command event
of the slider and the select event
of the dial, as they both return |Int| value
at event occurrences, and can be defined as below:

> instance Frameworks t => 
>	ClsController ModelType Int t where

\ignore{

> eventSelect :: (Frameworks t, Selecting w, Selection w) => w -> Moment t (Event t Int)
> eventSelect w = do
>	eSelect <- event0 w select
>	b <- (behavior w selection)
>	return (b <@ eSelect)

> eventCommand :: (Frameworks t, Commanding w, Selection w) => w -> Moment t (Event t Int)
> eventCommand w = do
>    addHandler <- liftIONow $ event1ToAddHandler w (event0ToEvent1 command)
>    fromAddHandler
>        $ mapIO (const $ get w selection) addHandler

}
\indent Two |View| instances are needed
to set the selection attribute of the slider and the dial
with the value of the model, and these
can be defined as below. 

> instance Frameworks t => 
>	ClsView (Slider ()) ModelType Int ViewType t where
> 		view = viewWidget 

> instance Frameworks t => 
>	ClsView (Dial ()) ModelType Int ViewType t where
> 		view = viewWidget

\ignore{

> viewWidget
>  :: (WxAttributes w a, Frameworks t) 	=> (w, AttrName) 
>					-> Model t m 
>					-> InterfaceLogic m a 
>					-> View t ()
> viewWidget (w, a) m il =		
>	m >>= (\m1 -> (sink w [(getWxAttr a) :== (il <$> m1)]))

> class WxAttributes w a where
>	getWxAttr :: AttrName -> Attr w a

> instance WxAttributes (Slider ()) Int where
>	getWxAttr "selection" = selection
> instance WxAttributes (Dial ()) Int where
>	getWxAttr "selection" = selection

}
\indent Business logics for both the slider and dial controllers
are the same, and they just replace the model
value by the event value of the slider or dial

> brVolChngBySlider :: BusinessRule Int ModelType
> brVolChngBySlider e m = e

> brVolChngByDial :: BusinessRule Int ModelType
> brVolChngByDial e m = e

\indent Interface logics for both slider and dial are also the same, and they just return 
the model value without any manipulation

> ilUpdateSliderDisplay :: InterfaceLogic ModelType Int
> ilUpdateSliderDisplay m = m

> ilUpdateDialDisplay :: InterfaceLogic ModelType Int
> ilUpdateDialDisplay m = m

\indent Below, instances of the slider and the dial widget
are created, and then positioned in a window panel:
\ignore{

> setGUILayoutAndVC :: 
>	Frameworks t => 
>		Moment t (	[Model t ModelType 
>					-> View t ViewType],  							 
>				[Controller t ModelType])
> setGUILayoutAndVC = do
>	  -- Set GUI Layout
>         win 		<- liftIONow 
>				$ frame 	[	text := "Volume Controller", 
>							visible := False]

}

>	  s    		<- liftIONow 
>				$ hslider win True 0 11 [selection := 0]
>	  d    		<- liftIONow 
>				$ makeDial  win 0 11 []

\ignore{

>	  liftIONow $ set win [	layout 	:= column 0 
>						[hfill $ widget s, widget d],
>				clientSize := sz 300 300]
>	  liftIONow $ set win [visible := True]
>
>	  -- Set View-Controller

}
Then, the business logic and interface logic (created above) of
the slider and dial are mapped to create the controllers
and the views

>	  evSldComm	 <-  eventCommand s
>	  evDialSel	 <-  eventSelect d

>	  let cSld	 =  controller evSldComm brVolChngBySlider
>	  let cDial	 =  controller evDialSel brVolChngByDial

>	  let vSld	 =  \m -> view (s, "selection") m ilUpdateSliderDisplay
>	  let vDial	 =  \m -> view (d, "selection") m ilUpdateDialDisplay

\indent The two lists of controllers and views, and the initial model value (e.g. 0)
are passed to the |mvc| function

>	  let lstC = [cSld, cDial]
>	  let lstV = [vSld, vDial]

<	  mvc modelInitVal lstV lstC 

\ignore{

>	  return (lstV, lstC)

> modelInitVal :: ModelType
> modelInitVal = 0

> main :: IO ()
> main = start $ do Main.gui

> gui :: IO ()
> gui = do

>	   let networkDescription :: 
>			forall t. Frameworks t => Moment t ()
>	       networkDescription = do

>			(lstV, lstC) <- setGUILayoutAndVC
>			mvc modelInitVal lstV lstC 

>	   network <- compile $ networkDescription
>	   actuate network

}

Now, the volume controller reactive system is ready to use (see ~\ref{fig:VolumeController}).\\

