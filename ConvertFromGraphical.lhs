> module ConvertFromGraphical where
>
> import ModelType
> import Language.Haskell.Exts.Annotated
> import SyntaxTreeManagement
> import TreeErrorManagement
>
> -- Synchronyze Graphical -> Textual
> convertFromVisual :: SystemState -> SystemState
> convertFromVisual m@(SystemState f t g ty h) = 
>	setGraTypeError $ proceedWithNoGraSyntaxErr m
>	{-let 	syntaxErr 	= checkGraSyntaxError m
>	in 	case syntaxErr of
>			[]	-> setGraTypeError $ proceedWithNoGraSyntaxErr m
>			err	-> proceedWithGraSyntaxErr m err-}

> convertFromVisual_Layout :: SystemState -> SystemState
> convertFromVisual_Layout m@(SystemState f t g ty h) = 
>	let 	m1		= createShapeTree $ setTextual m
>		syntaxErr 	= checkGraSyntaxError m1
>	in 	case syntaxErr of
>			[]	-> setGraTypeError $ proceedWithNoGraSyntaxErr m1
>			err	-> proceedWithGraSyntaxErr m1 err

> convertFromVisual_Type :: SystemState -> SystemState
> convertFromVisual_Type m@(SystemState f t g ty h) = 
>	let 	m1		= createShapeTree_Type $ setTextual m
>		syntaxErr 	= checkGraSyntaxError m1
>	in 	case syntaxErr of
>			[]	-> setGraTypeError $ proceedWithNoGraSyntaxErr m1
>			err	-> proceedWithGraSyntaxErr m1 err

> setGraTypeError :: SystemState -> SystemState
> setGraTypeError m@(SystemState f t g ty h) =
>	SystemState f t 
>	g {
>		visualErrors = getGraTypeErrFromTree m,
>		selectedVisualErr = -1
>	}
>	ty h

> setEmptyTextualError :: TextualState -> TextualState
> setEmptyTextualError t =
>	t {
>		textualErrors = [],
>		selectedTxtErr = -1
>	}

> setUnConvertableTextualError :: TextualState -> TextualState
> setUnConvertableTextualError t =
>	t {
>		textualErrors = [],--[ErrorMsg 0 0 "This version of textual view is not equivalent to the graphical view due to syntax error."],
>		selectedTxtErr = -1
>	}

> createShapeTree_Type :: SystemState -> SystemState
> createShapeTree_Type m0 = 
> 	m1
>	where
>	m@(SystemState f t g ty h)	= loadTypeInfo m0
>	mod	= shapeTree g
>	mdlGr1  = setType ty mod mod
>	curPos 	= graInitVal
>	g1	= g {shapeTree = mdlGr1}
>	m1	= SystemState f t g1 ty h

> setTextual m@(SystemState f t g ty h) = 
>	let 	t1 = TextualState (prettyPrint (shapeTree g)) 0 [] (-1)
>	in	(SystemState f t1 g ty h)

> proceedWithNoGraSyntaxErr m@(SystemState f t g ty h) = 
>	(SystemState f (setEmptyTextualError t) g ty h)

> createShapeTree :: SystemState -> SystemState
> createShapeTree m0 = 
> 	m3
>	where
>	m@(SystemState f t g ty h)	= loadTypeInfo m0
>	mod	= shapeTree g
>	mdlGr 	= snd $ setAutoLayoutGra curPos mod
>	mdlGr1  = setType ty mdlGr mdlGr
>	curPos 	= graInitVal
>	g1	= g {shapeTree = mdlGr1}
>	m1	= SystemState f t g1 ty h
>	-- Retrive Selection
>	getSelF		= getSelectedFn m "selected"
>	getSelM		= getSelectedMatch m "selected"
>	getSelLF	= getSelectedLclFn m "selected"
>	getSelLM	= getSelectedLclMatch m "selected"

>	setSelF	(Just node)	= selectFunc (getP node) "selected" m1
>	setSelF	Nothing		= m1
>	setSelM	(Just node)	= selectFunc (getP node) "selected" m1
>	setSelM	Nothing		= setSelF (getSelF)

>	m2			= setSelM (getSelM)

>	setSelLF (Just node)	= selectLclFunc (getP node) "selected" m2
>	setSelLF Nothing	= m2
>	setSelLM (Just node)	= selectLclFunc (getP node) "selected" m2
>	setSelLM Nothing	= setSelLF (getSelLF)

>	m3			= setSelLM (getSelLM)
> 	getP nod= itemPosition $ snd $ ann nod

> proceedWithGraSyntaxErr m@(SystemState f t g ty h) err = 
>	(SystemState f (setUnConvertableTextualError t) g {visualErrors = err, selectedVisualErr = -1} ty h)

> checkGraSyntaxError :: SystemState -> [ErrorMsg]
> checkGraSyntaxError m = getSyntaxErrFromTree m




