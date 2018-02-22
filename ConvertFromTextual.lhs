> module ConvertFromTextual where
>
> import System.IO.Unsafe
>
> import ModelType
> import TreeErrorManagement

> import Language.Haskell.Exts.Annotated as HSE
> import qualified Language.Haskell.Exts as HS
> import SyntaxTreeManagement
> import Language.Haskell.TypeCheckAsst.TypeCheck
>
> -- Synchronyze Textual -> Graphical
> convertFromTextual :: SystemState -> SystemState
> convertFromTextual m@(SystemState f t g ty h) = 
>	let pTree = parseFileContentsWithComments defaultParseMode (textStr t)
>	in case pTree of
>		ParseOk (mod,comments) -> proceedWithParseOK mod m
>		err 		       -> proceedWithParseFail err m

> proceedWithParseOK :: Module SrcSpanInfo -> SystemState -> SystemState
> proceedWithParseOK mod m@(SystemState f t g ty h) =
>	let 	m1@(SystemState _ _ _ ty1 _) 	= loadTypeInfo m
>		g1				= createShapeTree mod m1
>	in 	setSelectedFunc $ setTextualTypeError $ SystemState f t (setEmptyGraError g1) ty1 h

> proceedWithParseFail :: ParseResult p -> SystemState -> SystemState
> proceedWithParseFail err m@(SystemState f t g ty h) =
>	let 	t1	= (setSyntaxError err t)
>		g1	= g 
>	in 	SystemState f (setSyntaxError err t) (setUnConvertableGraError g) ty h

> setEmptyGraError :: VisualState -> VisualState
> setEmptyGraError g =
>	g {
>		visualErrors = [],
>		selectedVisualErr = -1
>	}

> setUnConvertableGraError :: VisualState -> VisualState
> setUnConvertableGraError g =
>	g {
>		visualErrors = [ErrorMsg 0 0 "Graphical view can not be created as there is syntax error in the textual view."],--"This version of graphical view is not equivalent to the textual view due to syntax error."],
>		selectedVisualErr = -1,
>		shapeTree=HSE.Module (srcSpanInfoInitVal, graInitVal) Nothing [] [] []
>	}

> setTextualTypeError :: SystemState -> SystemState
> setTextualTypeError m@(SystemState f t g ty h) =
>	SystemState f  
>	t {
>		textualErrors = getTextualTypeErrFromTree m,
>		selectedTxtErr = -1
>	}
>	g ty h

> setSyntaxError :: (ParseResult p) -> TextualState -> TextualState
> setSyntaxError (ParseFailed (SrcLoc f l c) msg) t =
>	t {
>		textualErrors = [(ErrorMsg l c msg)],
>		selectedTxtErr = -1
>	}

> createShapeTree :: (Module SrcSpanInfo) -> SystemState -> VisualState
> createShapeTree mod m@(SystemState f t g ty h) = 
> 	g {shapeTree = mdlGr1}
>	where
>	mdlGr 	= snd $ setAutoLayout curPos mod
>	mdlGr1  = setType ty mdlGr mdlGr
>	curPos 	= graInitVal













