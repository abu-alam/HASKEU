> module IL where

> import MVC
> import ModelType
> import Drawings
> import SyntaxTreeManagement

> import Graphics.UI.WX
> import Graphics.UI.WXCore hiding (Module)

> import Language.Haskell.Exts.Annotated

> ilSetWindowTitle :: InterfaceLogic SystemState String
> ilSetWindowTitle (SystemState f t g ty h) = 
>	let maybePath = path f
>	in case maybePath of
>            Nothing 	-> windowTitle
>            Just p 	-> windowTitle ++ " - " ++ p

> ilSetTextEditorText :: InterfaceLogic SystemState String
> ilSetTextEditorText (SystemState f t g ty h) = textStr t
> ilSetTextEditorInsertionPt :: InterfaceLogic SystemState Int
> ilSetTextEditorInsertionPt (SystemState f t g ty h) = insertionPt t

> ilSetSelectedTextualError :: InterfaceLogic SystemState Int
> ilSetSelectedTextualError (SystemState f t g ty h) = selectedTxtErr t

> ilShowTextualErrors :: InterfaceLogic SystemState [String]
> ilShowTextualErrors m = 
>	map p (textualErrors $ textualState m)
>	where
>	p (ErrorMsg l c e) = "Line " ++ (show l) ++ ", Col " ++ (show c) ++ ":: " ++ e 

> ilShowLineColStatus :: InterfaceLogic SystemState String
> ilShowLineColStatus m@(SystemState f t g ty h) = 
>	let 	str = textStr t
>		ins = insertionPt t
>	in 	setLineColumnStatus (str, ins)

> setLineColumnStatus :: (String, Int) -> String
> setLineColumnStatus (str, ins) =
>	let 	(l, c) 	= getRowColFromInsPt str ins 
>	in 	("L::" ++ " " ++ (show l) ++ ",C::" ++ " " ++ (show c))

> ilIsEndOfUndo :: InterfaceLogic SystemState Bool
> ilIsEndOfUndo (SystemState _ _ _ _ (ModelHistories h cPos)) = cPos > 0
> ilIsEndOfRedo :: InterfaceLogic SystemState Bool
> ilIsEndOfRedo (SystemState _ _ _ _ (ModelHistories h cPos)) =  (length h) > (cPos + 1)

> ilSetModuleName :: InterfaceLogic SystemState String
> ilSetModuleName (SystemState f t g ty h) = 
>	let Module _ mh _ _ _ = shapeTree g
>	    showMH Nothing = ""
>	    showMH (Just (ModuleHead _ (ModuleName _ n) _ _)) = n
>	in showMH mh
> ilSetModuleNameInsPt :: InterfaceLogic SystemState Int
> ilSetModuleNameInsPt (SystemState f t g ty h) = 
>	let Module _ mh _ _ _ = shapeTree g
>	    showMH Nothing = 0
>	    showMH (Just (ModuleHead _ (ModuleName (s,vi) n) _ _)) = annInsertionPoint vi
>	in showMH mh
> ilDrawFnView :: InterfaceLogic SystemState (DC () -> Rect -> IO ())
> ilDrawFnView m@(SystemState f t g ty h) dc r = 
>	drawFnView m dc

> ilDrawLclFnView :: InterfaceLogic SystemState (DC () -> Rect -> IO ())
> ilDrawLclFnView m@(SystemState f t g ty h) dc r = 
>	drawLclFnView m dc

> ilDrawFnArgView :: InterfaceLogic SystemState (DC () -> Rect -> IO ())
> ilDrawFnArgView m@(SystemState f t g ty h) dc r = 
>	drawFnArgView m dc

> ilDrawLclFnPatView :: InterfaceLogic SystemState (DC () -> Rect -> IO ())
> ilDrawLclFnPatView m@(SystemState f t g ty h) dc r = 
>	drawLclFnPatView m dc

> ilDrawFnBodyView :: InterfaceLogic SystemState (DC () -> Rect -> IO ())
> ilDrawFnBodyView m@(SystemState f t g ty h) dc r = 
>	drawFnBodyView m dc

> ilDrawFnBttonsView :: InterfaceLogic SystemState (DC () -> Rect -> IO ())
> ilDrawFnBttonsView m@(SystemState f t g ty h) dc r = 
>	drawFnButtonsView m dc

> ilDrawPatButtonsView :: InterfaceLogic SystemState (DC () -> Rect -> IO ())
> ilDrawPatButtonsView m@(SystemState f t g ty h) dc r = 
>	drawPatButtonsView m dc

> ilDrawExpButtonsView :: InterfaceLogic SystemState (DC () -> Rect -> IO ())
> ilDrawExpButtonsView m@(SystemState f t g ty h) dc r = 
>	drawIExpButtonsView m dc

> ilShowLineRowStatus :: InterfaceLogic SystemState String
> ilShowLineRowStatus m@(SystemState f t g ty h) = 
>	let (x,y) = mouseCoordinate g
>	in ("X=" ++ (show x) ++ ", Y=" ++ (show y))

> ilShowGraErrors :: InterfaceLogic SystemState [String]
> ilShowGraErrors m = 
>	map p (visualErrors $ visualState m)
>	where
>	p (ErrorMsg l c e) = "(" ++ (show l) ++ ", " ++ (show c) ++ ") :: " ++ e 

> defaultMouseCursor :: InterfaceLogic SystemState (IO (Cursor ())) 
> defaultMouseCursor (SystemState f t g ty h) =
>	   case (modeOfOperation g) of
>              ModeSelection -> cursorCreateFromStock wxCURSOR_ARROW
>              ModeLabelEdit -> cursorCreateFromStock wxCURSOR_PENCIL
>              _ -> cursorCreateFromStock wxCURSOR_ARROW

> ilChangeMouseCursorFn :: InterfaceLogic SystemState (IO (Cursor ())) 
> ilChangeMouseCursorFn m@(SystemState f t g ty h) =
>	   case (modeOfOperation g) of
>              ModeNewFn -> cursorCreateFromStock wxCURSOR_CROSS
>              ModeNewCl -> cursorCreateFromStock wxCURSOR_CROSS
>              _ 	 -> defaultMouseCursor m

> ilChangeMouseCursorPat :: InterfaceLogic SystemState (IO (Cursor ())) 
> ilChangeMouseCursorPat m@(SystemState f t g ty h) =
>	   case (modeOfOperation g) of
>              ModePatVar -> cursorCreateFromStock wxCURSOR_CROSS
>              ModePatWild -> cursorCreateFromStock wxCURSOR_CROSS
>              ModePatEmptyLst -> cursorCreateFromStock wxCURSOR_CROSS
>              ModePatListCons -> cursorCreateFromStock wxCURSOR_CROSS
>              ModePatStr -> cursorCreateFromStock wxCURSOR_CROSS
>              ModePatInt -> cursorCreateFromStock wxCURSOR_CROSS
>              ModePatChar -> cursorCreateFromStock wxCURSOR_CROSS
>	       ModePatBool -> cursorCreateFromStock wxCURSOR_CROSS
>              _ 	 -> defaultMouseCursor m

> ilChangeMouseCursorExp :: InterfaceLogic SystemState (IO (Cursor ())) 
> ilChangeMouseCursorExp m@(SystemState f t g ty h) =
>	   case (modeOfOperation g) of
>              ModeExpApp -> cursorCreateFromStock wxCURSOR_CROSS
>              ModeExpOp -> cursorCreateFromStock wxCURSOR_CROSS
>              ModeListCons -> cursorCreateFromStock wxCURSOR_CROSS
>              ModeEmptyList -> cursorCreateFromStock wxCURSOR_CROSS
>              ModeCnstStr -> cursorCreateFromStock wxCURSOR_CROSS
>              ModeCnstInt -> cursorCreateFromStock wxCURSOR_CROSS
>              ModeCnstChar -> cursorCreateFromStock wxCURSOR_CROSS
>              ModeCnstBool -> cursorCreateFromStock wxCURSOR_CROSS
>              ModeIfStmt -> cursorCreateFromStock wxCURSOR_CROSS
>              _ 	 -> defaultMouseCursor m

> origToVirtuScroll :: SystemState -> (String, ItemState) -> (String, ItemState)
> origToVirtuScroll m (s, origVi) = 
>		let sp 		= getSpecificScrollPos m
>		    origP 	= itemPosition origVi
>		    virtuVi 	= 	
>					origVi {itemPosition = pt (pointX origP - pointX sp) (pointY origP - pointY sp)}
>		in (s, virtuVi)

> ilAnnEdtTxtGblFn m@(SystemState f t g ty h) 	= 
>	ilLblEditorText $ origToVirtuScroll m $ getLEAttrFn m
> ilAnnEdtInsPtGblFn m@(SystemState f t g ty h) 	= 
>	ilLblEditorInsPt $ origToVirtuScroll m $ getLEAttrFn m
> ilAnnEdtPosGblFn m@(SystemState f t g ty h)	=
>	ilLblEditorPos $ origToVirtuScroll m $ getLEAttrFn m
> ilAnnEdtVisiGblFn m@(SystemState f t g ty h)	=
>	ilLblEditorVisi $ origToVirtuScroll m $ getLEAttrFn m

> ilAnnEdtTxtLclFn m@(SystemState f t g ty h) 	= 
>	ilLblEditorText $ origToVirtuScroll m $ getLEAttrLclFn m
> ilAnnEdtInsPtLclFn m@(SystemState f t g ty h) 	= 
>	ilLblEditorInsPt $ origToVirtuScroll m $ getLEAttrLclFn m
> ilAnnEdtPosLclFn m@(SystemState f t g ty h)	=
>	ilLblEditorPos $ origToVirtuScroll m $ getLEAttrLclFn m
> ilAnnEdtVisiLclFn m@(SystemState f t g ty h)	=
>	ilLblEditorVisi $ origToVirtuScroll m $ getLEAttrLclFn m

> ilAnnEdtTxtFnArg m@(SystemState f t g ty h) 	= 
>	ilLblEditorText $ origToVirtuScroll m $ getLEAttrFnPat m
> ilAnnEdtInsPtFnArg m@(SystemState f t g ty h) 	= 
>	ilLblEditorInsPt $ origToVirtuScroll m $ getLEAttrFnPat m
> ilAnnEdtPosFnArg m@(SystemState f t g ty h)	=
>	ilLblEditorPos $ origToVirtuScroll m $ getLEAttrFnPat m
> ilAnnEdtVisiFnArg m@(SystemState f t g ty h)	=
>	ilLblEditorVisi $ origToVirtuScroll m $ getLEAttrFnPat m

> ilAnnEdtTxtLclFnPat m@(SystemState f t g ty h) 	= 
>	ilLblEditorText $ origToVirtuScroll m $ getLEAttrLclFnPat m
> ilAnnEdtInsPtLclFnPat m@(SystemState f t g ty h) 	= 
>	ilLblEditorInsPt $ origToVirtuScroll m $ getLEAttrLclFnPat m
> ilAnnEdtPosLclFnPat m@(SystemState f t g ty h)	=
>	ilLblEditorPos $ origToVirtuScroll m $ getLEAttrLclFnPat m
> ilAnnEdtVisiLclFnPat m@(SystemState f t g ty h)	=
>	ilLblEditorVisi $ origToVirtuScroll m $ getLEAttrLclFnPat m

> ilAnnEdtTxtFnBody m@(SystemState f t g ty h) 	= 
>	ilLblEditorText $ origToVirtuScroll m $ getLEAttrExp m
> ilAnnEdtInsPtFnBody m@(SystemState f t g ty h) 	= 
>	ilLblEditorInsPt $ origToVirtuScroll m $ getLEAttrExp m
> ilAnnEdtPosFnBody m@(SystemState f t g ty h)	=
>	ilLblEditorPos $ origToVirtuScroll m $ getLEAttrExp m
> ilAnnEdtVisiFnBody m@(SystemState f t g ty h)	=
>	ilLblEditorVisi $ origToVirtuScroll m $ getLEAttrExp m


> ilLblEditorText :: (String, ItemState) -> String
> ilLblEditorText svi = fst $ svi

> ilLblEditorInsPt :: (String, ItemState) -> Int
> ilLblEditorInsPt svi = annInsertionPoint $ snd $ svi

> ilLblEditorPos :: (String, ItemState) -> Point
> ilLblEditorPos svi = itemPosition $ snd $ svi

> ilLblEditorVisi :: (String, ItemState) -> Bool
> ilLblEditorVisi svi = annEditMode $ snd $ svi

> getSpecificScrollPos (SystemState f t g ty h) =
>	case areaOfOperation g of
>		GblFn 		-> scrlPosAnnEdtGbl g
>		LclFn		-> scrlPosAnnEdtLcl g
>		FnArg		-> scrlPosAnnEdtGblPat g
>		LclFnPat	-> scrlPosAnnEdtLclPat g
>		FnBody		-> scrlPosAnnEdtBody g



> -----------This is a test of IO IL
> {-ilShowTextualErrorsTest :: InterfaceLogic SystemState (IO [String])
> ilShowTextualErrorsTest m = 
>	do
>	errs <- (getErrors m)
>	return $ map p (fst $ errs)
>	where
>	p (ErrorMsg l c e) = "Line " ++ (show l) ++ ", Col " ++ (show c) ++ ":: " ++ e -- ++ test m
>	--test (SystemState f t g (ModelHistories h cPos)) = " " ++ show (length h) ++ " " ++ show cPos-}


