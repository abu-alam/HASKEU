> module BR where

> import MVC
> import ModelType
> import ConvertFromTextual
> import ConvertFromGraphical
> import GraphicUtils
> import TypeManagement
> import HistoryManagement
> import SyntaxTreeManagement

> import Graphics.UI.WX 
> import Graphics.UI.WXCore hiding (Module)

> import Language.Haskell.Exts.Annotated

> import System.IO.Unsafe

> type BRName = String

> class BRSync e where
> 	br :: BRName -> BusinessRule e SystemState
> 	brDoNothing :: BusinessRule e SystemState
>	brSyncV :: BRName -> BusinessRule e SystemState
>	brSyncT :: BRName -> BusinessRule e SystemState
>	brSyncVLayout :: BRName -> BusinessRule e SystemState

>	brDoNothing _ m = m
>	--Business rule controller + synchronise Graphical -> Textual + History
>	brSyncV b e m = manageHistory VisualChange $ 
>			convertFromVisual (br b e m) 
>	brSyncT b e m = manageHistory TextualChange $ 
>			convertFromTextual (br b e m)
>	brSyncVLayout b e m = 
>		manageHistory VisualChange $
>		convertFromVisual_Layout (br b e m)

> instance BRSync () where
>	br "brNewModuleClicked" = brNewModuleClicked
>	br "brUndo" = brUndo
>	br "brRedo" = brRedo
>	br "brBtnSelectionClicked" = brBtnSelectionClicked
>	br "brBtnLblEditClicked" = brBtnLblEditClicked
>	br "brBtnDeleteClicked" = brBtnDeleteClicked

> instance BRSync (Maybe FilePath, String) where
>	br "brFileOpened" = brFileOpened
> instance BRSync (Maybe FilePath) where
>	br "brFileSave" = brFileSave
>	br "brFileSaveAs" = brFileSaveAs

> instance BRSync (String, Int) where
>	br "brTextEditorChanged" = brTextEditorChanged
>	br "brModuleNameChanged" = brModuleNameChanged
>	br "brFnNameChanged" = brFnNameChanged
>	br "brLclFnNameChanged" = brLclFnNameChanged
>	br "brFnPatNameChanged" = brFnPatNameChanged
>	br "brLclFnPatNameChanged" = brLclFnPatNameChanged
>	br "brExpNameChanged" = brExpNameChanged

> instance BRSync Int where
>	br "brListTxtErrSelected" = brListTxtErrSelected
> instance BRSync EventMouse where
>	br "brMseLeftClickGblFn" = brMseLeftClickGblFn
>	br "brMseLeftClickLclFn" = brMseLeftClickLclFn
>	br "brMseLeftClickFnArg" = brMseLeftClickFnArg
>	br "brMseLeftClickLclFnPat" = brMseLeftClickLclFnPat
>	br "brMseLeftClickExp" = brMseLeftClickExp
>	br "brMseLeftClickBtnsFn" = brMseLeftClickBtnsFn
>	br "brMseLeftClickBtnsPat" = brMseLeftClickBtnsPat
>	br "brMseLeftClickBtnsExp" = brMseLeftClickBtnsExp

>	br "brMseMvSwGblFn" = brMseMvSwGblFn
>	br "brMseMvSwFnLcl" = brMseMvSwFnLcl
>	br "brMseMvSwFnPat" = brMseMvSwFnPat
>	br "brMseMvSwLclFnPat" = brMseMvSwLclFnPat
>	br "brMseMvSwFnBody" = brMseMvSwFnBody
>	br "brMseMvBtnsFn" = brMseMvBtnsFn
>	br "brMseMvBtnsPat" = brMseMvBtnsPat
>	br "brMseMvBtnsExp" = brMseMvBtnsExp

> instance BRSync EventScroll where
>	br "brScrollSwGblFn" = brScrollSwGblFn
>	br "brScrollSwLclFn" = brScrollSwLclFn
>	br "brScrollSwFnArg" = brScrollSwFnArg
>	br "brScrollSwLclFnPat" = brScrollSwLclFnPat
>	br "brScrollSwFnBody" = brScrollSwFnBody

> brBtnSelectionClicked :: BusinessRule () SystemState
> brBtnSelectionClicked _ (SystemState f t g ty h) = deselectAllButtons $ (SystemState f t g {modeOfOperation = ModeSelection} ty h)

> brBtnLblEditClicked :: BusinessRule () SystemState
> brBtnLblEditClicked _ (SystemState f t g ty h) = (SystemState f t g {modeOfOperation = ModeLabelEdit} ty h)

> brBtnDeleteClicked :: BusinessRule () SystemState
> brBtnDeleteClicked _ m = deleteNode m

> brNewModuleClicked :: BusinessRule () SystemState
> brNewModuleClicked _ (SystemState f t g ty h) = 
>	(SystemState f {path=Nothing} t{textStr=""} g ty h)

> brFileOpened :: BusinessRule (Maybe FilePath, String) SystemState
> brFileOpened (maybePath,fileContents) (SystemState f t g ty h) = 
>	case maybePath of
>            Nothing 	-> (SystemState f t g ty h)
>            Just p 	-> (SystemState f {path=maybePath} t{textStr=fileContents} g ty h)

> brFileSave :: BusinessRule (Maybe FilePath) SystemState
> brFileSave maybePath (SystemState f t g ty h) = (SystemState f {path=maybePath} t g ty h)

> brFileSaveAs :: BusinessRule (Maybe FilePath) SystemState
> brFileSaveAs maybePath (SystemState f t g ty h) = 
>	case maybePath of
>            Nothing 	-> (SystemState f t g ty h)
>            Just p 	-> (SystemState f {path=maybePath} t g ty h)

> brUndo :: BusinessRule () SystemState
> brUndo txt (SystemState f t g ty (ModelHistories h pos)) = 
>	let cPos = pos - 1
>	    ModelHistory fh th gh tyh _ = h!!cPos
>	in SystemState fh th gh tyh (ModelHistories h cPos)

> brRedo :: BusinessRule () SystemState
> brRedo txt (SystemState f t g ty (ModelHistories h pos)) = 
>	let cPos = pos + 1
>	    ModelHistory fh th gh tyh _ = h!!cPos
>	in SystemState fh th gh tyh (ModelHistories h cPos)

> {-brBugReplace :: BusinessRule () SystemState
> brBugReplace _ (SystemState f t g h) = 
>	let mh = modelHistories h
>	    mh1= (take ((length mh) - 3) mh) ++ [last mh]
>	in (SystemState f t g h{modelHistories=mh1})-}

> brTextEditorChanged :: BusinessRule (String, Int) SystemState
> brTextEditorChanged (txt,i) (SystemState f t g ty h) = 
>	SystemState 
>	f 
>	t{textStr=txt,insertionPt=i} 
>	g--{mouseCoordinate=setLineColumnStatus (txt, i)}
>	ty
>	h

> brModuleNameChanged :: BusinessRule (String, Int) SystemState
> brModuleNameChanged (txt,i) m = 
>	manageHistory VisualChange $ convertFromVisual_Type $ editModuleHead m (txt,i)

> brFnNameChanged :: BusinessRule (String, Int) SystemState
> brFnNameChanged (txt,i) m = 
>	manageHistory VisualChange $ convertFromVisual_Type $ editSelFuncName m (txt,i)

> brLclFnNameChanged :: BusinessRule (String, Int) SystemState
> brLclFnNameChanged (txt,i) m = 
>	manageHistory VisualChange $ convertFromVisual_Type $ editSelLclFuncName m (txt,i)

> brFnPatNameChanged :: BusinessRule (String, Int) SystemState
> brFnPatNameChanged (txt,i) m = 
>	manageHistory VisualChange $ convertFromVisual_Type $ editSelFuncPatName m (txt,i)

> brLclFnPatNameChanged :: BusinessRule (String, Int) SystemState
> brLclFnPatNameChanged (txt,i) m = 
>	manageHistory VisualChange $ convertFromVisual_Type $ editSelLclFuncPatName m (txt,i)

> brExpNameChanged :: BusinessRule (String, Int) SystemState
> brExpNameChanged (txt,i) m = 
>	manageHistory VisualChange $ convertFromVisual_Type $ editSelFuncExp m (txt,i)

> brListTxtErrSelected :: BusinessRule Int SystemState
> brListTxtErrSelected (-1) m = m
> brListTxtErrSelected e (SystemState f t g ty h) = 
> 	let ErrorMsg l c _ = (textualErrors t)!!e
>	in (SystemState f t {insertionPt = getInsPtFromRowCol (textStr t) l c, selectedTxtErr = e} g ty h)

> brMseLeftClickGblFn :: BusinessRule EventMouse SystemState
> brMseLeftClickGblFn (MouseLeftUp p _) m@(SystemState f t g ty h) = 
>	let m1 = setGraTypeError $ SystemState f t g  {areaOfOperation = GblFn} ty h
> 	in case (modeOfOperation g) of
>		ModeSelection 	-> 
>			selectFunc p "selected" m1
>		ModeLabelEdit 	-> 
>			selectFuncEdit p "annEditMode" m1 
>		ModeNewFn 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewFunc m1
>		ModeNewCl 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewClause m1
>		_		->
>			selectFunc p "selected" m1

> brMseLeftClickLclFn :: BusinessRule EventMouse SystemState
> brMseLeftClickLclFn (MouseLeftUp p _) m@(SystemState f t g ty h) = 
>	let m1 = setGraTypeError $ SystemState f t g  {areaOfOperation = LclFn} ty h
> 	in case (modeOfOperation g) of
>		ModeSelection 	-> 
>			selectLclFunc p "selected" m1
>		ModeLabelEdit 	-> 
>			selectLclFunc p "annEditMode" m1 
>		ModeNewFn 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclFunc m1
>		ModeNewCl 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclClause m1
>		_		->
>			selectLclFunc p "selected" m1

> brMseLeftClickFnArg :: BusinessRule EventMouse SystemState
> brMseLeftClickFnArg (MouseLeftUp p _) m@(SystemState f t g ty h) = 
> 	let m1 = setGraTypeError $ SystemState f t g  {areaOfOperation = FnArg} ty h
> 	in case (modeOfOperation g) of
>		ModeSelection 	-> 
>			selectFuncPat p "selected" m1
>		ModeLabelEdit 	-> 
>			selectFuncPat p "annEditMode" m1 
>		ModePatVar 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewPat m1
>		ModePatWild 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewPat m1
>		ModePatEmptyLst	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewPat m1
>		ModePatListCons	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewPat m1
>		ModePatStr 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewPat m1
>		ModePatInt 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewPat m1
>		ModePatChar 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewPat m1
>		ModePatBool 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewPat m1
>		_		->
>			selectFuncPat p "selected" m1

> brMseLeftClickLclFnPat :: BusinessRule EventMouse SystemState
> brMseLeftClickLclFnPat (MouseLeftUp p _) m@(SystemState f t g ty h) = 
> 	let m1 = setGraTypeError $ SystemState f t g  {areaOfOperation = LclFnPat} ty h
> 	in case (modeOfOperation g) of
>		ModeSelection 	-> 
>			selectLclFuncPat p "selected" m1
>		ModeLabelEdit 	-> 
>			selectLclFuncPat p "annEditMode" m1 
>		ModePatVar 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclPat m1
>		ModePatWild 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclPat m1
>		ModePatEmptyLst	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclPat m1
>		ModePatListCons	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclPat m1
>		ModePatStr 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclPat m1
>		ModePatInt 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclPat m1
>		ModePatChar 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclPat m1
>		ModePatBool 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewLclPat m1
>		_		->
>			selectFuncPat p "selected" m1

> brMseLeftClickExp :: BusinessRule EventMouse SystemState
> brMseLeftClickExp (MouseLeftUp p _) m@(SystemState f t g ty h) = 
> 	let m1 = setGraTypeError $ SystemState f t g  {areaOfOperation = FnBody} ty h
> 	in case (modeOfOperation g) of
>		ModeSelection 	-> 
>			selectFuncExp p "selected" m1
>		ModeLabelEdit 	-> 
>			selectFuncExp p "annEditMode" m1 
>		ModeExpApp 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewExpression m1
>		ModeExpOp 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewExpression m1
>		ModeListCons 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewExpression m1
>		ModeEmptyList 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewExpression m1
>		ModeCnstStr	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewExpression m1
>		ModeCnstInt	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewExpression m1
>		ModeCnstChar	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewExpression m1
>		ModeCnstBool 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewExpression m1
>		ModeIfStmt 	-> manageHistory VisualChange $ convertFromVisual_Layout $ addNewExpression m1
>		_		->
>			selectFuncExp p "selected" m1

> brMseLeftClickBtnsFn :: BusinessRule EventMouse SystemState
> brMseLeftClickBtnsFn (MouseLeftUp p _) m@(SystemState f t g ty h) = 
> 	selectFnButton p "btnSel" $ deselectAllButtons m
> brMseLeftClickBtnsPat :: BusinessRule EventMouse SystemState
> brMseLeftClickBtnsPat (MouseLeftUp p _) m@(SystemState f t g ty h) = 
> 	selectPatButton p "btnSel" $ deselectAllButtons m
> brMseLeftClickBtnsExp :: BusinessRule EventMouse SystemState
> brMseLeftClickBtnsExp (MouseLeftUp p _) m@(SystemState f t g ty h) = 
> 	selectExpButton p "btnSel" $ deselectAllButtons m
> brMseMvBtnsFn :: BusinessRule EventMouse SystemState
> brMseMvBtnsFn (MouseMotion p _) m@(SystemState f t g ty h) = 
> 	selectFnButton p "mseInsideBtn" m
> brMseMvBtnsPat :: BusinessRule EventMouse SystemState
> brMseMvBtnsPat (MouseMotion p _) m@(SystemState f t g ty h) = 
> 	selectPatButton p "mseInsideBtn" m
> brMseMvBtnsExp :: BusinessRule EventMouse SystemState
> brMseMvBtnsExp (MouseMotion p _) m@(SystemState f t g ty h) = 
> 	selectExpButton p "mseInsideBtn" m

> brMseMvSwFnBody :: BusinessRule EventMouse SystemState
> brMseMvSwFnBody (MouseMotion p _) m@(SystemState f t g ty h) = 
>	setXYStatus p $ selectFuncExp p "mseInside" m
> 	{-let m1 = SystemState f t g  {areaOfOperation = FnBody} ty h
> 	in case (modeOfOperation g) of
>		ModeSelection 	-> 
>			selectFuncExp p "mseInside" m1
>		_		->
>			(SystemState f t g ty h)-}

> brMseMvSwGblFn :: BusinessRule EventMouse SystemState
> brMseMvSwGblFn (MouseMotion p _) m@(SystemState f t g ty h) = 
> 	setXYStatus p $ selectFunc p "mseInside" m
> brMseMvSwFnLcl :: BusinessRule EventMouse SystemState
> brMseMvSwFnLcl (MouseMotion p _) m@(SystemState f t g ty h) = 
> 	setXYStatus p $ selectLclFunc p "mseInside" m
> brMseMvSwFnPat :: BusinessRule EventMouse SystemState
> brMseMvSwFnPat (MouseMotion p _) m@(SystemState f t g ty h) = 
> 	setXYStatus p $ selectFuncPat p "mseInside" m
> brMseMvSwLclFnPat :: BusinessRule EventMouse SystemState
> brMseMvSwLclFnPat (MouseMotion p _) m@(SystemState f t g ty h) = 
> 	setXYStatus p $ selectLclFuncPat p "mseInside" m

> setXYStatus :: Point -> SystemState -> SystemState
> setXYStatus (Point x y) m@(SystemState f t g ty h) =
>	let 	msg	= ("X=" ++ (show x) ++ ", Y=" ++ (show y))
>	in 	SystemState 
>			f 
>			t
>			g{mouseCoordinate=(x,y)}--msg}
>			ty
>			h

> brScrollSwGblFn :: BusinessRule EventScroll SystemState
> brScrollSwGblFn st (SystemState f t g ty h) = 
>	let	Point x y = scrlPosAnnEdtGbl g
>		pos	= scrollSW st (x, y)
>	in	(SystemState f t g {scrlPosAnnEdtGbl = pos} ty h)

> brScrollSwLclFn :: BusinessRule EventScroll SystemState
> brScrollSwLclFn st (SystemState f t g ty h) = 
>	let	Point x y = scrlPosAnnEdtLcl g
>		pos	= scrollSW st (x, y)
>	in	(SystemState f t g {scrlPosAnnEdtLcl = pos} ty h)

> brScrollSwFnArg :: BusinessRule EventScroll SystemState
> brScrollSwFnArg st (SystemState f t g ty h) = 
>	let	Point x y = scrlPosAnnEdtGblPat g
>		pos	= scrollSW st (x, y)
>	in	(SystemState f t g {scrlPosAnnEdtGblPat = pos} ty h)

> brScrollSwLclFnPat :: BusinessRule EventScroll SystemState
> brScrollSwLclFnPat st (SystemState f t g ty h) = 
>	let	Point x y = scrlPosAnnEdtLclPat g
>		pos	= scrollSW st (x, y)
>	in	(SystemState f t g {scrlPosAnnEdtLclPat = pos} ty h)

> brScrollSwFnBody :: BusinessRule EventScroll SystemState
> brScrollSwFnBody st (SystemState f t g ty h) = 
>	let	Point x y = scrlPosAnnEdtBody g
>		pos	= scrollSW st (x, y)
>	in	(SystemState f t g {scrlPosAnnEdtBody = pos} ty h)

> scrollSW st (x, y) = 
>	case st of
>		(ScrollTop Horizontal sp) 		-> changeHorizontal sp (x, y)
> 		(ScrollBottom Horizontal sp) 		-> changeHorizontal sp (x, y)
>		(ScrollLineUp Horizontal sp) 		-> changeHorizontal sp (x, y)
> 		(ScrollLineDown Horizontal sp) 		-> changeHorizontal sp (x, y)
>		(ScrollPageUp Horizontal sp) 		-> changeHorizontal sp (x, y)
> 		(ScrollPageDown Horizontal sp) 		-> changeHorizontal sp (x, y)
>		(ScrollTrack Horizontal sp) 		-> changeHorizontal sp (x, y)
> 		(ScrollRelease Horizontal sp) 		-> changeHorizontal sp (x, y)
> 		(ScrollTop Vertical sp) 	 	-> changeVertical sp (x, y)
> 		(ScrollBottom Vertical sp) 	 	-> changeVertical sp (x, y)
> 		(ScrollLineUp Vertical sp) 	 	-> changeVertical sp (x, y)
> 		(ScrollLineDown Vertical sp) 	 	-> changeVertical sp (x, y)
> 		(ScrollPageUp Vertical sp) 		-> changeVertical sp (x, y)
> 		(ScrollPageDown Vertical sp) 	 	-> changeVertical sp (x, y)
> 		(ScrollTrack Vertical sp) 	 	-> changeVertical sp (x, y)
> 		(ScrollRelease Vertical sp) 	 	-> changeVertical sp (x, y)


> changeHorizontal sp (x, y) =
>	pt sp y

> changeVertical sp (x, y) =
>	pt x sp
