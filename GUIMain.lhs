> {-# LANGUAGE ScopedTypeVariables #-}

> module Main where

> import MVC
> import MVC_WX
> import ModelType
> import BR
> import IL

> import Graphics.UI.WX
> import Graphics.UI.WXCore hiding (View)
> import Reactive.Banana
> import Reactive.Banana.WX

> import Data.Bits -- (.|.)


> mnuID_NEW_WINDOW, mnuID_NEW_MODULE, mnuID_OPEN, mnuID_SAVE, mnuID_SAVEAS, mnuID_UNDO, mnuID_REDO, mnuID_QUIT :: Int
> mnuID_NEW_WINDOW = 1
> mnuID_NEW_MODULE = 2
> mnuID_OPEN = 3
> mnuID_SAVE = 4
> mnuID_SAVEAS = 5
> mnuID_UNDO = 6
> mnuID_REDO = 7
> mnuID_QUIT = 8

> toolbarIconPath :: String
> toolbarIconPath = "./resources/images/guiMain/"
> textualEditorIconPath :: String
> textualEditorIconPath = "./resources/images/guiTextual/"
> graphicalEditorIconPath :: String
> graphicalEditorIconPath = "./resources/images/guiGraphical/"

> main :: IO ()
> main = start $ do Main.gui

> gui :: IO ()
> gui = do
>	   let networkDescription :: 
>			forall t. Frameworks t => Moment t ()
>	       networkDescription = do
>			(lstView, lstCtlr) <- setLayoutAndVC
>			mvc modelInitVal lstView lstCtlr 
>	   network <- compile $ networkDescription
>	   actuate network


> -- set layout and view-controller
> setLayoutAndVC ::
>	Frameworks t => 
>		Moment t (	[Model t SystemState 
>					-> View t ViewType],  							 
>				[Controller t (SystemState 
>					-> SystemState)])
> setLayoutAndVC = do
> 	win <- liftIONow $ frame [style := frameDefaultStyle .|. wxMAXIMIZE, visible := False]
>	  -- Main Menu and Toolbar--------------------------------------------------------------

>	imgPathNewModule <- liftIONow $ getFilePathIO toolbarIconPath "newmodule.png"
>       imgPathOpen <- liftIONow $ getFilePathIO toolbarIconPath "open.png"
>       imgPathSave <- liftIONow $ getFilePathIO toolbarIconPath "save.png"
>       imgPathSaveAs <- liftIONow $ getFilePathIO toolbarIconPath "saveas.png"
>  	imgPathUndo <- liftIONow $ getFilePathIO toolbarIconPath "undo.png"
>       imgPathRedo <- liftIONow $ getFilePathIO toolbarIconPath "redo.png"
>       imgPathQuit <- liftIONow $ getFilePathIO toolbarIconPath "quit.png"
>        

>	-- Main Menu and Toolbar--------------------------------------------------------------

>	pnlMain		<- liftIONow $ panel win []

>	btnNewModule <- liftIONow $ bitmapButton pnlMain [text := "New Module", tooltip := "New Module", 
>				picture := getFilePath toolbarIconPath "newmodule.png", clientSize := sz 60 60]
>	btnOpenFile <- liftIONow $ bitmapButton pnlMain [text := "Open", tooltip := "Open File", 
>				picture := getFilePath toolbarIconPath "open.png", clientSize := sz 60 60]
>	btnSave <- liftIONow $ bitmapButton pnlMain [text := "Save", tooltip := "Save", 
>				picture := getFilePath toolbarIconPath "save.png", clientSize := sz 60 60]
>	btnSaveAs <- liftIONow $ bitmapButton pnlMain [text := "Save as", tooltip := "Save as", 
>				picture := getFilePath toolbarIconPath "saveas.png", clientSize := sz 60 60]
>	btnUndo <- liftIONow $ bitmapButton pnlMain [text := "Undo", tooltip := "Undo File", 
>				picture := getFilePath toolbarIconPath "undo.png", clientSize := sz 60 60]
>	btnRedo <- liftIONow $ bitmapButton pnlMain [text := "Redo", tooltip := "Redo", 
>				picture := getFilePath toolbarIconPath "redo.png", clientSize := sz 60 60]
>	btnQuit <- liftIONow $ bitmapButton pnlMain [text := "Quit", tooltip := "Quit", 
>				picture := getFilePath toolbarIconPath "quit.png", clientSize := sz 60 60,
>				on command:=close win]
>	btnNewWindow <- liftIONow $ bitmapButton pnlMain [text := "New Window", tooltip := "New Window", 
>				picture := getFilePath toolbarIconPath "newwindow.png", clientSize := sz 60 60,
>				on command := gui]

>	spltwMain 	<- liftIONow $ splitterWindow pnlMain []

>	-- Graphical---------------------------------------------------------------------------
> 	pnlGraphical   	<- liftIONow $ panel  spltwMain []

>	spltwGra 	<- liftIONow $ splitterWindow pnlGraphical []
> 	pnlGraEditor   	<- liftIONow $ panel  spltwGra []
> 	pnlGraErrors  	<- liftIONow $ panel  spltwGra []

>	spltwGraEditor 	<- liftIONow $ splitterWindow pnlGraEditor []
> 	pnlGraModule   	<- liftIONow $ panel spltwGraEditor []
> 	pnlGraFunc  	<- liftIONow $ panel spltwGraEditor []

>	spltwGraFunc	<- liftIONow $ splitterWindow pnlGraFunc []
> 	pnlGraFuncArgsLcl  	<- liftIONow $ panel  spltwGraFunc []
> 	pnlGraFuncBody		<- liftIONow $ panel  spltwGraFunc []

>	spltwGraModule 	<- liftIONow $ splitterWindow pnlGraModule []
>	spltwGraArgsLcl	<- liftIONow $ splitterWindow pnlGraFuncArgsLcl []


>	spltwGraFuncBody<- liftIONow $ splitterWindow pnlGraFuncBody []
>	pnlGraFuncBodyMain<- liftIONow $ panel spltwGraFuncBody []
> 	pnlGraFuncBodyButtons	<- liftIONow $ panel spltwGraFuncBody []

>	spltwGraButtonsFnPat	<- liftIONow $ splitterWindow pnlGraFuncBodyButtons []
> 	pnlGraButtonsPatExp	<- liftIONow $ panel  spltwGraButtonsFnPat []

>	spltwGraButtonsPatExp	<- liftIONow $ splitterWindow pnlGraButtonsPatExp []


>	btnSelection <- liftIONow $ bitmapButton pnlGraphical [text := "Sel", tooltip := "Selection", 
>				picture := getFilePath graphicalEditorIconPath "selection.png", clientSize := sz 40 40]
>	btnLabelEdit <- liftIONow $ bitmapButton pnlGraphical [text := "TModi", tooltip := "Modify Text", 
>				picture := getFilePath graphicalEditorIconPath "textModify.png", clientSize := sz 40 40]
>	btnDelete <- liftIONow $ bitmapButton pnlGraphical [text := "Del", tooltip := "Delete", 
>				picture := getFilePath graphicalEditorIconPath "delete.png", clientSize := sz 40 40]
>	lblLineRowStatus <- liftIONow $ staticText pnlGraphical [text := "Line : 100, Column : 100", clientSize := sz 120 20]

>       {-btnAddNewFunction <- liftIONow $ bitmapButton pnlGraModule [text := "Fn", tooltip := "Add New Function", 
>				picture := getFilePath graphicalEditorIconPath "addNewFunction.png", clientSize := sz 40 40]
>       btnAddNewClause <- liftIONow $ bitmapButton pnlGraModule [text := "Cl", tooltip := "Add New Clause", 
>				picture := getFilePath graphicalEditorIconPath "addNewClause.png", clientSize := sz 40 40]

>       btnFunctionPattern <- liftIONow $ bitmapButton pnlGraFuncArgsLcl [text := "Pat", tooltip := "Add Function Pattern", 
>				picture := getFilePath graphicalEditorIconPath "pattern.png", clientSize := sz 40 40]
>       btnPatternList01 <- liftIONow $ bitmapButton pnlGraFuncArgsLcl [text := "PatL1", tooltip := "Add Function Pattern List []", 
>				picture := getFilePath graphicalEditorIconPath "patternList01.png", clientSize := sz 40 40]
>       btnPatternList02 <- liftIONow $ bitmapButton pnlGraFuncArgsLcl [text := "PatL2", tooltip := "Add Function Pattern List (x:xs))", 
>				picture := getFilePath graphicalEditorIconPath "patternList02.png", clientSize := sz 40 40]
>	btnAddLclFn <- liftIONow $ bitmapButton pnlGraFuncArgsLcl [text := "AddLclFun", tooltip := "Add Local Function", 
>				picture := getFilePath graphicalEditorIconPath "addLocalFunction.png", clientSize := sz 40 40]

>       btnCallFunction <- liftIONow $ bitmapButton pnlGraFuncBodyMain [text := "F(x)", tooltip := "Call a Function", 
>	  			picture := getFilePath graphicalEditorIconPath "functionCall.png", clientSize := sz 40 40]
> 	btnOperator <- liftIONow $ bitmapButton pnlGraFuncBodyMain [text := "Op", tooltip := "Call Operator", 
>				picture := getFilePath graphicalEditorIconPath "operatorCall.png", clientSize := sz 40 40]
>       btnIfStmt <- liftIONow $ bitmapButton pnlGraFuncBodyMain [text := "If-Then", tooltip := "Add IF-Then-Else Statement", 
>				picture := getFilePath graphicalEditorIconPath "if_then.png", clientSize := sz 40 40]
>	btnCaseStmt <- liftIONow $ bitmapButton pnlGraFuncBodyMain [text := "Case", tooltip := "Add CASE Statement", 
>				picture := getFilePath graphicalEditorIconPath "case.png", clientSize := sz 40 40]
>	btnJoin <- liftIONow $ bitmapButton pnlGraFuncBodyMain [text := "Join", tooltip := "Joining", 
>				picture := getFilePath graphicalEditorIconPath "join.png", clientSize := sz 40 40]

>	btnConstant <- liftIONow $ bitmapButton pnlGraFuncBodyMain [text := "Cnst", tooltip := "Add Constant", 
>				picture := getFilePath graphicalEditorIconPath "constant.png", clientSize := sz 40 40]-}

>	swGraMdlFunc <- liftIONow $ scrolledWindow spltwGraModule [bgcolor := white, scrollRate := sz 1 1, 
>                               fullRepaintOnResize := False, 
>                               style := wxHSCROLL .|. wxVSCROLL, virtualSize := sz 0 5000]

>	txtModuleName  <- liftIONow $ textEntry swGraMdlFunc [text := "", position := pt 0 20, 
>							      clientSize := sz 120 25]
>	txtAnnEdtFnVw  <- liftIONow $ textEntry swGraMdlFunc [text := "txtAnnEdtFnVw", position := pt 0 105, 
>							      clientSize := sz 130 25, visible := False]

>	swGraFuncLcl <- liftIONow $ scrolledWindow spltwGraModule [bgcolor := white, scrollRate := sz 1 1, 
>                               fullRepaintOnResize := False, 
>                               style := wxHSCROLL .|. wxVSCROLL, virtualSize := sz 0 5000]

>	txtAnnEdtLclFnVw  <- liftIONow $ textEntry swGraFuncLcl [text := "", position := pt 0 70, 
>							         clientSize := sz 130 25, visible := False]
>	let rowGraCommonButtons	= row 0 [widget btnSelection, widget btnLabelEdit, hstretch $ widget btnDelete,
>					 alignRight $ hstretch $ boxed "" $ widget lblLineRowStatus]
>	--let rowGraMdlButtons	= row 0 [widget btnAddNewFunction, hstretch $ widget btnAddNewClause]
>	let cntrGraModule = container pnlGraModule $ column 5 --[rowGraCommonButtons, rowGraMdlButtons,
>				[fill $ hsplit spltwGraModule 5 260 (widget swGraMdlFunc) (widget swGraFuncLcl)]

>	swGraFuncArgs <- liftIONow $ scrolledWindow spltwGraArgsLcl [bgcolor := white, scrollRate := sz 1 1, 
>                               fullRepaintOnResize := False, 
>                               style := wxHSCROLL .|. wxVSCROLL, virtualSize := sz 0 5000]
>	swGraFuncLclArgs  <- liftIONow $ scrolledWindow spltwGraArgsLcl [bgcolor := white, scrollRate := sz 1 1, 
>                               fullRepaintOnResize := False, 
>                               style := wxHSCROLL .|. wxVSCROLL, virtualSize := sz 0 5000]

>	txtAnnEdtFnArg  <- liftIONow $ textEntry swGraFuncArgs [text := "", position := pt 0 70, 
>							         clientSize := sz 105 25, visible := False]
>	txtAnnEdtLclFnPat  <- liftIONow $ textEntry swGraFuncLclArgs [text := "", position := pt 0 70, 
>							         clientSize := sz 105 25, visible := False]


>	--let rowGraFuncArgs	= row 0 [widget btnFunctionPattern, widget btnPatternList01, 
>	--				 hstretch $ widget btnPatternList02]
>	--let rowGraFuncLcl	= row 0 [hstretch $ widget btnAddLclFn]
>	let cntrGraFuncArgsLcl = container pnlGraFuncArgsLcl $ column 5 --[rowGraFuncArgs, rowGraFuncLcl,
>				[fill $ hsplit spltwGraArgsLcl 5 260 (widget swGraFuncArgs) (widget swGraFuncLclArgs)]

>	swGraButtonsExp <- liftIONow $ scrolledWindow spltwGraButtonsPatExp [bgcolor := white, scrollRate := sz 1 1, 
>                               fullRepaintOnResize := False, 
>                               style := wxHSCROLL .|. wxVSCROLL, virtualSize := sz 0 0]
>	swGraButtonsPat <- liftIONow $ scrolledWindow spltwGraButtonsPatExp [bgcolor := white, scrollRate := sz 1 1, 
>                               fullRepaintOnResize := False, 
>                               style := wxHSCROLL .|. wxVSCROLL, virtualSize := sz 0 0]

>	let cntrGraFuncBodyCallFnLists = container pnlGraButtonsPatExp $ column 5 [fill $ 
>				hsplit spltwGraButtonsPatExp 5 260 (widget swGraButtonsPat) (widget swGraButtonsExp)]

>	swGraButtonsFn <- liftIONow $ scrolledWindow spltwGraButtonsFnPat [bgcolor := white, scrollRate := sz 1 1, 
>                               fullRepaintOnResize := False, 
>                               style := wxHSCROLL .|. wxVSCROLL, virtualSize := sz 0 0]

>       --let labelsChoiceImport = ["This Module", "Local Modules", "Standard Haskell"]
>       --choiceImport   <- liftIONow $ choice pnlGraFuncBodyButtons [items := labelsChoiceImport] 
>       --let labelsChoiceImportOf = ["Functions", "Operators", "Constants"]
>       --choiceImportOf   <- liftIONow $ choice pnlGraFuncBodyButtons [items := labelsChoiceImportOf] 
>	--let rowGraFuncBodyCallFn  = row 0 [boxed "Import" $ widget choiceImport, boxed "of" $ widget choiceImportOf]

>	let cntrGraFuncBodyCallFn = container pnlGraFuncBodyButtons $ column 5 [--rowGraFuncBodyCallFn, --vspace 23,
>				fill $ hsplit spltwGraButtonsFnPat 5 260 (widget swGraButtonsFn) (cntrGraFuncBodyCallFnLists)]

>	swGraFuncBody  <- liftIONow $ scrolledWindow pnlGraFuncBodyMain [bgcolor := white, scrollRate := sz 1 1, 
>                               fullRepaintOnResize := False, 
>                               style := wxHSCROLL .|. wxVSCROLL, virtualSize := sz 10000 10000]

>	txtAnnEdtFnBody  <- liftIONow $ textEntry swGraFuncBody [text := "", position := pt 0 70, 
>							         clientSize := sz 60 25, visible := False]--,
>								 {-on onText := do propagateEvent
>								    --repaint lstError
>								                 repaint swGraFuncBody]-}
>	--let rowGraFuncBody	= row 0 [widget btnCallFunction, widget btnOperator,  
>	--				 widget btnIfStmt, widget btnCaseStmt, hstretch $ widget btnJoin]
>	--let rowGraFuncBodyConst	= row 0 [hstretch $ widget btnConstant]

>	let cntrGraFuncBodyMain = container pnlGraFuncBodyMain $ column 5 --[rowGraFuncBody, rowGraFuncBodyConst,
>					 [fill $ widget swGraFuncBody]


>	let cntrGraFuncBody = container pnlGraFuncBody $ column 5 [fill $ 
>					 vsplit spltwGraFuncBody 5 260 (cntrGraFuncBodyMain) (cntrGraFuncBodyCallFn)]

>	let cntrGraFunc = container pnlGraFunc $ column 5 [fill $ 
>					 vsplit spltwGraFunc 5 260 (cntrGraFuncArgsLcl) (cntrGraFuncBody)]

>	let cntrGraEditor = container pnlGraEditor $ column 5 [fill $
>					 vsplit spltwGraEditor 5 260 (cntrGraModule) (cntrGraFunc)]

>	lstGraError <- liftIONow $ singleListBox pnlGraErrors []
>	let cntrGraErrors = container pnlGraErrors $ column 5 [fill $ boxed "Errors" $ fill $ widget lstGraError]

>	let cntrGraphical = container pnlGraphical $ boxed "" $ column 5 [rowGraCommonButtons,
>				fill $ hsplit spltwGra 5 260 (cntrGraEditor) (cntrGraErrors)]
>	  -- Graphical---------------------------------------------------------------------------
>	
>	-- Textual ---------------------------------------------------------------------------
>	pnlTextual 	<- liftIONow $ panel spltwMain []

>	btnCut 		<- liftIONow $ bitmapButton pnlTextual [text := "Cut", tooltip := "Cut", 
>			picture := getFilePath textualEditorIconPath "cut01.png", clientSize := sz 40 40]
>	btnCopy 	<- liftIONow $ bitmapButton pnlTextual [text := "Copy", tooltip := "Copy", 
>			picture := getFilePath textualEditorIconPath "copy01.png", clientSize := sz 40 40]
>	btnPaste 	<- liftIONow $ bitmapButton pnlTextual [text := "Paste", tooltip := "Paste", 
>			picture := getFilePath textualEditorIconPath "paste01.png", clientSize := sz 40 40]
>	btnFind 	<- liftIONow $ bitmapButton pnlTextual [text := "Find", tooltip := "Find", 
>			picture := getFilePath textualEditorIconPath "find01.png", clientSize := sz 40 40]
>	btnReplace 	<- liftIONow $ bitmapButton pnlTextual [text := "Replace", tooltip := "Replace", 
>			picture := getFilePath textualEditorIconPath "replace01.png", clientSize := sz 40 40]
>	lblLnColStatus <- liftIONow $ staticText pnlTextual [text := "Ln=0, Col=0", clientSize := sz 120 20]
>	-- We create a FindReplaceData that will hold the information about the
>        -- last search
>	findReplaceData	<- liftIONow $ findReplaceDataCreate wxFR_DOWN


>	spltwTextual 	<- liftIONow $ splitterWindow pnlTextual []
>       txtEditor  	<- liftIONow $ textCtrlRich spltwTextual [wrap := WrapLine]

>	

>	pnlTextualErrors<- liftIONow $ panel spltwTextual []
>	lstTextualError <- liftIONow $ singleListBox pnlTextualErrors []

>	let cntrTextualErrors = container pnlTextualErrors $ column 5 [fill $ boxed "Errors" $ fill $ widget lstTextualError]
>	let cntrTextual = container pnlTextual $ boxed "" $ column 5 
>				 [row 0 [widget btnCut, widget btnCopy, 
>					 widget btnPaste, widget btnFind, widget btnReplace,
>					 alignRight $ hstretch $ boxed "" $ widget lblLnColStatus],
>				  fill $ hsplit spltwTextual 5 260 (widget txtEditor) (cntrTextualErrors)]

>       liftIONow $ focusOn txtEditor
>	liftIONow $ set lstTextualError [on doubleClick := \p -> focusOn txtEditor]

>	--some unreactives
>	{-let swRepaints w = 
>		do 
>		propagateEvent 
>		repaint w--swGraMdlFunc
>		{-repaint swGraFuncLcl
>		repaint swGraFuncArgs
>		repaint swGraFuncLclArgs
>		repaint swGraFuncBody
>		repaint swGraButtonsExp
>		repaint swGraButtonsFn
>		repaint swGraMdlFunc-}
>	let lstSw =  
>		[swGraMdlFunc, swGraFuncLcl, swGraFuncArgs,
>		swGraFuncLclArgs, swGraFuncBody, swGraButtonsExp,
>		swGraButtonsFn, swGraMdlFunc]

>       let setSWRepaintsOnMse ev w = set w [ev := \a -> (swRepaints w)]
>       let setSWRepaintsOnText ev txt w = set txt [ev := (swRepaints w)]

>	liftIONow $ mapM (setSWRepaintsOnMse (on mouse)) lstSw
>	liftIONow $ mapM (setSWRepaintsOnText (on onText) txtEditor) lstSw

>	liftIONow $ set lstTextualError [on doubleClick := \p -> focusOn txtEditor]-}
>	-- Textual ---------------------------------------------------------------------------

>	let rowToolbarBtns = row 0 [widget btnNewModule, hspace 20, widget btnOpenFile, widget btnSave, widget btnSaveAs,
>				    hspace 20, widget btnUndo, widget btnRedo, hspace 20, 
>				    widget btnQuit, hspace 150,  
>				    alignRight $ hstretch $ widget btnNewWindow]
>	liftIONow $ set win [layout := container pnlMain $ margin 5 $ column 5 [
>					rowToolbarBtns,
>					fill $ vsplit spltwMain 5 260 (cntrTextual) (cntrGraphical)]
>		 	]

>	liftIONow $ splitterWindowSetSashPosition spltwMain 350 True

>	liftIONow $ splitterWindowSetSashPosition spltwTextual 500 True

>	liftIONow $ splitterWindowSetSashPosition spltwGra 550 True
>	liftIONow $ splitterWindowSetSashPosition spltwGraEditor 140 True
>	liftIONow $ splitterWindowSetSashPosition spltwGraFunc 150 True
>	liftIONow $ splitterWindowSetSashPosition spltwGraModule 350 True
>	liftIONow $ splitterWindowSetSashPosition spltwGraArgsLcl 350 True
>	liftIONow $ splitterWindowSetSashPosition spltwGraFuncBody 545 True
>	liftIONow $ splitterWindowSetSashPosition spltwGraButtonsFnPat 100 True
>	liftIONow $ splitterWindowSetSashPosition spltwGraButtonsPatExp 200 True


>	-- Controllers-----------------------------------------------------------------------
>	-- Textual Controlers----------------------------------------------------------------
>	eBtnNewModule <- evButtonCommand btnNewModule
>	let cBtnNewModuleCommand = controller (btnNewModule, eBtnNewModule) (brSyncT "brNewModuleClicked")
>	eBtnOpenFile <- evButtonCommandFileOpen btnOpenFile win
>	let cBtnFileOpenCommand = controller (btnOpenFile, eBtnOpenFile) (brSyncT "brFileOpened")
>	eBtnSaveFile <- evButtonCommandFileSave btnSave win txtEditor
>	let cBtnFileSaveCommand = controller (btnSave, eBtnSaveFile) (brSyncT "brFileSave")
>	eBtnSaveFileAs <- evButtonCommandFileSaveAs btnSaveAs win txtEditor
>	let cBtnFileSaveAsCommand = controller (btnSaveAs, eBtnSaveFileAs) (brSyncT "brFileSaveAs")
>	eBtnUndo <- evButtonCommand btnUndo
>	let cBtnUndoCommand = controller (btnUndo, eBtnUndo) (br "brUndo") -- No br sync in case of undo redo
>	eBtnRedo <- evButtonCommand btnRedo
>	let cBtnRedoCommand = controller (btnRedo, eBtnRedo) (br "brRedo") -- No br sync in case of undo redo
>	eBtnCut <- evButtonCommandCut btnCut txtEditor
>	let cBtnCutCommand = controller (btnCut, eBtnCut) (brDoNothing)
>	eBtnCopy <- evButtonCommandCopy btnCopy txtEditor
>	let cBtnCopyCommand = controller (btnCopy, eBtnCopy) (brDoNothing)
>	eBtnPaste <- evButtonCommandPaste btnPaste txtEditor
>	let cBtnPasteCommand = controller (btnPaste, eBtnPaste) (brDoNothing)
>	eBtnFind <- evButtonCommandFind btnFind win txtEditor findReplaceData
>	let cBtnFindCommand = controller (btnFind, eBtnFind) (brDoNothing)
>	eBtnReplace <- evButtonCommandReplace btnReplace win txtEditor findReplaceData
>	let cBtnReplaceCommand = controller (btnReplace, eBtnReplace) (brDoNothing)--(brBugReplace)--

>	eTxtChanged <- evTextChanged txtEditor
>	let cTextEditorChanged = controller (txtEditor, eTxtChanged) (brSyncT "brTextEditorChanged")
>	eLstTxtErrSel <- evSelListBox lstTextualError
>	let cLstTextErrSelected = controller (lstTextualError, eLstTxtErrSel) (brSyncT "brListTxtErrSelected")

>	--eChoiceMdlTySel <- evSelChoice choiceImport
>	--let cChoiceMdlTySelected = controller (choiceImport, eChoiceMdlTySel) (brSyncT "brChoiceMdlTySelected")
>	--eChoiceGblFnTySel <- evSelChoice choiceImportOf
>	--let cChoiceGblFnTySelected = controller (choiceImportOf, eChoiceGblFnTySel) (brSyncT "brChoiceGblFnTySelected")

>	---- Fix scrolled window @repaint@ event not being fired when Model change
>	let lstSw =  
>		[swGraMdlFunc, swGraFuncLcl, swGraFuncArgs,
>		swGraFuncLclArgs, swGraFuncBody, swGraButtonsExp,
>		swGraButtonsFn, swGraButtonsPat, swGraMdlFunc]

>	let eSwMouseRepaintSw w = evSwMouseRepaintSwAll w lstSw
>	eLstSwMouseRepaintSw <- mapM eSwMouseRepaintSw lstSw
>	let cSwMouseRepaintSw wNe = controller wNe (brDoNothing)
>	let cLstSwMouseRepaintSw = map cSwMouseRepaintSw (zip lstSw eLstSwMouseRepaintSw)
>	--test

>	eMseMvSwGraMdlFunc <- evMouseMove swGraMdlFunc
>	let cMseMvSwGraMdlFunc = controller (swGraMdlFunc, eMseMvSwGraMdlFunc) (br "brMseMvSwGblFn")
>	eMseMvSwGraFuncLcl <- evMouseMove swGraFuncLcl
>	let cMseMvSwGraFuncLcl = controller (swGraFuncLcl, eMseMvSwGraFuncLcl) (br "brMseMvSwFnLcl")
>	eMseMvSwGraFuncArgs <- evMouseMove swGraFuncArgs
>	let cMseMvSwGraFuncArgs = controller (swGraFuncArgs, eMseMvSwGraFuncArgs) (br "brMseMvSwFnPat")
>	eMseMvSwGraFuncLclArgs <- evMouseMove swGraFuncLclArgs
>	let cMseMvSwGraFuncLclArgs = controller (swGraFuncLclArgs, eMseMvSwGraFuncLclArgs) (br "brMseMvSwLclFnPat")
>	eMseMvSwGraFnBody <- evMouseMove swGraFuncBody
>	let cMseMvSwGraFnBody = controller (swGraFuncBody, eMseMvSwGraFnBody) (br "brMseMvSwFnBody")

>	eMseMvSwBtnFn <- evMouseMove swGraButtonsFn
>	let cMseMvSwBtnFn = controller (swGraButtonsFn, eMseMvSwBtnFn) (br "brMseMvBtnsFn")
>	eMseMvSwBtnPat <- evMouseMove swGraButtonsPat
>	let cMseMvSwBtnPat = controller (swGraButtonsPat, eMseMvSwBtnPat) (br "brMseMvBtnsPat")
>	eMseMvSwBtnExp <- evMouseMove swGraButtonsExp
>	let cMseMvSwBtnExp = controller (swGraButtonsExp, eMseMvSwBtnExp) (br "brMseMvBtnsExp")

>	{-eMseMvSwMdlFunc <- evMouseMove swGraMdlFunc
>	let cTest1 = controller (swGraMdlFunc, eMseMvSwMdlFunc) brMseMvGblFn
>	eMseMvSwFuncLcl <- evMouseMove swGraFuncLcl
>	let cTest2 = controller (swGraFuncLclArgs, eMseMvSwFuncLcl) brMseMvFnLcl-}
>	eBtnSelection <- evButtonCommand btnSelection
>	let cBtnSelectionCommand = controller (btnSelection, eBtnSelection) (br "brBtnSelectionClicked")
>	eBtnLabelEdit <- evButtonCommand btnLabelEdit
>	let cBtnLabelEditCommand = controller (btnLabelEdit, eBtnLabelEdit) (br "brBtnLblEditClicked")
>	eBtnDelete <- evButtonCommand btnDelete
>	let cBtnDeleteCommand = controller (btnDelete, eBtnDelete) (brSyncVLayout "brBtnDeleteClicked")

>	eBtnDelRepaintSw <- evBtnCommRepaintSw btnDelete lstSw 
>	let cBtnDelRepaintSw = controller (btnDelete, eBtnDelRepaintSw) (brDoNothing)
>	eBtnUndoRepaintSw <- evBtnCommRepaintSw btnUndo lstSw 
>	let cBtnUndoRepaintSw = controller (btnUndo, eBtnUndoRepaintSw) (brDoNothing)
>	eBtnRedoRepaintSw <- evBtnCommRepaintSw btnRedo lstSw 
>	let cBtnRedoRepaintSw = controller (btnRedo, eBtnRedoRepaintSw) (brDoNothing)

>	eMseLUpSwMdlFunc <- evMouseLeftUp swGraMdlFunc
>	let cMseLUpSwMdlFunc = controller (swGraMdlFunc, eMseLUpSwMdlFunc) (br "brMseLeftClickGblFn")
>	eMseLUpSwGraFuncLcl <- evMouseLeftUp swGraFuncLcl
>	let cMseLUpSwGraFuncLcl = controller (swGraFuncLcl, eMseLUpSwGraFuncLcl) (br "brMseLeftClickLclFn")
>	eMseLUpSwGraFuncArgs <- evMouseLeftUp swGraFuncArgs
>	let cMseLUpSwGraFuncArgs = controller (swGraFuncArgs, eMseLUpSwGraFuncArgs) (br "brMseLeftClickFnArg")
>	eMseLUpSwGraFuncLclArgs <- evMouseLeftUp swGraFuncLclArgs
>	let cMseLUpSwGraFuncLclArgs = controller (swGraFuncLclArgs, eMseLUpSwGraFuncLclArgs) (br "brMseLeftClickLclFnPat")
>	eMseLUpSwGraFuncBody <- evMouseLeftUp swGraFuncBody
>	let cMseLUpSwGraFuncBody = controller (swGraFuncBody, eMseLUpSwGraFuncBody) (br "brMseLeftClickExp")

>	eMseLUpSwBtnFn <- evMouseLeftUp swGraButtonsFn
>	let cMseLUpSwBtnFn = controller (swGraButtonsFn, eMseLUpSwBtnFn) (br "brMseLeftClickBtnsFn")
>	eMseLUpSwBtnPat <- evMouseLeftUp swGraButtonsPat
>	let cMseLUpSwBtnPat = controller (swGraButtonsPat, eMseLUpSwBtnPat) (br "brMseLeftClickBtnsPat")
>	eMseLUpSwBtnExp <- evMouseLeftUp swGraButtonsExp
>	let cMseLUpSwBtnExp = controller (swGraButtonsExp, eMseLUpSwBtnExp) (br "brMseLeftClickBtnsExp")

>	eScrollSwGblFn <- evSwScroll swGraMdlFunc
>	let cScrollSwMdlFunc = controller (swGraMdlFunc, eScrollSwGblFn) (br "brScrollSwGblFn")
>	eScrollSwLclFn <- evSwScroll swGraFuncLcl
>	let cScrollSwFuncLcl = controller (swGraFuncLcl, eScrollSwLclFn) (br "brScrollSwLclFn")
>	eScrollSwFnArg <- evSwScroll swGraFuncArgs
>	let cScrollSwFuncArgs = controller (swGraFuncArgs, eScrollSwFnArg) (br "brScrollSwFnArg")
>	eScrollSwLclFnPat <- evSwScroll swGraFuncLclArgs
>	let cScrollSwLclArgs = controller (swGraFuncLclArgs, eScrollSwLclFnPat) (br "brScrollSwLclFnPat")
>	eScrollSwFnBody <- evSwScroll swGraFuncBody
>	let cScrollSwFuncBody = controller (swGraFuncBody, eScrollSwFnBody) (br "brScrollSwFnBody")

>	eTxtChgMdlNm <- evTextChanged txtModuleName
>	let cTxtChgMdlNm = controller (txtModuleName, eTxtChgMdlNm) (brSyncV "brModuleNameChanged")
>	eTxtChgFnNm <- evTextChanged txtAnnEdtFnVw
>	let cTxtChgFnNm = controller (txtAnnEdtFnVw, eTxtChgFnNm) (brSyncV "brFnNameChanged")
>	eTxtChgLclFnNm <- evTextChanged txtAnnEdtLclFnVw
>	let cTxtChgLclFnNm = controller (txtAnnEdtLclFnVw, eTxtChgLclFnNm) (brSyncV "brLclFnNameChanged")
>	eTxtChgFnPatNm <- evTextChanged txtAnnEdtFnArg
>	let cTxtChgFnPatNm = controller (txtAnnEdtFnArg, eTxtChgFnPatNm) (brSyncV "brFnPatNameChanged")
>	eTxtChgLclFnPatNm <- evTextChanged txtAnnEdtLclFnPat
>	let cTxtChgLclFnPatNm = controller (txtAnnEdtLclFnPat, eTxtChgLclFnPatNm) (brSyncV "brLclFnPatNameChanged")
>	eTxtChgExpNm <- evTextChanged txtAnnEdtFnBody
>	let cTxtChgExpNm = controller (txtAnnEdtFnBody, eTxtChgExpNm) (brSyncV "brExpNameChanged")

>	eTxtChgRepaintSw <- evTxtChgRepaintSw txtEditor lstSw 
>	let cTxtChgRepaintSw = controller (txtEditor, eTxtChgRepaintSw) (brDoNothing)
>	eTxtFnVwChgRepaintSw <- evTxtChgRepaintSw txtAnnEdtFnVw lstSw 
>	let cTxtFnVwChgRepaintSw = controller (txtAnnEdtFnVw, eTxtFnVwChgRepaintSw) (brDoNothing)
>	eTxtLclFnVwChgRepaintSw <- evTxtChgRepaintSw txtAnnEdtLclFnVw lstSw 
>	let cTxtLclFnVwChgRepaintSw = controller (txtAnnEdtLclFnVw, eTxtLclFnVwChgRepaintSw) (brDoNothing)
>	eTxtFnArgChgRepaintSw <- evTxtChgRepaintSw txtAnnEdtFnArg lstSw 
>	let cTxtFnArgChgRepaintSw = controller (txtAnnEdtFnArg, eTxtFnArgChgRepaintSw) (brDoNothing)
>	eTxtLclFnPatChgRepaintSw <- evTxtChgRepaintSw txtAnnEdtLclFnPat lstSw 
>	let cTxtLclFnPatChgRepaintSw = controller (txtAnnEdtLclFnPat, eTxtLclFnPatChgRepaintSw) (brDoNothing)
>	eTxtFnBodyChgRepaintSw <- evTxtChgRepaintSw txtAnnEdtFnBody lstSw 
>	let cTxtFnBodyChgRepaintSw = controller (txtAnnEdtFnBody, eTxtFnBodyChgRepaintSw) (brDoNothing)

>	let lstC = [cBtnSelectionCommand, cBtnLabelEditCommand, cBtnDeleteCommand,
>		    cBtnNewModuleCommand, cBtnFileOpenCommand, 
>		    cBtnFileSaveCommand, cBtnFileSaveAsCommand, cBtnUndoCommand, cBtnRedoCommand,
>		    cBtnCutCommand, cBtnCopyCommand, cBtnPasteCommand,
>		    cBtnFindCommand, cBtnReplaceCommand,
>		    cTextEditorChanged, cLstTextErrSelected,
>		    cTxtChgMdlNm, cTxtChgFnNm, cTxtChgLclFnNm, cTxtChgFnPatNm, cTxtChgLclFnPatNm, cTxtChgExpNm,
>		    cMseLUpSwMdlFunc, cMseLUpSwGraFuncLcl, cMseLUpSwGraFuncArgs, cMseLUpSwGraFuncLclArgs, 
>		    cMseLUpSwGraFuncBody, cMseLUpSwBtnFn, cMseLUpSwBtnPat, cMseLUpSwBtnExp,
>		    cScrollSwMdlFunc, cScrollSwFuncLcl, cScrollSwFuncArgs, cScrollSwLclArgs, cScrollSwFuncBody,
>		    cMseMvSwGraFnBody, cMseMvSwGraMdlFunc, cMseMvSwGraFuncLcl, cMseMvSwGraFuncArgs, cMseMvSwGraFuncLclArgs,
>		    cMseMvSwBtnFn, cMseMvSwBtnPat, cMseMvSwBtnExp
>		    --cChoiceMdlTySelected, cChoiceGblFnTySelected --, cTest1--, cTest2]
>		    ] ++ [cTxtChgRepaintSw, cTxtFnVwChgRepaintSw, cTxtLclFnVwChgRepaintSw, cTxtFnArgChgRepaintSw, 
>		    cTxtLclFnPatChgRepaintSw, cTxtFnBodyChgRepaintSw, cBtnDelRepaintSw, cBtnUndoRepaintSw, cBtnRedoRepaintSw] 
>		    ++ cLstSwMouseRepaintSw
>	-- Controllers-----------------------------------------------------------------------

>	-- Views-----------------------------------------------------------------------------
>	let vFrameTitle = \m -> view (win, "text") m ilSetWindowTitle
>	let vBtnUndoEnabling = \m -> view (btnUndo, "enabled") m ilIsEndOfUndo
>	let vBtnRedoEnabling = \m -> view (btnRedo, "enabled") m ilIsEndOfRedo

>	let vTextEditor = \m -> view (txtEditor, "text") m ilSetTextEditorText
>	let vTextEditorInsPt = \m -> view (txtEditor, "insertionPoint") m ilSetTextEditorInsertionPt
>	let vLstTextualError = \m -> view (lstTextualError, "items") m ilShowTextualErrors
>	let vLstTextualErrorSelection = \m -> view (lstTextualError, "selection") m ilSetSelectedTextualError
>	let vLblLineColStatus = \m -> view (lblLnColStatus, "text") m ilShowLineColStatus

>	let vTextModuleName = \m -> view (txtModuleName, "text") m ilSetModuleName
>	let vTextModuleNameInsPt = \m -> view (txtModuleName, "insertionPoint") m ilSetModuleNameInsPt
>	let vSWGraMdlFunc = \m -> view (swGraMdlFunc, "on paint") m ilDrawFnView
>	let vSWGraMdlFuncCursor = \m -> view (swGraMdlFunc, "mouseCursor") m ilChangeMouseCursorFn
>	let vSWGraFuncLcl = \m -> view (swGraFuncLcl, "on paint") m ilDrawLclFnView
>	let vSWGraFuncLclCursor = \m -> view (swGraFuncLcl, "mouseCursor") m ilChangeMouseCursorFn
>	let vSWGraFuncArgs = \m -> view (swGraFuncArgs, "on paint") m ilDrawFnArgView
>	let vSWGraFuncArgsCursor = \m -> view (swGraFuncArgs, "mouseCursor") m ilChangeMouseCursorPat
>	let vSWGraFuncLclArgs = \m -> view (swGraFuncLclArgs, "on paint") m ilDrawLclFnPatView
>	let vSWGraFuncLclArgsCursor = \m -> view (swGraFuncLclArgs, "mouseCursor") m ilChangeMouseCursorPat
>	let vSWGraFuncBody = \m -> view (swGraFuncBody, "on paint") m ilDrawFnBodyView
>	let vSWGraFuncBodyCursor = \m -> view (swGraFuncBody, "mouseCursor") m ilChangeMouseCursorExp
>	let vswGraButtonsFn = \m -> view (swGraButtonsFn, "on paint") m ilDrawFnBttonsView
>	let vswGraButtonsPat = \m -> view (swGraButtonsPat, "on paint") m ilDrawPatButtonsView
>	let vswGraButtonsExp = \m -> view (swGraButtonsExp, "on paint") m ilDrawExpButtonsView
>	let vLstGraError = \m -> view (lstGraError, "items") m ilShowGraErrors
>	--let vChoiceImport = \m -> view (choiceImport, "selection") m ilSetSelectedImportMdlType
>	--let vChoiceImportOf = \m -> view (choiceImportOf, "selection") m ilSetSelectedImportGblFnType

>	let vAnnEdtVisiFnVw = \m -> view (txtAnnEdtFnVw, "visible") m ilAnnEdtVisiGblFn
>	let vAnnEdtTxtFnVw = \m -> view (txtAnnEdtFnVw, "text") m ilAnnEdtTxtGblFn
>	let vAnnEdtPosFnVw = \m -> view (txtAnnEdtFnVw, "position") m ilAnnEdtPosGblFn
>	let vAnnEdtInsPtFnVw = \m -> view (txtAnnEdtFnVw, "insertionPoint") m ilAnnEdtInsPtGblFn

>	let vAnnEdtVisiLclFn = \m -> 
>		view (txtAnnEdtLclFnVw, "visible") m ilAnnEdtVisiLclFn
>	let vAnnEdtTxtLclFn = \m -> 
>		view (txtAnnEdtLclFnVw, "text") m ilAnnEdtTxtLclFn
>	let vAnnEdtPosLclFn = \m -> 
>		view (txtAnnEdtLclFnVw, "position") m ilAnnEdtPosLclFn
>	let vAnnEdtInsPtLclFn = \m -> 
>		view (txtAnnEdtLclFnVw, "insertionPoint") m ilAnnEdtInsPtLclFn

>	let vAnnEdtVisiFnArg = \m -> view (txtAnnEdtFnArg, "visible") m ilAnnEdtVisiFnArg
>	let vAnnEdtTxtFnArg = \m -> view (txtAnnEdtFnArg, "text") m ilAnnEdtTxtFnArg
>	let vAnnEdtPosFnArg = \m -> view (txtAnnEdtFnArg, "position") m ilAnnEdtPosFnArg
>	let vAnnEdtInsPtFnArg = \m -> view (txtAnnEdtFnArg, "insertionPoint") m ilAnnEdtInsPtFnArg

>	let vAnnEdtVisiLclFnPat = \m -> view (txtAnnEdtLclFnPat, "visible") m ilAnnEdtVisiLclFnPat
>	let vAnnEdtTxtLclFnPat = \m -> view (txtAnnEdtLclFnPat, "text") m ilAnnEdtTxtLclFnPat
>	let vAnnEdtPosLclFnPat = \m -> view (txtAnnEdtLclFnPat, "position") m ilAnnEdtPosLclFnPat
>	let vAnnEdtInsPtLclFnPat = \m -> view (txtAnnEdtLclFnPat, "insertionPoint") m ilAnnEdtInsPtLclFnPat

>	let vAnnEdtVisiFnBody = \m -> view (txtAnnEdtFnBody, "visible") m ilAnnEdtVisiFnBody
>	let vAnnEdtTxtFnBody = \m -> view (txtAnnEdtFnBody, "text") m ilAnnEdtTxtFnBody
>	let vAnnEdtPosFnBody = \m -> view (txtAnnEdtFnBody, "position") m ilAnnEdtPosFnBody
>	let vAnnEdtInsPtFnBody = \m -> view (txtAnnEdtFnBody, "insertionPoint") m ilAnnEdtInsPtFnBody

>	let vLblLineRowStatus = \m -> view (lblLineRowStatus, "text") m ilShowLineRowStatus

>	let lstV = [vFrameTitle, vBtnUndoEnabling, vBtnRedoEnabling,
>		    vTextEditor, vTextEditorInsPt, vLstTextualError, vLstTextualErrorSelection, vLblLineColStatus,
>		    vTextModuleName, vTextModuleNameInsPt, vSWGraMdlFunc, vSWGraFuncLcl, vSWGraFuncArgs, vSWGraFuncLclArgs,
>		    vSWGraFuncBody, vswGraButtonsFn, vswGraButtonsPat, vswGraButtonsExp,
>		    vSWGraMdlFuncCursor, vSWGraFuncLclCursor, vSWGraFuncArgsCursor, vSWGraFuncLclArgsCursor, vSWGraFuncBodyCursor,
>		    vAnnEdtVisiFnVw, vAnnEdtTxtFnVw, vAnnEdtPosFnVw, vAnnEdtInsPtFnVw,
>		    vAnnEdtVisiLclFn, vAnnEdtTxtLclFn, vAnnEdtPosLclFn, vAnnEdtInsPtLclFn,
>		    vAnnEdtVisiFnArg, vAnnEdtTxtFnArg, vAnnEdtPosFnArg, vAnnEdtInsPtFnArg,
>		    vAnnEdtVisiLclFnPat, vAnnEdtTxtLclFnPat, vAnnEdtPosLclFnPat, vAnnEdtInsPtLclFnPat,
>		    vAnnEdtVisiFnBody, vAnnEdtTxtFnBody, vAnnEdtPosFnBody, vAnnEdtInsPtFnBody,
>		    vLstGraError, vLblLineRowStatus]--, vChoiceImport, vChoiceImportOf]
>	-- Views-----------------------------------------------------------------------------

>	liftIONow $ set win [visible := True]
>	return (lstV, lstC)

> getFilePathIO :: String -> String -> IO FilePath
> getFilePathIO path img = return (path ++ img)

> getFilePath :: String -> String -> String
> getFilePath path img = (path ++ img)


