> module ModelType where
>
> import System.IO.Unsafe
>
> import Graphics.UI.WX

> import Language.Haskell.Exts.Annotated as HSE
> import qualified Language.Haskell.Exts.Syntax as HSES -- next one confusing, must be removed
> import qualified Language.Haskell.Exts as HS
> import Language.Haskell.TypeCheck.InternalTypes

> import Language.Haskell.TypeCheckAsst.TypeCheck

> windowTitle :: String
> windowTitle = "HASKEU (Haskell for End-Users)" -- "Functional Programming"

> data SystemState = 
>	SystemState
>	{
>	fileInfo::FileInfo,
>	textualState::TextualState,
>	visualState::VisualState,
>	typeInfo::TypeInfo,
>	history::ModelHistories
>	}

> --(FileInfo, TextualState, VisualState, History)

> data ModelChangedBy = TextualChange | VisualChange

> data FileInfo = 
>	FileInfo 
>	{
>	path::Maybe FilePath, 
>	saveStatus::Bool--It has not been used
>	}

> data TextualState = 
>	TextualState 
>	{
>	textStr::String, 
>	insertionPt::Int,
>	textualErrors::[ErrorMsg],
>	selectedTxtErr::Int
>	}

> data AreaOfOperation = GblFn | LclFn | FnArg | LclFnPat | FnBody deriving (Eq)

> data VisualState = 
>	VisualState 
>	{
>	modeOfOperation::ModeOfOperation,
>	areaOfOperation::AreaOfOperation,
>	fnClButtons::[VirtualButton],
>	patButtons::[VirtualButton],
>	expButtons::[VirtualButton],
>	scrlPosAnnEdtGbl::Point,
>	scrlPosAnnEdtLcl::Point,
>	scrlPosAnnEdtGblPat::Point,
>	scrlPosAnnEdtLclPat::Point,
>	scrlPosAnnEdtBody::Point,
>	shapeTree::HSE.Module (SrcSpanInfo, ItemState),
>	visualErrors::[ErrorMsg],
>	selectedVisualErr::Int,
>	mouseCoordinate::(Int,Int)
>	}

> data TyDefined = Defined | UnDefined | Parameter | Recursive | SpecialTy | NotFound deriving (Show, Eq)

> data ItemState = ItemState 
>	{ 
>	itemPosition :: Point,
>	itemSize :: Size,
>	mseInside :: Bool,
>	selected :: Bool,
>	annEditMode :: Bool,
>	annInsertionPoint::Int,
>	argSelected :: Int,
>	outputSelected :: Bool,
>	nodeType :: (TyDefined, Maybe Sigma),
>	nodeTypeArgs :: [ItemState],
>       typeErr :: String,
>	synonym::String,
>	toolTipText::String,
>	joinToChildNodes :: JoinToChildNodes,
>	groupBox :: (Point, Size)
>	}
>	deriving (Show)

> type JoinToAChildNode = [Point]
> type JoinToChildNodes = [JoinToAChildNode]

> data TypeInfo = 
>	TypeInfo 
>	{
>	standardHaskellTypes ::[(ModuleSrc, [(HSES.QName, Sigma)], [(HSES.QName, Sigma)])],
>	otherLocalModuleTypes::[(ModuleSrc, [(HSES.QName, Sigma)], [(HSES.QName, Sigma)])],
>	thisModuleTypes::([(HSES.QName, Sigma)], [(HSES.QName, Sigma)]),
>	funcLocals::([(HSES.QName, Sigma)], [(HSES.QName, Sigma)], [(HSES.QName, Sigma)])
>	}
>	deriving (Show)


> newtype ModuleSrc = ModuleSrc String deriving (Show)

> data ModelHistories = 
>	ModelHistories 
>	{
>	modelHistories::[ModelHistory],
>	currentValPos::Int
>	}

> data ModelHistory = 
>	ModelHistory 
>	{
>	fileHistory::FileInfo,
>	textualHistory::TextualState,
>	graphicalHistory::VisualState,
>	globalTypeHistory::TypeInfo,
>	changedBy::ModelChangedBy
>	}

> srcSpanInfoInitVal::SrcSpanInfo
> srcSpanInfoInitVal=SrcSpanInfo srcSpanInitVal []

> srcSpanInitVal::SrcSpan
> srcSpanInitVal=SrcSpan "" 0 0 0 0 

> graInitVal :: ItemState
> graInitVal = ItemState 
>	{
>	itemPosition 		= Point 0 0, 
>	itemSize 		= sz 0 0, 
>	mseInside 		= False,
>	selected 		= False, 
>	annEditMode		= False,
>	annInsertionPoint 	= 0,
>	argSelected 		= 0,
>	outputSelected 		= False,
>	nodeType 		= (Defined, Nothing),
>	nodeTypeArgs		= [],
>       typeErr 		= "",
>	synonym			= "",
>	toolTipText		= "",
>	joinToChildNodes	= [],
>	groupBox		= (Point 0 0, Size 0 0)
>	}

> nodeTypeInitVal :: Sigma
> nodeTypeInitVal = TcTyVar (BoundTv "")

> data ModeOfOperation = ModeOfOperationNone | ModeSelection | ModeLabelEdit | 
>		  ModeNewFn | ModeNewCl | 
>		  ModePatVar | ModePatWild | ModePatEmptyLst | ModePatListCons | ModePatStr | ModePatInt | ModePatChar | ModePatBool |
>		  ModeExpApp | ModeExpOp | ModeEmptyList | ModeListCons | ModeCnstStr | ModeCnstInt | ModeCnstChar | ModeCnstBool |
>		  ModeIfStmt 
>		  deriving (Eq, Show)

> data VirtualButton = 
>	VirtualButton 
>	{
>	btnText::String,
>	toolTip::String,
>	btnPos::Point,
>	btnSz::Size,
>	btnSel::Bool,
>	mseInsideBtn::Bool,
>	mode::ModeOfOperation
>	}
>	deriving (Show)

> data ErrorMsg = 
>	ErrorMsg 
>	{
>	lineNo::Int,
>	colNo::Int,
>	errorMsg::String
>	}
>	deriving (Show)

> modelInitVal :: SystemState
> modelInitVal = 
>	SystemState
>		fileInitVal
>		textualInitVal
>		graphicalInitVal
>		(setStndHsTypes globalTypeInitVal)
>		historiesInitVal

> fileInitVal :: FileInfo
> fileInitVal = 
>	FileInfo 
>		{
>		path=Nothing,
>		saveStatus=False
>		} 

> textualInitVal :: TextualState
> textualInitVal =
>	TextualState 
>		{
>		textStr="",
>		insertionPt=1,
>		textualErrors=[],
>		selectedTxtErr=(-1)
>		}

> graphicalInitVal :: VisualState
> graphicalInitVal =
>	VisualState 
>		{
>		modeOfOperation=ModeSelection,
>		areaOfOperation=GblFn,
>		fnClButtons=fnViewBtns,
>		patButtons=fnArgBtns,
>		expButtons=fnBodyViewBtns,
>		scrlPosAnnEdtGbl=pt 0 0,
>		scrlPosAnnEdtLcl=pt 0 0,
>		scrlPosAnnEdtGblPat=pt 0 0,
>		scrlPosAnnEdtLclPat=pt 0 0,
>		scrlPosAnnEdtBody=pt 0 0,
>		shapeTree=HSE.Module (srcSpanInfoInitVal, graInitVal) Nothing [] [] [],
>		visualErrors=[],
>		selectedVisualErr=(-1),
>		mouseCoordinate=(0,0)--"X=0, Y= 0"
>		}

> globalTypeInitVal :: TypeInfo
> globalTypeInitVal = 
>	TypeInfo 
>		{
>		standardHaskellTypes=[],
>		otherLocalModuleTypes=[],
>		thisModuleTypes=([], []),
>		funcLocals=([], [], [])
>		} 

> historiesInitVal :: ModelHistories
> historiesInitVal =
>	ModelHistories
>		[historyInitVal]
>		0

> historyInitVal :: ModelHistory
> historyInitVal =
>	ModelHistory
>		fileInitVal
>		textualInitVal
>		graphicalInitVal
>		globalTypeInitVal
>		TextualChange

> fnViewBtns :: [VirtualButton]
> fnViewBtns = 
>	[VirtualButton "+f" "New function" (pt 10 30) (sz 16 16) False False ModeNewFn,
>	 VirtualButton "+c" "New clause" (pt 40 30) (sz 16 16) False False ModeNewCl]

> fnArgBtns :: [VirtualButton]
> fnArgBtns = 
>	[VirtualButton " v" "Variable" (pt 10 30) (sz 16 16) False False ModePatVar,
>	 VirtualButton "--" "Wild-Card" (pt 40 30) (sz 16 16) False False ModePatWild,
>	 VirtualButton " []" "Empty List" (pt 10 60) (sz 16 16) False False ModePatEmptyLst,
>	 VirtualButton "xs" "List (x:xs)" (pt 40 60) (sz 16 16) False False ModePatListCons,
>	 VirtualButton "ab" "String" (pt 10 90) (sz 16 16) False False ModePatStr,
>	 VirtualButton "12" "Int" (pt 40 90) (sz 16 16) False False ModePatInt,
>	 VirtualButton "'c'" "Char" (pt 10 120) (sz 16 16) False False ModePatChar,
>	 VirtualButton "TF" "Bool" (pt 40 120) (sz 16 16) False False ModePatBool]

> fnBodyViewBtns :: [VirtualButton]
> fnBodyViewBtns = 
>	[VirtualButton "(f)" "Function app" (pt 10 30) (sz 16 16) False False ModeExpApp,
>	 VirtualButton "+/" "Operator" (pt 40 30) (sz 16 16) False False ModeExpOp,
>	 VirtualButton " :" "List Cons" (pt 10 60) (sz 16 16) False False ModeListCons,
>	 VirtualButton " []" "Empty List" (pt 40 60) (sz 16 16) False False ModeEmptyList,
>	 VirtualButton "ab" "String" (pt 10 90) (sz 16 16) False False ModeCnstStr,
>	 VirtualButton "12" "Int" (pt 40 90) (sz 16 16) False False ModeCnstInt,
>	 VirtualButton "'c'" "Char" (pt 10 120) (sz 16 16) False False ModeCnstChar,
>	 VirtualButton "TF" "Bool" (pt 40 120) (sz 16 16) False False ModeCnstBool,
>	 VirtualButton "if" "IF-THEN" (pt 10 150) (sz 16 16) False False ModeIfStmt]

> ---Some sort of type management here

> setStndHsTypes :: TypeInfo -> TypeInfo
> setStndHsTypes gti = 
>	unsafePerformIO $ 
>	do
>	gti1 <- setStndHsFromFile gti  "./modulesToLoadTypes/Prelude.hs"
>	gti2 <- setStndHsFromFile gti1 "./modulesToLoadTypes/DataChar.hs"

>	return gti2

> setStndHsFromFile :: TypeInfo -> String -> IO TypeInfo
> setStndHsFromFile gti fileName = 
>	do
>	pTree   <- HS.parseFileWithComments defaultParseMode fileName
>	let setT ty = return $ gti {standardHaskellTypes = ((standardHaskellTypes gti) ++ [ty])}
>	case pTree of
>		ParseOk (mod, _)-> 
>			do
>			varEnv <- getAllVarEnv (getStndHsTypes gti) ([], []) mod
>			setT (ModuleSrc fileName, fst varEnv, snd varEnv)
>		err		-> setT (ModuleSrc fileName, [], [])

> getStndHsTypes :: TypeInfo -> ([(HS.QName, Sigma)], [(HS.QName, Sigma)])
> getStndHsTypes gti = 
>	let a = concat $ map getTypesD (standardHaskellTypes gti) 
>	    b = concat $ map getTypesUnD (standardHaskellTypes gti) 
>	in (a, b)
>	where
>	getTypesD (_, lstD, _) = lstD
>	getTypesUnD (_, _, lstUnD) = lstUnD

> getLocalModuleTypes :: TypeInfo -> ([(HS.QName, Sigma)], [(HS.QName, Sigma)])
> getLocalModuleTypes gti = 
>	let a = concat $ map getTypesD (otherLocalModuleTypes gti) 
>	    b = concat $ map getTypesUnD (otherLocalModuleTypes gti) 
>	in (a, b)
>	where
>	getTypesD (_, lstD, _) = lstD
>	getTypesUnD (_, _, lstUnD) = lstUnD

> mergeAllGlobalTypes :: TypeInfo -> ([(HS.QName, Sigma)], [(HS.QName, Sigma)])
> mergeAllGlobalTypes gti = 
>	let (hsD, hsUn) = getStndHsTypes gti
>	    (othD, othUn) = getLocalModuleTypes gti
>	    (thD, thUn) = thisModuleTypes gti
>	    (fnLP, fnLD, fnLUn) = funcLocals gti
>	in ((hsD ++ othD ++ thD ++ fnLD), (hsUn ++ othUn ++ thUn ++ fnLUn))

> instance SrcInfo (SrcSpanInfo, ItemState) where
>	toSrcInfo _ _ _ = sviInit
>  	fromSrcInfo _ 	= sviInit
>  	fileName _ = ""
>  	startLine _ = 0
>  	startColumn _ = 0

> viInit :: ItemState
> viInit = graInitVal
> sSpanInit :: SrcSpanInfo
> sSpanInit = SrcSpanInfo (SrcSpan "" 0 0 0 0) []
> sviInit :: (SrcSpanInfo, ItemState)
> sviInit = (sSpanInit, viInit)

> --loading type info for this module and other locals
> loadTypeInfo :: SystemState -> SystemState
> loadTypeInfo m = 
>	loadThisModuleTypeInfo $ loadLocalModulesTypeInfo m

> loadLocalModulesTypeInfo :: SystemState -> SystemState
> loadLocalModulesTypeInfo m = m

> loadThisModuleTypeInfo :: SystemState -> SystemState
> loadThisModuleTypeInfo m@(SystemState f t g ty h) =
>	unsafePerformIO $ 
>	do
>	let str = textStr t
>	let pTree   = HS.parseFileContentsWithComments defaultParseMode str

>	let (stdHsD, stdHsUnD)   = getStndHsTypes ty
>	let (lclMdlD, lclMdlUnD) = getLocalModuleTypes ty

>	let setT mdlTy = return $ (SystemState f t g  
>					ty {thisModuleTypes = mdlTy}
>					h)
>	case pTree of
>		ParseOk (mod, _)	-> 
>			do
>			varEnv <- getAllVarEnv ((stdHsD ++ lclMdlD), (stdHsUnD ++ lclMdlUnD)) ([], []) mod
>			--putStrLn (show varEnv)
>			setT varEnv
>		_			-> return $ m

> getInsPtFromRowCol :: String -> Int -> Int -> Int
> getInsPtFromRowCol s l c = 
>	let lenLines = map length (lines s)
>	    linesErr = take (l-1) lenLines
>	in sum linesErr + (l-1) + (c-1)

> getRowColFromInsPt :: String -> Int -> (Int, Int)
> getRowColFromInsPt s ins = 
>	let 	lenLines = map length (lines s)
>	in 	getLnCol (-1) 0 0 ins lenLines

> getLnCol accum ln lastX ins [] = 
>	if (accum == ins) 
>	then (ln, lastX)
>	else (ln + 1, 0)
> getLnCol accum ln lastX ins (x:xs) =
>	let 	accum1 	= x + accum + 1
>		ln1	= ln + 1
>	in 	if (accum1 > ins) 
>		then (ln1, x - (accum1 - ins))
>		else getLnCol accum1 ln1 x ins xs

> showLog msg fn =
>	unsafePerformIO $ 
>	do
>	putStrLn (msg)
>	return $ fn






