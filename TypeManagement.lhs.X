> module TypeManagement where

> import ModelType

> import Language.Haskell.Exts-- .Annotated hiding (layout, style)
> import qualified Language.Haskell.Exts.Annotated as HSE
> import Language.Haskell.Exts.SrcLoc

> import Language.Haskell.TypeCheck.TypeCheck
> import Language.Haskell.TypeCheck.Monad
> import Language.Haskell.TypeCheck.InternalTypes

> import System.IO.Unsafe
> import Data.Map

> manageType :: ModelType -> ModelType
> manageType m@(ModelType f t g ty h) =  
>	ModelType f (setTypeErrTxt m) (setTypeErrGr m) ty h


> -----------------------------------------------------

> setTypeErrTxt :: ModelType -> TextualForm
> setTypeErrTxt m@(ModelType f t g ty h) =
>	let (errT, errG) = getTypeErr m
>	in t {
>		textualErrors = errT,
>		selectedTxtErr = -1
>	}

> setTypeErrGr :: ModelType -> GraphicalForm
> setTypeErrGr m@(ModelType f t g ty h) =
>	let (errT, errG) = getTypeErr m
>	in g {
>		graErrors = errG,
>		selectedGraErr = -1
>	}

> getTypeErr :: ModelType -> ([a],[a])
> getTypeErr m@(ModelType f t g@GraphicalForm{shapeTree = shp} ty h) = 
>	unsafePerformIO $ 
>	do
>	(pEnv,pErr) <- getPreludeEnv
>	(lEnv,lErr) <- getLocalEnv m pEnv

>	let tc@(Tc s) = typecheckModule (fst$unAnn shp)

>	{-mTc2test <- mkTcEnv tc2 [] []
>	pr <- s mTc2test-}
>	{-case pr of
>		Left  e -> return (ErrorMsg 0 0 (show e))
>		Right e -> return (ErrorMsg 0 0 (show (e++tc2)))-}

>	return ([], [])

> getPreludeEnv :: IO ([(QName, Sigma)], [ErrorMsg])
> getPreludeEnv =
>	do
>	(tc1,err1) <- addToTcVarEnvFromFile [] "./modulesToLoadTypes/Prelude.hs"
>	(tc2,err2) <- addToTcVarEnvFromFile tc1 "./modulesToLoadTypes/PreludeList.hs"
>	return (tc2, err1 ++ err2)

> getLocalEnv :: ModelType -> [(QName, Sigma)] -> IO ([(QName, Sigma)], [ErrorMsg])
> getLocalEnv m pEnv =
>	do
>	--tc1 <- addToTcVarEnvFromFile [] "LocalFileNames.hs"
>	return ([],[])--tc1

> addToTcVarEnvFromFile :: [(QName, Sigma)] -> String -> IO ([(QName, Sigma)], [ErrorMsg])
> addToTcVarEnvFromFile pEnv fileName =
>	do
>	pTree <- parseFileWithComments defaultParseMode fileName
>	case pTree of
>		ParseOk (mod, _)-> getEnv mod
>		err		-> return (pEnv, [ErrorMsg 0 0 "Parse Error"])
>	where
>	getEnv mod =
>		do 
>		let Tc s = typecheckModule mod
>		tcEnv <- mkTcEnv pEnv [] []
>		vEnv <- s tcEnv
>		case vEnv of
>			Left  e -> return $ (pEnv, [ErrorMsg 0 0 "Type Error"])
>			Right e -> return $ ((e ++ pEnv),[])

> unAnn :: HSE.Module a -> (Module, a)
> unAnn (HSE.Module ann mh mp imp decl) = 
>	(Module noLoc (ModuleName "") [] Nothing Nothing [] [], ann)

> -----------------------------------------------------
> setTypeErrors t =
>	let (errT, errG) = getErrors t
>	in t {
>		textualErrors = errT,
>		selectedTxtErr = -1
>	     }

> --getErrors :: ModelType -> IO ([ErrorMsg], [ErrorMsg])
> getErrors t = 
>	unsafePerformIO $ 
>	do
>	let str = textStr t
>	let pTree = parseFileContentsWithComments defaultParseMode str
>	case pTree of
>		ParseOk (mod, _)-> getTypeErrors mod

> getSyntaxError :: (ParseResult p) -> IO [ErrorMsg]
> getSyntaxError (ParseFailed (SrcLoc f l c) msg) =
>	return [(ErrorMsg l c msg)]

> {-getTypeErrorsTextual mod =
>	getTypeErrors mod

> getTypeErrorsGra mod =
>	getTypeErrors mod-}

> --getTypeErrors :: Module (TextSrcInfo, GraInfo) -> [ErrorMsg]
> getTypeErrors mod =
>	do
>	test <- getTypeErrors2 mod
>	return ([test], [])
>	{-do
>	result <- parseResult
>	return (ErrorMsg 0 0 (show result))
>	where	
>	Tc s = typecheckModule mod
>	parseResult = 
>		do
>		m <- mkEmptyTcEnv
>		pr <- s m
>		case pr of
>			Left  e -> do  return e-}

> --getTypeErrors1 :: Module (TextSrcInfo, GraInfo) -> [ErrorMsg]
> getTypeErrors1 mod =
>	do
>	result <- parseResult
>	return (ErrorMsg 0 0 (show result))
>	where	
>	Tc s = typecheckModule mod
>	parseResult = 
>		do
>		eEnv <- mkEmptyTcEnv
>		env1 <- addToTcEnvFromFile2 eEnv "./modulesToLoadTypes/Prelude.hs"
>		env2 <- addToTcEnvFromFile2 env1 "./modulesToLoadTypes/PreludeList.hs"
>		pr <- s env2
>		case pr of
>			Left  e -> return (show e ++ "Left")
>			Right e -> return (show e)

> getTypeErrors2 mod =
>	do
>	emptEnv <- mkEmptyTcEnv
>	tc1 <- addToTcEnvFromFile1 [] "./modulesToLoadTypes/Prelude.hs"
>	tc2 <- addToTcEnvFromFile1 tc1 "./modulesToLoadTypes/PreludeList.hs"

>	let tc@(Tc s) = typecheckModule mod

>	mTc2test <- mkTcEnv tc2 [] []
>	pr <- s mTc2test
>	case pr of
>		Left  e -> return (ErrorMsg 0 0 (show e))
>		Right e -> return (ErrorMsg 0 0 (show (e++tc2)))

> addToTcEnvFromFile1 eEnv fileName = 
>	do
>	pTree <- parseFileWithComments defaultParseMode fileName
>	case pTree of
>		ParseOk (mod, _)-> getEnv1 mod
>		err		-> return eEnv
>	where
>	getEnv1 mod@(Module _ _ _ _ _ _ ds) =
>		do 
>		let Tc s = typecheckModule mod
>		test <- mkTcEnv eEnv [] []
>		vEnv <- s test
>		case vEnv of
>			Left  e -> return $ eEnv
>			Right e -> return $ (e ++ eEnv)
> {-getTypeErrors4 mod =
>	do
>	result <- parseResult
>	return (ErrorMsg 0 0 (show result))
>	where	
>	s = typecheckModule mod
>	parseResult = 
>		return (show getEnv)


> getTypeErrors3 mod =
>	do
>	result <- parseResult
>	return (ErrorMsg 0 0 (show result))
>	where	
>	Tc s = getTypeErrors2 mod
>	parseResult = 
>		do
>		m <- mkEmptyTcEnv
>		pr <- s m
>		case pr of
>			Left  e -> return (show e)
>			Right e -> return (show e)

> getTypeErrors2 mod@(Module _ _ _ _ _ _ ds) =
>		do
>		(ve, qRem, _) <- tcBindGroup ds
>		case qRem of
>			[] -> return (show ve)
>			q  -> return (show ve)-}

> addToTcEnvFromFile2 env fileName = 
>	do
>	pTree <- parseFileWithComments defaultParseMode fileName
>	case pTree of
>		ParseOk (mod, _)-> getEnv mod
>		err		-> return env
>	where
>	getEnv mod@(Module _ _ _ _ _ _ ds) =
>		do 
>		let Tc s = typecheckModule mod
>		vEnv <- s env
>		case vEnv of
>			Left  e -> return env
>			Right e -> mkTcEnv e [] []



