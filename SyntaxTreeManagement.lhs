> module SyntaxTreeManagement where

> import ModelType
> import GraphicUtils
> import TreeErrorManagement

> import System.IO.Unsafe

> import Language.Haskell.Exts.Annotated
> import qualified Language.Haskell.Exts.Syntax as HSES 
> import qualified Language.Haskell.Exts as HSE
> import Language.Haskell.Exts.Pretty

> import Language.Haskell.TypeCheck.InternalTypes
> import Language.Haskell.TypeCheck.TypeCheck
> import qualified Language.Haskell.TypeCheckAsst.TypeCheck as TyAsst
> import Language.Haskell.TypeCheck.Monad

> import Graphics.UI.WX
> import Data.List 
> import Data.Maybe
> import Data.Char
> import Data.String.Utils

> class (TreeManager t a b) => TypeManager t1 t a b where
>	setType 		:: TypeInfo -> t1 (a, b) -> t (a,b) -> t (a,b)
>	setType _ _ t	= t
>	mapSetType	:: TypeInfo -> t1 (a, b) -> [t (a,b)] -> [t (a,b)]
>	mapSetType ty t1 lstNode	= 
>		map (setType ty t1) lstNode

> instance TypeManager Module Module SrcSpanInfo ItemState where
>	setType ty t (Module (s, lp) mh mp imp decl) = 
>		findMultipleDeclErrInModule $ Module (s, lp) mh mp imp decl1
>		where
>		decl1 	= mapSetType ty t decl 

> instance TypeManager Module Decl SrcSpanInfo ItemState where
>	setType ty t fn@(FunBind (s, pos) lstMatch) 	=
>		let lstMatch1 = mapSetType ty t lstMatch
>		    lstMatch2 = checkMtchPattern ty lstMatch1
>		in setFnType ty $ FunBind (s, pos) lstMatch2
>	setType _ _ otherDecl			= 
>		otherDecl

> instance TypeManager Module Match SrcSpanInfo ItemState where
>	setType ty t mtch@(Match (s,cp) n lstP rhs binds) 	=
>		Match (s,cp) n lstP rhs1 binds1
>		where
>		ty1     = setFnParamTypes ty mtch
>		ty2	= setLocalFnTypes ty1 binds
>		rhs1 	= setType ty2 mtch rhs 
>		binds1 	= setTypeBinds ty2 t binds 
>	setType ty _ otherMatch		=
>		otherMatch

> setTypeBinds ty t b@(Just (BDecls svi l)) = Just (BDecls svi $ mapSetType ty t l)
> setTypeBinds ty _ b@(_) = b

> instance TypeManager Match Rhs SrcSpanInfo ItemState where
>	setType ty t (UnGuardedRhs (s, lp) exp) = 
>		UnGuardedRhs (s, lp) exp2
>		where
>		exp1 = setType ty t exp
>		exp2 = makeChildNodesJoins [] exp1
>	setType ty t otherRhs = 
>		otherRhs

> checkExp :: TypeInfo -> Match (SrcSpanInfo, ItemState) -> Exp (SrcSpanInfo, ItemState) -> String
> checkExp ty mtch@(Match (s1,vi1) n lstP rhs binds) exp = 
>	unsafePerformIO $ 
>		do
>		let mtch1 = Match (s1,vi1) (Ident (ann n) "temporaryMtch") lstP (UnGuardedRhs (ann rhs) exp) binds
>		let str = prettyPrint mtch1
>		--putStrLn (show ty)
>		let strR = case (HSE.parseFileContentsWithComments defaultParseMode str) of
>			ParseOk (mod, _) 	-> 
>				do
>				let Tc tc = typecheckModule mod
>				let (d, un) = mergeAllGlobalTypes ty
>				env <- mkTcEnv (d ++ un) [] []
>				tcRes <- tc env
>				case tcRes of
>					Left  e -> return $ (show e)
>					Right e -> return $ ("")
>			err			->
>				return "" 
>		strR

> checkMtchPattern :: TypeInfo -> [Match (SrcSpanInfo, ItemState)] -> [Match (SrcSpanInfo, ItemState)]
> checkMtchPattern _ [] = []
> checkMtchPattern ty (x:xs) = [x] ++ (checkMtchPatternTail ty x xs)

> checkMtchPatternTail _  _ [] = []
> checkMtchPatternTail ty x (x1:xs) = [checkPattern ty x x1] ++ (checkMtchPatternTail ty x xs)

> checkPattern :: TypeInfo -> Match (SrcSpanInfo, ItemState) -> Match (SrcSpanInfo, ItemState) -> Match (SrcSpanInfo, ItemState)
> checkPattern ty mtch1@(Match _ _ lstP1 rhs1 binds1) mtch2@(Match _ _ lstP2 rhs2 binds2) = 
>	unsafePerformIO $ 
>		do
>		let mtch11 = Match sviInit (Ident sviInit "temporaryMtch") lstP1 rhs1 binds1
>		let mtch21 = Match sviInit (Ident sviInit "temporaryMtch") lstP2 rhs2 binds2
>		let str = (prettyPrint mtch11) ++ "\n" ++ (prettyPrint mtch21)
>		let strR = case (HSE.parseFileContentsWithComments defaultParseMode str) of
>			ParseOk (mod, _) 			-> 
>				do
>				let Tc tc = typecheckModule mod
>				let (d, un) = mergeAllGlobalTypes ty
>				env <- mkTcEnv (d ++ un) [] []
>				tcRes <- tc env
>				case tcRes of
>					Left  e -> 
>						do
>						putStrLn (show e)
>						return (setPatternError mtch2)
>						{-if (startswith "Pattern subsumption check failed:" (show e)) 
>						then return (setPatternError mtch2)
>						else return (mtch2)-}
>					Right e -> return (mtch2)
>			(ParseFailed (SrcLoc f l c) msg)	->
>				if (startswith "arity mismatch" msg)
>				then return (setPatternError mtch2)
>				else return (mtch2)
>		strR 

> setPatternError mtch@(Match (s, vi) n lstP rhs binds) =
>	let 	vi1 	= vi {toolTipText = "Pattern mismatch betwn clauses!!",
>		      	      typeErr = "Pattern mismatch!!"}
>	in	Match (s, vi1) n lstP rhs binds



>			
>		{-let ParseOk (mod, _)   = HSE.parseFileContentsWithComments defaultParseMode str
>		let Tc tc = typecheckModule mod
>		let (d, un) = mergeAllGlobalTypes ty
>		env <- mkTcEnv (d ++ un) [] []
>		tcRes <- tc env
>		case tcRes of
>			Left  e -> return $ (show e)
>			Right e -> return $ ("")-}


> lookUpType :: TypeInfo -> Match (SrcSpanInfo, ItemState) -> HSES.QName -> (TyDefined, Maybe Sigma)
> lookUpType ty mtch@(Match (s1,vi1) n lstP rhs binds) qName = 
>	{-unsafePerformIO $ 
>	do
>	let (paramTest, _, _) = funcLocals ty
>	putStrLn ("--paramTest1--" ++ (show paramTest))
>	return $ -}result
>	where
>	(d, un)	= mergeAllGlobalTypes ty
>	dSearch = lookup qName d
>	result = case dSearch of
>			Just typ -> lookUpIfRecursive $ (Defined, Just typ)
>			Nothing ->  (lookUpInParam $ lookup qName un) 
> 	lookUpInParam (Just a) = (UnDefined, Just a)
> 	lookUpInParam Nothing =
>		let (paramNTy, _, _) = funcLocals ty
>		    inParam = lookup qName paramNTy -- check for (x:xs)
>		    res = case inParam of
>				Nothing -> (NotFound, Nothing)
>				Just a  -> (Parameter, Just a)
>		in res
> 	lookUpIfRecursive (tyD, tyV) 
>		| ((getFnName [mtch]) == (prettyPrint qName)) = (Recursive, tyV)
>		| otherwise				      = (tyD, tyV)

> setFnType :: TypeInfo -> Decl (SrcSpanInfo, ItemState) -> Decl (SrcSpanInfo, ItemState)
> setFnType ty fn@(FunBind (s, vi) lstMatch) = 
>	{-unsafePerformIO $ 
>	do
>	let (paramTest, _, _) = funcLocals ty
>	putStrLn ("--paramTest1--" ++ (show paramTest))
>	return $ -}result
>	where
>	(d, un)	= mergeAllGlobalTypes ty
>	dSearch = lookup (HSES.UnQual (HSES.Ident (getFnName lstMatch))) d
>	result = case dSearch of
>			Just typ -> setFnTyAsTooltip fn (show typ)
>			Nothing ->  setFnTyAsTooltip fn ""

> setFnTyAsTooltip fn@(FunBind (s, vi) lstMatch) typ =
>	let 	vi1 	= vi {toolTipText = typ}
>	in	FunBind (s, vi1) lstMatch

> setFnParamTypes :: TypeInfo -> Match (SrcSpanInfo, ItemState) -> TypeInfo
> setFnParamTypes ty mtch@(Match (s1,vi1) n lstP rhs binds) = 
>	let 	TypeInfo hs oth th (fnP, fnLD, fnLUn) = ty
>		(d, un)	= mergeAllGlobalTypes ty
>		fnName = prettyPrint n
>		fnUnqual = HSE.UnQual (unAnn n)
>		fnType = lookup fnUnqual d
>		--Just fnTy = lookup fnUnqual d
>		fnTyLst = 
>			case fnType of
>				Just fnTy -> init $ mkTypeToList fnTy []
>				Nothing   -> []
>		paramLst = map (\a -> HSE.UnQual (HSE.Ident (prettyPrint a))) lstP 
>		paramNTy = 
>			case fnType of
>				Just fnTy -> (zip paramLst fnTyLst) ++ (mkSplParamList (zip lstP fnTyLst) d)
>				Nothing   -> []
>	in 	TypeInfo hs oth th (fnP ++ paramNTy, fnLD, fnLUn)

> mkSplParamList lstP d =
>	concat $ map (splParam) lstP 
>	where
>	splParam ((PParen _ (PInfixApp (_, vi) p1@(PVar _ (Ident _ strP1)) (Special _ (Cons _)) p2@(PVar _ (Ident _ strP2)))),ty) =
>		let 	--strP1 = prettyPrint p1
>			--strP2 = prettyPrint p2
>			tyP1 = (HSE.UnQual (HSE.Ident strP1), TcTyVar (BoundTv (drop 2 (show ty))))
>		 	--tyP2 = (HSE.UnQual (HSE.Ident strP2), TcTyVar (BoundTv strP2))
>			--tyP1 = (HSE.UnQual (HSE.Ident strP1), (fromJust $ lookup (HSE.UnQual $ HSE.Ident "aVar") d))
>		 	tyP2 = (HSE.UnQual (HSE.Ident strP2), ty)
>		in 	[tyP1, tyP2]
>	splParam _ = []
>	

> {-mkSplParamList lstP d =
>	concat $ map (splParam) lstP 
>	where
>	splParam (PParen _ (PInfixApp (_, vi) p1@(PVar _ (Ident _ strP1)) (Special _ (Cons _)) p2@(PVar _ (Ident _ strP2)))) =
>		let 	--strP1 = prettyPrint p1
>			--strP2 = prettyPrint p2
>			--tyP1 = (HSE.UnQual (HSE.Ident strP1), TcTyVar (BoundTv strP1))
>		 	--tyP2 = (HSE.UnQual (HSE.Ident strP2), TcTyVar (BoundTv strP2))
>			tyP1 = (HSE.UnQual (HSE.Ident strP1), (fromJust $ lookup (HSE.UnQual $ HSE.Ident "aVar") d))
>		 	tyP2 = (HSE.UnQual (HSE.Ident strP2), (fromJust $ lookup (HSE.UnQual $ HSE.Ident "aList") d))
>		in 	[tyP1, tyP2]
>	splParam _ = []-}

> ppWhereModified :: Binds (SrcSpanInfo, ItemState) -> [String]
> ppWhereModified (BDecls _ []) = [""]
> ppWhereModified (BDecls _ l)  = map prettyPrint l
> ppWhereModified (IPBinds _ b) = [""]

> prettyBinds binds = unlines $ ppWhereModified binds

> setLocalFnTypes :: TypeInfo -> Maybe (Binds (SrcSpanInfo, ItemState)) -> TypeInfo
> setLocalFnTypes ty Nothing = ty
> setLocalFnTypes ty (Just binds@(BDecls _ lstDecl)) = 
>	unsafePerformIO $ 
>		do
>		let TypeInfo hs oth th (fnP, _, _) = ty
>		let str = prettyBinds binds
>		(lD, lUn) <- 
>			case (HSE.parseFileContentsWithComments defaultParseMode str) of
>				ParseOk (mod, _) 	-> 
>					TyAsst.getAllVarEnv (mergeAllGlobalTypes ty) ([], []) mod 
>				_			->
>					return ([], [])
>		return $ TypeInfo hs oth th (fnP, lD, lUn)
> setLocalFnTypes ty _ = ty


> {-checkExp :: TypeInfo -> Match (SrcSpanInfo, ItemState) -> Exp (SrcSpanInfo, ItemState) -> String
> checkExp ty mtch@(Match (s1,vi1) n lstP rhs binds) exp = 
>	unsafePerformIO $ 
>		do
>		let mtch1 = Match (s1,vi1) (Ident (ann n) "habijabi") lstP (UnGuardedRhs (ann rhs) exp) binds
>		let str = prettyPrint mtch1
>		putStrLn (show ty)
>		let ParseOk (mod, _)   = HSE.parseFileContentsWithComments defaultParseMode str
>		let Tc tc = typecheckModule mod
>		let (d, un) = mergeAllGlobalTypes ty
>		env <- mkTcEnv (d ++ un) [] []
>		tcRes <- tc env
	
>		case tcRes of
>			Left  e -> 
>				do
>				putStrLn (show e)
>				return $ (show e)
>			Right e -> return ("")
>		--return $ "xxx"


> lookUpType :: TypeInfo -> Match (SrcSpanInfo, ItemState) -> HSES.QName -> (TyDefined, Maybe Sigma)
> lookUpType ty mtch@(Match (s1,vi1) n lstP rhs binds) qName = 
>	unsafePerformIO $ 
>	do
>	--putStrLn ("mmm" ++ (show $ ty) ++ (show qName))
>	let (_, res) = result
>	let a= case res of
>		Just b -> show $ mkTypeToList b []
>		Nothing -> "Nothing"
>	putStrLn ("" ++ (show qName) ++ "--" ++ (a))
>	return $ result
>	where
>	TypeInfo hs oth (dTh, unTh) (fnLP, fnLD, fnLUn) = ty
>	(dHs, unHs) = unzip $ [(a, b) | (_, a, b) <- hs]
>	(dOth, unOth) = unzip $ [(a, b) | (_, a, b) <- oth]
>	(d, un)	= (((concat $ (dHs ++ dOth)) ++ dTh), ((concat $ (unHs ++ unOth)) ++ unTh) ++ (fnLP ++ fnLD ++ fnLUn))
>	dSearch = lookup qName d
>	result = case dSearch of
>			Just typ -> (Defined, Just typ)
>			Nothing ->  (lookUpInParam d $ lookup qName un) -- parameter
> 	lookUpInParam d (Just a) = (UnDefined, Just a)
> 	lookUpInParam d Nothing =
>		let fnName = prettyPrint n
>		    fnUnqual = HSE.UnQual (unAnn n)
>		    Just fnTy = lookup fnUnqual d
>		    fnTyLst = init $ mkTypeToList fnTy []
>		    paramLst = map prettyPrint lstP 
>		    paramNTy = zip paramLst fnTyLst
>		    inParam = lookup (prettyPrint qName) paramNTy
>		    res = 
>			unsafePerformIO $ 
>			do
>			putStrLn ("" ++ (fnName) ++ "--" ++ (show paramNTy))
>			return $ case inParam of
>				Nothing -> (NotFound, Nothing)
>				Just a  -> (Parameter, Just a)
>		in res-}

> setGiErr viToChg errMsg = viToChg {typeErr = errMsg}
> setGiExpErr errMsg exp = 
>	let (s, vi) = ann exp
>	in amap (\_ -> (s, setGiErr vi errMsg)) exp
> setGiType viToChg nType@(UnDefined, tyInfo) = viToChg {nodeType = nType, 
>				     nodeTypeArgs = setNodeTypeGra viToChg nType,
>				     toolTipText = setToolTip nType,
>				     typeErr = "Function not defined."}
> setGiType viToChg nType = viToChg {nodeType = nType, 
>				     nodeTypeArgs = setNodeTypeGra viToChg nType,
>				     toolTipText = setToolTip nType}

> setToolTip (UnDefined, tyInfo) = "Function not defined"
> setToolTip (_, Just tyInfo) = makeListToTypeToolTip tyInfo--show $ ppr tyInfo--
> setToolTip (_, _) = "Function not defined"

> setNodeTypeGra viToChg (_, Nothing) = []

> setNodeTypeGra viToChg (UnDefined, _) = []
> setNodeTypeGra viToChg (_, Just tyInfo) =
>	let fnTyLst = init $ mkTypeToList tyInfo []
> 	    lstGi   = snd $ mapAccumL setArgGi x fnTyLst
>	    Point x y  = itemPosition viToChg
>	    y1 = y - 10
>	    shpSz = sz 10 10
>	    setArgGi xNew argTy = (xNew + 10, viToChg {
>			    	   itemPosition = pt xNew y1,
>				   itemSize = shpSz,
>				   toolTipText = show argTy}
>				  )
>	in lstGi

> --typeToList nType lst =
> mkTypeToList :: TcType -> [TcType] -> [TcType]
> mkTypeToList (TcForAll a b r) lst = lst ++ (mkTypeToList r lst)
> mkTypeToList a@(TcTyFun r1 r2) lst  = lst ++ [(r1)] ++ (mkTypeToList r2 lst)
> mkTypeToList a@(TcTyCon _) lst = lst ++ [(a)]
> mkTypeToList a@(TcTyVar _) lst = lst ++ [(a)]
> mkTypeToList a@(TcTyApp _ _) lst = lst ++ [(a)]
> mkTypeToList a@(MetaTv _) lst = lst ++ [(a)]

> --typeToList nType lst =
> makeListToTypeToolTip ty = init $ init $ init $ concat $ map mkListToTypeString (mkTypeToList ty [])
> mkListToTypeString (TcForAll a b r) = ""
> mkListToTypeString a@(TcTyFun r1 r2)  = "(" ++ (show a) ++ ")" ++ " -> "
> mkListToTypeString a@(TcTyCon _) = (show a) ++ " -> "
> mkListToTypeString a@(TcTyVar _) = (show a) ++ " -> "
> mkListToTypeString a@(TcTyApp _ _) 
>	| (startswith "[]" (show a)) 	= "[" ++ (drop 3 (show a)) ++ "]" ++ " -> "
>	| otherwise			= (show a) ++ " -> "
> mkListToTypeString a@(MetaTv _) = (show a) ++ " -> "

> {-mkTypeToList (TcForAll a b r) lst = showLog ("TcForAll--"++ show a) $ lst ++ (mkTypeToList r lst)
> mkTypeToList a@(TcTyFun r1 r2) lst  = showLog ("TcTyFun--" ++ (show r1) ++ (show r2)) $ lst ++ [(r1)] ++ (mkTypeToList r2 lst)
> mkTypeToList a@(TcTyCon _) lst = showLog "TcTyCon--" $ lst ++ [(a)]
> mkTypeToList a@(TcTyVar _) lst = showLog "TcTyVar--" $ lst ++ [(a)]
> mkTypeToList a@(TcTyApp b c) lst = showLog ("TcTyApp--" ++ (show b) ++ (show c)) $ lst ++ (mkTypeToList b lst) ++ (mkTypeToList c lst)--[(a)]
> mkTypeToList a@(MetaTv _) lst = showLog "MetaTv--" $ lst ++ [(a)]-}

>	
> typeToList (TcForAll a b r) = "--TcForAll--" ++ (typeToList r)
> typeToList (TcTyFun r1 r2)  = "--TcTyFunR1--\n" ++ (typeToList r1) ++ "--TcTyFunR2--\n" ++ (typeToList r2)
> typeToList a@(TcTyCon _) = "--TcTyCon--"  ++ (show a)
> typeToList a@(TcTyVar _) = "--TcTyVar--"  ++ (show a)
> typeToList a@(TcTyApp _ _) = "--TcTyApp--" ++ (show a)
> typeToList a@(MetaTv _) = "--MetaTv--"  ++ (show a)


> class UnAnnotation t1 t2 where
> 	unAnn :: t1 a -> t2

> instance UnAnnotation QName HSES.QName where
> 	unAnn (UnQual _ qn) = HSES.UnQual (unAnn qn)
> 	unAnn (Special _ (Cons _)) = HSES.Special (HSES.Cons)
> 	unAnn (Special _ (ListCon _)) = HSES.Special (HSES.ListCon)

> instance UnAnnotation Name HSES.Name where
> 	unAnn (Ident _ qn)  = HSES.Ident qn
> 	unAnn (Symbol _ qn) = HSES.Symbol qn

> instance TypeManager Match Exp SrcSpanInfo ItemState where
>	{-setType ty mtch exp@(App (s2, vi2) eOne (Var (s3, vi3) qual)) =
>		App (s2, vi2) eOne1 (v2)		
>		where
>		errMsg = checkExp ty mtch exp
>		eOne1 = setType ty mtch eOne
>		v1 = Var (s3, setGiErr vi3 errMsg) qual
>		v2 = setType ty mtch v1

>	setType ty mtch exp@(App (s2, vi2) (Var (s3, vi3) qual) eTwo) =
>		App (s2, vi2) (v2) (setGiExpErr (s4, vi4) errMsg eTwo1)
>		where
>		errMsg = checkExp ty mtch exp
>		(s4, vi4) = ann eTwo1 
>		eTwo1 = setType ty mtch eTwo
>		v1 = Var (s3, setGiErr vi3 errMsg) qual
>		v2 = setType ty mtch v1-}

>	setType ty mtch exp@(App (s2, vi2) eOne eTwo) =
>		let 	errE = checkExp ty mtch exp
>			eOne1 = setType ty mtch eOne
>			eTwo1 = setType ty mtch eTwo
>		in 	
>			App (s2, vi2) eOne1 (setGiExpErr errE eTwo1)
>		{-let 	err1 = checkExp ty mtch eOne
>			err2 = checkExp ty mtch eTwo
>			errE = checkExp ty mtch exp
>			eOne1 = setType ty mtch eOne
>			eTwo1 = setType ty mtch eTwo
>			setErr False False False = App (s2, vi2) eOne eTwo
>			setErr False False True = App (s2, vi2) eOne (setGiExpErr errE eTwo)
>			--setErr _ _ _ = App (s2, vi2) eOne1 eTwo1
>			setErr True False _ = App (s2, vi2) eOne1 eTwo
>			setErr False True _ = App (s2, vi2) eOne eTwo1
>			setErr True True _ = App (s2, vi2) eOne1 eTwo1
>		in 	
>			setErr (err1 /= "") (err2 /= "") (errE /= "")-}

>	setType ty mtch exp@(If (s2, vi2) eOne eTwo eThree) =
>		let 	err1 = checkExp ty mtch eOne
>			err2 = checkExp ty mtch eTwo
>			err3 = checkExp ty mtch eThree
>			errE = checkExp ty mtch exp
>			eOne1 = setGiExpErr err1 $ setType ty mtch eOne
>			eTwo1 = setGiExpErr err2 $ setType ty mtch eTwo
>			eThree1 = setGiExpErr err3 $ setType ty mtch eThree
>			setErr False False False False = If (s2, vi2) eOne eTwo eThree
>			setErr False False False True = setGiExpErr errE $ If (s2, vi2) eOne eTwo eThree -- This needs more details
>			setErr _ _ _ _ = If (s2, vi2) eOne1 eTwo1 eThree1

>		in 	
>			unsafePerformIO $ 
>			do
>			putStrLn ("--err1--" ++ err1 ++ "--err2--" ++ err2 ++ "--errE--" ++ errE)
>			return $ setErr (err1 /= "") (err2 /= "") (err3 /= "") (errE /= "")

>			
>	setType ty mtch exp@(Paren (s3, vi3) eOne) =
>		let 	eOne1 = setType ty mtch eOne
>		in 	Paren (ann eOne1) eOne1

>	setType ty mtch exp@(Var (s3, vi3) qual) =
>		Var (s3, setGiType vi3 tyInfo) qual
>		where
>		tyInfo = lookUpType ty mtch (unAnn qual)

>	setType ty mtch exp@(List (s3, vi3) []) =
>		List (s3, setGiType vi3 tyInfo) []
>		where
>		tyInfo = lookUpType ty mtch (HSE.UnQual (HSE.Ident "aList"))

>	setType ty mtch exp@(Con (s3, vi3) qual@(Special _ (Cons _))) =
>		--Con (s3, setTyCons (setGiType vi3 tyInfo)) qual
>		Con (s3, setGiType vi3 tyInfo) qual
>		where
>		--tyInfo = (SpecialTy, Nothing)--lookUpType ty mtch (unAnn qual)
>		tyInfo = lookUpType ty mtch (HSE.UnQual (HSE.Ident "listCons"))

>	{-setType ty mtch exp@(Con (s3, vi3) qual@(Special _ (ListCon _))) =
>		Con (s3, (setGiType vi3 tyInfo){toolTipText="Special List Constructor :: []"}) qual
>		where
>		tyInfo = (SpecialTy, Nothing)-}

>	setType _ _ otherExp =
>		otherExp

> setTyCons vi =
>	let 	Point x y 	= itemPosition vi
>		gra1		= viInit {itemPosition = pt x (y-10), itemSize = sz 10 10, toolTipText = "a"}
>		gra2		= viInit {itemPosition = pt (x+10) (y-10), itemSize = sz 10 10, toolTipText = "[a]"}
>	in	vi 	{toolTipText="Special List Constructor :: a -> [a] -> [a]",
>		 	 nodeTypeArgs= [gra1, gra2]}

>	{-setType ty mtch exp@(Var (s3, vi3) qual) =
>		Var (s3, setGiType vi3 tyInfo) qual
>		where
>		tyInfo = lookUpType ty mtch (unAnn qual)-}

>	{-setType ty mtch (App (s, vi) eOne eTwo) =
>		App (s, vi) eOne1 eTwo1
>		where
>		eTwo1 = setType ty mtch eTwo 
>		eOne1 = setType ty mtch eOne -}
>	--setType ty t (InfixApp s eOne qop eTwo) =

>	--setType ty t (If s eOne eTwo eThree) =
 
>	--setType ty t (Paren s eOne) =

>	--setType ty t (Var s qual) =

>	--setType ty t (Lit s literal) =


> class (Functor t, Annotated t) => TreeManager t a b where
>	setAutoLayoutAndType 	:: TypeInfo -> b -> t a -> t (a,b)
>	--setType 		:: TypeInfo -> t (a,b) -> t (a,b)
>	setAutoLayout 		:: b -> t a -> (b, t (a,b))
>	setAutoLayoutGra	:: b -> t (a,b) -> (b, t (a,b))
>	defaultAutoLayout 	:: b -> t a -> (b, t (a,b))
>	setAutoLayoutMB		:: b -> Maybe (t a) -> (b, Maybe (t (a,b)))
>	--mapAutoLayoutAndType 	:: TypeInfo -> b -> [t a] -> (b, [t (a,b)])
>	mapAutoLayout	 	:: b -> [t a] -> (b, [t (a,b)])
>	setAutoLayoutPrec 	:: b -> Bool -> (Int, Int) -> t a -> ((Int, Int), t (a,b)) -- isSecond, (r , c)
>	annGI			:: t (a,b) -> b
>	makeChildNodesJoins 	:: [b] -> t (a,b) -> t (a,b)

>	setAutoLayoutAndType ty lp node	= snd $ setAutoLayout lp node
>		--setType ty $ (snd $ setAutoLayout lp node)
>	--setType _ node 			= node
>	setAutoLayout 	 		= defaultAutoLayout
>		--(lp, fmap (\s -> (s, lp)) node) 
>	defaultAutoLayout lp node 	= 
>		(lp, fmap (\s -> (s, lp)) node) 
>	setAutoLayoutGra lp node 	=
>		setAutoLayout lp $ fmap (\(s, _) -> s) node
>	setAutoLayoutMB lp Nothing	= 
>		(lp, Nothing)
>	setAutoLayoutMB lp (Just node)	= 
>		let 	(cp, node1) = setAutoLayout lp node
>		in 	(cp, Just node1)
>	--mapAutoLayoutAndType ty lp lstNode	= 
>	--	mapAccumL (setAutoLayoutAndType ty) lp lstNode
>	mapAutoLayout lp lstNode	= 
>		mapAccumL setAutoLayout lp lstNode
>	setAutoLayoutPrec lp _ (r, c) node = 
>		((r, c), (snd $ defaultAutoLayout lp node))
>	annGI t				=
>		snd $ ann t
>	makeChildNodesJoins _ unlessDe	=
>		unlessDe

> setSelectedFunc :: SystemState -> SystemState
> setSelectedFunc m@(SystemState f t g ty h) = 
>	let 	(Module (s,vi) mh mp imp lstDecl)	= shapeTree g 
>		lstDecl1				= map (setFuncSel m) lstDecl
>		mod					= (Module (s,vi) mh mp imp lstDecl1)
>	in	SystemState f t g {shapeTree = mod} ty h	

> setFuncSel :: SystemState -> Decl (SrcSpanInfo, ItemState) -> Decl (SrcSpanInfo, ItemState)
> setFuncSel m@(SystemState f t g ty h) decl@(FunBind svi lstMatch)	=
>	let	lstMtch1		= reverse $ snd $ mapAccumL (setSelectedMtch m) False (reverse lstMatch)
>	in	selectNodeOnVal "selected" (isAnySel lstMtch1 "selected") $ FunBind svi lstMtch1	
> setFuncSel m@(SystemState f t g ty h) decl	= decl

> setSelectedMtch :: SystemState -> Bool -> Match (SrcSpanInfo, ItemState) -> (Bool, Match (SrcSpanInfo, ItemState))	
> setSelectedMtch m@(SystemState f t g ty h) False mtch	= 	
>	let	str				= textStr t
>		insPt			       	= insertionPt t
>		(insLnNo, insClNo)		= getRowColFromInsPt str insPt				
>		srcStLn				= srcSpanStartLine $ srcInfoSpan $ fst $ ann mtch
>		srcEndCol			= srcSpanEndLine $ srcInfoSpan $ fst $ ann mtch
>	in	--showLog ((show srcStLn) ++ "--" ++ (show srcEndCol)) $ 
>		if (insLnNo >= srcStLn && insLnNo <= srcEndCol)
>		then (True, selectNodeOnVal "selected" True mtch)
>		else (False, mtch)
> setSelectedMtch m@(SystemState f t g ty h) True mtch	= (True, mtch)	

> instance TreeManager Module SrcSpanInfo ItemState where
>	setAutoLayout lp (Module s mh mp imp decl) = 
>		(lp, Module (s, lp) mh1 mp1 imp1 decl1)
>		where
>		(_, mh1)	= setAutoLayoutMB lp mh 
>		(_, mp1) 	= mapAutoLayout lp mp 
>		(_, imp1) 	= mapAutoLayout lp imp 
>		(_, decl1) 	= mapAutoLayout lp{itemPosition = Point 0 70} decl 

> instance TreeManager Decl SrcSpanInfo ItemState where
>	setAutoLayout lp (PatBind s (PVar _ n)  _ r b) =
>		let 	patToFn = FunBind s [Match s n [] r b]
>		in	setAutoLayout lp patToFn
>	setAutoLayout lp (FunBind s lstMatch) 	=
>		let (lpMtch, lstMatch1) = setAutoLayoutMatchList lp lstMatch
>		    Point x y 	= itemPosition lp
>		    pos 	= lp {itemPosition = Point 0 (y+10),
>			      	      itemSize = Size 125 20, 
>			              synonym = getFnName lstMatch}
>		    Point lx ly	= itemPosition lpMtch
>		    cp 		= lpMtch {itemPosition = Point 0 (ly+20)}
>		in (cp, FunBind (s, pos) lstMatch1)
>	setAutoLayout lp otherDecl		= 
>		defaultAutoLayout lp otherDecl

> getFnName :: [Match a] -> String
> getFnName [] 	= ""
> getFnName lstMatch 	= 
>	let 	Match _ n _ _ _ = lstMatch!!0
>       in 	prettyPrint n


> setAutoLayoutMatchList :: ItemState -> [Match SrcSpanInfo] -> (ItemState, [(Match (SrcSpanInfo, ItemState))])
> setAutoLayoutMatchList lp lstMatch =
>	let 	Point x y 		= itemPosition lp
>	    	pos       		= lp {itemPosition = Point 0 (y+45),
>	    		                      itemSize = Size 20 20}
>	    	((cp,_,_), lstM) 	= mapAccumL (setAutoLayoutMatch pos) (pos, 0, 0) lstMatch
>	in 	(cp, lstM)

> setAutoLayoutMatch :: ItemState -> (ItemState,Int,Int) -> Match SrcSpanInfo -> ((ItemState,Int,Int), Match (SrcSpanInfo, ItemState))
> setAutoLayoutMatch lp (_,c,r)  (Match s n lstP rhs binds) 	=
>	((cp, c1, r1), Match (s,cp) n1 lstP1 rhs1 binds1)
>	where
>	(_, n1) 	= setAutoLayout lp n 
>	(_, lstP1) 	= mapAutoLayout lp{itemPosition = Point 20 5} lstP
>	(_, rhs1) 	= setAutoLayout lp rhs
>	(_, binds1) 	= setAutoLayoutMB lp binds 
>	nextP		= nextPosMtch lp (c,r) (30,30)
>	cp 		= nextP {synonym = show $ getNoFrmRC c r 3}
>	c1		= incrC c (c < 3)
>	r1		= incrR r (c == 3)
>	incrC v False = 0
>	incrC v True = v + 1
>	incrR v False  = v
>	incrR v True = v + 1
>	getNoFrmRC c r noC = r * (noC+1) + c + 1
> setAutoLayoutMatch lp (_,c,r)  otherMatch 			=
>	let mtch = snd $ defaultAutoLayout lp otherMatch
>	in  ((lp,c,r), mtch)

> nextPosMtch :: ItemState -> (Int, Int) -> (Int, Int) -> ItemState
> nextPosMtch lp (c, r) (wu, hu) =
>	let Point x y 	= itemPosition lp
>	    Size w h 	= itemSize lp
>	    x1 		= x + (c * (wu+5)) 
>	    y1 		= y + (r * (hu+5)) 
>	in lp {itemPosition = Point x1 y1}

> instance TreeManager Binds SrcSpanInfo ItemState where
>	setAutoLayout lp (BDecls s decl) = 
>		(lp, BDecls (s, lp) decl1)
>		where
>		(_, decl1) 	= mapAutoLayout lp{itemPosition = Point 0 25} decl 
>	setAutoLayout lp otherBinds = 
>		defaultAutoLayout lp otherBinds

> instance TreeManager Pat SrcSpanInfo ItemState where
>	setAutoLayout lp (PVar s1 (Ident s2 idnt)) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+30),
>			      	      itemSize = Size 105 20}
>		in (cp, PVar (s1, cp) (Ident (s2, cp) idnt))
>	setAutoLayout lp (PWildCard s) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+30),
>			      	      itemSize = Size 105 20}
>		in (cp, PWildCard (s, cp))
>	setAutoLayout lp (PList s []) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+30),
>			      	      itemSize = Size 105 20}
>		in (cp, PList (s, cp) [])
>	setAutoLayout lp (PParen s (PInfixApp _ p1@(PVar _ _) q@(Special _ (Cons _)) p2@(PVar _ _))) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+55),
>			      	      itemSize = Size 105 20}
>		    (cp1, p11) 	= setAutoLayoutConsP1 lp p1
>		    (cp2, q1) 	= setAutoLayout lp q
>		    (cp3, p21) 	= setAutoLayoutConsP2 lp p2
>		    cp4		= cp1 {itemSize = Size 105 45}
>		in (cp, (PParen (s, cp4) (PInfixApp (s, cp4) p11 q1 p21)))
>	setAutoLayout lp (PParen s p) = 
>		let (cp, p1) = setAutoLayout lp p
>		in (cp, PParen (s, cp) (p1))
>	setAutoLayout lp (PInfixApp s p1@(PVar _ _) q@(Special _ (Cons _)) p2@(PVar _ _)) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+55),
>			      	      itemSize = Size 105 20}
>		in (cp, PInfixApp (s, cp) (snd $ setAutoLayoutConsP1 lp p1) (snd $ setAutoLayout lp q) 
>				      (snd $ setAutoLayoutConsP2 lp p2))
>	setAutoLayout lp (PLit s (Int _ i s1)) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+30),
>			      	      itemSize = Size 105 20}
>		in (cp, PLit (s, cp) (Int (s, cp) i s1))
>	setAutoLayout lp (PLit s (String _ i s1)) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+30),
>			      	      itemSize = Size 105 20}
>		in (cp, PLit (s, cp) (String (s, cp) i s1))
>	setAutoLayout lp (PLit s (Char _ i s1)) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+30),
>			      	      itemSize = Size 105 20}
>		in (cp, PLit (s, cp) (Char (s, cp) i s1))
>	setAutoLayout lp (PApp s (UnQual _ (Ident _ "True")) []) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+30),
>			      	      itemSize = Size 105 20}
>		in (cp, PApp (s, cp) (UnQual (s, cp) (Ident (s, cp) "True")) [])
>	setAutoLayout lp (PApp s (UnQual _ (Ident _ "False")) []) = 
>		let Point x y 	= itemPosition lp
>		    cp 	= lp {itemPosition = Point 20 (y+30),
>			      	      itemSize = Size 105 20}
>		in (cp, PApp (s, cp) (UnQual (s, cp) (Ident (s, cp) "False")) [])
>	setAutoLayout lp otherPat = 
>		defaultAutoLayout lp otherPat

> setAutoLayoutConsP1 lp (PVar s1 (Ident s2 idnt)) = 
>	let Point x y 	= itemPosition lp
>	    cp 		= lp {itemPosition = Point 20 (y+30),
>			      itemSize = Size 105 20}
>	in (cp, PVar (s1, cp) (Ident (s2, cp) idnt))
> setAutoLayoutConsP2 lp (PVar s1 (Ident s2 idnt)) = 
>	let Point x y 	= itemPosition lp
>	    cp 		= lp {itemPosition = Point 20 (y+55),
>			      itemSize = Size 105 20}
>	in (cp, PVar (s1, cp) (Ident (s2, cp) idnt))

> instance TreeManager Rhs SrcSpanInfo ItemState where
>	setAutoLayout lp (UnGuardedRhs s exp) = 
>		(lp, UnGuardedRhs (s, lp) exp1)
>		where
>		(_, exp1) = setAutoLayoutPrec lp1 True (-1, 0) exp
>		lp1 = lp {itemPosition = Point 20 50}
>		--exp2 = makeChildNodesJoins [] exp1
>	setAutoLayout lp otherRhs = 
>		defaultAutoLayout lp otherRhs

> instance TreeManager Exp SrcSpanInfo ItemState where
>	setAutoLayoutPrec lp _ (r, c) (App s eOne eTwo) =
>		((r2, c2), App (s, annGI eOne1) eOne1 eTwo1)
>		where
>		((r1, c1), eTwo1) = setAutoLayoutPrec lp True (r, c) eTwo 
>		((r2, c2), eOne1) = setAutoLayoutPrec lp False (r1, c1) eOne 
>	setAutoLayoutPrec lp _ (r, c) (InfixApp s eOne qop eTwo) =
>		((r3, c3), InfixApp (s, annGI qop1) eOne1 qop1 eTwo1)
>		where
>		((r1, c1), eTwo1) = setAutoLayoutPrec lp True (r, c) eTwo 
>		((r2, c2), eOne1) = setAutoLayoutPrec lp True (r1, c1) eOne 
>		((r3, c3), qop1) = setAutoLayoutPrec lp False (r2, c2) qop 
>	setAutoLayoutPrec lp _ (r, c) (If s eOne eTwo eThree) =
>		((r4, c4), If (s, lp1) eOne1 eTwo1 eThree1)
>		where
>		((r1, c1), eThree1) = setAutoLayoutPrec lp True (r, c) eThree 
>		((r2, c2), eTwo1) = setAutoLayoutPrec lp True (r1, c1) eTwo 
>		((r3, c3), eOne1) = setAutoLayoutPrec lp True (r2, c2) eOne 
>		((r4, c4), lp1) = setAutoLayoutPrecIf lp False (r3, c3)  
>	setAutoLayoutPrec lp _ (r, c) (Paren s eOne) =
>		((r1, c1), Paren (s, annGI eOne1) eOne1)
>		where
>		((r1, c1), eOne1) = setAutoLayoutPrec lp False (r, c) eOne 
>	setAutoLayoutPrec lp isSecond (r, c) (Var s qual) =
>		((r1, c1), Var (s, vi) (snd $ defaultAutoLayout lp qual))
>		where 
>		(r1, c1) = setRCExp isSecond (r, c)
>		vi = setGiExp (r1, c1) lp
>	setAutoLayoutPrec lp isSecond (r, c) (Lit s literal) =
>		((r1, c1), Lit (s, vi) (snd $ defaultAutoLayout lp literal))
>		where 
>		(r1, c1) = setRCExp isSecond (r, c)
>		vi = setGiExp (r1, c1) lp
>	setAutoLayoutPrec lp isSecond (r, c) (Con s unqual) =
>		((r1, c1), Con (s, vi) (snd $ defaultAutoLayout lp unqual))
>		where 
>		(r1, c1) = setRCExp isSecond (r, c)
>		vi = setGiExp (r1, c1) lp
>	setAutoLayoutPrec lp isSecond (r, c) (List s []) =
>		((r1, c1), List (s, vi) [])
>		where 
>		(r1, c1) = setRCExp isSecond (r, c)
>		vi = setGiExp (r1, c1) lp
>	setAutoLayoutPrec lp _ (r, c) otherExp =
>		((r, c), snd $ defaultAutoLayout lp otherExp)

>	{-makeChildNodesJoins lstGi (App (s, vi) (Paren (s1, vi1) e) eTwo) = 
>		App (s, vi) (Paren (s1, vi1) e1) eTwo1
>		where
>		viETwo		= annGI eTwo -- not only the second one, but all the consequtive
>		e1 = makeChildNodesJoins (viETwo:[]) e
>		eTwo1 = makeChildNodesJoins [] eTwo-}
>	makeChildNodesJoins lstGi (App (s, vi) eOne eTwo) = 
>		App (s, vi) eOne1 eTwo1
>		where
>		viETwo		= annGI eTwo
>		eOne1 = makeChildNodesJoins (viETwo:lstGi) eOne
>		eTwo1 = makeChildNodesJoins [] eTwo
>	makeChildNodesJoins lstGi (InfixApp (s, vi) eOne qop eTwo) = 
>		InfixApp (s, vi) eOne1 qop1 eTwo1
>		where
>		viETwo		= annGI eTwo
>		viEOne		= annGI eOne
>		qop1  = makeChildNodesJoins (viEOne:viETwo:lstGi) qop
>		eOne1 = makeChildNodesJoins [] eOne
>		eTwo1 = makeChildNodesJoins [] eTwo
>	makeChildNodesJoins lstGi (If (s, vi) eOne eTwo eThree) = 
>		If (s, vi1) eOne1 eTwo1 eThree1
>		where
>		viEThree	= annGI eThree
>		viETwo		= annGI eTwo
>		viEOne		= annGI eOne
>		eOne1 = makeChildNodesJoins [] eOne
>		eTwo1 = makeChildNodesJoins [] eTwo
>		eThree1 = makeChildNodesJoins [] eThree
>		vi1 = vi {joinToChildNodes = getChildNodeJoins vi (viEOne:viETwo:viEThree:lstGi),
>			  groupBox	   = getGroupBox vi (viEOne:viETwo:viEThree:lstGi)}
>	makeChildNodesJoins lstGi (Paren (s, vi) e) = 
>		Paren (s, vi) e1
>		where
>		e1 = makeChildNodesJoins [] e
>	makeChildNodesJoins lstGi (Var (s, vi) unQual) = 
>		Var (s, vi1) unQual
>		where
>		vi1 = vi {joinToChildNodes = getChildNodeJoins vi lstGi,
>			  groupBox	   = getGroupBox vi lstGi}
>	makeChildNodesJoins lstGi (Con (s, vi) unQual) = 
>		Con (s, vi1) unQual
>		where
>		vi1 = vi {joinToChildNodes = getChildNodeJoins vi lstGi,
>			  groupBox	   = getGroupBox vi lstGi}
>	makeChildNodesJoins lstGi (List (s, vi) []) = 
>		List (s, vi1) []
>		where
>		vi1 = vi {joinToChildNodes = getChildNodeJoins vi lstGi,
>			  groupBox	   = getGroupBox vi lstGi}
>	{-makeChildNodesJoins (Lit (s, vi) literal) =
>		Lit (s, vi1) literal)
>		where
>		vi1 = vi {joinToChildNodes = getChildNodeJoins vi lstGi}-}
>	makeChildNodesJoins _ otherExp =
>		otherExp

> instance TreeManager QOp SrcSpanInfo ItemState where
>	setAutoLayoutPrec lp isSecond (r, c) (QVarOp s qual) =
>		((r1, c1), QVarOp (s, vi) (snd $ defaultAutoLayout lp qual))
>		where 
>		(r1, c1) = setRCExp isSecond (r, c)
>		vi = setGiExp (r1, c1) lp
>	setAutoLayoutPrec lp isSecond (r, c) (QConOp s qual) =
>		((r1, c1), QConOp (s, vi) (snd $ defaultAutoLayout lp qual))
>		where 
>		(r1, c1) = setRCExp isSecond (r, c)
>		vi = setGiExp (r1, c1) lp
>	makeChildNodesJoins lstGi (QVarOp (s, vi) qual) = 
>		QVarOp (s, vi1) qual
>		where
>		vi1 = vi {joinToChildNodes = getChildNodeJoins vi lstGi,
>			  groupBox	   = getGroupBox vi lstGi}
>	makeChildNodesJoins lstGi (QConOp (s, vi) qual) = 
>		QConOp (s, vi1) qual
>		where
>		vi1 = vi {joinToChildNodes = getChildNodeJoins vi lstGi,
>			  groupBox	   = getGroupBox vi lstGi}

> setAutoLayoutPrecIf lp isSecond (r, c) =
>	((r1, c1), setGiExp (r1, c1) lp)
>	where 
>	(r1, c1) = setRCExp isSecond (r, c)

> setRCExp True (r, c) = (r + 1, c)
> setRCExp False (r, c) = (r + 1, c + 1)

> setGiExp (r, c) lp =
>	lp1
>	where
>	(w, h) = (60, 20)
>	(gw, gh) = (20, 15)
>	Point x y = itemPosition lp
>	x1 = x + (c * (w + gw))
>	y1 = y + (r * (h + gh))
>	lp1 = lp {itemPosition = Point x1 y1,
>		  itemSize = Size w h}

> instance TreeManager ModuleHead SrcSpanInfo ItemState where
> instance TreeManager ModulePragma SrcSpanInfo ItemState where
> instance TreeManager ImportDecl SrcSpanInfo ItemState where
> instance TreeManager Name SrcSpanInfo ItemState where
> instance TreeManager Match SrcSpanInfo ItemState where
> instance TreeManager QName SrcSpanInfo ItemState where
> instance TreeManager Literal SrcSpanInfo ItemState where

> getGroupBox :: ItemState -> [ItemState] -> (Point, Size)
> getGroupBox pGI [] = (Point 0 0, Size 0 0)
> getGroupBox pGI lstCGI = 
>	(Point (x2 - 10) (y2 - 10), Size w h)
>	where
>	Point x1 y1 = itemPosition pGI
>	Point x2 y2 = itemPosition $ last lstCGI
>	w = x1 - x2 + 20 + 60
>	h = y1 - y2 + 15 + 20


> getChildNodeJoins :: ItemState -> [ItemState] -> JoinToChildNodes
> getChildNodeJoins pGI lstCGI = snd $ mapAccumL (getChildNodeJoin pGI) 0 lstCGI

> szArgBox = sz 10 10
> getChildNodeJoin :: ItemState -> Int -> ItemState -> (Int, JoinToAChildNode)
> getChildNodeJoin pGI c cGI = joinCrossed pGI c cGI (typeErr cGI)
>	{-unsafePerformIO $ 
>	do
>	putStrLn (typeErr cGI)
>	return $ joinCrossed pGI c cGI (typeErr cGI)-}

> joinCrossed pGI c cGI "" = (,) (c + 1) $ [(pt x2 y2), (pt x1 y2), (pt x1 y1)] 
>					     ++ (makeArrow (pt x1 y1) (pt x1 y2))
>	where
>	pt1		= itemPosition pGI
>	Point x1 y1	= addToPoint pt1 (sz (c*10+5) (-10))
>	pt2		= itemPosition cGI
>	Point x2 y2	= addToPoint pt2 (sz 60 10)

> joinCrossed pGI c cGI errMsg = (,) (c + 1) $ [(pt x2 y2), 
>						cH1, cH2, cH3, cH4, cH5, cH6, cH7, cH8, cH9, 
>						(pt x1 y2), 
>						cV1, cV2, cV3, cV4, cV5, cV6, cV7, cV8, cV9, 
>						(pt x1 y1)] 
>					     ++ (makeArrow (pt x1 y1) (pt x1 y2))
>	where
>	pt1		= itemPosition pGI
>	Point x1 y1	= addToPoint pt1 (sz (c*10+5) (-10))
>	pt2		= itemPosition cGI
>	Point x2 y2	= addToPoint pt2 (sz 60 10)
>	xMid		= (x1 + x2) `div` 2
>	yMid		= (y1 + y2) `div` 2
>	cH1		= pt xMid y2
>	cH2		= pt (xMid - 2) (y2 - 2)
>	cH3		= cH1
>	cH4		= pt (xMid + 2) (y2 + 2)
>	cH5		= cH1
>	cH6		= pt (xMid + 2) (y2 - 2)
>	cH7		= cH1
>	cH8		= pt (xMid - 2) (y2 + 2)
>	cH9		= cH1

>	cV1		= pt x1 yMid
>	cV2		= pt (x1 - 2) (yMid - 2)
>	cV3		= cV1
>	cV4		= pt (x1 + 2) (yMid + 2)
>	cV5		= cV1
>	cV6		= pt (x1 + 2) (yMid - 2)
>	cV7		= cV1
>	cV8		= pt (x1 - 2) (yMid + 2)
>	cV9		= cV1

> makeArrow (Point px1 py1) (Point mx1 my1) 
>	| (py1 > my1)	= [(pt (px1-5) (py1-5)), (pt (px1+5) (py1-5)), (pt px1 py1)]
>	| otherwise	= [(pt px1 (py1+10)), (pt (px1-5) (py1+15)), (pt (px1+5) (py1+15)), (pt px1 (py1+10))]


> --class ShapeSelection a where
> getSelectedFn :: SystemState -> String -> Maybe (Decl (SrcSpanInfo, ItemState))
> getSelectedFn (SystemState _ _ g _ _) selTy = 
>	let	Module _ _ _ _ decl	= shapeTree g
>	in 	filterSel decl selTy

> getSelectedMatch :: SystemState -> String -> Maybe (Match (SrcSpanInfo, ItemState))
> getSelectedMatch m selTy = 
>	let	selFn					= getSelectedFn m "selected"
>		mtchList (Just (FunBind _ lstMtch))	= lstMtch
>		mtchList (_)			= []
>	in 	filterSel (mtchList selFn) selTy

> getSelectedLclFn :: SystemState -> String -> Maybe (Decl (SrcSpanInfo, ItemState))
> getSelectedLclFn m selTy = 
>	let	selMtch							= getSelectedMatch m "selected"
>		lclList (Just (Match _ _ _ _ (Just(BDecls _ lstDecl))))	= lstDecl
>		lclList (_)						= []
>	in 	filterSel (lclList selMtch) selTy

> getSelectedLclMatch :: SystemState -> String -> Maybe (Match (SrcSpanInfo, ItemState))
> getSelectedLclMatch m selTy = 
>	let	selFn					= getSelectedLclFn m "selected"
>		mtchList (Just (FunBind _ lstMtch))	= lstMtch
>		mtchList (_)			= []
>	in 	filterSel (mtchList selFn) selTy

> getSelectedFnPat :: SystemState -> String -> Maybe (Pat (SrcSpanInfo, ItemState))
> getSelectedFnPat m selTy = 
>	let	selMtch							= getSelectedMatch m "selected"
>		patList (Just (Match _ _ lstP _ _))			= lstP
>		patList (_)						= []
>	in 	checkPatParen (filterSel (patList selMtch) selTy) selTy

> getSelectedLclFnPat :: SystemState -> String -> Maybe (Pat (SrcSpanInfo, ItemState))
> getSelectedLclFnPat m selTy = 
>	let	selMtch							= getSelectedLclMatch m "selected"
>		patList (Just (Match _ _ lstP _ _))			= lstP
>		patList (_)						= []
>	in 	checkPatParen (filterSel (patList selMtch) selTy) selTy

> checkPatParen pat@(Just (PParen _ (PInfixApp _ p1 (Special _ (Cons _)) p2))) selTy
>	| (isSel p1 selTy)	= Just p1
>	| (isSel p2 selTy)	= Just p2
>	| otherwise		= Nothing
> checkPatParen pat@(Just p1) selTy = pat
> checkPatParen pat@(_) selTy 	= Nothing

> getSelectedFnExp :: SystemState -> String -> Maybe (Exp (SrcSpanInfo, ItemState))
> getSelectedFnExp m selTy = 
>	let	selMtch = getSelectedLclMatch m "selected"
>	in 	case selMtch of
>			Just 	(Match _ _ _ (UnGuardedRhs _ exp) _) 	-> getSelectedExp exp selTy
>	    		_						-> 
>				let selMtch = getSelectedMatch m "selected"
>				in	case selMtch of
>					Just 	(Match _ _ _ (UnGuardedRhs _ exp) _) 	-> getSelectedExp exp selTy
>	    				_						-> Nothing

> getSelectedExp :: Exp (SrcSpanInfo, ItemState) -> String -> Maybe (Exp (SrcSpanInfo, ItemState))
> getSelectedExp (App _ eOne eTwo) selTy = 
>	let	eOne1			= getSelectedExp eOne selTy
>		eTwo1			= getSelectedExp eTwo selTy
>		sel (Just exp) _  	= (Just exp)
>		sel _ (Just exp)  	= (Just exp)
>		sel _ _		  	= Nothing
>	in 	sel eOne1 eTwo1

> getSelectedExp (Paren _ eOne ) selTy = 
>	let	eOne1			= getSelectedExp eOne selTy
>		sel (Just exp)  	= (Just exp)
>		sel _		  	= Nothing
>	in 	sel eOne1

> getSelectedExp node@(Var _ _) selTy = 
>	let	sel True  	= (Just node)
>		sel False	= Nothing
>	in 	sel (isSel node selTy)

> getSelectedExp node@(Lit _ _) selTy = 
>	let	sel True  	= (Just node)
>		sel False	= Nothing
>	in 	sel (isSel node selTy)

> getSelectedExp node@(If _ eOne eTwo eThree) selTy 
>	| (isSel node selTy)	= Just node
>	| otherwise		=
>		let	eOne1			= getSelectedExp eOne selTy
>			eTwo1			= getSelectedExp eTwo selTy
>			eThree1			= getSelectedExp eThree selTy
>			sel (Just exp) _ _	= (Just exp)
>			sel _ (Just exp) _  	= (Just exp)
>			sel _ _ (Just exp) 	= (Just exp)
>			sel _ _ _	 	= Nothing
>		in 	sel eOne1 eTwo1 eThree1

> getSelectedExp node selTy = 
>	let	sel True  	= (Just node)
>		sel False	= Nothing
>	in 	sel (isSel node selTy)

> getLEAttrFn :: SystemState -> (String, ItemState)
> getLEAttrFn m = 
>	let	getAttr (Just fn@(FunBind _ lstMtch))	= (getFnName lstMtch, snd $ ann fn)
>		getAttr (_)				= ("", viInit)
>	in 	getAttr (getSelectedFn m "annEditMode") 

> getLEAttrLclFn :: SystemState -> (String, ItemState)
> getLEAttrLclFn m = 
>	let	getAttr (Just fn@(FunBind _ lstMtch))	= (getFnName lstMtch, snd $ ann fn)
>		getAttr (_)				= ("", viInit)
>	in 	getAttr (getSelectedLclFn m "annEditMode") 

> getLEAttrFnPat :: SystemState -> (String, ItemState)
> getLEAttrFnPat m = 
>	let	getAttr (Just pat@(PVar _ _))		= (prettyPrint pat, snd $ ann pat)
>		getAttr (Just pat@(PLit _ (String _ _ literal)))= (literal, snd $ ann pat)
>		getAttr (Just pat@(PLit _ (Int _ _ literal)))	= (literal, snd $ ann pat)
>		getAttr (Just pat@(PLit _ (Char _ _ literal)))	= (literal, snd $ ann pat)
>		getAttr (Just pat@(PApp _ (UnQual _ (Ident _ con)) []))= (con, snd $ ann pat)
>		getAttr (_)				= ("", viInit)
>	in 	getAttr (getSelectedFnPat m "annEditMode") 

> getLEAttrLclFnPat :: SystemState -> (String, ItemState)
> getLEAttrLclFnPat m = 
>	let	getAttr (Just pat@(PVar _ _))			= (prettyPrint pat, snd $ ann pat)
>		getAttr (Just pat@(PLit _ (String _ _ literal)))= (literal, snd $ ann pat)
>		getAttr (Just pat@(PLit _ (Int _ _ literal)))	= (literal, snd $ ann pat)
>		getAttr (Just pat@(PLit _ (Char _ _ literal)))	= (literal, snd $ ann pat)
>		getAttr (Just pat@(PApp _ (UnQual _ (Ident _ con)) []))= (con, snd $ ann pat)
>		getAttr (_)				= ("", viInit)
>	in 	getAttr (getSelectedLclFnPat m "annEditMode") 

> getLEAttrExp :: SystemState -> (String, ItemState)
> getLEAttrExp m = 
>	let	getAttr (Just exp@(Var _ (UnQual _ (Symbol _ symb))))	= (symb, snd $ ann exp)
>		getAttr (Just exp@(Var _ _))			= (prettyPrint exp, snd $ ann exp)
>		getAttr (Just exp@(Lit _ (String _ _ literal)))	= (literal, snd $ ann exp)
>		getAttr (Just exp@(Lit _ (Int _ _ literal)))	= (literal, snd $ ann exp)
>		getAttr (Just exp@(Lit _ (Char _ _ literal)))	= (literal, snd $ ann exp)
>		getAttr (Just exp@(Con _ ((UnQual _ (Ident _ con)))))	= (con, snd $ ann exp)
>		getAttr (_)				= ("", viInit)
>	in 	getAttr (getSelectedFnExp m "annEditMode") 

> deselectAllButtons m@(SystemState f t g ty h) = 
>	SystemState f t 
>		  g {fnClButtons = deselBtns (fnClButtons g),
>		     patButtons  = deselBtns (patButtons g),
>		     expButtons  = deselBtns (expButtons g)}
>		  ty h

> deselBtns lstButtons = 
>	map (\b -> b{btnSel=False}) lstButtons

> selectFnButton p selTy m@(SystemState f t g ty h) = 
>	setSelBtnMode $ SystemState f t g {fnClButtons = selectButton p selTy (fnClButtons g)} ty h
> selectPatButton p selTy m@(SystemState f t g ty h) = 
>	setSelBtnMode $ SystemState f t g {patButtons = selectButton p selTy (patButtons g)} ty h
> selectExpButton p selTy m@(SystemState f t g ty h) = 
>	setSelBtnMode $ SystemState f t g {expButtons = selectButton p selTy (expButtons g)} ty h

> setSelBtnMode m@(SystemState f t g ty h) = 
>	SystemState f t g{modeOfOperation = (getModeFrmMB getModeBtn)} ty h
>	where
>	getModeFrmMB Nothing = modeOfOperation g
>	getModeFrmMB (Just a) = a
>	getModeBtn = listToMaybe $ mapMaybe getMode ((expButtons g) ++ (patButtons g) ++ (fnClButtons g))
>	getMode btn 
>		| (btnSel btn)	= Just (ModelType.mode btn)
>		| otherwise	= Nothing

> 

> selectButton p selTy lstBtns = 
>	map (selBtn p selTy) lstBtns

> selBtn p selTy btn =
>	case selTy of
>		"btnSel"	-> btn {btnSel = (insideButtonArea p btn)}
>		"mseInsideBtn"	-> btn {mseInsideBtn = (insideButtonArea p btn)}



> deselectTree tree = 
>	fmap deselectAnn tree

> deselectNode node = 
>	amap deselectAnn node

> deselectList lstNode = 
>	map deselectNode lstNode

> deselectAnn (s,vi) = 
>			(s, vi{selected=False, annEditMode=False, mseInside=False})
>
> selectTree p selTy tree 	=
>	fmap (selectAnn p selTy) tree

> selectList p selTy lstNode 	=
>	map (selectNode p selTy) lstNode

> selectListGIOnly p selTy lstNode 	=
>	snd $ unzip $ map (\vi -> selectAnn p selTy (srcSpanInfoInitVal, vi)) lstNode

> selectNode p selTy node =
>	case selTy of
>		"selected" -> amap (selectAnn p selTy) (deselectTree node)
>		_	   -> amap (selectAnn p selTy) node

> selectNodeOnVal selTy val node =
>	amap (selectAnnOnType (pt 0 0) selTy val) node

> selectAnn p selTy svi@(s, vi) =
>	selectAnnOnType p selTy (insideGIArea p vi) svi

> selectAnnOnType p selTy val (s, vi) =
>	case selTy of
>		"selected" 	-> (s, vi{selected=val,annEditMode=False})
>		"annEditMode" 	-> (s, vi{annEditMode=val})
>		"mseInside" 	-> (s, checkInTypeOrOutput $ vi{mseInside=val})
>		_		-> (s, vi)
>	where
>	checkInTypeOrOutput vi = vi {
>					nodeTypeArgs = selectListGIOnly p selTy (nodeTypeArgs vi),
>					outputSelected = insideGIOutputArea p vi}

> addNewFunc (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g		
>		mod				 = Module (s, vi) mh mp imp (addNwFn lstDecl)			
>	in 	(SystemState f t g{shapeTree = mod} ty h)

> addNwFn [] = [newFunc]
> addNwFn lst = addNwFnInList lst

> addNwFnInList [] = []
> addNwFnInList (x:xs) 
>	| (isSel x "mseInside") 	= [x, newFunc] ++ addNwFnInList xs
>	| otherwise			= [x] ++ addNwFnInList xs

> newFunc = 
>	let 	var 	=  emptyBody --Var sviInit (UnQual sviInit (Ident sviInit "a"))
>           	rhs 	= UnGuardedRhs sviInit var
>	    	pat 	= (PVar sviInit (Ident sviInit "a"))
>           	mtch 	= Match sviInit (Ident sviInit "f") [] rhs Nothing
>	in 	FunBind sviInit [mtch]

> addNewClause (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g		
>		mod				 = Module (s, vi) mh mp imp (map checkInFnToAddCl lstDecl)			
>	in 	(SystemState f t g{shapeTree = mod} ty h)

> checkInFnToAddCl fn@(FunBind svi lstMtch) = FunBind svi (addNwCl lstMtch)
> checkInFnToAddCl fn = fn

> addNwCl [] = [newClause]
> addNwCl lst = addNwClInList lst

> addNwClInList [] = []
> addNwClInList (x:xs) 
>	| (isSel x "mseInside") 	= [x, newClause] ++ addNwClInList xs--[x, newSimilarCl x] ++ addNwClInList xs
>	| otherwise			= [x] ++ addNwClInList xs

> emptyBodyLitStr = "--FunctionIsNotDefinedYet--"
> emptyBody = Lit sviInit (String sviInit emptyBodyLitStr emptyBodyLitStr)

> newClause = 
>	let 	var 	= emptyBody
>           	rhs 	= UnGuardedRhs sviInit var
>	    	pat 	= (PVar sviInit (Ident sviInit "a"))
>	in 	Match sviInit (Ident sviInit "f") [] rhs Nothing

> newSimilarCl (Match _ idnt lstPat _ _)  = 
>	let 	var 	= Var sviInit (UnQual sviInit (Ident sviInit "a"))
>           	rhs 	= UnGuardedRhs sviInit var
>	in 	Match sviInit idnt lstPat rhs Nothing

> addNewLclFunc (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		lstDecl1			 = map checkSelDecl lstDecl		
>		mod				 = Module (s, vi) mh mp imp lstDecl1			
>	in 	(SystemState f t g{shapeTree = mod} ty h)
>	where
>	checkSelDecl decl@(FunBind svi lstMtch) 
>		| (isSel decl "selected") 	= FunBind svi (map checkSelMtch lstMtch)
>		| otherwise			= decl
>	checkSelDecl decl			 = decl
>	checkSelMtch mtch@(Match a b c d (Just(BDecls e lstDecl)))  
>		| (isSel mtch "selected")	= 
>			Match a b c d (Just(BDecls e (addNwFn lstDecl))) 
>		| otherwise			= mtch
>	checkSelMtch mtch@(Match a b c d Nothing)  
>		| (isSel mtch "selected")	= 
>			Match a b c d (Just(BDecls sviInit (addNwFn []))) 
>		| otherwise			= mtch
>	checkSelMtch mtch			= mtch  

> addNewLclClause (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		lstDecl1			 = map checkSelDecl lstDecl		
>		mod				 = Module (s, vi) mh mp imp lstDecl1			
>	in 	(SystemState f t g{shapeTree = mod} ty h)
>	where
>	checkSelDecl decl@(FunBind svi lstMtch) 
>		| (isSel decl "selected") 	= FunBind svi (map checkSelMtch lstMtch)
>		| otherwise			= decl
>	checkSelDecl decl			 = decl
>	checkSelMtch mtch@(Match a b c d (Just(BDecls e lstDecl)))  
>		| (isSel mtch "selected")	= 
>			Match a b c d (Just(BDecls e (map checkInFnToAddCl lstDecl))) 
>		| otherwise			= mtch
>	checkSelMtch mtch@(Match a b c d Nothing)  
>		| (isSel mtch "selected")	= 
>			Match a b c d (Just(BDecls sviInit (map checkInFnToAddCl []))) 
>		| otherwise			= mtch
>	checkSelMtch mtch			= mtch  

> addNewPat (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		lstDecl1			 = map (checkSelDeclToAddPat (modeOfOperation g)) lstDecl		
>		mod				 = Module (s, vi) mh mp imp lstDecl1			
>	in 	(SystemState f t g{shapeTree = mod} ty h)
>
> checkSelDeclToAddPat patTy decl@(FunBind svi lstMtch) 
>	| (isSel decl "selected") 	= FunBind svi (map checkSelMtch lstMtch)
>	| otherwise			= decl
>	where
>	checkSelMtch mtch@(Match a b lstP d e)  
>		| (isSel mtch "selected")	= 
>			Match a b (addNwPat patTy lstP) d e
>		| otherwise			= mtch
>	checkSelMtch mtch			= mtch  
> checkSelDeclToAddPat patTy decl		= decl

> addNwPat patTy [] = [newPat patTy]
> addNwPat patTy lst = addNwPatInList patTy lst

> addNwPatInList patTy [] = []
> addNwPatInList patTy (x:xs) 
>	| (isSel x "mseInside") 	= [x, newPat patTy] ++ addNwPatInList patTy xs
>	| otherwise			= [x] ++ addNwPatInList patTy xs

> newPat patTy = 
>	case patTy of
>		ModePatVar 	-> PVar sviInit (Ident sviInit "a")
>		ModePatWild 	-> PWildCard sviInit
>		ModePatEmptyLst	-> PList sviInit []
>		ModePatListCons	-> PParen sviInit (PInfixApp sviInit p1 sCons p2)
>				   where
>				   p1 = PVar sviInit (Ident sviInit "x")
>				   sCons = Special sviInit (Cons sviInit)
>				   p2 = PVar sviInit (Ident sviInit "xs")
>		ModePatStr 	-> PLit sviInit (String sviInit "abc" "abc")
>		ModePatInt 	-> PLit sviInit (Int sviInit 0 "0")
>		ModePatChar 	-> PLit sviInit (Char sviInit 'c' "c")
>		ModePatBool 	-> PApp sviInit (UnQual sviInit (Ident sviInit "True")) []

> addNewLclPat (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		lstDecl1			 = map checkSelDecl lstDecl		
>		mod				 = Module (s, vi) mh mp imp lstDecl1			
>	in 	(SystemState f t g{shapeTree = mod} ty h)
>	where
>	checkSelDecl decl@(FunBind svi lstMtch) 
>		| (isSel decl "selected") 	= FunBind svi (map checkSelMtch lstMtch)
>		| otherwise			= decl
>	checkSelDecl decl			 = decl
>	checkSelMtch mtch@(Match a b c d (Just(BDecls e lstDecl)))  
>		| (isSel mtch "selected")	= 
>			Match a b c d (Just(BDecls e (map (checkSelDeclToAddPat (modeOfOperation g)) lstDecl))) 
>		| otherwise			= mtch
>	checkSelMtch mtch			= mtch 

> addNewExpression :: SystemState -> SystemState
> addNewExpression (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		mod				 = Module (s, vi) mh mp imp (addNewExp (modeOfOperation g) lstDecl)			
>	in 	(SystemState f t g{shapeTree = mod} ty h) 

> addNewExp expTy lstDecl =
>	map checkSelFn lstDecl
>	where
>	checkSelFn decl@(FunBind (s, vi) lstMtch) 
>		| (isSel decl "selected") 	
>			= FunBind (s, vi) (map checkMtch lstMtch)
>		| otherwise			
>			= decl
>	checkSelFn decl	= decl
>	checkMtch mtch@(Match a b c (UnGuardedRhs svi exp) (Just(BDecls e lstDecl))) 
>		| (isSel mtch "selected") 	
>			= Match a b c (UnGuardedRhs svi (addExp exp)) (Just (BDecls e (addNewExp expTy lstDecl)))
>		| otherwise			
>			= mtch
>	checkMtch mtch@(Match a b c (UnGuardedRhs svi exp) Nothing) 
>		| (isSel mtch "selected") 	
>			= Match a b c (UnGuardedRhs svi (addExp exp)) Nothing
>		| otherwise			
>			= mtch
>	checkMtch mtch = mtch
>	--addExp exp@(App svi eOne@(If _ _ _ _) eTwo) = App svi eOne (addExp eTwo)
>	--addExp exp@(App svi eOne eTwo@(If _ _ _ _)) = App svi (addExp eOne) eTwo
>	addExp exp@(Lit _ (String _ _ literal))
>		| (literal == emptyBodyLitStr) = expVar expTy
>	addExp exp@(App (s,vi) eOne eTwo)
>		| (outputSelected vi)	 = newExp expTy exp ExpOutput
>		| (mseInside vi)	 = {-App (s,vi)-} (newExp expTy exp ExpInput) --eTwo
>		| otherwise		 = App (s,vi) (addExp eOne) (addExp eTwo)
>	addExp exp@(If (s,vi) eOne eTwo eThree) 
>		-- | (outputSelected vi)	 = App (s,vi) (newExp expTy eOne ExpOutput) eTwo
>		| (mseInside vi)	 = App (s,vi) (newExp expTy exp ExpInput) eTwo
>		| otherwise		 = If (s,vi) (addExp eOne) (addExp eTwo) (addExp eThree)
>	addExp exp@(Paren (s,vi) eOne) 
>		| (outputSelected vi)	 = Paren (s,vi) (newExp expTy eOne ExpOutput)
>		| (mseInside vi)	 = Paren (s,vi) (newExp expTy eOne ExpInput)
>		| otherwise		 = Paren (s,vi) (addExp eOne)
>	addExp exp@(Var (s,vi) v) 
>		| (outputSelected vi)	 = newExp expTy exp ExpOutput
>		| (mseInside vi)	 = newExp expTy exp ExpInput
>		| otherwise		 = exp
>	addExp exp@(Con (s,vi) (Special _ (Cons _))) 
>		| (outputSelected vi)	 = newExp expTy exp ExpOutput
>		| (mseInside vi)	 = newExp expTy exp ExpInput
>		| otherwise		 = exp
>	addExp exp@(Con (s,vi) v) 
>		| (outputSelected vi)	 = newExp expTy exp ExpOutput
>		-- | (mseInside vi)	 = newExp expTy exp ExpInput
>		| otherwise		 = exp
>	addExp exp@(Lit (s,vi) v) 
>		| (outputSelected vi)	 = newExp expTy exp ExpOutput
>		-- | (mseInside vi)	 = newExp expTy exp ExpInput
>		| otherwise		 = exp
>	addExp exp@(List (s,vi) []) 
>		| (outputSelected vi)	 = newExp expTy exp ExpOutput
>		-- | (mseInside vi)	 = newExp expTy exp ExpInput
>		| otherwise		 = exp
>	addExp exp = exp

> --newExp expTy = Var sviInit (UnQual sviInit (Ident sviInit "app"))
> expVar expTy = 
>	case expTy of
>		ModeExpApp 	-> Var sviInit (UnQual sviInit (Ident sviInit "a"))
>		ModeExpOp 	-> Var sviInit (UnQual sviInit (Symbol sviInit "+"))
>		ModeListCons 	-> Con sviInit (Special sviInit (Cons sviInit))
>		ModeEmptyList 	-> List sviInit []--Con sviInit (Special sviInit (ListCon sviInit))
>		ModeCnstStr	-> Lit sviInit (String sviInit "abc" "abc")
>		ModeCnstInt	-> Lit sviInit (Int sviInit 0 "0")
>		ModeCnstChar 	-> Lit sviInit (Char sviInit 'c' "c")
>		ModeCnstBool 	-> Con sviInit (UnQual sviInit (Ident sviInit "True"))
>		ModeIfStmt 	-> Var sviInit (UnQual sviInit (Ident sviInit "cond"))
>				   {-If sviInit condi v1 v2
>				   where
>				   condi = Var sviInit (UnQual sviInit (Symbol sviInit "+"))
>			           v1 	 = Var sviInit (UnQual sviInit (Ident sviInit "a1"))
>			           v2 	 = Var sviInit (UnQual sviInit (Ident sviInit "a2"))-}

> data ExpTy = ExpInput | ExpOutput
> newExp expTy prevExp ExpInput = 
>	App sviInit prevExp (expVar expTy)

> newExp expTy prevExp ExpOutput = 
>	case expTy of
>		ModeExpApp 	-> App sviInit (expVar expTy) prevExp 
>		ModeExpOp 	-> App sviInit (expVar expTy) prevExp 
>		ModeListCons 	-> App sviInit (expVar expTy) prevExp 
>		ModeEmptyList	-> prevExp
>		ModeCnstStr	-> prevExp
>		ModeCnstInt	-> prevExp
>		ModeCnstBool 	-> prevExp
>		ModeIfStmt 	-> App sviInit (expVar expTy) prevExp 



> selectFunc p selTy (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g		
>		mod				 = Module (s, vi) mh mp imp (selFn p selTy lstDecl)			
>	in 	(SystemState f t g{shapeTree = mod} ty h)

> selFn p selTy lstDecl =
>	map (selMtch p selTy) (selectList p selTy lstDecl)

> selectFuncEdit p selTy (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		lstDecl1			 = selectList p selTy lstDecl
>		mod				 = Module (s, vi) mh mp imp lstDecl1			
>	in 	(SystemState f t g{shapeTree = mod} ty h)

> selMtch p selTy decl@(FunBind svi lstMtch) =
>	let 	lstMtch1	= selectList p selTy lstMtch
>	in 	case selTy of
>			"selected" 	-> 
>				selectNodeOnVal selTy (isAnySel lstMtch1 "selected" || isSel decl "selected") $ FunBind svi lstMtch1
>			_		-> 
>				FunBind svi lstMtch1	
> selMtch p selTy decl				=
>	decl

> selectLclFunc p selTy (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		lstDecl1			 = map checkSelDecl lstDecl		
>		mod				 = Module (s, vi) mh mp imp lstDecl1			
>	in 	(SystemState f t g{shapeTree = mod} ty h)
>	where
>	checkSelDecl decl@(FunBind svi lstMtch)  = FunBind svi (map checkSelMtch lstMtch)
>	checkSelDecl decl			 = decl
>	checkSelMtch mtch@(Match a b c d (Just(BDecls e lstDecl)))  
>		| (isSel mtch "selected")	= 
>			case selTy of
>				--"selected" 	-> Match a b c d (Just(BDecls e (selFn p selTy lstDecl))) 
>				"annEditMode" -> Match a b c d (Just(BDecls e (selectList p selTy lstDecl))) 
>				_		-> Match a b c d (Just(BDecls e (selFn p selTy lstDecl))) 
>		| otherwise			 = mtch
>	checkSelMtch mtch			 = mtch  

> selectFuncPat p selTy (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		lstDecl1			 = selFnPat p selTy lstDecl		
>		mod				 = Module (s, vi) mh mp imp lstDecl1			
>	in 	(SystemState f t g{shapeTree = mod} ty h)


> selFnPat p selTy lstDecl = 
>	map checkSelDecl lstDecl
>	where
>	checkSelDecl decl@(FunBind svi lstMtch)  = FunBind svi (map checkSelMtch lstMtch)
>	checkSelDecl decl			 = decl
>	checkSelMtch mtch@(Match a b lstP d e)  
>		| (isSel mtch "selected")	= Match a b (map checkInPat lstP) d e 
>		| otherwise	= mtch
>	checkSelMtch mtch			 = mtch  
>	checkInPat pat				 = selectTree p selTy pat

> selectLclFuncPat p selTy (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		lstDecl1			 = map checkSelDecl lstDecl		
>		mod				 = Module (s, vi) mh mp imp lstDecl1			
>	in 	(SystemState f t g{shapeTree = mod} ty h)
>	where
>	checkSelDecl decl@(FunBind svi lstMtch)  = FunBind svi (map checkSelMtch lstMtch)
>	checkSelDecl decl			 = decl
>	checkSelMtch mtch@(Match a b c d (Just(BDecls e lstDecl)))  
>		| (isSel mtch "selected")	= 
>			Match a b c d (Just(BDecls e (selFnPat p selTy lstDecl))) 
>		| otherwise	= mtch
>	checkSelMtch mtch			 = mtch

> selectFuncExp p selTy m@(SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		mod				 = Module (s, vi) mh mp imp (selFnExp m p selTy lstDecl)			
>	in 	(SystemState f t g{shapeTree = mod} ty h) 

> selFnExp m p selTy lstDecl =
>	map checkSelDecl lstDecl		
>	where
>	checkSelDecl decl@(FunBind svi lstMtch)  = FunBind svi (map checkSelMtch lstMtch)
>	checkSelDecl decl			 = decl
>	checkSelMtch mtch@(Match a b c rhs (Just(BDecls e lstDecl)))  
>		| (isSel mtch "selected")	= 
>			case (getSelectedLclMatch m "selected") of
>				Just 	mtch 	->
>					Match a b c rhs (Just(BDecls e (selFnExp m p selTy lstDecl)))
>				Nothing		->
>					Match a b c (selectTree p selTy rhs) (Just(BDecls e lstDecl))
>		| otherwise	= mtch
>	checkSelMtch mtch@(Match a b c rhs Nothing)  
>		| (isSel mtch "selected")	= 
>			Match a b c (selectTree p selTy rhs) Nothing
>		| otherwise	= mtch
>	checkSelMtch mtch			 = mtch  

> --filterSel::[Node]-> Maybe Node
> filterSel lst selTy =
>	let 	filterSel node	= ((isSel node selTy) == True)
>		selNodes	= filter filterSel lst
>		sel True	= Just (selNodes!!0)
>		sel False	= Nothing
>	in	sel (length selNodes > 0)

> isAnySel lst selTy =
>	let 	filterSel node	= ((isSel node selTy) == True)
>		selNodes	= filter filterSel lst
>	in	(length selNodes > 0)

> isSel node selTy =
>	case selTy of
>		"selected" 	-> selected $ snd $ ann node
>		"annEditMode" -> annEditMode $ snd $ ann node
>		"mseInside" 	-> mseInside $ snd $ ann node
>		_		-> selected $ snd $ ann node
>	

> editModuleHead :: SystemState -> (String, Int) -> SystemState
> editModuleHead (SystemState f t g ty h) txt =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		mod				 = Module (s, vi) (editMH mh txt) mp imp lstDecl			
>	in 	(SystemState f t g{shapeTree = mod} ty h) 

> editMH (Just (ModuleHead svi mn wt el)) ("", _) 	= Nothing
> editMH Nothing ("", _) 				= Nothing
> editMH (Just (ModuleHead svi1 (ModuleName (s, vi) prev) wt el)) (str, ins) = 
>	let	mn	= ModuleName (s, vi{annInsertionPoint=ins}) (chkValidModuleName prev str)
>	in 	Just (ModuleHead svi1 mn wt el)
> editMH Nothing (str, ins) = 
>	let	mn	= ModuleName (sSpanInit, viInit{annInsertionPoint=ins}) str
>	in	Just (ModuleHead sviInit mn Nothing Nothing)

> editSelFuncName :: SystemState -> (String, Int) -> SystemState
> editSelFuncName (SystemState f t g ty h) txt =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		mod				 = Module (s, vi) mh mp imp (editSelFnName lstDecl txt)			
>	in 	(SystemState f t g{shapeTree = mod} ty h) 

> editSelFnName lstDecl (str, ins) =
>	map checkSelFn lstDecl
>	where
>	checkSelFn decl@(FunBind (s, vi) lstMtch) 
>		| (isSel decl "annEditMode") 	
>			= FunBind (s, vi{annInsertionPoint=ins, synonym=str}) (map editMtchNm lstMtch)
>		| otherwise			
>			= decl
>	checkSelFn decl	= decl
>	editMtchNm (Match a (Ident svi prev) c d e) = Match a (Ident svi $ chkValidFuncName prev str) c d e
>	editMtchNm mtch = mtch

> editSelLclFuncName :: SystemState -> (String, Int) -> SystemState
> editSelLclFuncName (SystemState f t g ty h) txt =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		mod				 = Module (s, vi) mh mp imp (editSelLclFnName lstDecl txt)			
>	in 	(SystemState f t g{shapeTree = mod} ty h) 

> editSelLclFnName lstDecl txt =
>	map checkSelFn lstDecl
>	where
>	checkSelFn decl@(FunBind (s, vi) lstMtch) 
>		| (isSel decl "selected") 	
>			= FunBind (s, vi) (map editMtchNm lstMtch)
>		| otherwise			
>			= decl
>	checkSelFn decl	= decl
>	editMtchNm mtch@(Match a b c d (Just(BDecls e lstDecl))) =
>		Match a b c d (Just(BDecls e (editSelFnName lstDecl txt)))
>	editMtchNm mtch = mtch

> editSelFuncPatName :: SystemState -> (String, Int) -> SystemState
> editSelFuncPatName (SystemState f t g ty h) txt =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		mod				 = Module (s, vi) mh mp imp (editSelFnPatName lstDecl txt)			
>	in 	(SystemState f t g{shapeTree = mod} ty h) 

> editSelFnPatName lstDecl (str, ins) =
>	map checkSelFn lstDecl
>	where
>	checkSelFn decl@(FunBind (s, vi) lstMtch) 
>		| (isSel decl "selected") 	
>			= FunBind (s, vi) (map checkMtch lstMtch)
>		| otherwise			
>			= decl
>	checkSelFn decl	= decl
>	checkMtch mtch@(Match a b lstP d e) 
>		| (isSel mtch "selected") 	
>			= Match a b (map editPatNm lstP) d e 
>		| otherwise			
>			= mtch
>	checkMtch mtch = mtch
>	editPatNm pat@(PVar (s,vi) (Ident svi prev))
>		| (isSel pat "annEditMode") 	
>			= PVar (s,vi{annInsertionPoint=ins}) (Ident svi $ chkValidPatVar prev str)
>		| otherwise			
>			= pat
>	editPatNm pat@(PLit (s,vi) (String svi _ _))
>		| (isSel pat "annEditMode") 	
>			= PLit (s,vi{annInsertionPoint=ins}) (String svi str str)
>		| otherwise			
>			= pat
>	editPatNm pat@(PLit (s,vi) (Int svi prevInt prev))
>		| (isSel pat "annEditMode" && (str /= "")) 	
>			= PLit (s,vi{annInsertionPoint=ins}) (Int svi (chkValidExpIntInt prevInt str) (chkValidExpIntStr prev str))
>		| otherwise			
>			= pat
>	editPatNm pat@(PLit (s,vi) (Char svi prev _))
>		| (isSel pat "annEditMode" && (str /= "")) 	
>			= PLit (s,vi{annInsertionPoint=ins}) (Char svi (chkValidExpChar prev str) ("" ++ [chkValidExpChar prev str]))
>		| otherwise			
>			= pat
>	editPatNm pat@(PApp (s,vi) (UnQual svi1 (Ident svi2 _)) [])
>		| (isSel pat "annEditMode") 	
>			= PApp (s,vi{annInsertionPoint=ins}) (UnQual svi1 (Ident svi2 str)) []
>		| otherwise			
>			= pat
>	editPatNm pat@(PParen svi1 (PInfixApp svi2 p1 (Special svi3 (Cons svi4)) p2)) =
>		PParen svi1 (PInfixApp svi2 (editPatNm p1) (Special svi3 (Cons svi4)) (editPatNm p2))
>	editPatNm pat = pat

> editSelLclFuncPatName :: SystemState -> (String, Int) -> SystemState
> editSelLclFuncPatName (SystemState f t g ty h) txt =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		mod				 = Module (s, vi) mh mp imp (editSelLclFnPatName lstDecl txt)			
>	in 	(SystemState f t g{shapeTree = mod} ty h) 

> editSelLclFnPatName lstDecl (str, ins) =
>	map checkSelFn lstDecl
>	where
>	checkSelFn decl@(FunBind (s, vi) lstMtch) 
>		| (isSel decl "selected") 	
>			= FunBind (s, vi) (map checkMtch lstMtch)
>		| otherwise			
>			= decl
>	checkSelFn decl	= decl
>	checkMtch mtch@(Match a b c d (Just(BDecls e lstDecl))) 
>		| (isSel mtch "selected") 	
>			= Match a b c d (Just (BDecls e (editSelFnPatName lstDecl (str, ins))))
>		| otherwise			
>			= mtch
>	checkMtch mtch = mtch

> editSelFuncExp :: SystemState -> (String, Int) -> SystemState
> editSelFuncExp m@(SystemState f t g ty h) txt =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		mod				 = Module (s, vi) mh mp imp (editSelFnExp m lstDecl txt)			
>	in 	(SystemState f t g{shapeTree = mod} ty h) 

> editSelFnExp m lstDecl (str, ins) =
>	map checkSelFn lstDecl
>	where
>	checkSelFn decl@(FunBind (s, vi) lstMtch) 
>		| (isSel decl "selected") 	
>			= FunBind (s, vi) (map checkMtch lstMtch)
>		| otherwise			
>			= decl
>	checkSelFn decl	= decl
>	checkMtch mtch@(Match a b c rhs@(UnGuardedRhs svi exp) (Just(BDecls e lstDecl))) 
>		| (isSel mtch "selected") =
>			case (getSelectedLclMatch m "selected") of
>				Just 	mtch 	->
>					Match a b c rhs (Just (BDecls e (editSelFnExp m lstDecl (str, ins))))
>				Nothing		->
>					Match a b c (UnGuardedRhs svi (editExp exp)) (Just(BDecls e lstDecl))	
>		| otherwise			
>			= mtch
>	checkMtch mtch@(Match a b c (UnGuardedRhs svi exp) Nothing) 
>		| (isSel mtch "selected") 	
>			= Match a b c (UnGuardedRhs svi (editExp exp)) Nothing
>		| otherwise			
>			= mtch
>	checkMtch mtch = mtch
>	editExp exp@(App svi eOne eTwo) =
>		App svi (editExp eOne) (editExp eTwo)
>	editExp exp@(If svi eOne eTwo eThree) =
>		If svi (editExp eOne) (editExp eTwo) (editExp eThree)
>	editExp exp@(Paren svi eOne) =
>		Paren svi (editExp eOne) 
>	editExp exp@(Var (s,vi) (UnQual svi2 (Ident svi3 prev))) 
>		| (isSel exp "annEditMode") 	
>			= Var (s,vi{annInsertionPoint=ins}) (UnQual svi2 (Ident svi3 $ chkValidExpVarStr prev str))
>		| otherwise			
>			= exp
>	editExp exp@(Var (s,vi) (UnQual svi2 (Symbol svi3 prev))) 
>		| (isSel exp "annEditMode" && (str /= "")) 	
>			= Var (s,vi{annInsertionPoint=ins}) (UnQual svi2 (Symbol svi3 $ chkValidExpVarOp prev str))
>		| otherwise			
>			= exp
>	editExp pat@(Lit (s,vi) (String svi _ _))
>		| (isSel pat "annEditMode") 	
>			= Lit (s,vi{annInsertionPoint=ins}) (String svi str str)
>		| otherwise			
>			= pat
>	editExp pat@(Lit (s,vi) (Int svi prevInt prev))
>		| (isSel pat "annEditMode" && (str /= "")) 	
>			= Lit (s,vi{annInsertionPoint=ins}) (Int svi (chkValidExpIntInt prevInt str) (chkValidExpIntStr prev str))
>		| otherwise			
>			= pat
>	editExp pat@(Lit (s,vi) (Char svi prev _))
>		| (isSel pat "annEditMode" && (str /= "")) 	
>			= Lit (s,vi{annInsertionPoint=ins}) (Char svi (chkValidExpChar prev str) ("" ++ [chkValidExpChar prev str]))
>		| otherwise			
>			= pat
>	editExp pat@(Con (s,vi) (UnQual svi1 (Ident svi2 _)))
>		| (isSel pat "annEditMode") 	
>			= Con (s,vi{annInsertionPoint=ins}) (UnQual svi1 (Ident svi2 str))
>		| otherwise			
>			= pat
>	editExp exp = exp

> deleteNode :: SystemState -> SystemState
> deleteNode (SystemState f t g ty h) =
>	let	Module (s, vi) mh mp imp lstDecl = shapeTree g	
>		mod	= 
>			case (areaOfOperation g) of
>				GblFn 	-> Module (s, vi) mh mp imp (delSelFn lstDecl)	
>				LclFn 	-> Module (s, vi) mh mp imp (delSelLclFn lstDecl)			
>				FnArg 	-> Module (s, vi) mh mp imp (delSelFnPat lstDecl)			
>				LclFnPat-> Module (s, vi) mh mp imp (delSelLclFnPat lstDecl)			
>				FnBody 	-> Module (s, vi) mh mp imp (delSelFnExp lstDecl)					
>	in 	(SystemState f t g{shapeTree = mod} ty h) 

> delSelFn lstDecl = 
>	mapMaybe delFn lstDecl
>	where
>	delFn decl@(FunBind svi lstMtch)
>		| (isAnySel lstMtch "selected") 	
>			= let	lstM = mapMaybe delMtch lstMtch
>			  in 	case lstM of 
>					[] -> Nothing
>					_  -> Just $ FunBind svi lstM
>		| (isSel decl "selected") 	
>			= Nothing
>		| otherwise			
>			= Just $ decl
>	delFn decl	= Just $ decl
>	delMtch mtch@(Match a b c d e)
>		| (isSel mtch "selected") 	
>			= Nothing 
>		| otherwise			
>			= Just $ mtch
>	delMtch mtch	= Just $ mtch

> delSelLclFn lstDecl = 
>	map checkFn lstDecl
>	where
>	checkFn decl@(FunBind svi lstMtch)
>		| (isSel decl "selected") 	
>			= FunBind svi (map checkMtch lstMtch)
>		| otherwise			
>			= decl
>	checkFn decl	= decl
>	checkMtch mtch@(Match a b c d (Just(BDecls e lstDecl)))
>		| (isSel mtch "selected") 	
>			= Match a b c d (Just(BDecls e (delSelFn lstDecl))) 
>		| otherwise			
>			= mtch
>	checkMtch mtch	= mtch

> delSelFnPat lstDecl = 
>	map checkFn lstDecl
>	where
>	checkFn decl@(FunBind svi lstMtch)
>		| (isSel decl "selected") 	
>			= FunBind svi (map checkMtch lstMtch)
>		| otherwise			
>			= decl
>	checkFn decl	= decl
>	checkMtch mtch@(Match a b lstP d e)
>		| (isSel mtch "selected") 	
>			= Match a b (mapMaybe delPat lstP) d e 
>		| otherwise			
>			= mtch
>	checkMtch mtch	= mtch
>	delPat pat
>		| (isSel pat "selected") 	
>			= Nothing 
>		| otherwise			
>			= Just $ pat

> delSelLclFnPat lstDecl = 
>	map checkFn lstDecl
>	where
>	checkFn decl@(FunBind svi lstMtch)
>		| (isSel decl "selected") 	
>			= FunBind svi (map checkMtch lstMtch)
>		| otherwise			
>			= decl
>	checkFn decl	= decl
>	checkMtch mtch@(Match a b c d (Just(BDecls e lstDecl)))
>		| (isSel mtch "selected") 	
>			= Match a b c d (Just(BDecls e (delSelFnPat lstDecl))) 
>		| otherwise			
>			= mtch
>	checkMtch mtch	= mtch

> delSelFnExp lstDecl = 
>	map checkFn lstDecl
>	where
>	checkFn decl@(FunBind svi lstMtch)
>		| (isSel decl "selected") 	
>			= FunBind svi (map checkMtch lstMtch)
>		| otherwise			
>			= decl
>	checkFn decl	= decl
>	checkMtch mtch@(Match a b c (UnGuardedRhs svi exp) (Just(BDecls e lstDecl)))
>		| (isSel mtch "selected") 	
>			= Match a b c (UnGuardedRhs svi (delExp exp)) (Just(BDecls e (delSelFnExp lstDecl))) 
>		| otherwise			
>			= mtch
>	checkMtch mtch@(Match a b c (UnGuardedRhs svi exp) Nothing) 
>		| (isSel mtch "selected") 	
>			= Match a b c (UnGuardedRhs svi (delExp exp)) Nothing
>		| otherwise			
>			= mtch
>	checkMtch mtch	= mtch

>	{-delExp exp@(App svi eOne eTwo) 
>		| (isSel eOne "selected") 	
>			= eTwo 
>		| (isSel eTwo "selected") 	
>			= eOne 
>		| otherwise			
>			= exp
>	delExp exp@(If svi eOne eTwo eThree) =
>		If svi (delExp eOne) (delExp eTwo) (delExp eThree)
>	delExp exp@(Paren svi eOne) =
>		Paren svi (delExp eOne) 
>	delExp exp@(Var svi v) =
>		exp
>	delExp exp	= exp-}
>	delExp exp@(App svi eOne eTwo) 
>		| (isSel eTwo "selected") 	= eOne
>		| (isSel eOne "selected") 	= eTwo
>		| otherwise			= App svi (delExp eOne) (delExp eTwo)
>	delExp exp@(If svi eOne eTwo eThree) =
>		If svi (delExp eOne) (delExp eTwo) (delExp eThree)
>	delExp exp@(Paren svi eOne) =
>		Paren svi (delExp eOne) 
>	delExp exp@(Var _ _) =
>		if 	(isSel exp "selected")
>		then 	Lit sviInit (String sviInit "--FunctionIsNotDefinedYet--" "--FunctionIsNotDefinedYet--")
>		else 	exp
>	delExp exp	= exp

> chkValidModuleName prevStr newStr
>		| (((ord $ head newStr) >= 97) && ((ord $ head newStr) <= 122))
>			= prevStr
>		| otherwise
>			= newStr

> chkValidFuncName prevStr newStr
>		| (newStr == "")
>			= prevStr
>		| (((ord $ head (newStr)) >= 65) && ((ord $ head (newStr)) <= 90))
>			= prevStr
>		{-| isNotAnString newStr
>			= prevStr-}		
>		| otherwise
>			= newStr

> chkValidPatVar prevStr newStr = chkValidFuncName prevStr newStr

> chkValidExpVarStr prevStr newStr = chkValidFuncName prevStr newStr

> chkValidExpVarOp prevStr newStr
>		| (newStr == "")
>			= prevStr
>		| isNotAnOperator newStr
>			= prevStr
>		| otherwise
>			= newStr

> chkValidExpIntStr prevStr newStr
>		| (newStr == "")
>			= prevStr
>		| isNotAnInt newStr
>			= prevStr
>		| otherwise
>			= newStr

> chkValidExpIntInt prevInt newStr
>		| (newStr == "")
>			= prevInt
>		| isNotAnInt newStr
>			= prevInt
>		| otherwise
>			= (read newStr :: Integer)

> chkValidExpChar prevStr newStr
>		| (newStr == "")
>			= prevStr
>		| otherwise
>			= (last newStr)

> isNotAnString newStr =
>	or $ map chkEachChar newStr
>	where
>	chkEachChar ch = ((ord ch < 32) || (ord ch > 47))

> isNotAnInt newStr =
>	or $ map chkEachChar newStr
>	where
>	chkEachChar ch = ((ord ch < 48) || (ord ch > 57))

> isNotAnOperator newStr =
>	or $ map chkEachChar newStr
>	where
>	chkEachChar ch = ((ord ch < 32) || (ord ch > 63))

> --newFunc a = app ((:) (app app app))
> -- ================================================================



> -- ===========================================tests

> -------------Test only-------------------------------------------------------------------
> --(||) :: Int -> Int -> Int
> a ||| b = a + b
 

> mod4 = do
>	ParseOk (mod, _) <- HSE.parseFileWithComments defaultParseMode "./TestHsFile.hs"
>	return mod


