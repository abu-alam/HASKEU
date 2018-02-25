-- ---------------------------------------------------------------------------
--
-- Module : MVC_WX.lhs
-- Author : Abu Alam
--
-- Maintainer : Abu Alam, s0408730@connect.glos.ac.uk
--
-- Purpose : Utility functions for creating wxHaskell events and attributes
-- and adjusting them for the reactive.banana libraray.
-- ---------------------------------------------------------------------------

> module MVC_WX where

> import MVC
> import ModelType
> import FindReplaceUtil

> import qualified Graphics.UI.WX as WX
> import Graphics.UI.WX  hiding (Event, Attr)
> import Graphics.UI.WXCore hiding (View, Event) 
> import Reactive.Banana.WX 
> import Reactive.Banana

> type ViewType = ()

> --Controller Instances
> instance Frameworks t => 
>	VCController (TextCtrl ()) SystemState (String, Int) t where
> instance Frameworks t => 
>	VCController (TextCtrl ()) SystemState () t where
> instance Frameworks t => 
>	VCController (BitmapButton ()) SystemState () t where
> instance Frameworks t => 
>	VCController (BitmapButton ()) SystemState String t where
> instance Frameworks t => 
>	VCController (BitmapButton ()) SystemState (Maybe FilePath, String) t where
> instance Frameworks t => 
>	VCController (BitmapButton ()) SystemState (Maybe FilePath) t where
> instance Frameworks t => 
>	VCController (ScrolledWindow ()) SystemState EventKey t where
> instance Frameworks t => 
>	VCController (ScrolledWindow ()) SystemState EventMouse t where
> instance Frameworks t => 
>	VCController (ScrolledWindow ()) SystemState EventScroll t where
> instance Frameworks t => 
>	VCController (ScrolledWindow ()) SystemState () t where
> instance Frameworks t => 
>	VCController (SingleListBox ()) SystemState Int t where
> instance Frameworks t => 
>	VCController (Menu ()) SystemState (Maybe String) t where
> instance Frameworks t => 
>	VCController (Choice ()) SystemState Int t where
> instance Frameworks t => 
>	VCController (Slider ()) SystemState Int t where

> --View Instances
> instance Frameworks t => 
>	VCView (Frame ()) SystemState String ViewType t where
> 		view = viewWidget
> instance Frameworks t => 
>	VCView (BitmapButton ()) SystemState Bool ViewType t where
> 		view = viewWidget

> instance Frameworks t => 
>	VCView (TextCtrl ()) SystemState String ViewType t where
> 		view = viewWidget
> instance Frameworks t => 
>	VCView (TextCtrl ()) SystemState Int ViewType t where
> 		view = viewWidget
> instance Frameworks t => 
>	VCView (StaticText ()) SystemState String ViewType t where
> 		view = viewWidget
> instance Frameworks t => 
>	VCView (TextCtrl ()) SystemState Bool ViewType t where
> 		view = viewWidget
> instance Frameworks t => 
>	VCView (TextCtrl ()) SystemState Point ViewType t where
> 		view = viewWidget

> instance Frameworks t => 
>	VCView (SingleListBox ()) SystemState [String] ViewType t where
> 		view = viewWidget
> {-instance Frameworks t => 
>	VCView (SingleListBox ()) SystemState (IO [String]) ViewType t where
> 		view = viewWidgetIO-}
> instance Frameworks t => 
>	VCView (SingleListBox ()) SystemState Int ViewType t where
> 		view = viewWidget
> instance Frameworks t => 
>	VCView (ScrolledWindow ()) SystemState (DC () -> Rect -> IO ()) ViewType t where
> 		view = viewWidget 
> instance Frameworks t => 
>	VCView (ScrolledWindow ()) SystemState (IO (Cursor ())) ViewType t where
> 		view = viewWidgetIO
> instance Frameworks t => 
>	VCView (Choice ()) SystemState Int ViewType t where
> 		view = viewWidget 
> instance Frameworks t => 
>	VCView (Slider ()) SystemState Int ViewType t where
> 		view = viewWidget 

> --Wx Widget Events to Wx Banana Events
> evButtonCommand :: (Frameworks t, Reactive w, Commanding w) => w -> Moment t (Event t ())
> evButtonCommand w = do
>	eCommand <- event0 w command
>	return (eCommand)

> evButtonCommandFileOpen :: Frameworks t =>
>     BitmapButton () -> Frame () -> Moment t (Event t (Maybe FilePath, String))
> evButtonCommandFileOpen b w = do
>     addHandler <- liftIONow $ event1ToAddHandler b (event0ToEvent1 command)
>     fromAddHandler
>        	$ mapIO (const $ openPage w) addHandler

> openPage :: Frame () -> IO (Maybe FilePath, String)
> openPage win =
>    do
>        maybePath <- fileOpenDialog win True True "Open file..." [("Haskells (*.hs)",["*.hs"]),
>                                                                  ("Texts (*.txt)", ["*.txt"]),
>                                                                  ("Any file (*.*)",["*.*"])] "" ""
>        case maybePath of
>            Nothing -> return (maybePath, "")
>            Just path -> do
>			fileContents <- readFile path
>			return (maybePath, fileContents)

> evButtonCommandFileSave :: Frameworks t =>
>     BitmapButton () -> Frame () -> TextCtrl() -> Moment t (Event t (Maybe FilePath))
> evButtonCommandFileSave b w t = do
>     addHandler <- liftIONow $ event1ToAddHandler b (event0ToEvent1 command)
>     fromAddHandler
>     		$ mapIO (const $ savePage w t) addHandler


> savePage :: Frame () -> TextCtrl() -> IO (Maybe FilePath)
> savePage w t = do
>     winTitle <- get w text
>     --infoDialog w winTitle winTitle
>     case (winTitle==windowTitle) of
>		True -> savePageAs w t
>		False->
>			do
>			let path = drop (length windowTitle + 3) winTitle
>			textCtrlSaveFile t path
>			return (Just path)

> evButtonCommandFileSaveAs :: Frameworks t =>
>     BitmapButton () -> Frame () -> TextCtrl() -> Moment t (Event t (Maybe FilePath))
> evButtonCommandFileSaveAs b w t = do
>     addHandler <- liftIONow $ event1ToAddHandler b (event0ToEvent1 command)
>     fromAddHandler
>        	$ mapIO (const $ savePageAs w t) addHandler

> savePageAs :: Frame () -> TextCtrl () -> IO (Maybe FilePath)
> savePageAs win txtEditor =
>    do
>        maybePath <- fileSaveDialog win True True "Save file..." [("Haskells (*.hs)",["*.hs"]),
>                                                                  ("Texts (*.txt)", ["*.txt"]),
>                                                                  ("Any file (*.*)",["*.*"])] "" ""
>        case maybePath of
>         	Nothing -> return Nothing
>		Just path ->
>	        	do
>                    	textCtrlSaveFile txtEditor path
>			return maybePath 

> evButtonCommandCut :: Frameworks t =>
>     BitmapButton () -> TextCtrl() -> Moment t (Event t ())
> evButtonCommandCut b t = do
>     addHandler <- liftIONow $ event1ToAddHandler b (event0ToEvent1 command)
>     fromAddHandler
>     		$ mapIO (const $ textCtrlCut t) addHandler

> evButtonCommandCopy :: Frameworks t =>
>     BitmapButton () -> TextCtrl() -> Moment t (Event t ())
> evButtonCommandCopy b t = do
>     addHandler <- liftIONow $ event1ToAddHandler b (event0ToEvent1 command)
>     fromAddHandler
>     		$ mapIO (const $ textCtrlCopy t) addHandler

> evButtonCommandPaste :: Frameworks t =>
>     BitmapButton () -> TextCtrl() -> Moment t (Event t ())
> evButtonCommandPaste b t = do
>     addHandler <- liftIONow $ event1ToAddHandler b (event0ToEvent1 command)
>     fromAddHandler
>     		$ mapIO (const $ textCtrlPaste t) addHandler

> evButtonCommandFind :: Frameworks t =>
>     BitmapButton () -> Frame () -> TextCtrl() -> FindReplaceData () -> Moment t (Event t ())
> evButtonCommandFind b w t fr = do
>     addHandler <- liftIONow $ event1ToAddHandler b (event0ToEvent1 command)
>     let guiCtx = GUICtx w t fr 
>     fromAddHandler
>        	$ mapIO (const $ justFind guiCtx) addHandler

> evButtonCommandReplace :: Frameworks t =>
>     BitmapButton () -> Frame () -> TextCtrl() -> FindReplaceData () -> Moment t (Event t ())
> evButtonCommandReplace b w t fr = do
>     addHandler <- liftIONow $ event1ToAddHandler b (event0ToEvent1 command)
>     let guiCtx = GUICtx w t fr 
>     fromAddHandler
>         $ mapIO (const $ findReplace guiCtx) addHandler

> evTextChanged :: Frameworks t =>
>     TextCtrl w -> Moment t (Event t (String, Int)) -- Text Editor String, Insertion Point, Textua Errs, Gra Errs
> evTextChanged w = do
>     addHandler <- liftIONow $ event1ToAddHandler w (event0ToEvent1 onText)
>     fromAddHandler
>         $ filterAddHandler (const $ textCtrlIsModified w)
>         $ mapIO (const $ (liftA2 (,)) (get w text) (get w insertionPoint) ) addHandler

> onText :: WX.Event (Control a) (IO ())
> onText = WX.newEvent "onText" controlGetOnText controlOnText

> evTextKBUp :: Frameworks t =>
>     TextCtrl w -> Moment t (Event t (String, Int)) -- Text Editor String, Insertion Point, Textua Errs, Gra Errs
> evTextKBUp txt = do
>	eKeyboardUp <- event1 txt keyboardUp
>	bText <- (behavior txt text)
>	bInsertionPoint <- (behavior txt insertionPoint) 
>	let eTextInsPt = (uncurry (liftA2 (,)) (bText, bInsertionPoint)) <@ eKeyboardUp
>	return eTextInsPt

> evSelChoice :: Frameworks t => 
>    Choice () -> Moment t (Event t Int)
> evSelChoice w = do
>    addHandler <- liftIONow $ event1ToAddHandler w (event0ToEvent1 select)
>    fromAddHandler $ mapIO (const $ get w selection) addHandler

> evSelListBox :: Frameworks t => -- From wx Banana
>    SingleListBox b -> Moment t (Event t Int)
> evSelListBox w = do
>    liftIONow $ fixSelectionEvent w
>    addHandler <- liftIONow $ event1ToAddHandler w (event0ToEvent1 select)
>    fromAddHandler $ mapIO (const $ get w selection) addHandler

> -- Fix @select@ event not being fired when items are *un*selected. -- From wx Banana
> fixSelectionEvent listbox =
>    set listbox [ on unclick := handler ]
>    where
>    handler _ = do
>        propagateEvent
>        s <- get listbox selection
>        when (s == -1) $ (get listbox (on select)) >>= id

> evMouseMove :: (Frameworks t, Reactive w) => w -> Moment t (Event t EventMouse)
> evMouseMove w = do
>	eMouse <- event1 w mouse
>	let eMouseMove = filterE (\event -> (isMouseMove event)) $ eMouse
>	return (eMouseMove)

> evMouseLeftUp :: (Frameworks t, Reactive w) => w -> Moment t (Event t EventMouse)
> evMouseLeftUp w = do
>	eMouse <- event1 w mouse
>	let eMouseLeftUp = filterE (\event -> (isMouseLeftUp event)) $ eMouse
>	return (eMouseLeftUp)

> evMouseLeftDown :: (Frameworks t, Reactive w) => w -> Moment t (Event t EventMouse)
> evMouseLeftDown w = do
>	eMouse <- event1 w mouse
>	let eMouseLeftDown = filterE (\event -> (isMouseLeftDown event)) $ eMouse
>	return (eMouseLeftDown)

> evReturnKeyPressed :: (Frameworks t, Reactive w) => w -> Moment t (Event t EventKey)
> evReturnKeyPressed w = do
>	eKB <- event1 w keyboard
>	let eKBReturn = filterE (\event -> isKBEnterPressed  event) $ eKB
>	return (eKBReturn)

> evSliderCommand :: Frameworks t => Slider () -> Moment t (Event t Int)
> evSliderCommand w = do
>    	addHandler <- liftIONow $ event1ToAddHandler w (event0ToEvent1 command)
>    	fromAddHandler
>        	$ mapIO (const $ get w selection) addHandler

> {-evMenuOpenFile :: Frameworks t => Menu () -> Moment t (Event t (Maybe String))
> evMenuOpenFile mnu = do
>	mitOpen <- liftIONow $ menuFindItem mnu wxID_OPEN
>	w <- liftIONow $ menuGetInvokingWindow mnu
>    	addHandler <- liftIONow $ event1ToAddHandler mitOpen (event0ToEvent1 command)
>    	fromAddHandler
>        	$ mapIO (const $ fileOpenDialog w True True "Open file..." [] "" "") addHandler

> evMenuOpenFile1 :: Frameworks t => MenuItem () -> Frame () -> Moment t (Event t (Maybe String))
> evMenuOpenFile1 mit w = do
>	e <- event0 mit command
>	bText <- fromPoll $ fileOpenDialog w True True "Open file..." [] "" ""
>	return (bText<@ e)-}

>	---- Fix scrolled window @repaint@ event not being fired when Model change
> evTxtChgRepaintSw :: Frameworks t =>
>     TextCtrl w -> [ScrolledWindow ()] -> Moment t (Event t ())
> evTxtChgRepaintSw t lstSw = do
>     addHandler <- liftIONow $ event1ToAddHandler t (event0ToEvent1 onText)
>     fromAddHandler
>         $ filterAddHandler (const $ textCtrlIsModified t)
>         $ mapIO (const $ (swRepaints lstSw)) addHandler

> evBtnCommRepaintSw :: Frameworks t =>
>     BitmapButton () -> [ScrolledWindow ()] -> Moment t (Event t ())
> evBtnCommRepaintSw b lstSw = do
>     addHandler <- liftIONow $ event1ToAddHandler b (event0ToEvent1 command)
>     fromAddHandler
>         $ mapIO (const $ (swRepaints lstSw)) addHandler

> {-evSwMouseRepaintSw :: Frameworks t =>
>     ScrolledWindow () -> Moment t (Event t ())
> evSwMouseRepaintSw w = do
>     addHandler <- liftIONow $ event1ToAddHandler w mouse
>     fromAddHandler
>         $ mapIO (const $ (swRepaint w)) addHandler-}
>         -- $ mapIO (const $ (swRepaints lstSw) addHandler -- we may need this 

> evSwMouseRepaintSwAll :: Frameworks t =>
>     ScrolledWindow () -> [ScrolledWindow ()] -> Moment t (Event t ())
> evSwMouseRepaintSwAll w lstSw = do
>     addHandler <- liftIONow $ event1ToAddHandler w mouse
>     fromAddHandler
>         -- $ mapIO (const $ (swRepaint w)) addHandler
>         $ mapIO (const $ (swRepaints lstSw)) addHandler -- we may need this

> swRepaints lstW = 
>	sequence_ (map swRepaint lstW)

> swRepaint w = 
>     do 
>     propagateEvent 
>     repaint w

> evSwScroll :: Frameworks t =>
>     ScrolledWindow () -> Moment t (Event t EventScroll)
> evSwScroll sw = do
> 	eScroll <- event1 sw windowScroll
>	return (eScroll)

> --Set wx widget attribute in a reactive way
> viewWidget
>  :: (WxAttributes w a, Frameworks t) 	=> (w, AttrName) 
>					-> Model t m 
>					-> InterfaceLogic m a 
>					-> View t ()
> viewWidget (w, a) 		
>	= \m il -> sink w [(getWxAttr a) :== (il <$> m)]

> viewWidgetIO
>  :: (WxAttributes w a, Frameworks t) 	=> (w, AttrName) 
>					-> Model t m 
>					-> InterfaceLogic m (IO a)
>					-> View t ()
> viewWidgetIO (w, a) m il = 
>	 do
>        x <- initial m
>	 val <- liftIONow $ il x
>        liftIOLater $ set w [(getWxAttr a) := val]
>        e <- changes m
>        reactimate $ applyChanges <$> e
>	 where
>	 applyChanges val = 
>	 	do
>		v <- il val
>		set w [(getWxAttr a) := v]


> --mapping attribute name string to a waHaskell attribute type
> class WxAttributes w a where
>	getWxAttr :: AttrName -> WX.Attr w a

> instance WxAttributes (TextCtrl ()) String where
>	getWxAttr "text" = text
> instance WxAttributes (TextCtrl ()) Int where
>	getWxAttr "insertionPoint" = insertionPoint
> instance WxAttributes (TextCtrl ()) Point where
>	getWxAttr "position" = position
> instance WxAttributes (TextCtrl ()) Bool where
>	getWxAttr "visible" = visible
> instance WxAttributes (StaticText ()) String where
>	getWxAttr "text" = text
> instance WxAttributes (SingleListBox ()) [String] where
>	getWxAttr "items" = items
> instance WxAttributes (Choice ()) Int where
>	getWxAttr "selection" = selection
> instance WxAttributes (SingleListBox ()) Int where
>	getWxAttr "selection" = selection
> instance WxAttributes (Slider ()) Int where
>	getWxAttr "selection" = selection
> instance WxAttributes (ScrolledWindow ()) (DC () -> Rect -> IO ()) where
>	getWxAttr "on paint" = on paint
> instance WxAttributes (ScrolledWindow ()) (Cursor ()) where
>	getWxAttr "mouseCursor" = mouseCursor
> instance WxAttributes (Frame ()) String where
>	getWxAttr "text" = text
> instance WxAttributes (BitmapButton ()) Bool where
>	getWxAttr "enabled" = enabled

> -- new wx attributes/ Events

> insertionPoint :: WX.Attr (TextCtrl a) Int
> insertionPoint = 	newAttr "insertionPoint" 
>			textCtrlGetInsertionPoint (textCtrlSetInsertionPoint)

> mouseCursor :: WX.Attr (Window a) (Cursor ())
> mouseCursor = 	newAttr "mouseCursor" windowGetCursor 
>			(\w c -> do 	windowSetCursor w c 
>					return ())

> -- Event Filters
> isKBEnterPressed :: EventKey -> Bool
> isKBEnterPressed (EventKey KeyReturn _ _) = True
> isKBEnterPressed _                   = False

> isMouseLeftUp :: EventMouse -> Bool
> isMouseLeftUp (MouseLeftUp _ _) = True
> isMouseLeftUp _                   = False

> isMouseLeftDown :: EventMouse -> Bool
> isMouseLeftDown (MouseLeftDown _ _) = True
> isMouseLeftDown _                   = False

> isMouseLeftDrag :: EventMouse -> Bool
> isMouseLeftDrag (MouseLeftDrag _ _) = True
> isMouseLeftDrag _                   = False

> isMouseMove :: EventMouse -> Bool
> isMouseMove (MouseMotion _ _) = True
> isMouseMove _                 = False

> windowScroll :: WX.Event (Window a) (EventScroll -> IO ())
> windowScroll = WX.newEvent "windowScroll" windowGetOnScroll windowOnScroll

> keyboardUp :: WX.Event (Window a) (EventKey -> IO ())
> keyboardUp  = WX.newEvent "keyboardUp" windowGetOnKeyUp (windowOnKeyUp)
> keyboardDown :: WX.Event (Window a) (EventKey -> IO ())
> keyboardDown  = WX.newEvent "keyboardDown" windowGetOnKeyDown (windowOnKeyDown)



