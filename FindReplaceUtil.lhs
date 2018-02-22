> module FindReplaceUtil where

> import Graphics.UI.WX
> import Graphics.UI.WXCore
> import Data.Bits
> import Data.Char (toLower)
> import Data.List

> -- | /FRFlags/ represents what the user choose in the FindReplaceDialog
> data FRFlags = FRFlags {frfGoingDown :: Bool,
>                         frfMatchCase :: Bool,
>                         frfWholeWord :: Bool,
>                         frfWrapSearch :: Bool}
>     deriving (Eq, Show)

> data GUIContext = GUICtx { guiWin    :: Frame (),
>                            guiEditor :: TextCtrl (),
>                            guiSearch :: FindReplaceData () -- ^ Needed to hold the what the user is looking for
>                            }

> justFind guiCtx = openFindDialog guiCtx "Find..." dialogDefaultStyle

> justFindNext guiCtx@GUICtx{guiSearch = search} =
>     do
>         -- We get the current search parameters
>         curFlags <- findReplaceDataGetFlags search
>         -- We set the finding direction to down
>         findReplaceDataSetFlags search $ curFlags .|. wxFR_DOWN
>         -- and we proceed with the search
>         findNextButton guiCtx

> justFindPrev guiCtx@GUICtx{guiSearch = search} =
>     do
>         -- We get the current search parameters
>         curFlags <- findReplaceDataGetFlags search
>         -- We set the finding direction to down
>         findReplaceDataSetFlags search $ curFlags .&. complement wxFR_DOWN
>         -- and we proceed with the search
>         findNextButton guiCtx

> -- We open a FindReplaceDialog with replace style
> findReplace guiCtx = 
>	openFindDialog guiCtx "Find and Replace..." $ dialogDefaultStyle .|. wxFR_REPLACEDIALOG

> -- | Auxiliary function to build a /FRFlags/
> buildFRFlags :: Bool    -- ^ Wrap the Search?
>                 -> Int  -- ^ BitMask for Direction, Match Case and Whole Word flags
>                 -> IO FRFlags
> buildFRFlags w x = return FRFlags {frfGoingDown = (x .&. wxFR_DOWN) /= 0,
>                                    frfMatchCase = (x .&. wxFR_MATCHCASE) /= 0,
>                                    frfWholeWord = (x .&. wxFR_WHOLEWORD) /= 0,
>                                    frfWrapSearch = w}

> -- | Opens a FindReplace Dialog
> openFindDialog :: GUIContext    -- ^ The current GUIContext
>                   -> String     -- ^ The title of the dialog
>                   -> Int        -- ^ The style of the dialog
>                   -> IO ()
> openFindDialog guiCtx@GUICtx{guiWin = win,
>                              guiSearch = search} title dlgStyle =
>     do
>         -- First we must create a dialog with the search parameters that we
>         -- already have.  The dialog itself is going to modify them according
>         -- to the user selections
>         frdialog <- findReplaceDialogCreate win search title $ dlgStyle + wxFR_NOWHOLEWORD
>         -- One of the weirdest functions on wxHaskell is windowOnEvent.
>         -- I did not really understand what are the parameters for exactly, but
>         -- if we use it this way, we manage to get a certain event with id k to
>         -- fire the function f... :)
>         let winSet k f = let hnd _ = f guiCtx >> propagateEvent
>                           in windowOnEvent frdialog [k] hnd hnd
>         -- Using that mavic trick, we associate our functions with the button
>         -- pressing events in the dialog...
>         winSet wxEVT_COMMAND_FIND findNextButton
>         winSet wxEVT_COMMAND_FIND_NEXT findNextButton
>         winSet wxEVT_COMMAND_FIND_REPLACE findReplaceButton
>         winSet wxEVT_COMMAND_FIND_REPLACE_ALL findReplaceAllButton
>         -- And... it's showtime!!
>         set frdialog [visible := True]

> -- These 3 functions handle the button events in the dialog but also handle the
> -- menuitems when the dialog is not there
> findNextButton, findReplaceButton, findReplaceAllButton :: GUIContext -> IO ()
> findNextButton guiCtx@GUICtx{guiEditor= editor,
>                              guiWin   = win,
>                              guiSearch= search} =
>     do
>         -- We check what the user is trying to find
>         s <- findReplaceDataGetFindString search
>         -- We parse it, assuming that the user wants to wrap its search
>         fs <- findReplaceDataGetFlags search >>= buildFRFlags True
>         -- We try to find a match in the text
>         mip <- findMatch s fs editor
>         case mip of
>             Nothing -> -- If there's no match, we inform that to the user
>                 infoDialog win "Find Results" $ s ++ " not found."
>             Just ip -> -- If there's a match, we select that text
>                 do
>                     textCtrlSetInsertionPoint editor ip
>                     textCtrlSetSelection editor ip (length s + ip)
                

> findReplaceButton guiCtx@GUICtx{guiEditor   = editor,
>                                 guiWin      = win,
>                                 guiSearch   = search} =
>     do
>         -- We check what the user is trying to find
>         s <- findReplaceDataGetFindString search
>         -- and what is he wanting to replace it with
>         r <- findReplaceDataGetReplaceString search
>         -- We parse it, assuming that the user wants to wrap its search
>         fs <- findReplaceDataGetFlags search >>= buildFRFlags True
>         -- We try to find a match in the text
>         mip <- findMatch s fs editor
>         case mip of
>             Nothing -> -- If there's no match, we inform that to the user
>                 infoDialog win "Find Results" $ s ++ " not found."
>             Just ip ->
>                 do -- If there's a match, we replace that text
>                     textCtrlReplace editor ip (length s + ip) r
>                     -- select the result
>		      propagateEvent
>		      txt <- get editor text
>		      textCtrlSetValue editor txt
>                     textCtrlSetInsertionPoint editor ip
>                     textCtrlSetSelection editor ip (length r + ip)
        
> findReplaceAllButton guiCtx@GUICtx{guiEditor = editor,
>                                    guiSearch = search} =
>     do
>         -- We check what the user is trying to find
>         s <- findReplaceDataGetFindString search
>         -- and what is he wanting to replace it with
>         r <- findReplaceDataGetReplaceString search
>         -- We parse it, assuming that the user wants to wrap its search
>         -- Note that we're NOT wrapping our search, to avoid infinite looping
>         fs <- findReplaceDataGetFlags search >>= buildFRFlags False
>         -- We start at the bevinning of the text
>         textCtrlSetInsertionPoint editor 0
>         -- And we go through the text replacing s by r until there's nothing
>         --  more to replace
>         replaceAllIn s r fs editor
>	  propagateEvent
>	  txt <- get editor text
>	  textCtrlSetValue editor txt
>     where replaceAllIn s r fs editor =
>             do
>                 mip <- findMatch s fs editor
>                 case mip of
>                     Nothing ->
>                         return () -- we're done here
>                     Just ip ->
>                         do
>                             textCtrlReplace editor ip (length s + ip) r
>                             textCtrlSetInsertionPoint editor $ length r + ip
>                             replaceAllIn s r fs editor -- we look for the next match

> -- | Tries to find a string in a text control
> findMatch :: String -- ^ The string to find
>              -> FRFlags -- ^ The flags to know how to look for it
>              -> TextCtrl () -- ^ The textControl
>              -> IO (Maybe Int) -- ^ Nothing or Just the position of the first match
> findMatch query flags editor =
>     do
>         -- We get the current text
>         txt <- get editor text
>         -- and the insertion point (that's where the search bevins)
>         ip <- textCtrlGetInsertionPoint editor
>         -- If we're not required to match the case we move everything to lower
>         let (substring, string) = if frfMatchCase flags
>                                     then (query, txt)
>                                     else (map toLower query, map toLower txt)
>             -- we choose what function to use depending on the direction
>             funct = if frfGoingDown flags
>                         then nextMatch (ip + 1)
>                         else prevMatch ip
>             (mip, wrapped) = funct substring string
>         -- if it had to wrap around and that was 'forbbiden', then the match didn't happen
>         -- otherwise, the result is valid
>         return $ if (not $ frfWrapSearch flags) && wrapped
>                     then Nothing
>                     else mip

> -- These functions try to find a string contained in another
> prevMatch, nextMatch :: Int -- ^ Starting point
>                         -> String -- ^ What to find
>                         -> String -- ^ Where to find it
>                         -> (Maybe Int, Bool) -- ^ (Nothing or Just the point where it was found, It needed to wrap around?)
> prevMatch _ [] _ = (Nothing, True) -- When looking for nothing, that's what you get
> prevMatch from substring string | length string < from || from <= 0 = prevMatch (length string) substring string
>                                 | otherwise =
>                                         case nextMatch (fromBack from) (reverse substring) (reverse string) of
>                                             (Nothing, wrapped) -> (Nothing, wrapped)
>                                             (Just ri, wrapped) -> (Just $ fromBack (ri + length substring), wrapped)
>     where fromBack x = length string - x

> nextMatch _ [] _ = (Nothing, True) -- When looking for nothing, that's what you get
> nextMatch from substring string | length substring > length string = (Nothing, True)
>                                 | length string <= from = nextMatch 0 substring string
>                                 | otherwise =
>                                         let after = drop from string
>                                             before = take (from + length substring) string
>                                             aIndex = indexOf substring after
>                                             bIndex = indexOf substring before
>                                          in case aIndex of
>                                                 Just ai ->
>                                                     (Just $ from + ai,  False)
>                                                 Nothing ->
>                                                     case bIndex of
>                                                         Nothing -> (Nothing, True)
>                                                         Just bi -> (Just bi, True)
    
> indexOf :: String -> String -> Maybe Int
> indexOf substring string = findIndex (isPrefixOf substring) $ tails string
