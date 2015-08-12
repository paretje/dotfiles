import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties
import Data.Monoid

main :: IO ()
main = xmonad $ xfceConfig
    { modMask = mod4Mask
    , terminal = "xfce4-terminal"
    , manageHook = hideNotifications <+> myManageHook <+> manageHook xfceConfig
    , layoutHook = smartBorders $ layoutHook xfceConfig
    , focusFollowsMouse = False
    , clickJustFocuses = False }
        `additionalKeysP`
    [ ("M-S-z", spawn "xflock4") ]

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "Xfce4-notifyd" --> doIgnore
    , className =? "Xfrun4" --> doFloat
    , className =? "mpv" --> doFloat
    , className =? "Wrapper" --> doFloat
    , className =? "Pinentry" --> doIgnore ]

hideNotifications :: Query (Endo WindowSet)
hideNotifications = className =? "Xfce4-notifyd" <&&> (focusedHasClassName "mpv" <||> focusedHasClassName "xbmc.bin") --> doKill

focusedHasClassName :: String -> Query Bool
focusedHasClassName cn = liftX $ focusedHasProperty $ ClassName cn

doKill :: ManageHook
doKill = ask >>= \w -> liftX (killWindow w) >> doF (W.delete w)
