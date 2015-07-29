import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders

main = xmonad $ xfceConfig
    { modMask = mod4Mask
    , terminal = "xfce4-terminal"
    , manageHook = myManageHook <+> manageHook xfceConfig
    , layoutHook = smartBorders $ layoutHook xfceConfig
    , focusFollowsMouse = False }
        `additionalKeysP`
    [ ("M-S-z", spawn "xflock4") ]

myManageHook = composeAll
    [ className =? "Xfce4-notifyd" --> doIgnore
    , className =? "Xfrun4" --> doFloat
    , className =? "mpv" --> doFloat
    , className =? "Wrapper" --> doFloat
    , className =? "Pinentry" --> doIgnore ]
