import XMonad
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties
import Data.Monoid
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import System.Exit

main :: IO ()
main = xmonad =<< xmobar myConfig

myConfig = defaultConfig
    { modMask = mod3Mask
    , terminal = "exec xfce4-terminal -x sh -c 'TERM=xterm-256color exec tmux'"
    , manageHook = myManageHook <+> manageHook defaultConfig
    , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
    , focusFollowsMouse = False
    , clickJustFocuses = False }
        `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command --lock")
    , ("M-p", spawn "exec gmrun")
    , ("M-S-p", spawn "xfrun4")
    , ("M-S-q", spawn "dmenu-logout")
    , ("M-C-S-q", io (exitWith ExitSuccess))
    , ("M-<F5>", spawn "scrot \"$HOME/cloud/screens/%Y-%m-%d_%H-%M-%S.png\"")
    , ("M-<F6>", spawn "if [ $(xbacklight | sed 's/\\..*$//') -ge 10 ] ; then xbacklight -dec 10 ; fi")
    , ("M-<F7>", spawn "xbacklight -inc 10")
    , ("M-<F8>", spawn "amixer set Master toggle")
    , ("M-<F9>", spawn "amixer set Master 10%-")
    , ("M-<F10>", spawn "amixer set Master 10%+")
    ]

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "Xfrun4" --> doFloat
    , className =? "mpv" --> doFloat
    , className =? "Pinentry" --> doIgnore ]
