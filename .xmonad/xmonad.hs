{-# LANGUAGE FlexibleContexts #-}
import XMonad
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders
import Data.Monoid
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import System.Exit
import XMonad.Layout.LayoutModifier

main :: IO ()
main = xmonad =<< myXmobar myConfig

myConfig = defaultConfig
    { modMask = mod3Mask
    , terminal = "exec urxvtcd -e sh -c 'session=$(tmux ls | grep -v -m 1 \"(attached)$\" | sed \"s/^\\([0-9]*\\):.*$/\\1/\"); if [ \"$session\" = \"\" ]; then exec tmux new-session ; else exec tmux attach-session -t $session ; fi'"
    , manageHook = myManageHook <+> manageHook defaultConfig
    , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
    , focusFollowsMouse = False
    , clickJustFocuses = False }
        `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command --lock")
    , ("M-p", spawn "exec gmrun")
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

myXmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar conf = statusBar xmobarCommand xmobarPP toggleStrutsKey conf where
    xmobarCommand = "if [ \"$(hostname)\" = 'kevin-laptop' ]; then exec xmobar -t '" ++ lTemplate ++ "' ; else exec xmobar -t '" ++ dTemplate ++ "' ; fi"
    dTemplate = "%StdinReader% }{ %vpnactivated.sh% %dynnetwork% | %memory% * %swap% | %cpu% %coretemp% | %default:Master%| %EBOS% | <fc=#ee9a00>%date%</fc>"
    lTemplate = "%StdinReader% }{ %batmonitor% %3gmonitor% %vpnactivated.sh% %wlan0wi% | %dynnetwork% | %memory% * %swap% | %cpu% %coretemp% | %battery% | %default:Master%| %EBOS% | <fc=#ee9a00>%date%</fc>"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
