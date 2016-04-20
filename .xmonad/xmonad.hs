{-# LANGUAGE FlexibleContexts #-}
import XMonad
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders
import Data.Monoid
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import System.Exit
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.IfMin

main :: IO ()
main = xmonad =<< myXmobar myConfig

myConfig = defaultConfig
    { modMask = mod3Mask
    , terminal = "exec urxvtcd -e sh -c 'session=$(tmux ls | grep -v -m 1 \"(attached)$\" | sed \"s/^\\([0-9]*\\):.*$/\\1/\"); if [ \"$session\" = \"\" ]; then exec tmux new-session ; else exec tmux attach-session -t $session ; fi'"
    , manageHook = myManageHook <+> manageHook defaultConfig
    , layoutHook = smartBorders $ avoidStruts $ myLayoutHook
    , focusFollowsMouse = False
    , clickJustFocuses = False }
        `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command --lock")
    , ("M-p", spawn "exec rofi -glob -modi run,ssh -ssh-client rtmux -show run")
    , ("M-S-p", spawn "exec gmrun")
    , ("M-S-q", spawn "dmenu-logout")
    , ("M-C-S-q", io exitSuccess)
    , ("M-<F5>", spawn "scrot --exec \"notify-send 'Screenshot saved' '\\$n'\" \"$HOME/cloud/screens/%Y-%m-%d_%H-%M-%S.png\"")
    , ("M-<F6>", spawn "if [ $(xbacklight | sed 's/\\..*$//') -ge 10 ] ; then xbacklight -dec 10 ; fi")
    , ("M-<F7>", spawn "xbacklight -inc 10")
    , ("M-<F8>", spawn "amixer set Master toggle")
    , ("M-<F9>", spawn "amixer set Master 10%-")
    , ("M-<F10>", spawn "amixer set Master 10%+")
    ]

myLayoutHook = ifMin 3 min3 $ ifMin 2 min2 min0 where
    min0 = Full
    min2 = tiled ||| Mirror tiled ||| min0
    min3 = ThreeColMid 1 (3/100) (1/2) ||| min2
    tiled = Tall 1 (3/100) (1/2)

myManageHook = composeAll
    [ className =? "Xfrun4" --> doFloat
    , className =? "mpv" --> doFloat
    , className =? "Pinentry" --> doIgnore ]

myXmobar :: LayoutClass l Window
         => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myXmobar = statusBar xmobarCommand xmobarPP toggleStrutsKey where
    xmobarCommand = "if [ \"$(hostname)\" = 'kevin-laptop' ]; then exec xmobar -t '" ++ lTemplate ++ "' ; else exec xmobar -t '" ++ dTemplate ++ "' ; fi"
    dTemplate = "%StdinReader% }{ %dynnetwork% | %memory% * %swap% | %cpu% %coretemp% | %default:Master%| %EBOS% | <fc=#ee9a00>%date%</fc>"
    lTemplate = "%StdinReader% }{ %3gmonitor% %wlan0wi% | %dynnetwork% | %memory% * %swap% | %cpu% %coretemp% | %battery% | %default:Master%| %EBOS% | <fc=#ee9a00>%date%</fc>"
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
