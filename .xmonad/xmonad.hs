-- minimal Ubuntu config file: ~/.xmonad/xmonad.hs
import XMonad
import XMonad.Layout
import XMonad.Layout.Circle
import XMonad.Layout.Column
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- Layout management
-- defaultLayoutHook = avoidStruts $ layoutHook defaultConfig 
myLayoutHook =
  avoidStruts $ (tall ||| threeCol ||| Full)
  where
    master = 1
    ratioInc = (3/100)
    ratio = (1/2)

    tall = Tall master ratioInc ratio
    threeCol = ThreeColMid master ratioInc ratio

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
