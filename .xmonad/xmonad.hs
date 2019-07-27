-- minimal Ubuntu config file: ~/.xmonad/xmonad.hs
import XMonad
import XMonad.Layout
import XMonad.Layout.Circle
import XMonad.Layout.Column
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

myMod = mod4Mask

myTerminal = "gnome-terminal"

-- Layout management
myLayoutHook =
  spacing 5 $ gaps [(L, 10), (R, 10)] $
    avoidStruts $ (tall ||| threeCol ||| Full)
  where
    master = 1
    ratioInc = (3/100)
    ratio = (1/2)

    tall = Tall master ratioInc ratio
    threeCol = ThreeColMid master ratioInc ratio

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ def
      { manageHook = manageDocks <+> manageHook def
      , layoutHook = myLayoutHook
      , handleEventHook = handleEventHook def <+> docksEventHook
      , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" "" . shorten 50
                  }
      , modMask = myMod
      , terminal = myTerminal
      } `additionalKeys`
      [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
      , ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
      , ((mod4Mask .|. shiftMask, xK_e), spawn "emacs")
      , ((controlMask, xK_Print       ), spawn "sleep 0.2; scrot -s")
      ]
