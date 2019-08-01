-- minimal Ubuntu config file: ~/.xmonad/xmonad.hs
import XMonad
import XMonad.Layout
import XMonad.Layout.Circle
import XMonad.Layout.Column
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
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
  spacing 10 $ gaps [(L, 10), (R, 10)] $
    avoidStruts $ (tall ||| Grid ||| Circle ||| threeCol ||| Full)
  where
    master = 1
    ratioInc = (3/100)
    ratio = (1/2)

    tall = Tall master ratioInc ratio
    cTall = centerMaster tall
    threeCol = ThreeColMid master ratioInc ratio

    cGrid = centerMaster Grid

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
      , ((mod4Mask .|. shiftMask, xK_s), spawn "slock")
      , ((mod4Mask .|. shiftMask, xK_a), spawn "arandr")
      , ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
      , ((mod4Mask .|. shiftMask, xK_e), spawn "emacs")
      , ((controlMask, xK_Print       ), spawn "sleep 0.2; scrot -s")
      ]
