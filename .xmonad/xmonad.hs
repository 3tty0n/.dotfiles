-- minimal Ubuntu config file: ~/.xmonad/xmonad.hs
import XMonad
import XMonad.Actions.SinkAll
import XMonad.Config.Kde
import XMonad.Layout
import XMonad.Layout.Circle
import XMonad.Layout.Cross
import XMonad.Layout.Column
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Maximize
import XMonad.Layout.Magnifier
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

myMod = mod4Mask
myTerminal = "tilda"
myBorderWidth = 3
myNormalBorderColor = "#dddddd"
myFocusedBorderColor = "#3399ff"

-- Layout management
myLayoutHook =
  doSpacing $ avoidStruts $ (tall ||| Grid ||| threeCol ||| magnify Circle ||| Full)
  where
    master = 1
    ratioInc = (3/100)
    ratio = (1/2)
    magRatio = 1.1

    tall = Tall master ratioInc ratio
    threeCol = ThreeColMid master ratioInc ratio
    cGrid = centerMaster Grid
    column = Column (16/10)
    doSpacing x = spacing 5 $ gaps [(L,5), (R,5)] $ x
    magnify = (\x -> magnifiercz magRatio x)

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myKeys =
  [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
  , ((mod4Mask .|. shiftMask, xK_s), spawn "slock")
  , ((mod4Mask .|. shiftMask, xK_a), spawn "arandr")
  , ((mod4Mask .|. shiftMask, xK_e), spawn "emacs")
  , ((mod4Mask .|. shiftMask, xK_b), spawn "google-chrome-stable")
  , ((mod4Mask .|. shiftMask, xK_l), spawn "~/.screenlayout/laptop.sh")
  , ((mod4Mask .|. shiftMask, xK_f), sinkAll)
  -- , ((mod4Mask .|. shiftMask, xK_q), spawn "xscreensaver-command -lock; xset dpms force off")
  , ((controlMask, xK_Print       ), spawn "sleep 0.2; scrot -s")
  , ((mod4Mask, xK_p), spawn "rofi -show run")
  , ((mod4Mask .|. controlMask              , xK_equal), sendMessage MagnifyMore)
  , ((mod4Mask .|. controlMask              , xK_minus), sendMessage MagnifyLess)
  , ((mod4Mask .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
  , ((mod4Mask .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
  , ((mod4Mask .|. controlMask              , xK_m    ), sendMessage Toggle     )
  ]

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
      , borderWidth = myBorderWidth
      , focusedBorderColor = myFocusedBorderColor
      -- , mouseBindings = myMouseBindings
      } `additionalKeys` myKeys
