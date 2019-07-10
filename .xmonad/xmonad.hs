-- minimal Ubuntu config file: ~/.xmonad/xmonad.hs
import XMonad

myWorkSpaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myTerminal = "gnome-terminal"

main = xmonad defaultConfig
  { modMask = mod1Mask -- Use left Alt instead of Alt
  , terminal = myTerminal
  , workspaces = myWorkSpaces 
  }
