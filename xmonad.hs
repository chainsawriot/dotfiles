import XMonad
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.ToggleLayouts
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiColumns
-- import XMonad.Actions.MouseResize

myStartupHook :: X ()
myStartupHook = do
    spawn "xrandr --output HDMI-1 --same-as eDP-1"
    spawn "xrandr --output eDP-1 --off"
    spawn "xss-lock -- i3lock -c 000000 &"

myLayoutHook = avoidStruts $ mcol ||| Full
  where
    mcol = multiCol [1] 1 0.01 (-0.5)
     -- tiled   = Tall nmaster delta ratio
     -- nmaster = 1
     -- ratio   = 1/2
     -- delta   = 3/100

myConfig = def {
      modMask = mod4Mask,
      borderWidth = 6,
      focusedBorderColor = "#61afef",
      startupHook = myStartupHook,
      layoutHook = myLayoutHook,
      focusFollowsMouse = False
      -- logHook = dynamicLogWithPP $ xmobarPP {
      --   ppOutput = hPutStrLn xmproc
      --   , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
      --   , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
      --   , ppSep = "   "
      --   }
    }
  `additionalKeysP`
    [
      ("<Print>", unGrab *> spawn "gnome-screenshot --area"),
      ("C-. e", runOrRaise "emacs" (className =? "Emacs")),
      --("C-. e", runOrRaise "emacsclient -c" (className =? "emacs")),
      ("C-. ]", spawn "rofi -modi drun -show drun"),
      -- ("C-. [", runOrRaise "thunar" (className =? "thunar")),
      ("C-. b"  , runOrRaise "firefox" (className =? "firefox")),
      ("C-. c"  , runOrRaise "gnome-terminal" (className =? "Gnome-terminal")),
      ("C-. s"  , runOrRaise "spotify" (className =? "Spotify")),
      ("<F12>", spawn "xrandr --output eDP-1 --auto")
    ]

main :: IO ()
main = do
--  xmproc <- spawnPipe "xmobar -x 0 ~/.xmonad/xmobarrc"
  xmonad $ myConfig
