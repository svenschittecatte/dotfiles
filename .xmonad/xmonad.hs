import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ def
    { layoutHook=avoidStruts $ myLayoutHook
    , manageHook=myManageHook <+> manageDocks
    , workspaces=myWorkspaces
    , startupHook = myStartupHook
    , terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = myBorderWidth
    , logHook     = myLogHook xmproc
    } `additionalKeys`
    [ ((myModMask, xK_w), spawn "google-chrome-stable")
    , ((myModMask, xK_c), spawn "find $HOME/code/ -mindepth 2 -maxdepth 2 | dmenu | xargs neovide")
    ]

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace workspace program = do
                                      spawn program
                                      windows $ W.greedyView workspace

myTerminal    = "alacritty"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 3
myWorkspaces = [" \xe62b  dev", " \xf268  www", " \xf303  sys", " \xf718  doc", " \xf1d7  com", " \xf1bc  mus", " \xfa7b  vid"]
myStartupHook = do
    setWMName "LG3D"
    spawn "teams"
    spawn "spotify"
myLayoutHook = spacing 4 $ (Tall 1 (3/100) (1/2) ||| Full)
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices
myManageHook = composeAll
    [ appName =? "neovide"                          --> doShift ( myWorkspaces !! 0 )
    , appName =? "google-chrome"                    --> doShift ( myWorkspaces !! 1 )
    , appName =? "microsoft teams - preview"        --> doShift ( myWorkspaces !! 4 )
    , appName =? "JetBrains Toolbox"                --> doFloat
    , className =? ""                          --> doShift ( myWorkspaces !! 5 )
    , isFullscreen --> myDoFullFloat
    ]
myDoFullFloat = doF W.focusDown <+> doFullFloat
myLogHook h  = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
    { ppOutput = hPutStrLn h
        , ppCurrent = xmobarColor "#fff" "" . wrap "" ""
        , ppHidden = xmobarColor "#c3cb71" "" . wrap "" "" . clickable
        , ppHiddenNoWindows = xmobarColor "#559e83" "" . wrap "" "" . clickable
        , ppTitle = xmobarColor "#fff" "" . shorten 60
        , ppLayout = const ""
        , ppSep = " <fc=#fff><fn=1> \xe0d2 </fn></fc>"
        , ppWsSep = "<fc=#1b85b8><fn=1> \xe0b9 </fn></fc>"
    }
