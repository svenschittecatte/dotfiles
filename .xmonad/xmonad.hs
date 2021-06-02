import XMonad
import XMonad.Config.Desktop
import Data.Maybe (fromJust)
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.SetWMName
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import XMonad.Layout.Reflect
import qualified Data.Map as M

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ def
    { layoutHook=avoidStruts $ myLayoutHook
    , manageHook=myManageHook <+> manageDocks
    , workspaces=myWorkspaces
    , startupHook = setWMName "LG3D"
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
myLayoutHook = spacing 4 $ (Tall 1 (3/100) (1/2) ||| Full)
myWorkspaces = [" dev ", " www ", " sys ", " doc ", " chat ", " mus ", " vid "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices
myManageHook = composeAll
    [ appName =? "google-chrome"   --> doShift ( myWorkspaces !! 1 )
    , appName =? "microsoft teams - preview"                  --> doShift ( myWorkspaces !! 4 )
    , appName =? "neovide"                  --> doShift ( myWorkspaces !! 0 )
    , isFullscreen --> myDoFullFloat
    ]
myDoFullFloat = doF W.focusDown <+> doFullFloat
myLogHook h  = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
    { ppOutput = hPutStrLn h
        , ppCurrent = xmobarColor "#fff" "#5a5255" . wrap "<fc=#5a5255><fn=1>\xe0be </fn></fc>" "<fc=#5a5255><fn=1>\xe0b8 </fn></fc>"
        , ppVisible = xmobarColor "#fff" "" . wrap "" "" . clickable
        , ppHidden = xmobarColor "#fff" "" . wrap "" "" . clickable
        , ppHiddenNoWindows = xmobarColor "#fff" "" . wrap "" "" . clickable
        , ppTitle = xmobarColor "#fff" "" . shorten 60
        , ppLayout = const ""
        , ppSep = "<fc=#5a5255><fn=1> \xe0c4 </fn></fc>"
    }
