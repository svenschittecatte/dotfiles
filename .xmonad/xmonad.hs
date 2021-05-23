import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Layout.Reflect

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ def
    { layoutHook=avoidStruts $ myLayoutHook
    , manageHook=manageHook def <+> manageDocks
    , workspaces=myWorkspaces
    , terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = myBorderWidth
    , logHook     = myLogHook xmproc
    } `additionalKeys`
    [ ((myModMask, xK_w), spawn "google-chrome-stable")
    , ((myModMask, xK_c), spawn "find $HOME/Code/ -mindepth 2 -maxdepth 2 | dmenu | xargs neovide")
    ]

myTerminal    = "alacritty"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 3
myLayoutHook = spacing 4 $ (Tall 1 (3/100) (1/2) ||| Full)
myWorkspaces = [" dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]
myLogHook h  = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
    { ppOutput = hPutStrLn h
        , ppCurrent = xmobarColor "#fff" "#5a5255" . wrap "<fc=#5a5255><fn=1>\xe0be </fn></fc>" "<fc=#5a5255><fn=1>\xe0b8 </fn></fc>"
        , ppVisible = xmobarColor "#fff" "" . wrap "" ""
        , ppHidden = xmobarColor "#fff" "" . wrap "" ""
        , ppHiddenNoWindows = xmobarColor "#fff" "" . wrap "" ""
        , ppTitle = xmobarColor "#fff" "" . shorten 60
        , ppLayout = const ""
        , ppSep = "<fc=#5a5255><fn=1> \xe0c4 </fn></fc>"
    }
