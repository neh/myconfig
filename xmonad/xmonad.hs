import System.Posix.Unistd

import XMonad hiding (Tall)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad (filterM)
import Data.Char (ord)
import Data.List
import Data.Maybe
import Data.Ratio
import System.IO
import System.Exit
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.FloatKeys
import XMonad.Actions.PerLayoutKeys
import XMonad.Actions.RotSlaves
import XMonad.Actions.Submap
import XMonad.Actions.UpdateFocus
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Layout.BoringWindows as B
import XMonad.Layout.HintedGrid
import XMonad.Layout.HintedTile
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare

-- Makes ~? usable in manageHook and other places to match
-- window properties against regexes.
-- From: http://thread.gmane.org/gmane.comp.lang.haskell.xmonad/7058/focus=7062
import Text.Regex.Posix ((=~))
q ~? x = fmap (=~ x) q


bg = "#222222"
fg = "#f3431b"
fn = "-*-terminal-medium-r-*-*-17-*-*-*-*-*-iso8859-*"

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
    D.requestName dbus (D.busName_ "org.xmonad.Log")
                  [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant (UTF8.decodeString str)]
        }
    D.emit dbus signal

main :: IO ()
main = do
  dbus <- D.connectSession
  getWellKnownName dbus
  hostname <- fmap nodeName getSystemID
  --spawn "xcompmgr -nFf -I 0.056 -O 0.06"
  xmonad $ withUrgencyHook NoUrgencyHook
         $ ewmh defaultConfig
    { borderWidth        = 1
    , terminal           = "urxvtcd"
    , normalBorderColor  = "#222222"
    , focusedBorderColor = fg
    , modMask            = mod4Mask
    , workspaces         = myWorkSpaces hostname
    , keys               = myKeys hostname
    , mouseBindings      = myMouse
    , startupHook        = adjustEventInput
    , handleEventHook    = focusOnMouseMove
                           <+> fullscreenEventHook
    , manageHook         = myPreManageHook
                           <+> placeHook (withGaps (24,0,0,0)
                                         (inBounds
                                         (underMouse (0.5, 0.5))))
                           <+> manageDocks
                           <+> myManageHook
    , logHook            = myLog dbus hostname
                           -- >> fadeInactiveLogHook 0x99999999
                           >> takeTopFocus
                           >> setWMName "LG3D"
    , layoutHook         = smartBorders
                           $ layoutHintsWithPlacement (0.5, 0.5)
                           $ maximize
                           $ minimize
                           $ B.boringWindows
                           $ toggleLayouts (noBorders Full)
                           $ onWorkspace "vm" (noBorders Full)
                           $ avoidStruts
                           $ onWorkspace "comm" (noBorders Full)
                           $ onWorkspace "im" im
                           $ onWorkspace "files" file
                           $ onWorkspace "gimp" gimp
                           $ onWorkspace "d" (noBorders Full)
                           $ onWorkspace "db" (noBorders Full)
                           $ onWorkspace "mon" (monlayout ||| Full)
                           $ trackFloating (useTransientFor tp)
                           ||| rtp
                           ||| rtiled
                           ||| file
                           ||| grid
                           ||| (noBorders Full)
    }
    where
      myWorkSpaces hostname = case hostname of
          "nathan" -> ["mon", "comm", "files", "d", "web", "db", "lose", "vm"]
          _ -> ["org", "comm", "files", "web", "misc", "vm"]

      tiled = HintedTile 1 (3%100) 0.648 TopLeft Tall
      rtiled = Mag.magnifier' (ResizableTall 1 (3%100) 0.648 [])
      tp = TwoPane 0.03 0.62
      monlayout = withIM (0.34) (ClassName "Gnome-terminal") $ Full
      rtp = Mirror $ TwoPane 0.03 0.6
      im = withIM (0.15) (Role "contact_list") $ reflectHoriz $
           withIM (0.15) (Title "Hangouts") $ reflectHoriz $
           multiCol [1] 1 0.01 (-0.5)
      rgrid = Grid True
      grid = Grid False
      file = ThreeCol 1 (3/100) (0.33)
      gimp = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $
             withIM (0.15) (Role "gimp-dock") Full
      read = withIM (0.12) (ClassName "Rox") $ Full



myLog dbus hostname = withWindowSet $ \ws -> do
        dynamicLogWithPP $ defaultPP
          { ppCurrent         = pangoStyle "#be462a" "#dfd8c3" "bold"
          , ppUrgent          = pangoUrgent "#d7c529"
          , ppHidden          = wrap " " " "
          , ppVisible         = pangoStyle "#df684c" "#333" "bold"
          , ppHiddenNoWindows = pangoColor "#dfd8c3"
          , ppWsSep           = ""
          , ppSep             = " "
          , ppSort            = DO.getSortByOrder
          , ppTitle           = pangoBold "#efefef" . shorten 255
          , ppOrder           = \(workspaces:layout:title:xs) ->
                                 (myWCount ws:workspaces:title:xs)
          , ppOutput          = dbusOutput dbus
          }
  where
      -- myWCount provides a count of open windows, and
      -- indicates which has focus. e.g. 2/4 means that
      -- the second window of four is focused.
      myWCount = W.with (sc ++ "0/0" ++ ec)
                   (\s -> sc ++ (show (length (W.up s) + 1))
                   ++ "/" ++
                   (show (length (W.integrate s))) ++ ec)
      sc = "<span foreground=\"#000\" background=\"#dfd8c3\" font_weight=\"bold\"> "
      ec = " </span>"

      pangoStyle :: String -> String -> String -> String -> String
      pangoStyle fg bg fw = wrap left right
       where
        left  = "<span foreground=\"" ++ fg ++ "\" background=\"" ++ bg ++ "\" font_weight=\"" ++ fw ++ "\"> "
        right = " </span>"

      pangoColor :: String -> String -> String
      pangoColor fg = wrap left right
       where
        left  = "<span foreground=\"" ++ fg ++ "\"> "
        right = " </span>"

      pangoBold :: String -> String -> String
      pangoBold fg = wrap left right
       where
        left  = "<span foreground=\"" ++ fg ++ "\" font_weight=\"bold\"> "
        right = " </span>"

      pangoUrgent :: String -> String -> String
      pangoUrgent fg = wrap left right
       where
        left  = "<span underline=\"single\" foreground=\"" ++ fg ++ "\" font_weight=\"bold\"> "
        right = " </span>"


--data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

--instance UrgencyHook LibNotifyUrgencyHook where
  --urgencyHook LibNotifyUrgencyHook w = do
    --name <- getName w
    --ws <- gets windowset
    --whenJust (W.findTag w ws) (flash name)
    --where
      --flash name index = safeSpawn "notify-send" ("Activity in " ++ show name ++ " on workspace " ++ index)


myPConfig = defaultXPConfig
  { font              = fn
  , bgColor           = bg
  , fgColor           = "#dfd8c3"
  , height            = 36
  }


-- This manageHook is meant to be used before placeHook
-- to avoid its effects for some apps
myPreManageHook :: ManageHook
myPreManageHook = composeAll
  [ title     =? "handy"                   --> doCenterFloat
  , className =? "feh"                     --> doCenterFloat
  , className =? "MPlayer"                 --> doCenterFloat
  ]

-- And this manageHook goes after the placeHook
myManageHook :: ManageHook
myManageHook = composeAll
  [ className ~? "(Do|Do.exe)"             --> doIgnore
  , className =? "Synapse"                 --> doIgnore
  , resource  =? "Dialog"                  --> doFloat
  , title     ~? "Page.s. Unresponsive"    --> doFloat
  , title     =? "Options"                 --> doFloat
  , title     =? "Edit Bookmark"           --> doFloat
  , title     =? "Session Manager"         --> doFloat
  , title     =? "Bulk rename files"       --> doFloat
  , className =? "Apt-listchanges"         --> doFloat
  , title     =? "Firefox Preferences"     --> doFloat
  , title     =? "Thunderbird Preferences" --> doFloat
  , title     =? "Add-ons"                 --> doFloat
  , className =? "Xmessage"                --> doFloat
  , title     ~? "mythfrontend"            --> doNewHWS "tv"
  , className ~? "(Gimp-2.6|Gimp)"         --> doNewWS "gimp"
  , title     ~? ".*VirtualBox.*"          --> doNewWS "vm"
  , className =? "Savebox"                 --> doOpenUnder
  , className =? "rdesktop"                --> doNewWS "rdp"
  , role      =? "buddy_list"              --> doFloat
  , role      =? "conversation"            --> doFloat
  , className =? "Unity-2d-panel"          --> doIgnore
  , className =? "Unity-2d-launcher"       --> doIgnore
  , title     =? "Sublime Color Picker"    --> doFloat
  , resource  ~? "crx_.*"                  --> doShift "im"
  , role      =? "gimp-screenshot"         --> doFloat
  ]
  where
    unFloat = ask >>= doF . W.sink
    doOpenUnder = doF W.swapDown
    role = stringProperty "WM_WINDOW_ROLE"

    doNewWS tg = (liftX $ addUniqueWS tg) >> doShift tg
    addUniqueWS tg = withWindowSet $ \s ->
      if null (filter ( (== tg) . W.tag) (W.workspaces s))
        then addWorkspace tg
        else return()
    doNewHWS tg = (liftX $ addUniqueHiddenWS tg) >> doShift tg
    addUniqueHiddenWS tg = withWindowSet $ \s ->
      if null (filter ( (== tg) . W.tag) (W.workspaces s))
        then addHiddenWorkspace tg
        else return()



myKeys hostname conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask,                 xK_c     ), spawn $ XMonad.terminal conf)
  , ((modMask, xK_a), submap . M.fromList $
    [ ((0, xK_e), spawn "appmenu")
    , ((0, xK_s), spawn "screenmenu")
    , ((0, xK_x), shellPrompt myPConfig)
    , ((0, xK_m), manPrompt myPConfig)
    , ((0, xK_g), windowPromptGoto myPConfig)
    , ((0, xK_b), windowPromptBring myPConfig)
    , ((0, xK_t), appendFilePrompt myPConfig "/home/nathan/notes/s-o-c")
    ])

  , ((0            , 0x1008ff14), spawn "mpc --no-status toggle")
  , ((0            , 0x1008ff16), spawn "mpc --no-status prev")
  , ((0            , 0x1008ff17), spawn "mpc --no-status next")
  , ((modMask,                 xK_m     ), submap . M.fromList $
    [ ((0, xK_l), spawn $ XMonad.terminal conf ++ " -e ncmpc")
    , ((0, xK_g), raiseNext (title ~? ".* Google Play Music$"))
    , ((0, xK_m), spawn "musicmenu mpc")
    , ((0, xK_t), spawn "musicmenu totem")
    , ((0, xK_b), spawn "musicmenu banshee-1")
    , ((0, xK_r), spawn "musicmenu rhythmbox-client")
    , ((0, xK_p), spawn "musicmenu beep-media-player")
    , ((0, xK_v), spawn "pavucontrol")
    ])

  , ((modMask, xK_apostrophe), submap . M.fromList $
    [ ((0, xK_i), B.markBoring)
    , ((0, xK_u), B.clearBoring)
    , ((0, xK_m), sendMessage Mag.Toggle)
    , ((0, xK_t), windows W.focusMaster)
    , ((0, xK_r), refresh)
    ])
  , ((modMask,                 xK_u     ), focusUrgent)
  , ((modMask,                 xK_k     ), kill1)
  , ((modMask,                 xK_r     ), toggleFloat)
  , ((modMask,                 xK_z     ), withFocused (sendMessage . maximizeRestore))

  , ((modMask,                 xK_i     ), windows $ W.greedyView "me")

  , ((modMask,                 xK_o     ), toggleWindow (title =? "handy")
      (spawn $ XMonad.terminal conf ++
      " -title handy -geometry "++ handySize ++" -e sh -c \"tmux attach -t handy || tmux new-session -s handy\""))

  , ((modMask, xK_g), submap . M.fromList $
    [ ((0, xK_m), raiseNext (className =? "MPlayer"))
    , ((0, xK_f), raiseNext (className =? "Rox"))
    , ((0, xK_t), raiseNext (title ~? "mythfrontend(.real)?"))
    ])
  , ((modMask,                 xK_b     ), raiseNext (className ~? "(Firefox|Google-chrome)") )

  , ((modMask,                 xK_space ), sendMessage NextLayout)
  , ((modMask .|. shiftMask,   xK_space ), setLayout $ XMonad.layoutHook conf)

  , ((modMask,                 xK_BackSpace), removeEmptyWorkspace)
  , ((modMask,                 xK_l     ), selectWorkspace myPConfig)
  , ((modMask .|. shiftMask,   xK_p     ), withWorkspace myPConfig (windows . W.shift))
  , ((modMask .|. controlMask, xK_p     ), withWorkspace myPConfig (windows . copy))
  , ((modMask .|. shiftMask,   xK_r     ), renameWorkspace myPConfig)

  --, ((mod1Mask,                xK_Tab   ), toggleWS )
  , ((modMask .|. shiftMask,    xK_Tab   ), toggleWS )
  , ((modMask,                  xK_Tab   ), toggleWS )

  , ((modMask,                 xK_F12   ), spawn "gnome-screensaver-command --lock")

  , ((modMask .|. controlMask, xK_period), rotSlavesUp)
  , ((modMask .|. controlMask, xK_comma ), rotSlavesDown)

  , ((modMask,           xK_bracketright ), withFocused minimizeWindow)
  , ((modMask,           xK_bracketleft), sendMessage RestoreNextMinimizedWin)

  , ((modMask,                 xK_t     ), bindOnLayout [("TwoPane", rotSlavesUp), ("", B.focusDown)])
  , ((modMask,                 xK_n     ), bindOnLayout [("TwoPane", rotSlavesDown), ("", B.focusUp)])

  , ((modMask,                 xK_Return), windows W.swapMaster)
  , ((modMask .|. shiftMask,   xK_t     ), windows W.swapDown  )
  , ((modMask .|. shiftMask,   xK_n     ), windows W.swapUp    )

  , ((modMask .|. controlMask, xK_h     ), withFocused (keysMoveWindow (-200,0)))
  , ((modMask .|. controlMask, xK_s     ), withFocused (keysMoveWindow (200,0)))
  , ((modMask .|. controlMask, xK_t     ), myFocusDown)
  , ((modMask .|. controlMask, xK_n     ), myFocusUp)

  , ((modMask,                 xK_comma ), myShrink)
  , ((modMask,                 xK_period), myExpand)
  , ((modMask .|. shiftMask,   xK_slash ), sendMessage MirrorShrink)
  , ((modMask .|. shiftMask,   xK_equal ), sendMessage MirrorExpand)

  , ((modMask .|. shiftMask,   xK_comma ), sendMessage (IncMasterN 1))
  , ((modMask .|. shiftMask,   xK_period), sendMessage (IncMasterN (-1)))

  , ((modMask,                 xK_minus ), sendMessage ToggleStruts)

  , ((modMask,                 xK_h     ), DO.moveTo Prev AnyWS)
  , ((modMask,                 xK_s     ), DO.moveTo Next AnyWS)

  -- move WS left/right
  , ((modMask .|. shiftMask,   xK_h     ), DO.swapWith Prev AnyWS)
  , ((modMask .|. shiftMask,   xK_s     ), DO.swapWith Next AnyWS)

  , ((modMask,                 xK_w     ), prevScreen)
  , ((modMask .|. shiftMask,   xK_w     ), shiftNextScreen)

  , ((modMask,                 xK_f     ), sendMessage ToggleLayout )
  , ((modMask .|. shiftMask,   xK_f     ), spawn "wmctrl -i -r $(xprop -root | awk '/_NET_ACTIVE_WINDOW\\(WINDOW\\)/{print $NF}') -b toggle,fullscreen")

  , ((modMask,                 xK_0     ), spawn "/home/nathan/bin/toggle_layout.sh")

  , ((modMask .|. shiftMask,   xK_q     ), spawn "gnome-session-save --gui --logout-dialog")
  , ((modMask,                 xK_q     ), spawn "xmonad --recompile && xmonad --restart")
  ]
  ++
  -- mod-[1..9]           %! Switch to workspace N
  zip (zip (repeat modMask) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
  ++
  -- mod-shift-[1..9]     %! Move client to workspace N
  zip (zip (repeat (modMask .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
  ++
  -- mod-control-[1..9]   %! Copy client to workspace N
  zip (zip (repeat (modMask .|. controlMask)) [xK_1..xK_9]) (map (withNthWorkspace copy) [0..])
  where
    handySize = case hostname of
      "tak" -> "105x46"
      _ -> "105x65"

    role = stringProperty "WM_WINDOW_ROLE"

    toggleFloat = withWindowSet $ \ws ->
                    if M.member (fromJust $ W.peek ws) (W.floating ws)
                      then withFocused $ windows . W.sink
                      else withFocused (keysMoveWindowTo (960,600) (1%2,1%2))

    myFocusDown = withWindowSet $ \ws ->
                 if M.member (fromJust $ W.peek ws) (W.floating ws)
                   then withFocused (keysMoveWindow (0,200))
                   else B.focusDown
    myFocusUp = withWindowSet $ \ws ->
                 if M.member (fromJust $ W.peek ws) (W.floating ws)
                   then withFocused (keysMoveWindow (0,-200))
                   else B.focusUp

    myExpand = withWindowSet $ \ws ->
                 if M.member (fromJust $ W.peek ws) (W.floating ws)
                   then withFocused (keysResizeWindow (40,40) (1%2,1%2))
                   else sendMessage Expand
    myShrink = withWindowSet $ \ws ->
                 if M.member (fromJust $ W.peek ws) (W.floating ws)
                   then withFocused (keysResizeWindow (-40,-40) (1%2,1%2))
                   else sendMessage Shrink

    showDesktop = withWindowSet $ \ws ->
      if null (filter ( (== ".Z") . W.tag) (W.workspaces ws))
        then addWorkspace ".Z"
        else removeWorkspace

    toggleWindow wTest action = withWindowSet $ \ws -> do
      filterAll <- filterM (runQuery wTest) (W.allWindows ws)
      curr <- gets (W.currentTag . windowset)
      case filterAll of
        --(x:_) -> do t <- gets (fromJust (W.findTag x ws))
                    --if t == curr
        --(x:_) -> if fromJust (W.findTag x ws) == curr
                     --then killWindow x
                     --else bringWindow x
        (x:_) -> sendKeyWindow controlMask xK_underscore x
        []    -> action



button6     =  6 :: Button
button7     =  7 :: Button
button8     =  8 :: Button
button9     =  9 :: Button
myMouse (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
  , ((modMask, button4), (\_ -> DO.moveTo Prev AnyWS))
  , ((modMask, button5), (\_ -> DO.moveTo Next AnyWS))
  -- Need a decent side scroll mouse for these two
  --, ((0, button6), (\_ -> DO.moveTo Prev AnyWS))
  --, ((0, button7), (\_ -> DO.moveTo Next AnyWS))
  , ((modMask .|. controlMask, button2), (\w -> focus w >> kill1))
  , ((modMask .|. controlMask, button4), (\_ -> bindOnLayout [("TwoPane", rotSlavesDown), ("", B.focusUp)]))
  , ((modMask .|. controlMask, button5), (\_ -> bindOnLayout [("TwoPane", rotSlavesUp), ("", B.focusDown)]))
  , ((0, button9), (\_ -> toggleWS))
  , ((modMask, button9), (\_ -> raiseNext (title ~? ".* Google Play Music$")))
  , ((modMask .|. shiftMask, button4), (\_ -> windows W.swapUp))
  , ((modMask .|. shiftMask, button5), (\_ -> windows W.swapDown))
  ]
