import XMonad hiding (Tall)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad (filterM)
import Control.OldException
import Data.Char (ord)
import Data.List
import Data.Maybe
import Data.Ratio
import System.IO
import System.Exit
import DBus
import DBus.Connection
import DBus.Message

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.RotSlaves
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
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
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
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
--fn = "-*-andale mono-medium-r-*-*-17-*-*-*-*-*-*-*"


getWellKnownName :: Connection -> IO ()
getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) ->
                                                getWellKnownName dbus)
 where
  tryGetName = do
    namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
    addArgs namereq [String "org.xmonad.Log", Word32 5]
    sendWithReplyAndBlock dbus namereq 0
    return ()


main :: IO ()
main = withConnection Session $ \ dbus -> do
  putStrLn "Getting well-known name."
  getWellKnownName dbus
  putStrLn "Got name, starting XMonad."
  --spawn "xcompmgr -nFf -I 0.056 -O 0.06"
  xmonad $ withUrgencyHook NoUrgencyHook
         $ defaultConfig
    { borderWidth        = 1
    , terminal           = "urxvtcd"
    , normalBorderColor  = "#222222"
    , focusedBorderColor = fg
    , modMask            = mod4Mask
    , workspaces         = ["comm", "im", "files", "web"]
    , keys               = myKeys
    , mouseBindings      = myMouse
    , handleEventHook    = ewmhDesktopsEventHook
    , manageHook         = placeHook (inBounds(underMouse (0.5, 0.5)))
                           <+> manageDocks
                           <+> myManageHook
    , logHook            = myLog dbus >>
                           --fadeInactiveLogHook 0x99999999 >>
                           ewmhDesktopsLogHook >>
                           updatePointer (Relative 0.01 0.5) >>
                           setWMName "LG3D"
    , layoutHook         = smartBorders $
                           --layoutHintsToCenter $
                           layoutHintsWithPlacement (0.5, 0.5) $
                           maximize $
                           B.boringWindows $
                           toggleLayouts (noBorders Full) $
                           onWorkspace "vm" (noBorders Full) $
                           avoidStruts $
                           onWorkspace "0" (tp ||| grid) $
                           onWorkspace "comm" (noBorders Full) $
                           onWorkspace "im" im $
                           onWorkspace "files" file $
                           onWorkspace "gimp" gimp $
                           rtiled |||
                           tp |||
                           file
    }
    where
      tiled = HintedTile 1 (3%100) 0.648 TopLeft Tall
      rtiled = Mag.magnifier' (ResizableTall 1 (3%100) 0.648 [])
      tp = TwoPane 0.03 0.5
      im = withIM (0.13) (Role "buddy_list") $ ResizableTall 1 (1/100) (0.40) [1]
      rgrid = Grid True
      grid = Grid False
      file = ThreeCol 1 (3/100) (0.33)
      gimp = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $
             withIM (0.15) (Role "gimp-dock") Full


myLog dbus = withWindowSet $ \ws -> do
        dynamicLogWithPP $ defaultPP
          { ppCurrent         = pangoStyle "#be462a" "#dfd8c3" "bold"
          , ppUrgent          = pangoUrgent "#d7c529"
          , ppHidden          = wrap " " " "
          , ppVisible         = pangoColor "#663366" . wrap "<" ">"
          , ppHiddenNoWindows = id
          , ppWsSep           = ""
          , ppSep             = " "
          , ppTitle           = pangoBold "#efefef" . shorten 90
          , ppOrder           = \(workspaces:layout:title:xs) ->
                                 (myWCount ws:workspaces:title:xs)
          , ppOutput   = \ str -> do
              let str'  = "<span font=\"Liberation Sans Mono\">" ++ str ++ 
                          "</span>"
                  str'' = sanitize str'
              msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" 
                         "Update"
              addArgs msg [String str'']
              -- If the send fails, ignore it.
              send dbus msg 0 `catchDyn`
                (\ (DBus.Error _name _msg) ->
                  return 0)
              return ()
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

      sanitize :: String -> String
      sanitize [] = []
      sanitize (x:rest) | fromEnum x > 127 = "&#" ++ show (fromEnum x) ++ "; " ++
                                             sanitize rest
                        | otherwise        = x : sanitize rest


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


myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "stalonetray"           --> doIgnore
  , className ~? "(Do|Do.exe)"           --> doIgnore
  , resource  =? "Dialog"                --> doFloat
  , title     =? "Edit Bookmark"         --> doFloat
  , title     =? "Session Manager"       --> doFloat
  , title     =? "Bulk rename files"     --> doFloat
  , className =? "Apt-listchanges"       --> doFloat
  , title     =? "Shiretoko Preferences" --> doFloat
  , className =? "feh"                   --> doCenterFloat
  , className =? "Xmessage"              --> doCenterFloat
  , title     =? "handy"                 --> (doSetRole "handy" >> doCenterFloat) 
  , title     ~? "mythfrontend"          --> doNewHWS "tv"
  , className ~? "(Gimp-2.6|Gimp)"       --> doNewWS "gimp"
  , title     ~? ".*VirtualBox.*"        --> doNewWS "vm"
  , title     =? "Add-ons"               --> doOpenUnder
  , className =? "Savebox"               --> doOpenUnder
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

    -- Apparently this function is a bad idea, since it likely
    -- violates the ICCCM. I only use it as a workaround for
    -- apps that can't set their own role, like urxvt.
    doSetRole rl = ask >>= \w ->
      (liftX $ withDisplay $ \dpy -> do
        r <- getAtom "WM_WINDOW_ROLE"
        t <- getAtom "STRING"
        io $ changeProperty8 dpy w r t propModeReplace
             (map (fromIntegral . ord) rl)
      ) >> idHook


myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask,                 xK_c     ), spawn $ XMonad.terminal conf)
  , ((modMask, xK_e), submap . M.fromList $
    [ ((0, xK_e), spawn "appmenu")
    , ((0, xK_s), spawn "screenmenu")
    , ((0, xK_x), shellPrompt myPConfig)
    , ((0, xK_m), manPrompt myPConfig)
    , ((0, xK_g), windowPromptGoto myPConfig)
    , ((0, xK_b), windowPromptBring myPConfig)
    , ((0, xK_t), appendFilePrompt myPConfig "/home/nathan/notes/s-o-c")
    ])

  , ((modMask,                 xK_apostrophe ), spawn "mpc --no-status toggle")
  , ((modMask,                 xK_m     ), submap . M.fromList $
    [ ((0, xK_l), spawn $ XMonad.terminal conf ++ " -e ncmpc")
    , ((0, xK_m), spawn "musicmenu mpc")
    , ((0, xK_t), spawn "musicmenu totem")
    , ((0, xK_b), spawn "musicmenu banshee")
    , ((0, xK_r), spawn "musicmenu rhythmbox-client")
    , ((0, xK_p), spawn "musicmenu beep-media-player")
    , ((0, xK_v), spawn "pavucontrol")
    ])

  , ((modMask, xK_w), submap . M.fromList $
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

  , ((modMask,                 xK_o     ), toggleWindow (role =? "handy")
      (spawn $ XMonad.terminal conf ++
      " -title handy -geometry 100x52 -e screen -D -R handy"))

  , ((modMask, xK_g), submap . M.fromList $
    [ ((0, xK_m), raiseNext (className =? "MPlayer"))
    , ((0, xK_f), raiseNext (className =? "Rox"))
    , ((0, xK_t), raiseNext (title ~? "mythfrontend(.real)?"))
    ])
  , ((modMask,                 xK_b     ), raiseNext (className ~? "(Firefox|Shiretoko|Namoroka|Google-chrome)") )
  , ((modMask,                 xK_v     ), raiseNext (title ~? "VIM$") )

  , ((modMask,                 xK_space ), sendMessage NextLayout)
  , ((modMask .|. shiftMask,   xK_space ), setLayout $ XMonad.layoutHook conf)

  , ((modMask,                 xK_BackSpace), removeWorkspace)
  , ((modMask,                 xK_l     ), selectWorkspace myPConfig)
  , ((modMask .|. shiftMask,   xK_p     ), withWorkspace myPConfig (windows . W.shift))
  , ((modMask .|. controlMask, xK_p     ), withWorkspace myPConfig (windows . copy))
  , ((modMask .|. shiftMask,   xK_r     ), renameWorkspace myPConfig)

  , ((mod1Mask,                xK_Tab   ), toggleWS )

  , ((modMask,                 xK_F12   ), spawn "gnome-screensaver-command --lock")

  , ((modMask .|. controlMask, xK_comma ), rotSlavesUp)
  , ((modMask .|. controlMask, xK_period), rotSlavesDown)

  , ((modMask,                 xK_t     ), B.focusDown)
  , ((modMask,                 xK_n     ), B.focusUp  )

  , ((modMask,                 xK_Return), windows W.swapMaster)
  , ((modMask .|. shiftMask,   xK_t     ), windows W.swapDown  )
  , ((modMask .|. shiftMask,   xK_n     ), windows W.swapUp    )

  , ((modMask .|. controlMask, xK_h     ), withFocused (keysMoveWindow (-200,0)))
  , ((modMask .|. controlMask, xK_s     ), withFocused (keysMoveWindow (200,0)))
  , ((modMask .|. controlMask, xK_t     ), withFocused (keysMoveWindow (0,200)))
  , ((modMask .|. controlMask, xK_n     ), withFocused (keysMoveWindow (0,-200)))

  , ((modMask,                 xK_comma ), myShrink)
  , ((modMask,                 xK_period), myExpand)
  , ((modMask .|. shiftMask,   xK_slash ), sendMessage MirrorShrink)
  , ((modMask .|. shiftMask,   xK_equal ), sendMessage MirrorExpand)

  , ((modMask .|. shiftMask,   xK_comma ), sendMessage (IncMasterN 1))
  , ((modMask .|. shiftMask,   xK_period), sendMessage (IncMasterN (-1)))

  , ((modMask,                 xK_minus ), sendMessage ToggleStruts)
  
  , ((modMask,                 xK_h     ), prevWS)
  , ((modMask,                 xK_s     ), nextWS)
  , ((modMask .|. shiftMask,   xK_h     ), shiftToPrev)
  , ((modMask .|. shiftMask,   xK_s     ), shiftToNext)

  , ((modMask,                 xK_Left     ), nextScreen)
  --, ((modMask,                 xK_Right    ), prevWS)

  , ((modMask,                 xK_f     ), sendMessage ToggleLayout )

  , ((modMask .|. shiftMask,   xK_q     ), io (exitWith ExitSuccess))
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
    role = stringProperty "WM_WINDOW_ROLE"

    toggleFloat = withWindowSet $ \ws ->
                    if M.member (fromJust $ W.peek ws) (W.floating ws)
                      then withFocused $ windows . W.sink
                      else withFocused (keysMoveWindowTo (960,600) (1%2,1%2))

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
        (x:_) -> killWindow x
        []    -> action



myMouse (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
  , ((modMask, button4), (\_ -> prevWS))
  , ((modMask, button5), (\_ -> nextWS))
  , ((modMask .|. shiftMask, button4), (\_ -> windows W.swapUp))
  , ((modMask .|. shiftMask, button5), (\_ -> windows W.swapDown))
  ]
