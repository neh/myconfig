import XMonad hiding (Tall)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad (filterM)
import Data.Char (ord)
import Data.Ratio
import System.IO

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EventHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Combo
import XMonad.Layout.DragPane
import XMonad.Layout.HintedGrid
import XMonad.Layout.HintedTile
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ThreeColumnsMiddle
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Util.Run
 
-- Makes ~? usable in manageHook and other places to match
-- window properties against regexes.
-- From: http://thread.gmane.org/gmane.comp.lang.haskell.xmonad/7058/focus=7062
import Text.Regex.Posix ((=~))
q ~? x = fmap (=~ x) q


bg = "#222222"
fg = "#de8221" --orange
fn = "-*-fixed-medium-r-*-*-20-*-*-*-*-*-iso8859-*"
statusBarCmd = "dzen2 -bg '" ++ bg ++ "' -fg '" ++ fg ++ "' -x 0 -y 0 -h 24 -w 1332 -fn '" ++ fn ++ "' -e 'onstart=lower' -ta l"

-- icons
i_three = "^i(/home/nathan/.xmonad/three.xbm)"
i_hgrid = "^i(/home/nathan/.xmonad/hgrid.xbm)"
i_full = "^i(/home/nathan/.xmonad/full.xbm)"
i_dp = "^i(/home/nathan/.xmonad/dp.xbm)"
i_tall = "^i(/home/nathan/.xmonad/tall.xbm)"

main = do
  din <- spawnPipe statusBarCmd
  sp <- mkSpawner
  xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
    { borderWidth        = 0
    , terminal           = "urxvtcd"
    , normalBorderColor  = "#444444"
    , focusedBorderColor = "#de8221" --orange
    , modMask            = mod4Mask
    , workspaces         = ["0", "comm", "im", "files", "web"]
    , keys               = \c -> myKeys c sp `M.union` keys defaultConfig c
    , mouseBindings      = myMouse
    , handleEventHook    = ewmhDesktopsEventHook
    , manageHook         = manageSpawn sp <+> myManageHook
    , logHook            = myLog din >>
                           fadeInactiveLogHook 0x99999999 >>
                           ewmhDesktopsLogHook >>
                           updatePointer (Relative 0.01 0.5) >>
                           setWMName "LG3D"
    , layoutHook         = smartBorders $
                           layoutHints $
                           toggleLayouts full $
                           onWorkspace "tv" full $
                           avoidStruts $
                           onWorkspace "0" (dp ||| grid) $
                           onWorkspace "comm" full $
                           onWorkspace "im" im $
                           onWorkspace "files" file $
                           onWorkspace "gimp" gimp $
                           onWorkspace "vm" full $
                           rtiled |||
                           dp |||
                           file
    }
    where
      tiled = named i_tall (HintedTile 1 (3%100) 0.648 TopLeft Tall)
      rtiled = named i_tall (ResizableTall 1 (3%100) 0.648 [])
      dp = named i_dp (dragPane Vertical 0.03 0.5)
      im = named i_three (withIM (0.13) (Role "buddy_list") $
           ResizableTall 1 (1/100) (0.40) [1])
      full = named i_full Full
      gridr = named i_hgrid (Grid True)
      grid = named i_hgrid (Grid False)
      file = named i_three (ThreeCol 1 (3/100) (0.5))
      gimp = named i_three (withIM (0.11) (Role "gimp-toolbox") $
             reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full)


myLog h = withWindowSet $
  \ws -> dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor "black" fg . pad
    , ppUrgent          = dzenColor "black" "yellow" . pad
    , ppSep             = " "
    , ppHiddenNoWindows = id
    , ppTitle           = (" ^fg(#eeeeee)" ++) . dzenEscape
    , ppOrder           = \(workspaces:layout:title:xs) ->
                           (layout:mywc ws:workspaces:title:xs)
    , ppOutput          = hPutStrLn h
    }
    where
      sc = "^fg(#000000)^bg(" ++ fg ++ ") "
      ec = " ^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")"
      mywc = W.with (sc ++ "0/0" ++ ec)
             (\s -> sc ++ (show (length (W.up s) + 1))
             ++ "/" ++
             (show (length (W.integrate s))) ++ ec)


myPConfig = defaultXPConfig
  { font              = "-*-fixed-medium-r-*-*-20-*-*-*-*-*-iso8859-*"
  , bgColor           = bg
  , fgColor           = fg
  , height            = 36
  }


-- Other window properties are available too:
-- (stringProperty "WM_WINDOW_ROLE") =? "the-role-string"
myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Do"                    --> doIgnore
  , resource  =? "Dialog"                --> doFloat
  , title     =? "Edit Bookmark"         --> doFloat
  , title     =? "Session Manager"       --> doFloat
  , title     =? "Bulk rename files"     --> doFloat
  , title     =? "List Changes"          --> doFloat
  , title     =? "Shiretoko Preferences" --> doFloat
  , className =? "feh"                   --> doCenterFloat
  , title     ~? ".*VirtualBox.*"        --> doNewWS "vm"
  , className =? "stalonetray"           --> doIgnore
  , className =? "Mythfrontend.real"     --> doNewHWS "tv"
  , className =? "Gimp-2.6"              --> doNewWS "gimp"
  , title     =? "Add-ons"               --> doOpenUnder
  , className =? "Savebox"               --> doOpenUnder
  , title     =? "handy"                 --> (doSetRole "handy" >> doCenterFloat) 
  , manageDocks
  ]
  where
    unFloat = ask >>= doF . W.sink
    doOpenUnder = doF W.swapDown
    role = stringProperty "WM_WINDOW_ROLE"

    doNewHWS tg = (liftX $ addUniqueHiddenWS tg) >> doShift tg
    addUniqueHiddenWS tg = withWindowSet $ \s ->
      if null (filter ( (== tg) . W.tag) (W.workspaces s))
        then addHiddenWorkspace tg
        else return()
    doNewWS tg = (liftX $ addUniqueWS tg) >> doShift tg
    addUniqueWS tg = withWindowSet $ \s ->
      if null (filter ( (== tg) . W.tag) (W.workspaces s))
        then addWorkspace tg
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


myKeys conf@(XConfig {XMonad.modMask = modMask}) sp = M.fromList $
  [ ((modMask,                 xK_c     ), spawnHere sp $ XMonad.terminal conf)
  , ((modMask, xK_e), submap . M.fromList $
    [ ((0, xK_e), spawnHere sp "appmenu")
    , ((0, xK_s), spawnHere sp "screenmenu")
    , ((0, xK_x), shellPrompt myPConfig)
    , ((0, xK_m), manPrompt myPConfig)
    , ((0, xK_h), sshPrompt myPConfig)
    , ((0, xK_g), windowPromptGoto myPConfig)
    , ((0, xK_b), windowPromptBring myPConfig)
    , ((0, xK_t), appendFilePrompt myPConfig "/home/nathan/notes/s-o-c")
    ])
  , ((modMask,                 xK_k     ), kill1)

  , ((modMask,                 xK_space ), sendMessage NextLayout)

  , ((modMask,                 xK_r     ), refresh)

  , ((modMask,                 xK_u     ), focusUrgent)

  , ((modMask,                 xK_o     ), toggleWindow (role =? "handy")
      (spawnHere sp $ XMonad.terminal conf ++
      " -title handy -geometry 100x40 -e screen -D -R handy"))
  , ((modMask,                 xK_i     ), toggleWindow (title =? "insp")
      (spawnHere sp "feh --title insp $HOME/Pictures/cultofdone-wp.png"))

  , ((modMask,                 xK_BackSpace), removeWorkspace)
  , ((modMask,                 xK_l     ), selectWorkspace myPConfig)
  , ((modMask .|. shiftMask,   xK_p     ), withWorkspace myPConfig (windows . W.shift))
  , ((modMask .|. controlMask, xK_p     ), withWorkspace myPConfig (windows . copy))
  , ((modMask .|. shiftMask,   xK_r     ), renameWorkspace myPConfig)

  , ((mod1Mask,                xK_Tab   ), toggleWS )

  , ((modMask,                 xK_w     ), raiseNext (className ~? "(Shiretoko|Firefox)") )
  , ((modMask,                 xK_v     ), raiseNext (title ~? "VIM$") )

  , ((modMask,                 xK_F12   ), spawn "gnome-screensaver-command --lock")

  , ((modMask,                 xK_t     ), windows W.focusDown)
  , ((modMask,                 xK_n     ), windows W.focusUp  )
  , ((modMask,                 xK_b     ), windows W.focusMaster  )

  , ((modMask,                 xK_Return), windows W.swapMaster)
  , ((modMask .|. shiftMask,   xK_t     ), windows W.swapDown  )
  , ((modMask .|. shiftMask,   xK_n     ), windows W.swapUp    )

  , ((modMask .|. shiftMask,   xK_d     ), withFocused $ windows . W.sink)

  , ((modMask,                 xK_comma ), sendMessage Shrink)
  , ((modMask,                 xK_period), sendMessage Expand)
  , ((modMask .|. shiftMask,   xK_slash ), sendMessage MirrorShrink)
  , ((modMask .|. shiftMask,   xK_equal ), sendMessage MirrorExpand)

  , ((modMask .|. shiftMask,   xK_comma ), sendMessage (IncMasterN 1))
  , ((modMask .|. shiftMask,   xK_period), sendMessage (IncMasterN (-1)))

  , ((modMask,                 xK_minus ), sendMessage ToggleStruts)
  
  , ((modMask,                 xK_h     ), prevWS)
  , ((modMask,                 xK_s     ), nextWS)
  , ((modMask .|. shiftMask,   xK_h     ), shiftToPrev)
  , ((modMask .|. shiftMask,   xK_s     ), shiftToNext)

  , ((modMask,                 xK_f     ), sendMessage ToggleLayout )

  , ((modMask,                 xK_apostrophe ), spawn "mpc --no-status toggle")
  , ((modMask,                 xK_m     ), submap . M.fromList $
    [ ((0, xK_l), spawnHere sp "urxvt -e ncmpc")
    , ((0, xK_m), spawnHere sp "musicmenu mpc")
    , ((0, xK_t), spawnHere sp "musicmenu totem")
    , ((0, xK_b), spawnHere sp "musicmenu banshee")
    , ((0, xK_r), spawnHere sp "musicmenu rhythmbox-client")
    , ((0, xK_p), spawnHere sp "musicmenu beep-media-player")
    ])
  ]
  -- > -- mod-[1..9]       %! Switch to workspace N
  -- > -- mod-shift-[1..9] %! Move client to workspace N
  ++
  zip (zip (repeat modMask) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
  ++
  zip (zip (repeat (modMask .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
  ++
  zip (zip (repeat (modMask .|. mod1Mask)) [xK_1..xK_9]) (map (withNthWorkspace copy) [0..])
  where
    role = stringProperty "WM_WINDOW_ROLE"

    showDesktop = withWindowSet $ \s ->
      if null (filter ( (== ".Z") . W.tag) (W.workspaces s))
        then addWorkspace ".Z"
        else removeWorkspace

    toggleWindow wTest action = withWindowSet $ \s -> do
      filterCurrent <- filterM (runQuery wTest)
                       ( (maybe [] W.integrate
                          . W.stack
                          . W.workspace
                          . W.current) s)
      case filterCurrent of
        (x:_) -> do
          killWindow x
        []    -> do
          action


myMouse (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
  , ((modMask, button4), (\_ -> prevWS))
  , ((modMask, button5), (\_ -> nextWS))
  -- TODO figure out why these next two lines work even without modMask actually pressed (kills browser text zooming)
  --, ((modMask .|. controlMask, button4), (\_ -> windows W.focusUp))
  --, ((modMask .|. controlMask, button5), (\_ -> windows W.focusDown))
  , ((modMask .|. shiftMask, button4), (\_ -> windows W.swapUp))
  , ((modMask .|. shiftMask, button5), (\_ -> windows W.swapDown))
  ]
