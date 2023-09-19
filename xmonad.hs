{-# LANGUAGE DeriveDataTypeable #-}

import XMonad

import XMonad.StackSet qualified as W

import Data.Char (isSpace)
import Data.List (find, (\\))
import Data.Map qualified as M
import Data.Maybe (catMaybes, isJust)
import System.Exit (
  ExitCode (ExitSuccess),
  exitWith,
 )

import Control.Concurrent (threadDelay)
import System.Posix.Unistd

import XMonad.Layout.MagicFocus

-- Hooks -----------------------------------------------------

import XMonad.Hooks.DynamicLog hiding (pprWindowSet)
import XMonad.Hooks.UrgencyHook

--      on IRC
import XMonad.Hooks.ManageDocks

--      status bar with windows
import XMonad.Hooks.ManageHelpers

--      windows in the middle of the
--      screen
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.WorkspaceHistory

-- Layout ----------------------------------------------------

import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.WorkspaceDir

--      per-workspace
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect

import XMonad.Layout.Gaps

--       of the screen useful for things
--       like screencasts or projectors
--       which cut off part of the screen

-- Actions ---------------------------------------------------

-- (8)  navigate between windows
import XMonad.Actions.Navigation2D

import XMonad.Actions.CycleWS

--      goodness
import XMonad.Actions.CycleRecentWS

--      in most-recently-used order
import XMonad.Actions.Search hiding (Query, images)
import XMonad.Actions.Submap
import XMonad.Actions.Warp

-- (20) some predefined web searches
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TopicSpace

-- (22c)
import XMonad.Actions.TopicSpace qualified as TS

import XMonad.Actions.DynamicWorkspaceGroups

-- (22d)

import XMonad.Actions.DynamicWorkspaceOrder qualified as DO

-- (22e)

import XMonad.Actions.FloatKeys
import XMonad.Actions.PerWorkspaceKeys

-- Prompts ---------------------------------------------------

import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh

--      making more generic search
--      prompts than those in
--      XMonad.Prompt.Search
import XMonad.Prompt.Unicode
import XMonad.Prompt.Workspace

-- Utilities -------------------------------------------------

import XMonad.Util.Loggers

--      status bar
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

import XMonad.Util.ExtensibleState qualified as XS

import Control.Monad (when)

-- (31)
main :: IO ()
main = do
  h <- spawnPipe "dzen2 -ta r -fg '#a8a3f7' -bg '#3f3c6d' -dock -xs 2"
  host <- getHost
  checkTopicConfig (myTopicNames host) (myTopicConfig host) -- (22b)
  xmonad $ byorgeyConfig h host -- (0)

data Host
  = -- | Are the desktop screens large enough for three columns?
    Desktop Bool
  | -- | Does the laptop have a Windows key?
    Laptop Bool
  deriving (Eq, Read, Show)

getHost :: IO Host
getHost = do
  hostName <- nodeName `fmap` getSystemID
  return $ case hostName of
    "hippasus" -> Desktop True
    "hypatia" -> Desktop True
    "augustine" -> Laptop True
    "plato" -> Laptop True
    "diophantus" -> Laptop True
    _ -> Desktop False

myTerminal, myShell :: String
myTerminal = "urxvt --perl-lib ~/.urxvt -fg lightgrey -bg black +sb"
myShell = "zsh"

byorgeyConfig h host =
  docks $
    ewmh $
      --     noahProof host $
      withNavigation2DConfig def $
        myUrgencyHook $ -- (2)
          def
            { borderWidth = 2
            , terminal = myTerminal
            , workspaces = myTopicNames host
            , modMask =
                if host == Laptop False
                  then modMask def
                  else mod4Mask
            , normalBorderColor = "#dddddd"
            , focusedBorderColor = "#0033ff"
            , -- (22)
              logHook = do
                myDynamicLog h host
                workspaceHistoryHookExclude [scratchpadWorkspaceTag]
            , manageHook =
                manageSpawn
                  <+> myManageHook
                  <+> manageHook def
            , layoutHook = myLayoutHook host
            , focusFollowsMouse = False
            , -- XXX fixme: comment!                                 -- (29)
              startupHook =
                return ()
                  >> checkKeymap
                    (byorgeyConfig h host)
                    (myKeys h host)
            , -- (0c), and see below
              handleEventHook = followOnlyIf (queryFocused whenToFollow)
            }
            `removeKeysP` ["M-S-q"]
            `additionalKeysP` (myKeys h host) -- (29)

-- have urgent events flash a yellow dzen bar with black text
myUrgencyHook =
  withUrgencyHook
    dzenUrgencyHook -- (2)
      { args = ["-bg", "yellow", "-fg", "black"]
      }

-- define some custom topics for use with the TopicSpace module.
myTopics :: Host -> [TopicItem]
myTopics host =
  [ TI "web" "" (liftIO (threadDelay (10 * 1000000)) >> spawnOn "web" "firefox")
  , TI "irc" "" (liftIO (threadDelay (10 * 1000000)) >> ircAction host)
  , ti "read" "papers"
  , ti "write" "writing"
  , TI
      "org"
      "notes"
      (edit "--name org ~/notes/journal.org -f vc-status -f other-window -f toggle-window-split")
  , TI "Roam" "" (spawnOn "Roam" "roam-research --no-sandbox")
  , TI "draw" "" (spawnOn "draw" "inkscape")
  , TI "xm-conf" ".xmonad" $ do
      edit "~/.xmonad/xmonad.hs"
      shell
  , ti "xm-hack" "src/xmonad/xmonad-contrib"
  , TI "em-conf" "" (edit "~/.emacs")
  , ti "music" "music"
  , ti "conf" ""
  , ti "misc" ""
  , ti "ref" "documents/reference"
  , ti "play" ""
  , TI "tex-conf" "texmf/tex" (edit "~/texmf/tex/brent.sty")
  , ti "mlt" "writing/mlt"
  , ti "blog" "writing/blog"
  , ti "dia" "src/diagrams"
  ]
    ++ map
      diaWS
      [ "doc"
      , "core"
      , "lib"
      , "cairo"
      , "contrib"
      , "svg"
      , "gtk"
      , "canvas"
      , "builder"
      , "haddock"
      , "play"
      , "ci"
      , "travis"
      , "html5"
      , "input"
      , "pgf"
      , "solve"
      , "reflex"
      ]
    ++ map
      (uncurry diaWS')
      [ ("dia-ps", "postscript")
      , ("active", "active")
      , ("dia-raster", "rasterific")
      , ("palette", "palette")
      , ("SVGFonts", "SVGFonts")
      , ("dia-geo", "geometry")
      ]
    ++ [ ti "sp" "research/species"
       , TI "anki" "" (spawnOn "anki" "anki")
       , TI "video" "video" (spawnOn "video" "cinelerra")
       , ti "bl" "src/BlogLiterately"
       , TI "view" "" (return ())
       , -- , TI "heb" "documents/bible/study"
         --   (edit "~/documents/bible/study/Hebrews.tex" >>
         --    spawn "evince ~/documents/bible/study/Hebrews.pdf")
         -- , TI "noah" "documents/noah/emacs" (edit "~/documents/noah/emacs/noah.txt")
         ti "150" "teaching/150"
       , -- , ti "151" "teaching/151"
         -- , ti "M240" "teaching/M240"
         -- , ti "360" "teaching/360"
         -- , ti "CSO" "teaching/322"
         -- , ti "FP" "teaching/365"
         ti "382" "teaching/382"
       , -- , ti "410" "teaching/410"
         -- , ti "exp" "teaching/explorations"
         -- , ti "TEC" "teaching/TEC"
         TI "joyal" "writing/translation/series-formelles" $ do
          edit "~/writing/translation/series-formelles/series-formelles.lhs"
          spawn "evince ~/writing/translation/series-formelles/series-formelles.pdf"
       , -- , TI "GCBP"  "research/GCBP" $ do
         --     edit "~/research/GCBP/talk/GCBP-talk.lhs"
         --     spawn "evince ~/research/GCBP/talk/GCBP-talk.pdf"
         ti "fenwick" "research/fenwick"
       , ti "disco" "projects/disco"
       , ti "aoc" "learning/AoC"
       , -- , ti "idris" "src/Idris-dev"
         ti "kattis" "learning/Kattis"
       , ti "cf" "learning/cf"
       , TI "progteam" "teaching/programming-team/reference" $ do
          edit "~/teaching/programming-team/reference/Hendrix-comprog-reference.tex"
          spawn "evince ~/teaching/programming-team/reference/Hendrix-comprog-reference.pdf"
       , ti "acweb" "documents/sites/academic-web"
       , ti "adv" "teaching/advising"
       , ti "CCSC" "projects/CCSC"
       , ti "NAQ" "projects/NAQ/Feb2021"
       , ti "obs" ""
       , ti "teams" ""
       , TI "discord" "" $ spawnOn "discord" "discord"
       , ti "swarm" "projects/swarm"
       , TI "cpih" "writing/cpih" $ do
          edit "~/writing/cpih/CPiH.tex"
          spawn "evince ~/writing/cpih/CPiH.pdf"
       , ti "CAI" "documents/Hendrix/CAI/23L"
       , ti "keybase" ""
       ]
 where
  -- Make a default topic item that just spawns a shell.
  ti t d = TI t d shell
  shell = spawnShell host

  diaWS repo = diaWS' ("dia-" ++ repo) repo
  diaWS' wsName dirName =
    TI
      wsName
      ("src/diagrams/" ++ dirName)
      (shell >> editVC ("~/src/diagrams/" ++ dirName ++ "/*.cabal"))

ircAction :: Host -> X ()
ircAction _ = runInTerm "" "mosh thales -- screen -dRR"

edit :: String -> X ()
edit = spawn . ("emacs " ++)

editVC :: String -> X ()
editVC file = edit $ file ++ " -f vc-status -f toggle-window-split"

myTopicNames :: Host -> [Topic]
myTopicNames = topicNames . myTopics

-- (22b)
myTopicConfig :: Host -> TopicConfig
myTopicConfig host =
  def
    { topicDirs = M.fromList $ map (\(TI n d _) -> (n, d)) myTopics'
    , defaultTopicAction = const (return ())
    , defaultTopic = "web"
    , topicActions = M.fromList $ map (\(TI n _ a) -> (n, a)) myTopics'
    }
 where
  myTopics' = myTopics host

-- | Switch to the given topic, using the specified viewing method
--   (usually 'view' or 'greedyView').
switchTopic' ::
  (WorkspaceId -> WindowSet -> WindowSet) ->
  TopicConfig ->
  Topic ->
  X ()
switchTopic' viewMethod tg topic = do
  windows $ viewMethod topic
  wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  when (null wins) $ TS.topicAction tg topic

spawnShell :: Host -> X ()
spawnShell host = currentTopicDir (myTopicConfig host) >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminal ++ " -title urxvt -e sh -c 'cd ''" ++ dir ++ "'' && " ++ myShell ++ "'"

delay :: X ()
delay = io (threadDelay 0) -- I no longer remember what this is for

goto :: Host -> Topic -> X ()
goto host t = do
  delay
  let hostView = case host of
        Desktop _ -> W.greedyView
        _ -> W.view
  switchHook $ switchTopic' hostView (myTopicConfig host) t -- (22b)

promptedGoto :: Host -> X ()
promptedGoto = workspacePrompt myXPConfig . goto -- (27)

promptedGotoOtherScreen :: Host -> X ()
promptedGotoOtherScreen host =
  workspacePrompt myXPConfig $ \ws -> do
    -- (27)
    nextScreen
    goto host ws

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift -- (27)

scratchpadSize :: W.RationalRect
scratchpadSize = W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)

offsetRR :: (Rational, Rational) -> W.RationalRect -> W.RationalRect
offsetRR (dl, dt) (W.RationalRect l t w h) = W.RationalRect (l + dl) (t + dt) w h

customTerm :: String
customTerm = "urxvt"

scratchpads :: [NamedScratchpad]
scratchpads = zipWith (\o s -> s (customFloating (offsetRR o scratchpadSize))) offsets sps
 where
  step = 1 / 60
  n = length sps
  steps = map (subtract (step * (fromIntegral n / 2))) $ take n [0, step ..]
  offsets = zip steps (reverse steps)
  sps =
    [ NS "mixer" (customTerm ++ " -e alsamixer") (title =? "alsamixer")
    , NS "term" (customTerm ++ " -title term") (title =? "term")
    , NS "term2" (customTerm ++ " -title term2") (title =? "term2")
    , NS "fv" (customTerm ++ " -e /home/brent/.local/mybin/fv") (title =? "fv")
    , NS "ghci" (customTerm ++ " -title ghci -e /home/brent/.ghcup/bin/ghci") (title =? "ghci")
    , NS "top" (customTerm ++ " -e htop") (title =? "htop")
    , NS "cal" (customTerm ++ " -title cal -e sh -c 'ncal -3 -b && sleep 100'") (title =? "cal")
    , NS "timer" (customTerm ++ " -e timer 25m") (title =? "timer")
    , NS "ping" (customTerm ++ " -e ping google.com") (title =? "ping")
    , NS "blink" (customTerm ++ " -e blink-class") (title =? "blink-class")
    , NS "disco" (customTerm ++ " -title disco -e stack --stack-yaml /home/brent/projects/disco/stack.yaml exec disco") (title =? "disco")
    , NS "ozark" (customTerm ++ " -title ozark -e ssh ozark") (title =? "ozark")
    , NS "python" (customTerm ++ " -title python -e python3 -i") (title =? "python")
    ]

myDynamicLog h host =
  dynamicLogWithPP $
    byorgeyPP -- (1)
      { ppVisible = dzenColor "blue" "#a8a3f7" . pad
      , ppExtras =
          [ logCmd "TZ='America/Chicago' date +\"%a %b %d  %I:%M %p\""
          , loadAvg -- (28)
          ]
            ++ ( case host of
                  Laptop _ -> [battery]
                  _ -> []
               )
      , ppOrder = \(ws : l : t : exs) -> [t, l, ws] ++ exs -- (1)
      , ppOutput = hPutStrLn h -- (1,31)
      , ppTitle = shorten 60
      , ppSort = fmap (filterOutWs [scratchpadWorkspaceTag] .) DO.getSortByOrder
      , ppHiddenNoWindows = const ""
      }
 where
  battery :: Logger
  battery = logCmd "/usr/bin/acpi | tail -n 1 | sed -r 's/.*?: (.*%).*/\\1/; s/[dD]ischarging, ([0-9]+%)/\\1-/; s/[cC]harging, ([0-9]+%)/\\1+/; s/[cC]harged, //'"

-- my custom keybindings.
myKeys h host = myKeymap host (byorgeyConfig h host)

-- Lock down Noah's workspace.
noahProof :: Host -> XConfig l -> XConfig l
noahProof host conf =
  conf {keys = M.map disableForNoah . keys conf}
    `additionalKeysP` noahEscape
 where
  -- special key sequence to get out of Noah's workspace
  noahEscape = [("M-S-C-n M-S-C-o M-S-C-a M-S-C-h", goto host "web")]
  -- disable keybindings on Noah's workspace
  disableForNoah act = bindOn [("noah", return ()), ("", act)]

moveMode f =
  submap . M.fromList $
    [ ( (m, k)
      , do
          withFocused (f (c * dx, c * dy))
          moveMode f
      )
    | let big = 15
          small = 1
    , (k, dx, dy) <-
        [ (xK_h, -1, 0)
        , (xK_l, 1, 0)
        , (xK_k, 0, -1)
        , (xK_j, 0, 1)
        ]
    , (c, m) <- [(big, 0), (small, controlMask)]
    ]

hiddenNonEmpty = hiddenWS :&: Not emptyWS

myKeymap :: Host -> XConfig l -> [(String, X ())]
myKeymap host conf =
  [("M-S-q q", io exitSuccess)]
    ++
    -- mod-[1..],       Switch to workspace N
    -- mod-shift-[1..], Move client to workspace N
    -- mod-ctrl-[1..],  Switch to workspace N on other screen
    [ (m ++ "M-" ++ [k], f i) -- (0)
    | (i, k) <- zip (XMonad.workspaces conf) "1234567890-=[]\\" -- (0)
    , (f, m) <-
        [ (goto', "") -- (0a)
        , (windows . W.shift, "S-")
        , (\ws -> nextScreen >> goto' ws, "C-")
        ]
    ]
    ++ [ ("M-S-x", spawnShell host) -- (0)
       , ("M-S-b", spawn "$HOME/.local/mybin/urxvt-big")
       , ("M-g", promptedGoto host)
       , ("M-C-g", promptedGotoOtherScreen host)
       , ("M-S-g", promptedShift)
       ,
         ( "M-S-C-g"
         , workspacePrompt myXPConfig $ \ws ->
            -- (27)
            withAll' (W.shiftWin ws) >> goto host ws -- (22)
         )
       , ("M-S-t", setTouchpadState Nothing)
       , -- in conjunction with manageHook, open a small temporary
         -- floating terminal
         ("M-a a", namedScratchpadAction scratchpads "mixer")
       , ("M-a s", namedScratchpadAction scratchpads "term") -- (30)
       , ("M-a d", namedScratchpadAction scratchpads "term2")
       , ("M-a f", namedScratchpadAction scratchpads "fv")
       , ("M-a g", namedScratchpadAction scratchpads "ghci")
       , ("M-a t", namedScratchpadAction scratchpads "top")
       , ("M-a c", namedScratchpadAction scratchpads "cal")
       , ("M-a w", namedScratchpadAction scratchpads "timer")
       , ("M-a p", namedScratchpadAction scratchpads "ping")
       , ("M-a b", namedScratchpadAction scratchpads "blink")
       , ("M-a h", namedScratchpadAction scratchpads "python")
       , ("M-a o", namedScratchpadAction scratchpads "disco")
       ]
    -- sync using Unison in a new floating window, but only on my laptop
    ++ ( case host of
          Laptop _ ->
            [("M-a y", namedScratchpadAction scratchpads "sync")]
          _ -> []
       )
    ++ [ ("M-S-a", kill) -- (0)
       , ("M-S-C-a", killAll) -- (22)
       , -- some gap-toggling
         ("M-C-p b", sendMessage $ ToggleStrut D) -- (3)
       , ("M-C-p t", sendMessage $ ToggleStrut U) --  "
       , ("M-C-p a", sendMessage $ ToggleStruts) --  "
       , ("M-C-p g", sendMessage $ ToggleGaps) -- (15a)
       ]
    ++ [ ("M-C-p " ++ f ++ " <" ++ dk ++ ">", sendMessage $ m d) -- (15a)
       | (dk, d) <- [("L", L), ("D", D), ("U", U), ("R", R)]
       , (f, m) <- [("v", ToggleGap), ("h", IncGap 40), ("f", DecGap 10)]
       ]
    ++ [ ("M-m", moveMode keysMoveWindow)
       ]
    ++
    -- rotate workspaces.
    --    [ ("M-C-<R>",   nextWS )                   -- (16)
    --    , ("M-C-<L>",   prevWS )                   --  "
    [ ("M-C-<R>", DO.swapWith Next (Not emptyWS)) -- (22e)
    , ("M-C-<L>", DO.swapWith Prev (Not emptyWS)) -- "
    , ("M-S-<R>", DO.shiftTo Next hiddenNonEmpty) -- "
    , ("M-S-<L>", DO.shiftTo Prev hiddenNonEmpty) -- "
    , ("M-<R>", delay >> switchHook (DO.moveTo Next hiddenNonEmpty)) -- "
    , ("M-<L>", delay >> switchHook (DO.moveTo Prev hiddenNonEmpty)) -- "
    , ("M-f", switchHook $ newCodeWS) -- see below
    , -- Don't want to kill default M-r binding now that I have 3
      -- monitors If I want these later, find new keybindings for them.
      -- I never used them often enough to remember the keybindings
      -- though.

      -- expand/shrink windows
      -- , ("M-r k", sendMessage MirrorExpand)                       -- (5)
      -- , ("M-r j", sendMessage MirrorShrink)                       -- (5)
      -- , ("M-r h", sendMessage Shrink)                             -- (0)
      -- , ("M-r l", sendMessage Expand)                             -- (0)

      -- switch to previous workspace
      ("M-z", delay >> switchHook toggleWS) -- (16)
    , -- cycle workspaces in most-recently-used order
      -- see definition of custom cycleRecentWS' below, and also     (17)

      ( "M-S-<Tab>"
      , switchHook $
          cycleRecentWS' [xK_Super_L, xK_Shift_L] xK_Tab xK_grave
      )
    , -- close all windows on current workspace and move to next
      ("M-S-z", switchHook $ killAll >> DO.moveTo Prev hiddenNonEmpty) -- (22, 22e)
    , -- dynamic workspace bindings
      ("M-n", addWorkspacePrompt myXPConfig) -- (22c)
    , ("M-S-n", renameWorkspace myXPConfig) -- "
    , ("M-C-r", switchHook $ removeWorkspace) -- "
    , ("M-C-S-r", switchHook $ killAll >> removeWorkspace) --
    ]
    -- xinerama screen numbers seem to be backwards in my setup
    ++ [ ("M-" ++ m ++ [key], screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip "rew" [0 ..]
       , (f, m) <- [(W.view, ""), (W.shift, "S-")]
       ]
    ++
    -- Swap workspaces
    [ ("M-C-t", switchHook $ swapNextScreen) -- Still useful with 2 monitors sometimes
    ]
    ++
    -- move between screens
    [ ("M-s", switchHook $ nextScreen)
    , -- , ("M-w", switchHook $ swapNextScreen)    -- These aren't so useful now with 3 monitors
      -- , ("M-e", switchHook $ shiftNextScreen)   -- I have M-{w,e,r} bound to 1 screen each

      ("M-S-d d", spawn "~/.screenlayout/dual.sh")
    , ("M-S-d h", spawn "~/.screenlayout/hybrid.sh")
    , ("M-S-d n", spawn "~/local/mybin/display-normal")
    , -- Move the current window to the "view" workspace and switch to
      -- that workspace on the other monitor. A common operation I do
      -- with PDFs.

      ( "M-v"
      , do
          windows (W.shift "view")
          nextScreen
          goto' "view"
          nextScreen
      )
    , -- lock the screen with xscreensaver
      ("M-S-l", spawn "xscreensaver-command -lock") -- (0)
    ,
      ( "M-S-s"
      , spawn $
          case host of
            Laptop _ -> "pm-suspend"
            _ -> "systemctl suspend"
      )
    , -- bainsh and unbanish the pointer
      ("M-'", banishScreen LowerRight) -- (18)
    , ("M-b", warpToWindow (1 / 2) (1 / 2))
    , -- some programs to start with keybindings.
      ("M-x f", spawnOn "web" "firefox") -- (22a)
    , ("M-x c", spawnOn "web" "chromium-browser")
    , ("M-x g", spawnOn "draw" "gimp") -- "
    , ("M-x m", spawn "rhythmbox") -- (0)
    , ("M-x t", spawn "gnome-terminal --hide-menubar") -- (0)
    , ("M-x S-g", spawn "javaws ~/playing/go/cgoban.jnlp") -- (0)
    , ("M-x n", goto' "org")
    , ("M-x r", goto' "Roam")
    , ("M-x e", switchHook $ runOrRaise "evince" (className =? "Evince"))
    , ("M-x l", spawn "evince ~/documents/personal/liturgy/Psalm90.pdf")
    , -- configuration.
      ("M-c x", goto' "xm-conf")
    , ("M-c e", goto' "em-conf")
    , ("M-c t", goto' "tex-conf")
    ]
    ++ ( case host of
          Laptop _ -> [("M-c n", goto' "net")]
          _ -> []
       )
    ++ [ ("M-c v", spawn "urxvt -e alsamixer -c 0") -- (0)
       , ("M-c k", spawn "xkill")
       , ("M-c M-S-a", killAll)
       , ("C-<R>", windowGo R False)
       , ("C-<L>", windowGo L False)
       , ("C-<U>", windowGo U False)
       , ("C-<D>", windowGo D False)
       , ("S-C-<R>", windowSwap R False)
       , ("S-C-<L>", windowSwap L False)
       , ("S-C-<U>", windowSwap U False)
       , ("S-C-<D>", windowSwap D False)
       , -- switch to urgent window
         ("M-u", focusUrgent)
       , ("M-C-u", clearUrgents)
       , -- toggles: fullscreen, flip x, flip y, mirror, no borders
         ("M-C-<Space>", sendMessage $ Toggle NBFULL) -- (14)
       , ("M-C-x", sendMessage $ Toggle REFLECTX) -- (14,13)
       , ("M-C-y", sendMessage $ Toggle REFLECTY) -- (14,13)
       , ("M-C-m", sendMessage $ Toggle MIRROR) --  "
       , ("M-C-b", sendMessage $ Toggle NOBORDERS) --  "
       , -- some prompts.
         -- ability to change the working dir for a workspace.
         -- , ("M-p d", changeDir myXPConfig)                           -- (11)

         -- man page prompt
         ("M-p m", manPrompt myXPConfig) -- (24)
         -- add single lines to my NOTES file from a prompt.       -- (25)
       , ("M-p n", appendFilePrompt myXPConfig "$HOME/NOTES")
       , -- shell prompt.
         ("M-p s", sshPrompt myXPConfig) -- (26)
       , ("M-p e", spawn "exe=`echo | yeganesh -x` && eval \"exec $exe\"")
       , ("M-p u", unicodePrompt "/usr/share/unicode/UnicodeData.txt" myXPConfig)
       , -- some searches.
         ("M-/", submap . mySearchMap $ myPromptSearch) -- (19,20)
       , ("M-C-/", submap . mySearchMap $ mySelectSearch) -- (19,20)
       , -- some random utilities.
         ("M-C-c", spawn "dzen-cal") -- calendar
       , ("<Print>", spawn "scrot")
       , ("C-<Print>", spawn "sleep 0.2; scrot -s")
       , ("M-y n", promptWSGroupAdd myXPConfig "Name this group: ") -- (22d)
       , ("M-y g", switchHook $ promptWSGroupView myXPConfig "Go to group: ") -- (22d)
       , ("M-y d", promptWSGroupForget myXPConfig "Forget group: ") -- (22d)
       , -- volume and music control.
         ("M-S-C-0", spawn "amixer -q -c 0 set Master toggle") -- <XF86AudioMute>
       , ("M-S-C--", spawn "amixer -q -c 0 set Master 5%- unmute") -- <XF86AudioLowerVolume>
       , ("M-S-C-=", spawn "amixer -q -c 0 set Master 5%+ unmute") -- <XF86AudioRaiseVolume>
       , ("<XF86AudioLowerVolume>", spawn "amixer -q -c 0 set Master 5%- unmute")
       , ("<XF86AudioRaiseVolume>", spawn "amixer -q -c 0 set Master 5%+ unmute")
       , ("<XF86AudioPlay>", spawn "cmus-remote -u")
       ]
    ++ [("M-S-C-" ++ [k], spawn ("cmus-remote -" ++ [c])) | [k, c] <- ["zr", "xp", "cu", "vs", "bn"]]
    ++ [
         -- backlight control.
         ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
       , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
       , -- disable browser back/forward keys.
         ("<XF86Back>", return ())
       , ("<XF86Forward>", return ())
       , -- diagrams github shortcuts
         ("M-d c", diagramsGithub "core")
       , ("M-d l", diagramsGithub "lib")
       , ("M-d o", diagramsGithub "cairo")
       , ("M-d s", diagramsGithub "svg")
       , ("M-d p", diagramsGithub "postscript")
       , ("M-d d", diagramsGithub "doc")
       , ("M-d b", diagramsGithub "contrib")
       , ("M-d h", diagramsGithub "haddock")
       , ("M-d r", diagramsGithub "builder")
       , ("M-d a", visitGithub "diagrams/active")
       , ("M-d m", visitGithub "diagrams/monoid-extras")
       , -- lockdown mode
         ("M-C-l l", toggleLockdown)
         -- , ("M-C-l o c k d o w n 2 1 4 3 6 5 8 7 0 9", releaseLockdown)
       ]
 where
  goto' = goto host

-- Find the first empty workspace named "code<i>" for <i> some integer,
-- or create a new one
newCodeWS :: X ()
newCodeWS = withWindowSet $ \w -> do
  let wss = W.workspaces w
      cws = map W.tag $ filter (\ws -> "code" `isPrefixOf` W.tag ws && isJust (W.stack ws)) wss
      num = head $ [0 :: Integer ..] \\ catMaybes (map (readMaybe . drop 4) cws)
      new = "code" ++ show num
  when (not $ new `elem` (map W.tag wss)) $ addWorkspace new
  windows $ W.view new
 where
  readMaybe s = case reads s of
    [(r, _)] -> Just r
    _ -> Nothing

-- modified variant of cycleRecentWS from XMonad.Actions.CycleRecentWS (17)
-- which does not include visible but non-focused workspaces in the cycle
cycleRecentWS' :: [KeySym] -> KeySym -> KeySym -> X ()
cycleRecentWS' = cycleWindowSets recentTags
 where
  options w = map (W.view `flip` w) (recentTags w)
  recentTags w = map W.tag $ W.hidden w ++ [W.workspace (W.current w)]

-- Perform a search, using the given method, based on a keypress
mySearchMap :: (SearchEngine -> a) -> M.Map (KeyMask, KeySym) a
mySearchMap method =
  M.fromList $ -- (0b)
    [ ((0, xK_g), method google) -- (20)
    , ((0, xK_w), method wikipedia) --  "
    , ((0, xK_h), method hoogle) --  "
    , ((shiftMask, xK_h), method hackage) --  "
    , ((0, xK_s), method scholar) --  "
    , ((0, xK_m), method mathworld) --  "
    , ((0, xK_p), method maps) --  "
    , ((0, xK_a), method alpha) --  "
    , ((0, xK_l), method lucky) --  "
    , ((0, xK_z), method amazon) --  "
    , -- custom searches (see below)
      ((0, xK_i), method images)
    , ((0, xK_k), method greek)
    , ((0, xK_d), method merriamWebster)
    , ((0, xK_b), method bibleGateway)
    , ((0, xK_n), method stepNT)
    , ((0, xK_o), method stepOT)
    , ((shiftMask, xK_g), method leoGerman)
    , ((shiftMask, xK_k), method kattis)
    ]

-- Search Perseus for ancient Greek dictionary entries
greek :: SearchEngine
greek = searchEngine "greek" "http://www.perseus.tufts.edu/hopper/morph?la=greek&l="

-- for some strange reason the image search that comes with the Search module
-- is for google.fr
images :: SearchEngine
images = searchEngine "images" "http://www.google.com/search?hl=en&tbm=isch&q="

-- Use Merriam-Webster instead of the default dictionary search
merriamWebster :: SearchEngine
merriamWebster = searchEngine "dict" "http://www.merriam-webster.com/dictionary/"

bibleGateway :: SearchEngine
bibleGateway = searchEngine "bible" "http://www.biblegateway.com/quicksearch/?quicksearch="

stepNT :: SearchEngine
stepNT = searchEngineF "step-NT" $ \q ->
  "https://www.stepbible.org/?q=version=ESV|reference=" ++ q ++ "|version=SBLG&options=HVGUN&display=COLUMN"

stepOT :: SearchEngine
stepOT = searchEngineF "step-OT" $ \q ->
  "https://www.stepbible.org/?q=version=ESV|version=OHB|reference=" ++ q ++ "|version=LXX&options=HVGUN&display=COLUMN"

leoGerman :: SearchEngine
leoGerman = searchEngine "leo" "https://dict.leo.org/ende/index_de.html#/search="

kattis :: SearchEngine
kattis = searchEngine "kattis" "https://open.kattis.com/problems/"

-- Prompt search: get input from the user via a prompt, then
--   run the search in firefox and automatically switch to the web
--   workspace
myPromptSearch :: SearchEngine -> X ()
myPromptSearch (SearchEngine _ site) =
  inputPrompt myXPConfig "Search" ?+ \s ->
    -- (27)
    switchHook (search "firefox" site s >> viewWeb) -- (0,20)

-- Select search: do a search based on the X selection
mySelectSearch :: SearchEngine -> X ()
mySelectSearch eng = switchHook (selectSearch eng >> viewWeb) -- (20)

-- Switch to the "web" workspace
viewWeb :: X ()
viewWeb = switchHook $ windows (W.view "web") -- (0,0a)

diagramsGithub :: String -> X ()
diagramsGithub = visitGithub . ("diagrams/diagrams-" ++)

visitGithub :: String -> X ()
visitGithub r = safeSpawn "firefox" ["http://github.com/" ++ r] >> viewWeb

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig :: XPConfig
myXPConfig =
  def -- (23)
    { fgColor = "#a8a3f7"
    , bgColor = "#3f3c6d"
    , font = "xft:Monospace:pixelsize=12"
    , promptKeymap = emacsLikeXPKeymap' myWordSep
    , historyFilter = deleteConsecutive
    }

myWordSep :: Char -> Bool
myWordSep c = isSpace c || c == '/'

-- Set up a customized manageHook (rules for handling windows on
--   creation)
myManageHook :: ManageHook -- (0)
myManageHook =
  composeAll $
    -- auto-float certain windows
    [className =? c --> doCenterFloat | c <- myFloats] -- (4)
      ++ [fmap (t `isPrefixOf`) title --> doFloat | t <- myFloatTitles]
      ++
      -- send certain windows to certain workspaces
      [ className =? "Rhythmbox" --> doF (W.shift "music") -- (0,0a)
      , className =? "Keybase" --> doF (W.shift "keybase")
      , -- unmanage docks such as gnome-panel and dzen
        manageDocks -- (3)
        -- manage the scratchpad terminal window
      , namedScratchpadManageHook scratchpads -- (30)
      , appName =? "xbuffy-main" --> doFloatAt 0.92 0.66
      , appName =? "xbuffy-aux" --> doFloatAt 0.92 0.81
      , className =? "Microsoft Teams Notification" --> doFloat
      , appName =? "Caml graphics" --> doFloat
      ]
 where
  -- windows to auto-float
  myFloats =
    [ "Volume"
    , "XClock"
    , "Network-admin"
    , "Xmessage"
    , "gnome-search-tool"
    , "Qjackctl.bin"
    , "Icfp"
    , "Floating"
    , "Game"
    , "Caml graphics"
    , "Sifflet"
    , "Gimp"
    ]
  myFloatTitles =
    [ "Bridge Bid"
    , "Pong"
    , "Floating"
    , "alsamixer"
    , "TagTime"
    , "Caml graphics"
    ]

-- specify a custom layout hook.
myLayoutHook host =
  -- automatically avoid overlapping my dzen status bar.
  avoidStruts $ -- (3)

    -- make manual gap adjustment possible.
    gaps (zip [U, D, L, R] (repeat 0)) $
      -- start all workspaces in my home directory, with the ability
      -- to switch to a new working dir.                          -- (10,11)
      -- workspaceDir "~" $
      -- XXX this doesn't seem to be working, 29 August 2013

      {-
          -- navigate directionally rather than with mod-j/k
          configurableNavigation (navigateColor "#00aa00") $          -- (8)
      -}

      -- ability to toggle between fullscreen, reflect x/y, no borders,
      -- and mirrored.
      mkToggle1 NBFULL $ -- (14)
        mkToggle1 REFLECTX $ -- (14,13)
          mkToggle1 REFLECTY $ -- (14,13)
            mkToggle1 NOBORDERS $ --  "
              mkToggle1 MIRROR $ --  "

                -- borders automatically disappear for fullscreen windows.
                smartBorders $ -- (7)

                  -- "web", "irc", and "view" start in Full mode and can switch to tiled...
                  onWorkspaces ["web", "irc", "view"] (Full ||| myTiled host) $ -- (10,0)

                    -- ...whereas all other workspaces start tall and can switch
                    -- to a grid layout with the focused window magnified.
                    myTiled host
                      ||| TwoPane (3 / 100) (1 / 2) -- resizable tall layout

-- use ResizableTall instead of Tall, but still call it "Tall".
-- myTiled (Desktop True)  = named "Tall" $ ThreeCol 1 (3/100) (1/2)
myTiled _ = named "Tall" $ ResizableTall 1 0.03 0.5 [] -- (9,5)

{-
findTag :: (a -> Bool) -> W.StackSet a l a1 s sd -> Maybe a
findTag p = find p . map W.tag . W.workspaces

selectWorkspace' :: XPConfig -> X ()
selectWorkspace' conf = workspacePrompt conf $ \w ->
                        do s <- gets windowset
                           case findTag (w `isPrefixOf`) s of
                             Just w' -> windows $ W.greedyView w'
                             Nothing -> return ()

-- Improved version of followOnlyIf from MagicFocus
followOnlyIfQ :: Query Bool -> Event -> X All
followOnlyIfQ cond e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify && ev_mode e == notifyNormal
    = whenX (runQuery cond w) (focus w) >> return (All False)
followOnlyIfQ _ _ = return $ All True
-}

-- Focus follows mouse only for Gimp windows
whenToFollow :: Query Bool
whenToFollow = (className =? "Gimp")

queryFocused :: Query Bool -> X Bool
queryFocused q = withWindowSet $ maybe (return False) (runQuery q) . W.peek

------------------------------------------------------------
-- Lockdown mode (for Getting Work Done)

data LockdownState = LockdownState Bool
  deriving (Typeable, Read, Show)

instance ExtensionClass LockdownState where
  initialValue = LockdownState False
  extensionType = PersistentExtension

setLockdown :: X ()
setLockdown = XS.put (LockdownState True)

releaseLockdown :: X ()
releaseLockdown = XS.put (LockdownState False)

toggleLockdown :: X ()
toggleLockdown = XS.modify (\(LockdownState l) -> LockdownState (not l))

-- | Perform the given action only if not on lockdown
withLockdown :: X () -> X ()
withLockdown act = do
  LockdownState l <- XS.get
  when (not l) act

------------------------------------------------------------
-- Touchpad

-- | Set the touchpad state to the given Bool value (True = on, False
--   = off), or toggle it if given Nothing.
setTouchpadState :: Maybe Bool -> X ()
setTouchpadState s = do
  sc <- (map words . lines) <$> liftIO (runProcessWithInput "/usr/bin/synclient" ["-l"] "")
  case find ((== ["TouchpadOff"]) . take 1) sc of
    Just [_, _, b] -> do
      let b' :: Int
          b' = case s of
            Just False -> 1
            Just True -> 0
            Nothing -> 1 - read b
      when (b' == 0) $ liftIO (threadDelay (3 * 1000000))
      spawn ("synclient TouchpadOff=" ++ show b')
      if (b' == 1)
        then banishScreen LowerRight
        else (when ((read b :: Int) == 1) $ warpToWindow (1 / 2) (1 / 2))
    Nothing -> do
      return ()

------------------------------------------------------------
-- Workspace switching hook

switchHook :: X () -> X ()
switchHook = withLockdown

-- . (setTouchpadState (Just False) >>)
