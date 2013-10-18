{-# LANGUAGE DeriveDataTypeable #-}

import           XMonad

import qualified XMonad.StackSet                       as W

import           Data.Char                             (isSpace)
import           Data.List                             ((\\))
import qualified Data.Map                              as M
import           Data.Maybe                            (catMaybes, isJust)

import           Control.Concurrent                    (threadDelay)
import           System.Posix.Unistd

import           XMonad.Layout.MagicFocus

-- Hooks -----------------------------------------------------

import           XMonad.Hooks.DynamicLog               hiding (pprWindowSet)
import           XMonad.Hooks.UrgencyHook
                                   --      on IRC
import           XMonad.Hooks.ManageDocks
                                   --      status bar with windows
import           XMonad.Hooks.ManageHelpers
                                   --      windows in the middle of the
                                   --      screen

-- Layout ----------------------------------------------------

import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WorkspaceDir
                                   --      per-workspace
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect

import           XMonad.Layout.Gaps
                                   --       of the screen useful for things
                                   --       like screencasts or projectors
                                   --       which cut off part of the screen

-- Actions ---------------------------------------------------

                                   -- (8)  navigate between windows
import           XMonad.Actions.Navigation2D

import           XMonad.Actions.CycleWS
                                   --      goodness
import           XMonad.Actions.CycleRecentWS
                                    --      in most-recently-used order
import           XMonad.Actions.Search                 hiding (Query, images)
import           XMonad.Actions.Submap
import           XMonad.Actions.Warp
                                   -- (20) some predefined web searches
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.WindowGo
import           XMonad.Actions.WithAll

import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.TopicSpace
                                   -- (22c)

import           XMonad.Actions.DynamicWorkspaceGroups
                                   -- (22d)

import qualified XMonad.Actions.DynamicWorkspaceOrder  as DO
                                   -- (22e)

import           XMonad.Actions.FloatKeys
import           XMonad.Actions.PerWorkspaceKeys

-- Prompts ---------------------------------------------------

import           XMonad.Prompt
import           XMonad.Prompt.AppendFile
import           XMonad.Prompt.Input
import           XMonad.Prompt.Man
import           XMonad.Prompt.Ssh
                                    --      making more generic search
                                    --      prompts than those in
                                    --      XMonad.Prompt.Search
import           XMonad.Prompt.Workspace

-- Utilities -------------------------------------------------

import           XMonad.Util.Loggers
                                    --      status bar
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run

import qualified XMonad.Util.ExtensibleState           as XS

import           Control.Monad                         (when)
                                                                -- (31)
main :: IO ()
main = do h <- spawnPipe "dzen2 -ta r -fg '#a8a3f7' -bg '#3f3c6d' -e 'onstart=lower'"
          host <- getHost
          checkTopicConfig (myTopicNames host) (myTopicConfig host) -- (22b)
          xmonad $ byorgeyConfig h host                         -- (0)

data Host = Desktop | Laptop Bool -- ^ Does the laptop have a Windows key?
  deriving (Eq, Read, Show)

getHost :: IO Host
getHost = do
  hostName <- nodeName `fmap` getSystemID
  return $ case hostName of
    "archimedes" -> Laptop True
    "euclid"     -> Laptop False
    "LVN513-12"  -> Desktop
    _            -> Desktop

myTerminal, myShell :: String
myTerminal = "urxvt --perl-lib ~/.urxvt -fg lightgrey -bg black +sb"
myShell = "zsh"

byorgeyConfig h host =
     noahProof host $
     withNavigation2DConfig def $
     myUrgencyHook $                         -- (2)
     def
       {
         borderWidth        = 2
       , terminal           = myTerminal
       , workspaces         = myTopicNames host
       , modMask            = if host == Laptop False
                                then modMask def
                                else mod4Mask

       , normalBorderColor  = "#dddddd"
       , focusedBorderColor = "#0033ff"
                                                                -- (22)
       , logHook            = myDynamicLog h host
       , manageHook         = manageSpawn
                              <+> myManageHook
                              <+> manageHook def
       , layoutHook         = myLayoutHook
       , focusFollowsMouse  = False

         -- XXX fixme: comment!                                 -- (29)
       , startupHook        = return () >>
                              checkKeymap (byorgeyConfig h host)
                                          (myKeys h host)

                                              -- (0c), and see below
       , handleEventHook    = followOnlyIf (queryFocused whenToFollow)
       }
       `additionalKeysP` (myKeys h host)                        -- (29)

-- have urgent events flash a yellow dzen bar with black text
myUrgencyHook = withUrgencyHook dzenUrgencyHook                 -- (2)
    { args = ["-bg", "yellow", "-fg", "black"] }

data TopicItem = TI { topicName   :: Topic   -- (22b)
                    , topicDir    :: Dir
                    , topicAction :: X ()
                    }

-- define some custom topics for use with the TopicSpace module.
myTopics :: Host -> [TopicItem]
myTopics host =
  [ TI "web" "" (spawnOn "web" "firefox")
  , TI "irc" "" (ircAction host)
  , TI "mail" "" (runInTerm "" "ssh en")
  , ti "read" "papers"
  , ti "write" "writing"
  , TI "org" "notes"
    (spawn "emacs --name org ~/notes/journal.org ~/notes/research.org")
  , TI "draw" "" (spawnOn "draw" "inkscape")
  , TI "xm-conf" ".xmonad"
    (edit "~/.xmonad/xmonad.hs" >>
     shell)
  , ti "xm-hack" "src/xmonad/XMonadContrib"
  , TI "em-conf" "" (edit "~/.emacs")
  , TI "music" "" (runInTerm "" "ssh milo")
  , TI "net" "" (spawnOn "net" "wicd-client -n" >>
                 runInTerm "" "sudo tail -f /var/log/wicd/wicd.log")
  , ti "conf" ""
  , ti "misc" ""
  , ti "500" "teaching/500/sf"
  , ti "ref" "documents/reference"
  , ti "play" ""
  , TI "tex-conf" "texmf/tex" (edit "~/texmf/tex/brent.sty")
  , ti "mlt" "writing/mlt"
  , ti "dia" "src/diagrams"
  , ti "dia-doc" "src/diagrams/doc"
  , ti "dia-core" "src/diagrams/core"
  , ti "dia-lib" "src/diagrams/lib"
  , ti "dia-cairo" "src/diagrams/cairo"
  , ti "dia-contrib" "src/diagrams/contrib"
  , ti "dia-svg" "src/diagrams/svg"
  , ti "dia-gtk" "src/diagrams/gtk"
  , ti "dia-canvas" "src/diagrams/canvas"
  , ti "dia-ps" "src/diagrams/postscript"
  , ti "dia-builder" "src/diagrams/builder"
  , ti "dia-haddock" "src/diagrams/haddock"
  , ti "dia-play" "src/diagrams/play"
  , ti "active" "src/diagrams/active"
  , ti "sp" "research/species/jc"
  , ti "pweb" "documents/sites/upenn"
  , ti "hask" "teaching/haskell"
  , TI "anki" "" (spawnOn "anki" "anki")
  , ti "CG" "documents/CG"
  , ti "replib" "src/replib"
  , ti "unbound" "src/replib/Unbound"
  , TI "video" "video" (spawnOn "video" "cinelerra")
  , ti "tc" "writing/typeclassopedia"
  , ti "aor" "teaching/aor"
  , ti "monoid-pearl" "src/diagrams/monoid-pearl"
  , ti "bl" "src/BlogLiterately"
  , TI "view" "" (return ())
  , TI "heb" "documents/bible/study"
    (edit "~/documents/bible/study/Hebrews.tex" >>
     spawn "evince ~/documents/bible/study/Hebrews.pdf")
  , TI "noah" "documents/noah/emacs" (edit "~/documents/noah/emacs/noah.txt")
  , ti "diss" "research/thesis"
  , ti "typmat" "research/species/type-matrices"
  , ti "hac" "documents/hac/2013"
  ]
  where
    -- Make a default topic item that just spawns a shell.
    ti t d = TI t d shell
    shell = spawnShell host

ircAction :: Host -> X ()
ircAction host = case host of
  Laptop _ -> runInTerm "" "ssh byorgey@eniac.seas.upenn.edu"
  Desktop  -> runInTerm "" "screen -dRR"

edit :: String -> X ()
edit = spawn . ("em "++)

myTopicNames :: Host -> [Topic]
myTopicNames = map topicName . myTopics

-- (22b)
myTopicConfig :: Host -> TopicConfig
myTopicConfig host = def
  { topicDirs = M.fromList $ map (\(TI n d _) -> (n,d)) myTopics'
  , defaultTopicAction = const (return ())
  , defaultTopic = "web"
  , maxTopicHistory = 10
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics'
  }
 where myTopics' = myTopics host

-- | Switch to the given topic, using the specified viewing method
--   (usually 'view' or 'greedyView').
switchTopic' :: (WorkspaceId -> WindowSet -> WindowSet)
                -> TopicConfig -> Topic -> X ()
switchTopic' viewMethod tg topic = do
  windows $ viewMethod topic

spawnShell :: Host -> X ()
spawnShell host = currentTopicDir (myTopicConfig host) >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminal ++ " -title urxvt -e sh -c 'cd ''" ++ dir ++ "'' && " ++ myShell ++ "'"

delay :: X ()
delay = io (threadDelay 0)  -- I no longer remember what this is for

goto :: Host -> Topic -> X ()
goto host t = do
  delay
  withLockdown $ switchTopic' W.view (myTopicConfig host) t  -- (22b)

promptedGoto :: Host -> X ()
promptedGoto = workspacePrompt myXPConfig . goto    -- (27)

promptedGotoOtherScreen :: Host -> X ()
promptedGotoOtherScreen host =
  workspacePrompt myXPConfig $ \ws -> do            -- (27)
    nextScreen
    goto host ws

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift  -- (27)

-- XXX offset scratchpad windows by a bit --- each one different?
scratchpadSize :: W.RationalRect
scratchpadSize = W.RationalRect (1/4) (1/4) (1/2) (1/2)

mySPFloat :: ManageHook
mySPFloat = customFloating scratchpadSize

customTerm :: String
customTerm = "urxvt-custom"

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "term"  (customTerm ++ " -title term") (title =? "term") mySPFloat
  , NS "term2" (customTerm ++ " -title term2") (title =? "term2") mySPFloat
  , NS "ghci"  (customTerm ++ " -e ghci") (title =? "ghci") mySPFloat
  , NS "sync"  (customTerm ++ " -e sy") (title =? "sy") mySPFloat
  , NS "top"   (customTerm ++ " -e htop") (title =? "htop") mySPFloat
  , NS "stopwatch" (customTerm ++ " -e utimer -s") (title =? "utimer") mySPFloat
  , NS "timer" (customTerm ++ " -e timer 25m") (title =? "timer") mySPFloat
  , NS "ping"  (customTerm ++ " -e ping google.com") (title =? "ping") mySPFloat
  ]

myDynamicLog h host = dynamicLogWithPP $ byorgeyPP              -- (1)
  { ppVisible = dzenColor "blue" "#a8a3f7" . pad
  , ppExtras = [ date "%a %b %d  %I:%M %p"                      -- (1,28)
               , loadAvg                                        -- (28)
               ]
               ++ (case host of Laptop _ -> [battery]
                                _        -> [])
  , ppOrder  = \(ws:l:t:exs) -> [t,l,ws]++exs                   -- (1)
  , ppOutput = hPutStrLn h                                      -- (1,31)
  , ppTitle  = shorten (case host of Laptop _ -> 45
                                     Desktop  -> 60)
  , ppSort   = fmap (namedScratchpadFilterOutWorkspace.) DO.getSortByOrder
  , ppHiddenNoWindows = const ""
  }

-- my custom keybindings.
myKeys h host = myKeymap host (byorgeyConfig h host)

-- Lock down Noah's workspace.
noahProof :: Host -> XConfig l -> XConfig l
noahProof host conf =
  conf { keys = \c -> M.map disableForNoah (keys conf c) }
    `additionalKeysP` noahEscape
  where
    -- special key sequence to get out of Noah's workspace
    noahEscape = [("M-S-C-n M-S-C-o M-S-C-a M-S-C-h", goto host "web")]
    -- disable keybindings on Noah's workspace
    disableForNoah act = bindOn [("noah", return ()), ("", act)]

myKeymap :: Host -> XConfig l -> [(String, X ())]
myKeymap host conf =

    -- mod-[1..],       Switch to workspace N
    -- mod-shift-[1..], Move client to workspace N
    -- mod-ctrl-[1..],  Switch to workspace N on other screen
    [ (m ++ "M-" ++ [k], f i)                                   -- (0)
        | (i, k) <- zip (XMonad.workspaces conf) "1234567890-=[]\\" -- (0)
        , (f, m) <- [ (goto', "")                    -- (0a)
                    , (windows . W.shift, "S-")
                    , (\ws -> nextScreen >> (goto' $ ws), "C-")
                    ]
    ]

    ++
    [ ("M-S-x", spawnShell host)                          -- (0)
    , ("M-S-b", spawn "urxvt-big")
    , ("M-g",   promptedGoto host)
    , ("M-C-g", promptedGotoOtherScreen host)
    , ("M-S-g", promptedShift)
    , ("M-S-C-g", workspacePrompt myXPConfig $ \ws ->          -- (27)
                    withAll' (W.shiftWin ws) >> goto host ws)  -- (22)

      -- in conjunction with manageHook, open a small temporary
      -- floating terminal
    , ("M-a s", namedScratchpadAction scratchpads "term")       -- (30)
    , ("M-a d", namedScratchpadAction scratchpads "term2")
    , ("M-a g", namedScratchpadAction scratchpads "ghci")
    , ("M-a t", namedScratchpadAction scratchpads "top")
    , ("M-a c", namedScratchpadAction scratchpads "stopwatch")
    , ("M-a w", namedScratchpadAction scratchpads "timer")
    , ("M-a p", namedScratchpadAction scratchpads "ping")
    ]
    ++

    -- move floating windows with keybindings                   -- (22f)
    [ ("M-a " ++ m ++ "M-<" ++ dir ++ ">", withFocused (keysMoveWindow (dx,dy)))
      | let big   = 50
            small = 20
      , (dir,m,dx,dy) <- [ ("L", "", -small, 0)
                         , ("R", "", small, 0)
                         , ("U", "", 0, -small)
                         , ("D", "", 0, small)
                         , ("L", "M-a ", -big, 0)
                         , ("R", "M-a ", big, 0)
                         , ("U", "M-a ", 0, -big)
                         , ("D", "M-a ", 0, big)
                         ]
    ]

    -- sync using Unison in a new floating window, but only on my laptop
    ++ (case host of Laptop _ ->
                       [("M-a y", namedScratchpadAction scratchpads "sync")]
                     _ -> []
       )

    ++
    [ ("M-S-a", kill)                                           -- (0)
    , ("M-S-C-a", killAll)                                      -- (22)

    -- some gap-toggling
    , ("M-C-p b", sendMessage $ ToggleStrut D)                    -- (3)
    , ("M-C-p t", sendMessage $ ToggleStrut U)                    --  "
    , ("M-C-p a", sendMessage $ ToggleStruts)                     --  "

    , ("M-C-p g", sendMessage $ ToggleGaps)                       -- (15a)
    ]

    ++
    [ ("M-C-p " ++ f ++ " <" ++ dk ++ ">", sendMessage $ m d)     -- (15a)
        | (dk, d) <- [("L",L), ("D",D), ("U",U), ("R",R)]
        , (f, m)  <- [("v", ToggleGap), ("h", IncGap 40), ("f", DecGap 10)]
    ]

    ++
    -- rotate workspaces.
--    [ ("M-C-<R>",   nextWS )                   -- (16)
--    , ("M-C-<L>",   prevWS )                   --  "
    [ ("M-C-<R>",   DO.swapWith Next NonEmptyWS)                -- (22e)
    , ("M-C-<L>",   DO.swapWith Prev NonEmptyWS)                -- "
    , ("M-S-<R>",   DO.shiftTo Next HiddenNonEmptyWS)           -- "
    , ("M-S-<L>",   DO.shiftTo Prev HiddenNonEmptyWS)           -- "
    , ("M-<R>",     delay >> withLockdown (DO.moveTo Next HiddenNonEmptyWS))   -- "
    , ("M-<L>",     delay >> withLockdown (DO.moveTo Prev HiddenNonEmptyWS))   -- "
    , ("M-f",       withLockdown $ newCodeWS)                   -- see below

    -- expand/shrink windows
    , ("M-r k", sendMessage MirrorExpand)                       -- (5)
    , ("M-r j", sendMessage MirrorShrink)                       -- (5)
    , ("M-r h", sendMessage Shrink)                             -- (0)
    , ("M-r l", sendMessage Expand)                             -- (0)

    -- switch to previous workspace
    , ("M-z", delay >> withLockdown toggleWS)                   -- (16)

    -- cycle workspaces in most-recently-used order
    -- see definition of custom cycleRecentWS' below, and also     (17)
    , ("M-S-<Tab>", withLockdown
                  $ cycleRecentWS' [xK_Super_L, xK_Shift_L] xK_Tab xK_grave)

    -- close all windows on current workspace and move to next
    , ("M-S-z", killAll >> DO.moveTo Prev HiddenNonEmptyWS)     -- (22, 22e)

    -- dynamic workspace bindings
    , ("M-n", addWorkspacePrompt myXPConfig)                    -- (22c)
    , ("M-S-n", renameWorkspace myXPConfig)                     -- "
    , ("M-C-r", removeWorkspace)                                -- "
    , ("M-C-S-r", killAll >> removeWorkspace)                   --

    -- move between screens
    , ("M-s", nextScreen)
    , ("M-w", swapNextScreen)
    , ("M-e", shiftNextScreen)

      -- lock the screen with xscreensaver
    , ("M-S-l", spawn "xscreensaver-command -lock")             -- (0)

    , ("M-S-s", spawn "systemctl suspend")

    -- bainsh the pointer
    , ("M-'", banishScreen LowerRight)                          -- (18)
    , ("M-b", warpToWindow (1/2) (1/2))

    -- some programs to start with keybindings.
    , ("M-x f", spawnOn "web" "firefox")                        -- (22a)
    , ("M-x o", spawnOn "web" "opera")                          -- "
    , ("M-x g", spawnOn "draw" "gimp")                          -- "
    , ("M-x m", spawn "rhythmbox")                              -- (0)
    , ("M-x t", spawn "xclock -update 1")                       -- (0)
    , ("M-x S-g", spawn "javaws ~/playing/go/cgoban.jnlp")      -- (0)
    , ("M-x n", goto' "org")
    , ("M-x e", withLockdown $ runOrRaise "evince" (className =? "Evince"))
    , ("M-x x", spawnOn "web" "chromium")

    -- configuration.
    , ("M-c x", goto' "xm-conf")
    , ("M-c e", goto' "em-conf")
    , ("M-c t", goto' "tex-conf")
    ] ++
    (case host of Laptop _ -> [("M-c n", goto' "net")]
                  _        -> [])
    ++
    [ ("M-c v", spawn "urxvt -e alsamixer")                     -- (0)
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

    -- switch to urgent window
    , ("M-u", focusUrgent)

    -- toggles: fullscreen, flip x, flip y, mirror, no borders
    , ("M-C-<Space>", sendMessage $ Toggle NBFULL)              -- (14)
    , ("M-C-x",       sendMessage $ Toggle REFLECTX)            -- (14,13)
    , ("M-C-y",       sendMessage $ Toggle REFLECTY)            -- (14,13)
    , ("M-C-m",       sendMessage $ Toggle MIRROR)              --  "
    , ("M-C-b",       sendMessage $ Toggle NOBORDERS)           --  "

    -- some prompts.
      -- ability to change the working dir for a workspace.
      -- , ("M-p d", changeDir myXPConfig)                           -- (11)

      -- man page prompt
    , ("M-p m", manPrompt myXPConfig)                           -- (24)
      -- add single lines to my NOTES file from a prompt.       -- (25)
    , ("M-p n", appendFilePrompt myXPConfig "$HOME/NOTES")
      -- shell prompt.
    , ("M-p s", sshPrompt myXPConfig)                         -- (26)
    , ("M-p e", spawn "exe=`echo | yeganesh -x` && eval \"exec $exe\"")

      -- some searches.
    , ("M-/", submap . mySearchMap $ myPromptSearch)            -- (19,20)
    , ("M-C-/", submap . mySearchMap $ mySelectSearch)          -- (19,20)

    -- some random utilities.
    , ("M-C-c", spawn "dzen-cal")  -- calendar
    , ("<Print>", spawn "scrot")
    , ("C-<Print>", spawn "sleep 0.2; scrot -s")

    , ("M-y n", promptWSGroupAdd myXPConfig "Name this group: ")  -- (22d)
    , ("M-y g", promptWSGroupView myXPConfig "Go to group: ")     -- (22d)
    , ("M-y d", promptWSGroupForget myXPConfig "Forget group: ")  -- (22d)

    -- volume control.
    , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+ unmute")

--    , ("<XF86Display>", spawn "sudo pm-suspend")   -- this never worked very well

    -- disable browser back/forward keys.
    , ("<XF86Back>", return ())
    , ("<XF86Forward>", return ())

    -- diagrams github shortcuts
    , ("M-d c", diagramsGithub "core")
    , ("M-d l", diagramsGithub "lib")
    , ("M-d o", diagramsGithub "cairo")
    , ("M-d s", diagramsGithub "svg")
    , ("M-d p", diagramsGithub "postscript")
    , ("M-d d", diagramsGithub "doc")
    , ("M-d b", diagramsGithub "contrib")
    , ("M-d h", diagramsGithub "haddock")
    , ("M-d u", diagramsGithub "builder")
    , ("M-d a", github         "diagrams/active")
    , ("M-d m", github         "diagrams/monoid-extras")

    -- lockdown mode
    , ("M-C-l l", setLockdown)
    , ("M-C-l o c k d o w n 2 1 4 3 6 5 8 7 0 9", releaseLockdown)
    ]
  where goto' = goto host

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
 where readMaybe s = case reads s of
                       [(r,_)] -> Just r
                       _       -> Nothing

-- modified variant of cycleRecentWS from XMonad.Actions.CycleRecentWS (17)
-- which does not include visible but non-focused workspaces in the cycle
cycleRecentWS' :: [KeySym] -> KeySym -> KeySym -> X ()
cycleRecentWS' = cycleWindowSets options
 where options w = map (W.view `flip` w) (recentTags w)
       recentTags w = map W.tag $ W.hidden w ++ [W.workspace (W.current w)]

-- Perform a search, using the given method, based on a keypress
mySearchMap :: (SearchEngine -> a) -> M.Map (KeyMask, KeySym) a
mySearchMap method = M.fromList $                               -- (0b)
        [ ((0, xK_g), method google)                            -- (20)
        , ((0, xK_w), method wikipedia)                         --  "
        , ((0, xK_h), method hoogle)                            --  "
        , ((shiftMask, xK_h), method hackage)                   --  "
        , ((0, xK_s), method scholar)                           --  "
        , ((0, xK_m), method mathworld)                         --  "
        , ((0, xK_p), method maps)                              --  "
        , ((0, xK_a), method alpha)                             --  "
        , ((0, xK_l), method lucky)                             --  "

        -- custom searches (see below)
        , ((0, xK_i), method images)
        , ((0, xK_k), method greek)
        , ((0, xK_d), method merriamWebster)
        , ((0, xK_b), method bibleGateway)
        ]

-- Search Perseus for ancient Greek dictionary entries
greek :: SearchEngine
greek  = searchEngine "greek"  "http://www.perseus.tufts.edu/hopper/morph?la=greek&l="

-- for some strange reason the image search that comes with the Search module
-- is for google.fr
images :: SearchEngine
images = searchEngine "images" "http://www.google.com/search?hl=en&tbm=isch&q="

-- Use Merriam-Webster instead of the default dictionary search
merriamWebster :: SearchEngine
merriamWebster = searchEngine "dict" "http://www.merriam-webster.com/dictionary/"

bibleGateway :: SearchEngine
bibleGateway = searchEngine "bible" "http://www.biblegateway.com/quicksearch/?quicksearch="

-- Prompt search: get input from the user via a prompt, then
--   run the search in firefox and automatically switch to the web
--   workspace
myPromptSearch :: SearchEngine -> X ()
myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->                    -- (27)
      (search "firefox" site s >> viewWeb)                      -- (0,20)

-- Select search: do a search based on the X selection
mySelectSearch :: SearchEngine -> X ()
mySelectSearch eng = selectSearch eng >> viewWeb                -- (20)

-- Switch to the "web" workspace
viewWeb :: X ()
viewWeb = windows (W.view "web")                                -- (0,0a)

diagramsGithub :: String -> X ()
diagramsGithub = github . ("diagrams/diagrams-" ++)

github :: String -> X ()
github r = safeSpawn "firefox" ["http://github.com/" ++ r] >> viewWeb

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig :: XPConfig
myXPConfig = def                                                -- (23)
    { fgColor = "#a8a3f7"
    , bgColor = "#3f3c6d"
    , promptKeymap = emacsLikeXPKeymap' myWordSep
    , historyFilter = deleteConsecutive
    }

myWordSep :: Char -> Bool
myWordSep c = isSpace c || c == '/'

-- Set up a customized manageHook (rules for handling windows on
--   creation)
myManageHook :: ManageHook                                      -- (0)
myManageHook = composeAll $
                   -- auto-float certain windows
                 [ className =? c --> doCenterFloat | c <- myFloats ] -- (4)
                 ++
                 [ fmap (t `isPrefixOf`) title --> doFloat | t <- myFloatTitles ]
                 ++
                   -- send certain windows to certain workspaces
                 [ className =? "Rhythmbox" --> doF (W.shift "music") -- (0,0a)
                   -- unmanage docks such as gnome-panel and dzen
                 , manageDocks                                     -- (3)
                   -- manage the scratchpad terminal window
                 , namedScratchpadManageHook scratchpads           -- (30)
                 , appName =? "xbuffy-main" --> doFloatAt 0.92 0.66
                 , appName =? "xbuffy-aux"  --> doFloatAt 0.92 0.81
                 , appName =? "Caml graphics" --> doFloat
                 ]
    -- windows to auto-float
    where myFloats = [ "Volume"
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
myLayoutHook =

    -- automatically avoid overlapping my dzen status bar.
    avoidStrutsOn [U] $                                        -- (3)

    -- make manual gap adjustment possible.
    gaps (zip [U,D,L,R] (repeat 0)) $

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
    mkToggle1 NBFULL $                                  -- (14)
    mkToggle1 REFLECTX $                                -- (14,13)
    mkToggle1 REFLECTY $                                -- (14,13)
    mkToggle1 NOBORDERS $                               --  "
    mkToggle1 MIRROR $                                  --  "

    -- borders automatically disappear for fullscreen windows.
    smartBorders $                                              -- (7)

    -- "web", "irc", and "view" start in Full mode and can switch to tiled...
    onWorkspaces ["web","irc","view"] (Full ||| myTiled) $               -- (10,0)

    -- ...whereas all other workspaces start tall and can switch
    -- to a grid layout with the focused window magnified.
    myTiled |||           -- resizable tall layout
    TwoPane (3/100) (1/2)

-- use ResizableTall instead of Tall, but still call it "Tall".
myTiled = named "Tall" $ ResizableTall 1 0.03 0.5 []            -- (9,5)

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
  initialValue  = LockdownState False
  extensionType = PersistentExtension

setLockdown :: X ()
setLockdown = XS.put (LockdownState True)

releaseLockdown :: X ()
releaseLockdown = XS.put (LockdownState False)

-- | Perform the given action only if not on lockdown
withLockdown :: X () -> X ()
withLockdown act = do
  LockdownState l <- XS.get
  when (not l) act
