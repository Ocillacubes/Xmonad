-- Ocillacubes' XMonad Configuration --

---- IMPORTS ----
-- Base --
import XMonad
import XMonad.Config.Desktop
import System.Exit

-- Hooks --
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition

-- Actions --
import XMonad.Actions.CycleWS

-- Layouts--

-- Data --

-- Utils --
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (safeSpawn)

-- Other -- Took this and the frame of the keybinds section from the arch wiki but it makes stuff work
import qualified XMonad.StackSet as W
import qualified Data.Map as M




---- VARIABLES ----
-- Program Vars --
termEmulator :: String
termEmulator = "kitty"


-- Appearance --
border_width :: Dimension
border_width = 3
border_color_normal :: String
border_color_normal = "#FFD6E4"
border_color_focused :: String
border_color_focused = "#C6A2E0"

-- Bar --
tenshiPP = defaultPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" -- This is copy pasted from the arch wiki for now, will change colors soon
                     , ppHidden = xmobarColor "#C98F0A" ""
                     , ppHiddenNoWindows = xmobarColor "#C9A34E" ""
                     , ppLayout = xmobarColor "#C9A34E" ""
                     , ppTitle =  xmobarColor "#C9A34E" "" . shorten 80
                     , ppSep = xmobarColor "#429942" "" " | "
                     }

-- Window Management --
hook_m = composeAll [ isFullscreen                   --> doFullFloat -- Rules on what programs should be floating
                    , className =? "mpv"             --> doFloat
                    , className =? "Gimp"            --> doFloat
                    , className =? "Trackma-gtk"     --> doFloat
                    , className =? "KeePassXC"       --> doFloat
                    ]


-- Workspace Names --
workspace_names = ["emacs", "net", "term", "file", "chat", "game", "gimp", "mpv", "μ's"]


-- Config --
epicConfig = defaultConfig { modMask = modKey
                       , borderWidth = border_width
                       , terminal = termEmulator
                       , normalBorderColor = border_color_normal
                       , focusedBorderColor = border_color_focused
                       , manageHook = hook_m
                       , workspaces = workspace_names
                       , keys = keyBinds
                       , startupHook = startupPrograms
                       }
---- KEYBINDINGS ----
modKey = mod4Mask -- Sets mod key to super
keyConfig :: XConfig Layout -> (KeyMask, KeySym)
keyConfig XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
keyBinds :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keyBinds conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

  -- Launch programs
  [ ((modMask,               xK_Return), safeSpawn (XMonad.terminal conf) [])
  , ((modMask,               xK_d     ), safeSpawn "dmenu_run"            [])
  , ((modMask,               xK_w     ), safeSpawn "qutebrowser"          [])
  , ((modMask .|. shiftMask, xK_w     ), safeSpawn "firefox"              [])
  , ((modMask,               xK_r     ), safeSpawn "pcmanfm"              [])
  , ((modMask,               xK_e     ), safeSpawn "emacs"                [])
  , ((modMask,               xK_p     ), safeSpawn "keepassxc"            [])
  , ((modMask,               xK_a     ), safeSpawn "trackma-gtk"          [])
  , ((modMask,               xK_c     ), safeSpawn "element-desktop"      [])
  , ((modMask,               xK_g     ), safeSpawn "gimp"                 [])

  -- Focus windows
  , ((modMask,               xK_j     ), windows W.focusDown)
  , ((modMask,               xK_k     ), windows W.focusUp)
  , ((modMask .|. shiftMask, xK_Return), windows W.focusMaster)


  -- Swap windows
  , ((modMask,               xK_space ), windows W.swapMaster)
  , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
  , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

  -- Change window size
  , ((modMask .|. shiftMask, xK_h     ), sendMessage Shrink)
  , ((modMask .|. shiftMask, xK_l     ), sendMessage Expand)

  -- Switch montiors
  , ((modMask,                xK_Right ), nextScreen)
  , ((modMask,                xK_Left  ), prevScreen)

  -- Kill programs
  , ((modMask,                xK_q     ), kill)

  -- Quit/restart XMonad
  , ((modMask .|. shiftMask, xK_Delete ), io (exitWith ExitSuccess))
  , ((modMask .|. shiftMask, xK_r      ), spawn "xmonad --recompile; xmonad --restart")
  ]
  ++ -- Copied from Arch wiki -- allows switch of workspaces/moving windows to other workspaces
  [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


---- FUNCTIONS ----
-- Main --
main :: IO ()
main = xmonad =<< statusBar cmd pp kb conf
    where
      cmd = "bash -c \"tee >(xmobar -x0) | xmobar -x1\""
      pp = tenshiPP
      kb = keyConfig
      conf = epicConfig

-- Launch Programs on startup --
startupPrograms :: X ()
startupPrograms = do
  spawnOnce "~/.fehbg &"
  spawnOnce "picom --config /home/ocilla/.config/picom/picom.conf -b &"
  spawnOnce "unclutter &"
  spawnOnce "flameshot &"
