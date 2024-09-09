{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

import BuildScript
import Data.Map qualified as Map
import Data.Text qualified as T
import Relude

rebuildSystem :: (MonadIO m) => m ()
rebuildSystem = do
  logInfo "🔨 begin rebuilding the NixOS + HomeManager configuration from JJBA dotfiles"
  logLicense
  runCommand
    (Description "🔨 formatting Nix files in the project")
    (Command "find . -name '*.nix' -exec nixfmt {} \\;")
  runCommand
    (Description "🔨 linting Nix files in the project")
    (Command "statix check")
  runCommand
    (Description "🔨 checking for dead code in Nix files in the project")
    (Command "deadnix")
  removePaths
    . map Path
    $ [ "/etc/nixos/users",
        "/etc/nixos/sys",
        "/etc/nixos/flake.nix",
        "/etc/nixos/flake.lock",
        "/etc/nixos/configuration.nix"
      ]
  copyPaths
    . Map.fromList
    . map (bimap Path Path)
    $ [ ("*.nix", "/etc/nixos/"),
        ("flake.lock", "/etc/nixos/flake.lock"),
        ("COPYING", "/etc/nixos/"),
        ("README.org", "/etc/nixos/"),
        ("users", "/etc/nixos/users"),
        ("sys", "/etc/nixos/sys")
      ]
  runCommand
    (Description "🔨 rebuilding NixOS system + HomeManager flake")
    (Command "sudo nixos-rebuild switch -L --verbose --show-trace --impure --flake '/etc/nixos#nixos'")
  runCommand
    (Description "🔨 list files in /etc/nixos")
    (Command "ls -lAh /etc/nixos")
  runCommand
    (Description "🔨 send success notification to user")
    (Command "notify-send 'Successful rebuilding of NixOS system + Home Manager flake'")
  runCommand
    (Description "🔨 restart Emacs daemon")
    (Command "systemctl --user --no-pager restart emacs")
  runCommand
    (Description "🔨 check Emacs daemon status")
    (Command "systemctl --user --no-pager status emacs")    

updateSystem :: (MonadIO m) => m ()
updateSystem = do
  logInfo "🔨 begin updating the NixOS + HomeManager configuration from JJBA dotfiles"
  logLicense
  runCommand
    (Description "🔨 updating flake inputs")
    (Command "nix flake update")
  runCommand
    (Description "🔨 fetching archive for flake inputs")
    (Command "nix flake archive")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [task] -> runTimed . runTask . T.pack $ task
    _ -> logError "❌ no suitable task has been found!"

runTask :: (MonadIO m) => Text -> m ()
runTask "rebuild-system" = rebuildSystem
runTask "update-system" = updateSystem
runTask _ = logError "❌ no suitable task has been found!"
