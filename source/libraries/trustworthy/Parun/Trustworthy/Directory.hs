{-# LANGUAGE Trustworthy #-}

-- | The filesystem operations Parun uses from @System.Directory@, which its
-- package does not mark Safe. Trustworthy because each is an ordinary 'IO'
-- action on paths, breaking neither type safety nor module abstraction.
module Parun.Trustworthy.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    removePathForcibly,
  )
where

import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    removePathForcibly,
  )
