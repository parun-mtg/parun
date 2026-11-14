{-# LANGUAGE Trustworthy #-}

-- | The parts of "System.Directory" Parun uses. Trustworthy because they are
-- plain file system actions in 'IO'; the directory package marks the module
-- neither Safe nor Trustworthy, so a Safe module cannot import it directly.
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
