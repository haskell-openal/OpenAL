--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.StringQueries
-- Copyright   :  (c) Sven Panne 2003-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.1.2 (String Queries) of the OpenAL
-- Specification and Reference (version 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.StringQueries (
   alVendor, alRenderer
) where

import Data.StateVar
import Sound.OpenAL.AL.QueryUtils ( StringName(..), getString )

--------------------------------------------------------------------------------

-- | Contains the name of the vendor.

alVendor :: GettableStateVar String
alVendor = makeGettableStateVar (getString Vendor)

-- | Contains information about the specific renderer.

alRenderer :: GettableStateVar String
alRenderer = makeGettableStateVar (getString Renderer)
