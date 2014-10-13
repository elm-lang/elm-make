module TheMasterPlan where
{-| I'm trying something a little weird here. This file models each step in
the build process, so you will see a sequence of types representing "all the
data we have so far" formatted in a way that will be nice for the next stage.

The idea is that our implementation should be guiding us between these models.
-}

import qualified Data.Map as Map
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Name as Pkg


-- UNIQUE IDENTIFIERS FOR MODULES

data ModuleID = ModuleID
    { packageName :: Pkg.Name
    , moduleName :: Module.Name
    }
    deriving (Eq, Ord)


-- CRAWL AN INDIVIDUAL PACKGE

{-| Basic information about all modules that are part of a certain package.
We obtain this information by doing a depth first search starting with a
file or package description.

  * packageData
      file path to module and modules depended upon
  * packageForeignDependencies
      any foreign modules that are needed locally and which package owns them

-}
data PackageSummary = PackageSummary
    { packageData :: Map.Map Module.Name PackageData
    , packageForeignDependencies :: Map.Map Module.Name Pkg.Name
    }

data PackageData = PackageData
    { packagePath :: FilePath
    , packageDepenencies :: [Module.Name]
    }


-- COMBINE ALL PACKAGE SUMMARIES

{-| Very similar to a PackageSummary, but we now have made each module name
unique by adding which package it comes from. This makes it safe to merge a
bunch of PackageSummaries together, so we can write the rest of our code
without thinking about package boundaries.
-}
type ProjectSummary a =
    Map.Map ModuleID (ProjectData a)

data ProjectData a = ProjectData
    { projectLocation :: a
    , projectDependencies :: [ModuleID]
    }

data Location = Location
    { package :: Pkg.Name
    , relativePath :: FilePath
    }


-- BUILD-FRIENDLY SUMMARY

{-| Combines the ProjectSummary with all cached build information. At this
stage we crawl any cached interface files. File changes may have invalidated
these cached interfaces, so we filter out any stale interfaces.

The resulting format is very convenient for managing parallel builds.
-}
type BuildSummary =
    Map.Map ModuleID BuildData


{-| Everything you need to know to build a file.

  * blocking - modules I depend upon that are not ready yet
  * ready - modules I depend upon that are ready to go
  * location - location of source code for when its time to compile

We move modules from 'blocking' to 'ready' as the build progresses and
interfaces are produced. When 'blocking' is empty, it is safe to add this
module to the build queue.
-}
data BuildData = BuildData
    { blocking :: [ModuleID]
    , ready :: Map.Map ModuleID Module.Interface
    , location :: Location
    }

