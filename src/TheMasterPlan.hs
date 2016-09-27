module TheMasterPlan where
{-| I'm trying something a little weird here. This file models each step in
the build process, so you will see a sequence of types representing "all the
data we have so far" formatted in a way that will be nice for the next stage.

The idea is that our implementation should be guiding us between these models.
-}

import Data.Binary
import qualified Data.Map as Map
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg



-- UNIQUE IDENTIFIERS FOR MODULES


data CanonicalModule = CanonicalModule
    { package :: Package
    , name :: Module.Raw
    }
    deriving (Eq, Ord)


simplifyModuleName :: CanonicalModule -> Module.Canonical
simplifyModuleName (CanonicalModule (pkg,_) name) =
    Module.Canonical pkg name


type Package = (Pkg.Name, Pkg.Version)



-- CRAWL AN INDIVIDUAL PACKGE


{-| Basic information about all modules that are part of a certain package.
We obtain this information by doing a depth first search starting with a
file or package description.

  * packageData
      file path to module and modules depended upon
  * packageForeignDependencies
      any foreign modules that are needed locally and which package owns them

-}
data PackageGraph = PackageGraph
    { packageData :: Map.Map Module.Raw PackageData
    , packageNatives :: Map.Map Module.Raw FilePath
    , packageForeignDependencies :: Map.Map Module.Raw Package
    }


data PackageData = PackageData
    { packagePath :: FilePath
    , packageDepenencies :: [Module.Raw]
    }



-- COMBINE ALL PACKAGE SUMMARIES


{-| Very similar to a PackageGraph, but we now have made each module name
unique by adding which package it comes from. This makes it safe to merge a
bunch of PackageGraphs together, so we can write the rest of our code
without thinking about package boundaries.
-}
data ProjectGraph a = ProjectGraph
    { projectData :: Map.Map CanonicalModule (ProjectData a)
    , projectNatives :: Map.Map CanonicalModule Location
    }


data ProjectData a = ProjectData
    { projectLocation :: a
    , projectDependencies :: [CanonicalModule]
    }


data Location = Location
    { _relativePath :: FilePath
    , _package :: Package
    }



-- BUILD-FRIENDLY SUMMARY


{-| Combines the ProjectGraph with all cached build information. At this
stage we crawl any cached interface files. File changes may have invalidated
these cached interfaces, so we filter out any stale interfaces.

The resulting format is very convenient for managing parallel builds.
-}
data BuildGraph = BuildGraph
    { blockedModules :: Map.Map CanonicalModule BuildData
    , completedInterfaces :: Map.Map CanonicalModule Module.Interface
    }


{-| Everything you need to know to build a file.

  * blocking - modules I depend upon that are not ready yet
  * location - location of source code for when its time to compile

We remove modules from 'blocking' as the build progresses and interfaces are
produced. When 'blocking' is empty, it is safe to add this module to the build
queue.
-}
data BuildData = BuildData
    { blocking :: [CanonicalModule]
    , location :: Location
    }



-- BINARY


instance Binary CanonicalModule where
  get =
    CanonicalModule <$> get <*> get

  put (CanonicalModule pkg nm) =
    do  put pkg
        put nm


instance (Binary a) => Binary (ProjectGraph a) where
  get =
    ProjectGraph <$> get <*> get

  put (ProjectGraph elms jss) =
    do  put elms
        put jss


instance (Binary a) => Binary (ProjectData a) where
  get =
    ProjectData <$> get <*> get

  put (ProjectData locations dependencies) =
    put locations >> put dependencies


instance Binary Location where
  get =
    Location <$> get <*> get

  put (Location relative pkg) =
    do  put relative
        put pkg
