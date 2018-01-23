module Types.ConfiguredCabal where

import Development.Shake.Classes
import Types.Package
import GHC.Generics

data ConfiguredCabal = ConfiguredCabal
    { dependencies :: [PackageName]
    , name         :: PackageName
    , version      :: String
    -- , packageDesc  :: C.PackageDescription
    -- * used to be pkg Data
    , componentId  :: String
    , modules      :: [String]
    , otherModules :: [String]
    , synopsis     :: String
    , description  :: String
    , srcDirs      :: [String]
    , deps         :: [String]
    , depIpIds     :: [String]
    , depNames     :: [String]
    , depCompIds   :: [String]
    , includeDirs  :: [String]
    , includes     :: [String]
    , installIncludes :: [String] -- TODO: do we need this one?
    , extraLibs    :: [String]
    , extraLibDirs :: [String]
    , asmSrcs      :: [String]
    , cSrcs        :: [String]
    , cmmSrcs      :: [String]
    , dataFiles    :: [String]
    , hcOpts       :: [String]
    , asmOpts      :: [String]
    , ccOpts       :: [String]
    , cmmOpts      :: [String]
    , cppOpts      :: [String]
    , ldOpts       :: [String]
    , depIncludeDirs :: [String]
    , depCcOpts    :: [String]
    , depLdOpts    :: [String]
    , buildGhciLib :: Bool
    } deriving (Eq, Read, Show, Typeable, Generic)

instance Binary ConfiguredCabal

instance Hashable ConfiguredCabal where
    hashWithSalt salt = hashWithSalt salt . show

instance NFData ConfiguredCabal where
    rnf (ConfiguredCabal a b c d e f g h i j k l m n o p q r s t u v w x z y
          aa ab ac ad ae af)
      = a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` i `seq` j
        `seq` k `seq` l `seq` m `seq` n `seq` o `seq` p `seq` q `seq` r `seq` s `seq` t
        `seq` u `seq` v `seq` w `seq` x `seq` y `seq` z `seq` aa `seq` ab `seq` ac `seq` ad
        `seq` ae `seq` af `seq` ()
