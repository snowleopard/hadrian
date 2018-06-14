module Context.Path where

import Base
import Context.Type
import Hadrian.Expression

-- | The build directory of the current 'Stage'.
stageDir :: Context -> FilePath
stageDir Context {..} = stageString stage

-- | The build path of the current 'Stage'.
stagePath :: Context -> Action FilePath
stagePath context = buildRoot <&> (-/- stageDir context)

-- | The expression that evaluates to the build path of the current 'Stage'.
getStagePath :: Expr Context b FilePath
getStagePath = expr . stagePath =<< getContext

-- | The directory in 'buildRoot' containing build artifacts of a given 'Context'.
contextDir :: Context -> FilePath
contextDir Context {..} = stageString stage -/- pkgPath package

-- | The path to the directory in 'buildRoot' containing build artifacts of a
-- given 'Context'.
contextPath :: Context -> Action FilePath
contextPath context = buildRoot <&> (-/- contextDir context)

-- | The expression that evaluates to the path to the directory in 'buildRoot'
-- containing build artifacts of a given 'Context'.
getContextPath :: Expr Context b FilePath
getContextPath = expr . contextPath =<< getContext

-- | The directory in 'buildRoot' containing the object artifacts.
buildDir :: Context -> FilePath
buildDir context = contextDir context -/- "build"

-- | Path to the directory containing build artifacts of a given 'Context'.
buildPath :: Context -> Action FilePath
buildPath context = buildRoot <&> (-/- buildDir context)

-- | The expression that evaluates to the build path of the current 'Context'.
getBuildPath :: Expr Context b FilePath
getBuildPath = expr . buildPath =<< getContext
