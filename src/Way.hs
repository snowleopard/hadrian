module Way (
    WayUnit (..), Way, wayUnit, removeWayUnit, wayFromUnits, allWays,

    vanilla, profiling, dynamic, profilingDynamic, threaded, debug, logging,
    threadedDebug, threadedProfiling, threadedLogging, threadedDynamic,
    threadedDebugProfiling, threadedDebugDynamic, threadedProfilingDynamic,
    threadedLoggingDynamic, debugProfiling, debugDynamic, loggingDynamic,

    wayPrefix, waySuffix, hisuf, osuf, hcsuf, obootsuf, hibootsuf, ssuf
    ) where

import Way.Type

-- | Various combinations of RTS only ways.
threadedDebug, threadedProfiling, threadedLogging, threadedDynamic,
    threadedDebugProfiling, threadedDebugDynamic, threadedProfilingDynamic,
    threadedLoggingDynamic, debugProfiling, debugDynamic, loggingDynamic :: Way
threadedDebug            = wayFromUnits [Threaded, Debug]
threadedProfiling        = wayFromUnits [Threaded, Profiling]
threadedLogging          = wayFromUnits [Threaded, Logging]
threadedDynamic          = wayFromUnits [Threaded, Dynamic]
threadedDebugProfiling   = wayFromUnits [Threaded, Debug, Profiling]
threadedDebugDynamic     = wayFromUnits [Threaded, Debug, Dynamic]
threadedProfilingDynamic = wayFromUnits [Threaded, Profiling, Dynamic]
threadedLoggingDynamic   = wayFromUnits [Threaded, Logging, Dynamic]
debugProfiling           = wayFromUnits [Debug, Profiling]
debugDynamic             = wayFromUnits [Debug, Dynamic]
loggingDynamic           = wayFromUnits [Logging, Dynamic]

-- | All ways supported by the build system.
allWays :: [Way]
allWays =
    [ vanilla, profiling, dynamic, profilingDynamic, threaded, debug, logging
    , threadedDebug, threadedProfiling, threadedLogging, threadedDynamic
    , threadedDebugProfiling, threadedDebugDynamic, threadedProfilingDynamic
    , threadedLoggingDynamic, debugProfiling, debugDynamic, loggingDynamic ]

wayPrefix :: Way -> String
wayPrefix way | way == vanilla = ""
              | otherwise      = show way ++ "_"

waySuffix :: Way -> String
waySuffix way | way == vanilla = ""
              | otherwise      = "_" ++ show way

osuf, ssuf, hisuf, hcsuf, obootsuf, hibootsuf :: Way -> String
osuf      = (++ "o"      ) . wayPrefix
ssuf      = (++ "s"      ) . wayPrefix
hisuf     = (++ "hi"     ) . wayPrefix
hcsuf     = (++ "hc"     ) . wayPrefix
obootsuf  = (++ "o-boot" ) . wayPrefix
hibootsuf = (++ "hi-boot") . wayPrefix
