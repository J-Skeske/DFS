-- This file is commented extensively for non-haskell programmers

-- | These are language extensions. Haskell has a great many language
-- extensions but in practice you do not need to knwo much about them. If you
-- use a library that needs them, then the library documentation will tell you which
-- extensions you neeed to include. If you try to write code that needs particular extensions,
-- then the haskell compiler is smart enough typically to be able to suggest which extensions
-- you should switch on by including an entry here.

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Haskell code is structured as sets of functions that sit within Modules. The basic rule is that a module with a
-- particular name (for example Lib) sits within a .hs file of the same name (eg. Lib.hs). The module statement is of
-- the form `module MODULE_NAME (EXPORTED_FUNCTIONS) where`. Everything following this is part of the module. There are
-- no brackets or any other syntax to worry about.
module Lib
    ( startApp
    ) where

-- | Imports work like most other languages and are essentially library includes. The functions of the lirbary become
-- immediately accessible in the code of the module. There are various ways in which imports can be modified. For
-- example, one may `import qualified X as Y` which imports a library in such a way that the functions of the library
-- must be prefixed with `Y.`. One can always prefix a libraries functions with the import string, when calling them.
-- You will occasionally have reason to import libraries that have common function names by coincidence. You can use
-- qualified imports of full prefixes to disambiguate. The compiler will tell you where the problem is if this occurs.

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           RestClient
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           UseHaskellAPI
import           System.Directory




directoryPath="/Users/jackskehan/coding/theONE/use-haskell/contents/" :: String 


startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting use-haskell."

  forkIO $ taskScheduler 5

  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

-- this is the original startApp that stack new servant builds
--startApp :: IO ()
--startApp = run 8080 app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion

-- | the function app calls serve, passing api and server. You can see that there is a kind of structure of function
-- composition here that is to do with how servant operates. The first parameter defines the type of the REST service,
-- (which is defined as type `Proxy API` - yes types of this formt are allowed - and we should note that the second part
-- of that type is out previously defined REST API) and the second parameter defines the implementation of the REST service.
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server API
server = loadEnvironmentVariable
    :<|> getFILE
    :<|> performRESTCall
    :<|> directoryContents

  where
    -- | where is just a way of ensuring that the following functions are scoped to the server function. Each function
    -- below hasa type matching the end point parameters, with the return type being of type `Handler a`, where a is the
    -- type of data returned by the endpoint. Handler is a Monad. This may strike terror - it should not, but th great
    -- news is that we don't have to understand Monads at all really, providing we follow some simple programming
    -- conventions when writing handlers.
    --
    -- Now we will explain each handler in turn, along with any helper functions we write to help us implement the
    -- various endpoints.
    loadEnvironmentVariable :: Maybe String -> Handler ResponseData
    loadEnvironmentVariable ms = liftIO $ do
      warnLog $ "request to load environment variable: " ++ show ms
      -- Something that is of type Maybe String will either have a value Nothing
      -- or a value Just s, where s is a String. The following case statement allows us to
      -- distinguish between the two possibilities and act accordingly. If we get nothing in this case,
      -- we throw an exception. Otherwise, we look for the environment variable, throwing an exception
      -- if it is not set (probably not sensible program behaviour but this is just a demonstration).
      --
      -- Note that this function has deliberately been written in an empirical style that would be familiar to
      -- Java of C or C++ programmers, essentially as a kind of long drawn out if/else structure (although in fact
      -- written with a case statement structure). See the searchMessage implementation below for a more elegant and
      -- therefore less complex implementation, that takes some advantage of Haskell coding style.
      case ms of
        Nothing -> do
          warnLog "No environment variable requested"
          return $ ResponseData "WAT? No environment variable requested."
        Just s  -> liftIO $ do
          e <- lookupEnv s -- lookupEnv is an IO function, so we must use the `<-` notation
          case e of
            -- If the environment variable is not set, then create a lof entry and return and exception
            Nothing -> do
              warnLog $ "Environment variable " ++ s ++ " is not set."
              return $ ResponseData $  "Environment variable " ++ s ++ " is not set."

            -- Otherwise, return the envrionment variable. Note how variable names can use ', eg. x', x''
            -- Haskell programmers often use this format for variables that are essentially referring to the same thing
            Just e' -> return $ ResponseData e'

 

    getFILE :: Maybe String -> Handler Message -- fns with no input, second getREADME' is for demo below
    getFILE (Just fileName)= liftIO $ do        -- alternatively (rPath:xs) <- getArgs
      s       <- readFile $ directoryPath ++ fileName
      return $ Message fileName s

  

    -- | Performing a REST call
    -- The following function performs a REST call to a remote service 'hackage.haskell.org'. This remote service is a
    -- searchable documentation server. The API to the service is accessible at http://hackage.haskell.org
    performRESTCall :: Maybe String -> Handler ResponseData
    performRESTCall (Just filt) = liftIO $ do
      warnLog $ "recieved request to perform REST call with param " ++ filt
      doRest $ DL.filter (DL.isInfixOf filt)

    -- | An implementation when no parameter is passed, no filtering so.
    performRESTCall Nothing = liftIO $ do
      warnLog $ "recieved request to perform REST call, but no param "
      doRest id

    -- | the performRESTCall is delegated to this function, with a filtering function passed as a parameter
    doRest :: ([String] -> [String]) -> IO ResponseData
    doRest flt = do
      -- first we perform the call to hackage.org, then we will extract the package names and filter
      -- to include only package names matching the 'filt' parameter, returning a comma separated string of those
      -- packages.
      res <- SC.runClientM getPackages =<< env   -- the actual REST call
      case res of
        Left err -> do
          warnLog $ "Rest call failed with error: " ++ show err
          return $ ResponseData $ "Rest call failed with error: " ++ show err
        Right pkgs -> do
          return $ ResponseData $ DL.intercalate ", " $                          -- reduce to comma separated string
                                  flt $                                          -- run the filtering function
                                  DL.map (unpack . RestClient.packageName) pkgs  -- extract name and convert to string
      where env = do
             manager <- newManager defaultManagerSettings
             return (SC.ClientEnv manager (SC.BaseUrl SC.Http "hackage.haskell.org" 80 ""))

    directoryContents :: Maybe FilePath -> Handler Filesp
    directoryContents (Just filePath) = liftIO $ do
      s <- getDirectoryContents $ directoryPath ++ filePath
      return $ Filesp s

    directoryContents Nothing = liftIO $ do
      s <- getDirectoryContents directoryPath
      return $ Filesp s 







-- | error stuff
custom404Error msg = err404 { errBody = msg }


-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger




-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def




