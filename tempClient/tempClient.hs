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

--------G
--------i
--------t
------h
------u
------b
---------e
---------r
---------r

import           Control.Monad                      (join, when)
import qualified Data.List as  L
import           Data.Semigroup                     ((<>))
import           System.Environment
import           Control.Concurrent
import           System.IO
import           Network.HTTP.Conduit
import           System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Lazy as S --import qualified Data.ByteString.Lazy as B
import           Data.Aeson (FromJSON, ToJSON, decode, eitherDecode)
import           GHC.Generics
import           Network.HTTP.Conduit (simpleHttp)
import           Data.Text
import           Control.Applicative
import qualified Data.ByteString.Char8 as G
import           System.Directory

--defining needed urls
--server contents
directoryContent = "http://localhost:8080/directoryContents" :: String 
--get file request
getFileUrl = "http://localhost:8080/getFILE/?file=" :: String 
--local  directory link
localDirectory = "/Users/jackskehan/coding/theONE/tempClient/contents/" :: String
--cache directory
cache = "/Users/jackskehan/coding/theONE/tempClient/cache/" :: String



data Filesp = Filesp { filep :: [FilePath]
                     } deriving (Generic, ToJSON, FromJSON, Show)

data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)

main :: IO ()
main = do
  -- manager <- newManager defaultManagerSettings
  putStrLn "what fucntion would you like to use?? --type: LF (to list functions)"
  message <- getLine
  

  case message of
      "DC"                 -> getDirectory
      "LF"                 -> listF
      "GF"                 -> getFILE
      "LC"                 -> localContents
      _                    -> putStrLn "not a valid function" >> putStrLn "-" >> main

listF :: IO ()
listF = do
    putStrLn "-"
    putStrLn "FUNCTIONS"
    putStrLn "DC = directory contents list all files on remote server"
    putStrLn "LF = lists all fucntions (this command)"
    putStrLn "GF = preprares the getFile request then asks what file you want"
    putStrLn "LC = List contents of Local directory"
    putStrLn "-"
    main


--list the server directory
getDirectory :: IO ()
getDirectory = do
 putStrLn "listing all contents of Server Directory Folder:"
 d <- ( decode <$> getJSONDirectory directoryContent) :: IO (Maybe Filesp)
 let s = filep1 d
 printAll s
 
 main

--the actual get request
getJSONDirectory :: String -> IO S.ByteString
getJSONDirectory jsonURL = do
    simpleHttp jsonURL

--a helper function to extract the name form Message
name1 :: Maybe Message -> String
name1 (Just (Message name _)) = name 

--a helper function to extract the message form Message
message1 :: Maybe Message -> String
message1 (Just(Message _ message)) = message

--fucntion to get a file
getFILE :: IO ()
getFILE = do
    putStrLn "What file would you like?"
    file <- getLine

    d <- ( decode <$> getJSONFile file getFileUrl) :: IO (Maybe Message)
    b <- listDirectory cache
    c <- listDirectory localDirectory
    if file `elem` b 
      then do putStrLn "file already downloaded in cache"
              s <- readFile $ cache ++ file
              writeFile (localDirectory ++ file) s
              putStrLn "file moved to localDirectory now"
      else if file `elem` c  
      then putStrLn "file already donwloaded dummy"  
      else do writeFile (localDirectory ++ (name1 d)) (message1 d)
              writeFile (cache ++ (name1 d)) (message1 d)
              putStrLn "-"
              print $  "file: " ++ (name1 d) ++ " downloaded!" 
              putStrLn "-"
    main
    
--calls and form the get request to getFile    
getJSONFile :: String -> String -> IO S.ByteString
getJSONFile file jsonURL = do
    simpleHttp $ jsonURL ++ file


--helper function to extract filep (filepath) from data type filep1
filep1 :: Maybe Filesp -> [FilePath]
filep1 (Just (Filesp filep)) = filep

--list the local directory
localContents :: IO ()
localContents = do 
   s <- listDirectory localDirectory 
   printAll s
   main

--helper function to print all contents    
printAll :: [FilePath] -> IO ()   
printAll xs = if L.null xs        
  then return ()              
  else do print (L.head xs)     
          printAll (L.tail xs)








