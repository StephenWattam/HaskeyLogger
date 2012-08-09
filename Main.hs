module Main where

-- Yes, this is a mess ;-)
import System.IO
import System( getArgs )
import Directory(doesFileExist)
import qualified Data.ByteString as B
import Data.List (intercalate, foldl')
import Data.Word
import Data.Bits


{- 
Event Format
 * http://www.comanswer.com/question/dev-input-keyboard-format
 * http://www.gnugeneration.com/books/linux/2.6.20/kernel-api/re1253.html
 * struct input_event {
       struct timeval time; 128 bits {long tv_sec, long tv_usec}, 64 each
       __u16 type;          16  bits, 
       __u16 code;          16  bits, 
       __s32 value;         32  bits
   }; 


Keycodes available from
 * http://code.google.com/p/logkeys/source/browse/wiki/Keymaps.wiki
-}
data Event = Event Word64 Word64 Word16 Word16 Word32



-- Ways of returning the various details of an event
getSec  (Event x _ _ _ _) = x
getUSec (Event _ x _ _ _) = x
getType (Event _ _ x _ _) = x
getCode (Event _ _ _ x _) = x
getVal  (Event _ _ _ _ x) = x




main :: IO ()
main = do
	args    <- getArgs
	if(length args < 2) then 
	    putStrLn "Please provide two arguments: a key config file, and a file (/dev or normal) to read event data from."
	else do
	    keys	<- loadConfig (args !! 0) 
	    handle	<- openFile   (args !! 1) ReadMode 
	    watchOutput (getPrintHandler keys) handle []
	    --putStrLn "----"
	    hClose handle

-- Load a massive array from the config file.
loadConfig :: String -> IO([String])
loadConfig confpath = do
	fileExists <- doesFileExist confpath
	if(fileExists) then do
	    k <- readFile confpath
	    return $ lines k
	else
	    return []

-- Returns a function to transfer the scancode into a string for output.
getPrintHandler :: [String] -> (Int -> String)
getPrintHandler keys = (\code -> if (code > 0) && (code < (length keys)) then (keys !! (code - 1)) else ("(" ++ (show code) ++ ")"))

-- Maintain a buffer of events 64 entries long, to handle all input.
watchOutput :: (Int -> String) -> Handle -> [Event] -> IO ()
watchOutput printFunc handle buffer@(e1:e2:next) = do   
	if  ((getType e2) == 1) && ((getVal e2) == 1)  || ((getVal e1) == 32) 
	    then do
		putStr $ printFunc $ fromIntegral $ getCode e2
		hFlush stdout
		watchOutput printFunc handle next
	    else do
		e <- readEvent handle
		watchOutput printFunc handle (e:buffer)

-- If there is not enough data on the buffer, then simply add to it and run again 
-- (some events are not keypress events)
watchOutput printFunc handle buffer = do
	e <- readEvent handle
	watchOutput printFunc handle (e:buffer)


-- Outputs the event raw, handy for debug.
printEvent :: Event -> IO ()
printEvent e = do
	putStrLn $ "{s: " ++ (show $ getSec e) ++ ", us: " ++ (show $ getUSec e) ++ "}"
	putStrLn $ "{t: " ++ (show $ getType e) ++ ", c: " ++ (show $ getCode e) ++ ", v: " ++ (show $ getVal e) ++ "}"


-- Reads a single event from the file, and converts it into a useful format.
readEvent :: Handle -> IO Event
readEvent handle = do
	-- TODO: get all 24 bytes in one read, then process.
	--event	    <- B.hGet handle 24
	-- Struct timeval
	etimeSec    <- getOctets 8 fromOctets64
	etimeuSec   <- getOctets 8 fromOctets64
	-- Event items
	etype	    <- getOctets 2 fromOctets16
	ecode	    <- getOctets 2 fromOctets16
	evalue	    <- getOctets 4 fromOctets32
	-- Construct the event
	return $ Event etimeSec etimeuSec etype ecode evalue
    where
	getOctets n func = do
	    bs <- B.hGet handle n
	    return $ func $ reverse $ B.unpack bs


-- =========================================== Utility/Conversion Functions ======================================

-- Convert [Word8, Word8] to Word16
fromOctets16 :: [Word8] -> Word16
fromOctets16 = foldl' accum 0
	where
	    accum a o = (a `shiftL` 8) .|. fromIntegral o

-- Convert [Word8, Word8,...] to Word32
fromOctets32 :: [Word8] -> Word32
fromOctets32 = foldl' accum 0
	where
	    accum a o = (a `shiftL` 8) .|. fromIntegral o

-- Convert [Word8, Word8,...] to Word64
fromOctets64 :: [Word8] -> Word64 
fromOctets64 = foldl' accum 0
	where
	    accum a o = (a `shiftL` 8) .|. fromIntegral o

-- Decode characters read from the event file
decode :: [Word8] -> String
decode bytes = "[" ++ (intercalate " " (map show bytes)) ++ "]"





















-- Maintain a buffer of events 64 entries long, to handle all input.
{-watchOutput :: Handle -> [Event] -> IO ()
watchOutput handle buffer = do   
	e1 <- readEvent handle
	e2 <- readEvent handle

	--printEvent e1
	--printEvent e2

	if  ((getType e2) == 1) && ((getVal e2) == 1) -- && ((getVal e1) == 32) 
	    then do
		putStr $ "\n" ++ (getKey $ fromIntegral $ getCode e2)
	    else putStr "...o"
	
	watchOutput handle buffer 
    where 
	getKey code = if (code > 0) && (code < (length keys)) then (keys !! (code - 1)) else ("? (" ++ (show code) ++ ")")
	nextFree = length buffer-}
