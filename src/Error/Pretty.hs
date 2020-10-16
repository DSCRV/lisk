module Error.Pretty ( showError
                    , Options (..)
                    , defaults
                    ) where

import           Data.List                     (intercalate, nub)
import           Error.Base                    (LispError (..))
import           Text.Parsec
import           Text.Parsec.Error
import           Text.Parsec.Pos
import           Text.Parsec.String            (Parser)
import           Text.ParserCombinators.Parsec

data Options =
    Options
        { color            :: Bool
        , contextLineCount :: Int
        , loudEscapeCode   :: String
        , softEscapeCode   :: String
        }

defaults :: Options
defaults =
    Options True 1 "\ESC[31m" "\ESC[38;5;240m"

showError :: Options -> String -> LispError -> String
showError (Options color clc lec sec) source (Parse error) =
    let
        -- Colors
        dull = if color then "\ESC[0m" else ""
        loud = if color then lec else ""
        soft = if color then sec else ""

        -- Helper functions
        spaces n = replicate n ' '
        pad n s = spaces (n - length s) ++ s
        joinOr []      = ""
        joinOr [s]     = s
        joinOr [s,t]   = s ++ " or " ++ t
        joinOr (s:t:u) = s ++ ", " ++ joinOr (t:u)

        -- Data about the error
        msgs = errorMessages error
        pos = errorPos error
        name = sourceName pos
        y = sourceLine pos - 1
        x = sourceColumn pos - 1
        sourceLines = lines source
        address = name ++ ":" ++ show (y + 1) ++ ":" ++ show (x + 1)

        -- Message display
        showMsg (SysUnExpect s) = "unexpected " ++ s
        showMsg (UnExpect s)    = "unexpected " ++ s
        showMsg (Expect s)      = "expected " ++ s
        showMsg (Message s)     = s
        showMsgs []     = "unknown parse error"
        showMsgs [m]    = showMsg m
        showMsgs (m:ms) = showMsg m ++ "\n" ++ showMsgs ms
        unexpections = joinOr $ nub $ [s | SysUnExpect s <- msgs, s /= ""]
                                   ++ [s | UnExpect s <- msgs, s /= ""]
        expections = joinOr $ nub [s | Expect s <- msgs, s /= ""]
        cleanMsgs = [UnExpect unexpections | unexpections /= ""]
                 ++ [Expect expections | expections /= ""]
                 ++ nub [Message s | Message s <- msgs]

        -- Margin display
        marginSize = max 3 $ length $ show $ length sourceLines
        margin l r = soft ++ pad marginSize l ++ " | " ++ dull ++ r
        number i = margin (show i)
        numbered = zipWith number [1..] sourceLines

        -- Explanation display
        -- (Wrap lines to "not much more than 50 chars" at any indentation.)
        -- (The wrapping is for readability, not to meet a term width)
        continue = margin ""
        pointer = continue (spaces x) ++ loud ++ "^-- "
        newline = dull ++ "\n" ++ continue (spaces (x + 4)) ++ loud
        wrap n [] = dull
        wrap n (w:ws) | n >= 50 = newline ++ w ++ wrap (length w) ws
                      | otherwise = " " ++ w ++ wrap (n + length w + 1) ws
        msgLines = lines (showMsgs cleanMsgs)
        wrappedLines = map (drop 1 . wrap 0 . words) msgLines
        explanationBody = intercalate newline wrappedLines
        explanation = pointer ++ explanationBody

        -- Final output
        flower = replicate marginSize '-' ++ "-> "
        header = soft ++ flower ++ address ++ dull
        before = drop (y - clc) $ take y numbered
        focused = numbered !! y
        after = take clc $ drop (y + 1) numbered
    in
        unlines $ header : before ++ (focused : explanation : after)
showError _ _ err = show err
