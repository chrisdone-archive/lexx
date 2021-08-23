{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | Show instance lexer and pretty-printer.

module Lexx

  ( -- * Lexer
    lexx
  , Command(..)
  , Token(..)
  , commandsToString
  , tokenString
  -- * HTML
  , commandsToHtml
  , prettyPrintHtml
  -- * Terminal
  , prettyWrite
  , prettyPrintTerm
  , commandsToRainbow
  ) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Maybe
import Data.String
import Data.Time
import Lucid
import Rainbow

-- | Commands are actions that the renderer will do.
data Command token
  = Type token
  | Return
  | Indent Int
  | Deindent Int
  deriving (Show, Eq, Functor)

-- | Tokens are very liberal and a small number of them.
data Token
  = Constructor String
  | Delimiter String
  | String String
  | Misc String
  | Digits String
  deriving (Show, Eq)

-- | Lex the input into commands.
lexx :: String -> [Command Token]
lexx = cleanUp . execWriter . go
  where
    go i =
      case span (not . flip elem boundaries) i of
        (before, after) -> do
          unless
            (null before)
            (tell
               [ Type
                   (if maybe False isUpper (listToMaybe before)
                      then Constructor before
                      else if all isDigit before
                             then Digits before
                             else Misc before)
               ])
          case after of
            ('[':']':rest) -> do
              tell [Type (Delimiter "[]")]
              go rest
            ('(':')':rest) -> do
              tell [Type (Delimiter "()")]
              go rest
            (')':' ':'(':rest) -> do
              tell [Type (Delimiter ")"), Return, Type (Delimiter "(")]
              go rest
            (c:rest) -> do
              let tellNGo x = tell x *> go rest
              case c of
                '{' -> tellNGo [Type (Delimiter (pure c)), Indent 2, Return]
                '}' -> tellNGo [Deindent 2, Return, Type (Delimiter (pure c))]
                '[' -> tellNGo [Type (Delimiter (pure c)), Indent 1, Return]
                ']' -> tellNGo [Deindent 1, Return, Type (Delimiter (pure c))]
                ',' -> tellNGo [Type (Delimiter (pure c)), Return]
                '=' -> tellNGo [Type (Delimiter (pure c))]
                '(' -> tellNGo [Type (Delimiter (pure c)), Indent 1]
                ')' -> tellNGo [Type (Delimiter (pure c)), Deindent 1]
                '"' ->
                  case reads (c : rest) of
                    [(string, rest')] -> do
                      tell [Type (String string)]
                      go rest'
                    _ -> tellNGo [Type (Misc (pure c))]
                _ -> tellNGo [Type (Misc (pure c))]
            [] -> pure ()
    boundaries = "={}[]()\"\"'', " :: String

-- | Get the plain string from a token.
tokenString :: Token -> String
tokenString =
  \case
    Delimiter s -> s
    Digits s -> s
    Constructor s -> s
    String s -> show s
    Misc s -> s

-- | Clean up redundancy.
cleanUp :: [Command Token] -> [Command Token]
cleanUp (Return:Type (Misc spaces):xs) | all isSpace spaces = cleanUp (Return:xs)
cleanUp (Return:Return:xs)= cleanUp (Return:xs)
cleanUp (x:xs) = x : cleanUp xs
cleanUp [] = []

-- | Render to plain string.
commandsToString :: [Command String] -> String
commandsToString = execWriter . flip runStateT 0 . mapM_ go
  where
    go =
      \case
        Type str -> tell str
        Return -> do
          tell "\n"
          c <- get
          tell (replicate c ' ')
        Indent i -> modify (+ i)
        Deindent i -> modify (subtract i)

-- | Render to TTY colored terminal chunks.
commandsToRainbow :: [Command Token] -> [Rainbow.Chunk]
commandsToRainbow = execWriter . flip runStateT 0 . mapM_ go
  where
    go =
      \case
        Type token ->
          tell
            [ case token of
                String str -> fore yellow (fromString (show str))
                Constructor str -> fore blue (fromString str)
                Delimiter d -> fore green (fromString d)
                Misc text -> fromString text
                Digits d -> fore red (fromString d)
            ]
        Return -> do
          tell ["\n"]
          c <- get
          tell [fromString (replicate c ' ')]
        Indent i -> modify (+ i)
        Deindent i -> modify (subtract i)

prettyWrite :: (Show a, MonadIO m) => a -> m ()
prettyWrite x =
  liftIO
    (do now <- getCurrentTime
        write now x)
  where
    write now = S8.putStrLn . prettyPrintTerm now

prettyPrintTerm :: (Show a) => UTCTime -> a -> S8.ByteString
prettyPrintTerm now =
  S.concat .
  Rainbow.chunksToByteStrings Rainbow.toByteStringsColors8 .
  ([Rainbow.underline (Rainbow.bold (fromString (show now))), "\n"] <>) .
  Lexx.commandsToRainbow . Lexx.lexx . show

prettyPrintHtml :: (Show a) => a -> L.ByteString
prettyPrintHtml = renderBS . commandsToHtml . Lexx.lexx . show

-- | Render to HTML with class names.
commandsToHtml :: Monad m => [Command Token] -> HtmlT m ()
commandsToHtml = flip evalStateT 0 . mapM_ go
  where
    go =
      \case
        Type token ->
          case token of
            String str -> fore' "string" (toHtml (show str))
            Constructor str -> fore' "constructor" (toHtml str)
            Delimiter d -> fore' "delimiter" (toHtml d)
            Digits d -> fore' "digits" (toHtml d)
            Misc text -> fore' "misc" (toHtml text)
        Return -> do
          lift "\n"
          c <- get
          lift (fromString (replicate c ' '))
        Indent i -> modify (+ i)
        Deindent i -> modify (subtract i)
    fore' color inner = lift (span_ [class_ ("lexx-" <> color)] inner)
