{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PubSub where

import           Prelude                 (String)
import           Protolude               hiding (note)

import           Control.Lens            ((^.), (^?), (^?!))
import           Control.Lens.Setter     ((.~))
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy    as BL
import           Data.Pool               (Pool)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Database.MySQL.Simple   (Connection)
import qualified Network.Wreq            as W
import           Text.HTML.TagSoup

import           System.Log.FastLogger

import           RMS.MQ.Messages.Events
import           RMS.MQ.Types

import           Text.HTML.SanitizeXSS

subscriptions :: Pool Connection -> [Subscription]
subscriptions pool =
  -- Sub sourceEx queue routingKey callback
  [ Sub newNotesChannel (filterContent pool)
  , Sub editedNotesChannel (filterContent pool)
  -- , Sub commentsChannel (filterContent pool)
  ]
  where
    newNotesChannel = "notes.new"
    editedNotesChannel = "notes.edit"
    -- commentsChannel = "comments.all"

subscribeAndFilter :: Pool Connection -> MQActions -> IO ()
subscribeAndFilter pool mqActions = do
  let sub = _mqSubscribe mqActions
  mapM_ sub $ subscriptions pool

filterContent :: Pool Connection -> BL.ByteString -> IO ()
filterContent pool msg = do
  let r :: Either String MQNoteMessage
      r = eitherDecode msg
  mqNoteMessage <- either (die . toS) pure r
  let noteId = _mqeNoteId mqNoteMessage
  noteBody <- getNoteBody noteId
  let badAttrs = getProblematicAttributes $ TL.toStrict . TL.decodeUtf8 $ noteBody
  if badAttrs == []
  then return ()
  else do
    logMsg "NoteBody:"
    logMsg $ toS noteBody
    alertXSS badAttrs


getNoteBody :: Int -> IO BL.ByteString
getNoteBody = noteFilter 1 -- FIXME change "1" to actual superuser id

-- | return note body
noteFilter :: Int -> Int -> IO BL.ByteString
noteFilter userId noteId = do
  let opts = W.defaults & W.header "X-RMS-USER" .~ [show userId]
  res <- W.getWith opts $ "http://mackey:3000/note_filter?note_id=" <> show noteId
  let responseBody :: BL.ByteString = res ^. W.responseBody
  let noteJSON :: Value
      noteJSON = responseBody ^?! _JSON
  let note = fromMaybe "Expected `notes`" $ noteJSON ^? key "notes" . nth 0
  return $ encode $ note ^? key "body"

-- getCommentBody :: Int -> Int -> IO BL.ByteString
-- getCommentBody noteId commentId = do
--   return ""


-- send admin email or something
alertXSS :: [Tag Text] -> IO ()
alertXSS tags = do
  logMsg "XSS:"
  mapM_ (logMsg . show) tags
  logMsg ""


logMsg :: Text -> IO ()
logMsg msg = do
  formattedTime <- newTimeCache simpleTimeFormat'
  let attachTime t = "[" <> toLogStr t <> "] " <> toLogStr msg
  (doLogging, cleanup) <- newTimedFastLogger formattedTime (LogStdout defaultBufSize)
  doLogging attachTime
  doLogging $ const "\n"
  cleanup
