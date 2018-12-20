{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PubSub where

import           Data.Pool              (Pool)
import           Database.MySQL.Simple  (Connection)

import           RMS.MQ.Messages.Events
import           RMS.MQ.Types

subscriptions :: Pool Connection -> [Subscription]
subscriptions pool =
  -- Sub sourceEx queue routingKey callback
  [ Sub newNotesChannel (filterContent pool)
  , Sub editedNotesChannel (filterContent pool)
  -- , Sub commentsChannel filterContent) -- ?
  ]
  where
    newNotesChannel = "notes.new"
    editedNotesChannel = "notes.edit"
    -- commentsChannel = "comments.all"

subscribeAndFilter :: Pool Connection -> MQActions -> IO ()
subscribeAndFilter pool mqActions = do
  let sub = _mqSubscribe mqActions
  mapM_ sub $ subscriptions pool

filterContent :: Pool Connection -> MessageBody -> IO ()
filterContent pool msg = undefined
