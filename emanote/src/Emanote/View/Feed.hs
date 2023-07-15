{-# LANGUAGE NamedFieldPuns #-}

module Emanote.View.Feed where

import Data.Aeson.Optics (key, _String)
import Emanote.Model (Model)
import Emanote.Model.Meta (lookupRouteMeta)
import Emanote.Model.Note (Feed (..), Note (..), lookupMeta)
import Emanote.Model.Query (Query, parseQuery, runQuery)
import Emanote.Model.Title (toPlain)
import Emanote.Model.Type qualified as M
import Emanote.Route.SiteRoute
import Optics.Operators ((^?))
import Optics.Optic ((%))
import Relude
import Text.Atom.Feed qualified as Atom
import Text.Atom.Feed.Export qualified as Export (textFeed)
import Text.Pandoc.Definition hiding (lookupMeta)

noteToEntry :: (Note -> Text) -> Note -> Atom.Entry
noteToEntry noteUrl note = entry {Atom.entrySummary}
  where
    entry = Atom.nullEntry (noteUrl note) noteTitle noteDate
    noteDate = getNoteDate note
    noteTitle = (Atom.TextString (toPlain $ _noteTitle note))
    entrySummary = Atom.TextString <$> lookupMeta ("page" :| ["description"]) note

getNoteDate :: Note -> Atom.Date
getNoteDate note = fromMaybe "1970-01-01" $ (_noteMeta note) ^? key "date" % _String

getNoteQuery :: Note -> Either LText Query
getNoteQuery note = case _noteDoc note of
  Pandoc _meta [] -> Left "empty note"
  Pandoc _meta blocks -> go blocks
  where
    go [] = Left "can't find note query"
    go (block : rest) = case block of
      CodeBlock ("", classes, _) txt | "query" `elem` classes -> case parseQuery txt of
        Nothing -> Left ("invalid query: " <> toLazy txt)
        Just query -> case go rest of
          -- Check that only query exists
          Right _ -> Left "multiple ```query found"
          Left _ -> Right query
      _ -> go rest

renderFeed :: Model -> Note -> LByteString
renderFeed model baseNote = encodeUtf8 $ case eFeedText of
  Left err -> "<error>" <> err <> "</error>"
  Right feedText -> feedText
  where
    eFeedText = do
      -- get the note feed
      feed <- maybeToRight "feed attribute missing" $ _noteFeed baseNote

      -- find the query and get the feed notes
      feedQuery <- getNoteQuery baseNote
      notes <- case runQuery (_noteRoute baseNote) model feedQuery of
        [] -> Left "no notes matched the query"
        x : xs -> Right (x :| xs)

      -- process the notes
      let noteFeedUrl, indexUrl :: Maybe Text
          noteFeedUrl = lookupMeta ("feed" :| ["url"]) baseNote
          indexRoute = M.modelIndexRoute model
          indexUrl = lookupRouteMeta Nothing ("site" :| ["url"]) indexRoute model
      feedUrl <- maybeToRight "index.yaml or note doesn't have url" (noteFeedUrl <|> indexUrl)
      let noteUrl note =
            let sr = SiteRoute_ResourceRoute $ ResourceRoute_LML $ _noteRoute note
             in feedUrl <> "/" <> siteRouteUrl model sr
      let feedEntries = noteToEntry noteUrl <$> toList notes

      -- render the feed
      let feedTitle = fromMaybe (toPlain $ _noteTitle baseNote) (_feedTitle feed)
      let feedName = Atom.TextString feedTitle
      let feedUpdated = getNoteDate (head notes)
      let atomFeed = (Atom.nullFeed feedUrl feedName feedUpdated) {Atom.feedEntries}
      maybeToRight "invalid feed" $ Export.textFeed atomFeed
