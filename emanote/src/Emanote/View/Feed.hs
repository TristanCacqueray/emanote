{-# LANGUAGE NamedFieldPuns #-}

module Emanote.View.Feed where

import Data.Aeson.Optics (key, _String)
import Emanote.Model (Model)
import Emanote.Model.Note (Feed (..), Note (..))
import Emanote.Model.Query (Query, parseQuery, runQuery)
import Emanote.Model.Title (toPlain)
import Emanote.Route.ModelRoute
import Emanote.Route.R (indexRoute)
import Emanote.Route.SiteRoute
import Optics.Operators ((^?))
import Optics.Optic ((%))
import Relude
import Text.Atom.Feed qualified as Atom
import Text.Atom.Feed.Export qualified as Export (textFeed)
import Text.Pandoc.Definition

getNotes :: Text -> Model -> [Note]
getNotes queryTxt model = case parseQuery queryTxt of
  Nothing -> []
  Just query -> runQuery (LMLRoute_Md indexRoute) model query

notesToFeed :: (Note -> Text) -> Atom.Date -> [Note] -> Atom.URI -> Text -> Atom.Feed
notesToFeed noteUrl feedUpdated notes feedUrl title =
  (Atom.nullFeed feedUrl feedTitle feedUpdated) {Atom.feedEntries}
  where
    feedTitle = Atom.TextString title
    feedEntries = map toEntry notes
    toEntry :: Note -> Atom.Entry
    toEntry note = Atom.nullEntry (noteUrl note) noteTitle noteDate
      where
        noteDate = getNoteDate note
        noteTitle = (Atom.TextString (toPlain $ _noteTitle note))

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

      -- render the feed
      let feedUrl = "http://example.com" -- TODO: get from index.yaml
      let feedName = _feedTitle feed
      let noteUrl note =
            let sr = SiteRoute_ResourceRoute $ ResourceRoute_LML $ _noteRoute note
             in feedUrl <> "/" <> siteRouteUrl model sr
      let updated = getNoteDate (head notes)
      let final = notesToFeed noteUrl updated (toList notes) feedUrl feedName
      maybeToRight "invalid feed" $ Export.textFeed final
