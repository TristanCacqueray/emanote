{-# LANGUAGE NamedFieldPuns #-}

module Emanote.View.Feed where

import Data.Aeson.Optics (key, _String)
import Data.Time (UTCTime)
import Data.Time.Format
import Emanote.Model (Model)
import Emanote.Model.Note (Note (..), Feed (..))
import Emanote.Model.Query (parseQuery, runQuery)
import Emanote.Model.Title (toPlain)
import Emanote.Route.ModelRoute
import Emanote.Route.R (indexRoute)
import Emanote.Route.SiteRoute
import Optics.Operators ((^?))
import Optics.Optic ((%))
import Relude
import Text.Atom.Feed qualified as Atom
import Text.Atom.Feed.Export qualified as Export (textFeed)

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
        noteDate = fromMaybe "1970-01-01" $ (_noteMeta note) ^? key "date" % _String
        noteTitle = (Atom.TextString (toPlain $ _noteTitle note))

renderFeed :: UTCTime -> Model -> Note -> LByteString
renderFeed now model baseNote = encodeUtf8 $ fromMaybe "<error>bad feed</error>" mFeedTxt
  where
    mFeedTxt = do
      feed <- _noteFeed baseNote
      let feedQuery = "tag:blog" -- TODO: use the first (and only) query in the body of the Markdown note.
      let feedUrl = "http://example.com" -- TODO: get from index.yaml
      let feedName = _feedTitle feed
      let noteUrl note =
            let sr = SiteRoute_ResourceRoute $ ResourceRoute_LML $ _noteRoute note
             in feedUrl <> "/" <> siteRouteUrl model sr
      let final = notesToFeed noteUrl age (getNotes feedQuery model) feedUrl feedName
      Export.textFeed final
    age = toText $ formatTime defaultTimeLocale "%F" now
