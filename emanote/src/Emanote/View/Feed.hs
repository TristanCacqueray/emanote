{-# LANGUAGE NamedFieldPuns #-}

module Emanote.View.Feed where

import Data.Aeson.Optics (key, _String)
import Data.Time (UTCTime)
import Data.Time.Format
import Emanote.Model (Model)
import Emanote.Model.Note (Note (..))
import Emanote.Model.Query (parseQuery, runQuery)
import Emanote.Model.Title (toPlain)
import Emanote.Route.ModelRoute
import Emanote.Route.R (indexRoute)
import Optics.Operators ((^?))
import Optics.Optic ((%))
import Relude
import Text.Atom.Feed qualified as Atom
import Text.Atom.Feed.Export qualified as Export (textFeed)

getNotes :: Model -> [Note]
getNotes model = case parseQuery "tag:blog" of
  Nothing -> []
  Just query -> runQuery (LMLRoute_Md indexRoute) model query

notesToFeed :: Atom.Date -> [Note] -> Atom.URI -> Text -> Atom.Feed
notesToFeed feedUpdated notes feedUrl title =
  (Atom.nullFeed feedUrl feedTitle feedUpdated) {Atom.feedEntries}
  where
    feedTitle = Atom.TextString title
    feedEntries = map toEntry notes
    toEntry :: Note -> Atom.Entry
    toEntry note = Atom.nullEntry noteUrl noteTitle noteDate
      where
        noteUrl = "http://example.local/todo"
        noteDate = fromMaybe "1970-01-01" $ (_noteMeta note) ^? key "date" % _String
        noteTitle = (Atom.TextString (toPlain $ _noteTitle note))

renderFeed :: UTCTime -> Model -> LByteString
renderFeed now model = encodeUtf8 $ fromMaybe "" $ Export.textFeed feed
  where
    age = toText $ formatTime defaultTimeLocale "%F" now
    feed = notesToFeed age (getNotes model) "https://tristancacqueray.github.io" "Tristan's Zettelkasten"
