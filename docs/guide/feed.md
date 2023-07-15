# ATOM feed

An atom feed can be generated for a given note query by adding the following to the note metadata:

~~~markdown
---
feed:
  enabled: true
---

# My blog

```query
path:blog/*
```
~~~

If the note is named `blog.md`, then the feed will be available at `blog.xml`.

## Configuration

Here are the supported settings:

- `feed.url`: the site url, default to `site.url` from the index.yaml.
- `feed.title`: the feed title, default to the note title.

The feed is constructed from the first and only query of the note.
