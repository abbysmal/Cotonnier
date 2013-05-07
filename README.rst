Cotonnier
=========

Intro
-----

Cotonnier is a semi-static blog-engine written in Haskell with the Yesod web
framework.

It's goal is to provide a simple cms to write easily with a pleasant syntax
(Markdown) articles and papers.

Architecture
------------

Articles are stored in a user-defined directory and are simple files, written in
markdown. The articles tags and informations (timestamps, author) are stored in
a database (MongoDB) to make search easier and more efficient than a continuous
read on articles directory tree. Comments will be stored in this database too

Install
-------

You'll need yesod-static and yesod 1.2, mongodb and yesod-markdown, all are available through cabal.
