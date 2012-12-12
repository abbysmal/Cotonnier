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
read on articles directory tree. Comments will be stored in this database too.

Why ?
-----

The goal of this project is to demonstrate the usability of Yesod, it will try
to stay the simpler possible to provide a clear demonstration of what web
programming in Haskell can looks like.
