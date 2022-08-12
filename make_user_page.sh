#!/bin/sh
# usage: ./make_user_page.sh <user>

DIR="public/users/$1"

mkdir -p $DIR

echo "body {
  background-color: #;
}" >> $DIR/style.css

echo "<!DOCTYPE html>
<html>
  <head>
    <title>The Page Of $1</title>
    <link rel='stylesheet' href='style.css' />
    <meta name='viewport' content='width=device-width, initial-scale=1'>
  </head>
  <body>
    <h1>$1</h1>
    <p>Lorem ipsum.</p>
  </body>
</html>
" >> $DIR/index.html
