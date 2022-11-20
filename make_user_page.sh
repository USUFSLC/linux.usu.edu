#!/bin/sh

if [ -z "$1" ]
  then
    echo "Usage: ./make_user_page.sh <username>"

    exit 1
fi


DIR="public/users/$1"
mkdir -p $DIR

# CSS from http://bettermotherfuckingwebsite.com/
echo "body {
  margin:40px auto;
  max-width:650px;
  line-height:1.6;
  font-size:18px;
  color:#444;
  padding:0 10px
}
h1,h2,h3 {
  line-height:1.2
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
    <h3>I'm a member of the USUFSLC.</h3>
    <p>Lorem ipsum.</p>
  </body>
</html>" >> $DIR/$1.html
