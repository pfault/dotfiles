#!/bin/sh
goobook query "$*" | sed 's/\(.*\)\t\(.*\)\t.*/\2 \<\1\>/' | sed '/^$/d'
