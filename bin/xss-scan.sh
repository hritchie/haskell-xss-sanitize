#!/bin/bash

mys() {
  # echo 'Calling private mys'
  local mysql_socket_option
  if [ -z $MYSQL_SOCKET ]
  then
    mysql_socket_option=
  else
    mysql_socket_option="-s$MYSQL_SOCKET"
  fi
  # pipe to /dev/null to silence warnigs about password on command line
  mysql -u$MYSQL_USERNAME -p$MYSQL_PASSWORD -h$MYSQL_HOST $mysql_socket_option $@ 2>/dev/null
}

echo 'select id from notes where deleted_at is null order by id asc' | 
  mys -ss $MYSQL_DATABASE | 
  head |
  while read note_id
  do
    # Report note_id if not clean.
    echo "select body from notes where id = $note_id" |
      mys -ss $MYSQL_DATABASE |
      (xss-sanitize-mackey -q || echo $note_id)
    
    # TODO some note bodies need escaping, report "argument list too long"
    # echo "=========================="
    # echo "$note_id"
    # note_body=$(echo "select body from notes where id = $note_id" | mys -ss $MYSQL_DATABASE)
    # xss-sanitize-mackey -f "$note_body"
    # echo
      
  done
