# quick copy script
# check there's nothing in the destination and off we go..

#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Incorrect number of arguments - provide just the day number to initialise"
  exit 1
fi

TARGET_FOLDER="../day$1"
echo "Confirming that target $TARGET_FOLDER does not exist."

if [ -d "$TARGET_FOLDER" ]; then
  echo "$TARGET_FOLDER does exist - aborting!"
  exit 2
fi

echo "Copying in template files.."
mkdir -p "$TARGET_FOLDER"

SOURCE_FILES="input.txt test_part1.txt test_part2.txt dayXXp1.hs Makefile"
for THIS_FILE in $SOURCE_FILES; do
  TARGET_FILE=`echo $THIS_FILE | sed "s/XX/$1/g"`
  TARGET_PATH=$TARGET_FOLDER/$TARGET_FILE
  echo "Must copy $THIS_FILE to $TARGET_PATH"
  cat $THIS_FILE | sed "s/XX/$1/g" > $TARGET_PATH
done

echo "Done.. happy hacking!"