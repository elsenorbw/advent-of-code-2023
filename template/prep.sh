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