set -u

COOKIE="cookie:session=$AOC_SESSION_COOKIE"
URL="https://adventofcode.com/2023/day/$1/input"
FILE="inputs/$1"

curl --fail --silent --show-error -H $COOKIE $URL --output $FILE
