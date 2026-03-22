#!/bin/bash
set -e

# Get runtime classpath from sbt (fail if empty)
CLASSPATH=$(sbt --batch 'export runtime:fullClasspath' 2>/dev/null | tail -1 | sed 's/\\/\//g')
# CLASSPATH=$(sbt --batch 'export runtime:fullClasspath' 2>/dev/null)
echo "Using CLASSPATH=$CLASSPATH"

if [ -z "$CLASSPATH" ]; then
    echo "Error: sbt returned empty classpath" >&2
    exit 1
fi

# Run Java with the classpath and pass through arguments
java -classpath "$CLASSPATH" com.wolfskeep.TaxonomySortMain "$@"
