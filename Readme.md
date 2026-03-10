## Lego taxonomy sorting

This is a tool to sort model parts lists
according to the taxonomy in https://brickarchitect.com/parts/.

It is also a testbed for me to use AI coding tools.
Environment setup is using:
* JDK 25.0.2
* Scala 2.13.17
* sbt 1.11.2
* VS Code
* Metals plugin for Scala support

All the rest should be pulled in by dependencies in build.sbt.

I also used the beginning of https://www.youtube.com/watch?v=tNLS6rOGBlo
to help get the environment started,
including making Metals import the project properly.

### Running

This tool processes parts lists in CSV format,
supporting both Studio model info export,
and Brickset set inventories.

Given a parts list in "summary.csv" run either:
* sbt run summary.csv
* sbt "run summary.csv"

depending on exactly which platform (Windows, Mac, Linux, whatever) you're running on.

The system then outputs a new file "summary-sorted.csv"
with the parts in taxonomy order.
Any parts that could not be matched against the taxonomy are listed at the end.
