## Lego taxonomy sorting

This is a tool to sort model parts lists
according to the taxonomy in https://brickarchitect.com/parts/.


## Features

* Upload parts lists in CSV or Studio .io format (auto-detected)
* Look up set inventories by Brickset set number
* Parts sorted according to the taxonomy from https://brickarchitect.com/parts/

### Running in Web Server Mode

To run the web server:

```
sbt run -- -web
```

This starts:
* HTTP server on port 37080
* HTTPS server on port 37443

Open your browser to:
* http://localhost:37080/
* https://localhost:37443/

The web interface allows you to:
* Upload a parts list file (.csv or .io/Studio format)
* Enter a Brickset set number to fetch the inventory automatically

### Running in Batch Mode

This tool also supports command-line processing in CSV format,
supporting both Studio model info export,
and Brickset set inventories.

Given a parts list in "summary.csv" run either:
* sbt run summary.csv
* sbt "run summary.csv"

depending on exactly which platform (Windows, Mac, Linux, whatever) you're running on.

The system then outputs a new file "summary-sorted.csv"
with the parts in taxonomy order.
Any parts that could not be matched against the taxonomy are listed at the end.

### Development notes

This project also served as a testbed for me to use AI coding tools.
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

After exhausting the free trial for Copilot,
I switched to using OpenCode (https://opencode.ai/) as my AI harness.
This also allowed me to return to my beloved vi editor.
Sure, I'm not getting the occasionally useful tab completion for inserting lots of code,
but that just means that I'm being more deliberate in my use of AI,
actually telling it what my goals are,
instead of having it guess (often incorrectly) what I'm thinking about.

Telling the AI to plan a change and reviewing that plan
before letting the AI actually implement the change is crucially important.
Even so, there have been multiple times when the AI has gotten part way through implementation and hit a roadblock,
and then switched back to using an approach that I explicitly told it not to use just because that seemed easier.
The AI is as bad as a terminally short-sighted junior dev,
and needs constant supervision to make sure it hasn't gone and done something stupid.

