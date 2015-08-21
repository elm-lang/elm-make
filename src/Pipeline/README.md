
The pipeline for building a project goes like this:

  * Crawl - start at the root file and figure out everything it depends on.
  * Organize - organize all these dependencies, loading any relevant cached information
  * Compile - run elm-compile on everything that needs it
  * Generate - take all that info and smash it together into JS

These steps are all managed in a BuildManager.Task which has a fixed set of BuildManager.Error reports. This means we can report errors in a structured way.