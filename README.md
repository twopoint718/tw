The Transparent Web: Code Examples
==================================

Because the code used in this book varies quite a bit, the steps needed to
run it also vary. I'll list the general steps here (e.g. for JavaScript
code, or Opa). More specific steps are found in each directory.

The directories are named according to how they occur in the book. For
example, a JavaScript example that is in the "Composition and Modularity"
section will be listed in the code examples under:

    01/composition_and_modularity/composition.js

General instructions
--------------------

These are instructions to make sure the code that you're running is set up
correctly. In most cases this will mean that you've installed the compiler
and can compile and test the code.

I have made an effort to include a Makefile to help build any source code.
If the relevant compilers are installed, just switching to the directory
and running:

    make

should build everything.

### JavaScript

To run the JavaScript tests:

    npm install
    npm test

### Meteor

Meteor has a simple installation script:

    curl https://install.meteor.com/ | sh

After this is installed, you can run a meteor app by changing into the
directory and simply running `meteor`.

### Opa (Opalang)

Be sure you have the Opa compiler installed. Installation instructions are
found on the main Opalang site: <http://opalang.org/>.

Once you have a working Opa compiler, you can build an Opa program and then
run it like this:

    opa filename.opa
    ./filename.js

### Haskell

The Haskell code can be built using the [Stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md) tool.
Then just change into the appropriate directory and run:

    stack build
    stack exec <project_name>

to run the program.

### Elm

Elm has a pre-built package for Mac OS X. You can find this on the website:
<http://elm-lang.org/Install.elm>. For me this was as easy as just
installing the package.

Once installed, you can build programs with `elm-make`:

    elm-make Main.elm --output=main.html
