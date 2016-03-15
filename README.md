# stack-machine

An little programming language written in clojurescript as an
experiment in just how inefficient I could make an implementation :)
all output goes through console.log at the moment.

The interesting part of this project (aside from the fun of
clojurescript itself) is the language implementation: it runs on an
immutable stack machine, with an instruction and a value stack, and a
binding environment. When the top instruction is executed, it returns
a new immutable machine state. Where it gets interesting is the fact
that an instruction or special form can store the previous machine
state in the new machine state -- allowing for a sort of time
travel. This is in fact how looping is implemented in the language.

## Overview

FIXME: Write a paragraph about the library/project and highlight its goals.

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
