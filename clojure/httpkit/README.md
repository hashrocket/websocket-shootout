You'll need java (at least version 1.7).

This Clojure project uses [boot](https://github.com/boot-clj/boot).
Install it following the directions in that README.

Building
--------

To build the server:

    which java # ensure you have java installed
    which boot # ensure you have boot installed
    boot -u    # keep boot up-to-date
    export BOOT_JVM_OPTIONS=-Dclojure.compiler.direct-linking=true  # enable direct linking
    boot build # builds an uberjar to the build/ directory

Then, to run the server:

    java -jar build/app.jar # defaults to port 3000

OR

    java -jar build/app.jar 4000


Dev
---

If you like REPLs:

    boot repl
