Rosetta
=======

Is a code generation tool that follows the Model Driven Architecture (MDA) paradigm. It takes a UML model from PlantUML code and generates components (java clasess, erlang modules or other) specific for your application architecture.

Why Code Generator Tools?
-------------------------
Code generation from an abstract description is how to construct the code. It converts human-written high-level code into a low-level language. In other words, it creates source code from the project’s description or model. It means that the description, rather than the code, becomes the source of truth for us.
It can save time and the code could be reused many times which increases productivity.
Rosetta allows you to target numerous platforms at once (portability), or to put it another way, the very same abstract description can be used to build various types of artifacts.


Benefits of using Rosetta:
--------------------------
We’d like to share with you some benefits of code-generation tools.
1. Time-saving and faster turnaround to tests.
2. Less hand coding so fewer human errors.
3. Reuse same models across multiple applications and technologies.
4. Better coverage for testing.
5. Consistent architecture.
6. Improvement of code quality and better code documentation.
7. Allows a better maintain.

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/rosetta
