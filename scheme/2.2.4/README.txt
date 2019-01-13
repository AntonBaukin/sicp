SICP §2.2.4 uses painter routines that are not available
in Gambit Scheme implementation. To compele the tasks we
have to use else LISP system, well-known Racket, see
racket-lang.org for the manuals and binaries.

The paining package is descriped in this article:
http://planet.racket-lang.org/package-source/soegaard/sicp.plt/2/0/planet-docs/sicp-manual/index.html

You need to start DrRacket IDE with Gui. On the first start of

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))

it will download and install the packages from the repository.
The package has primitive painter (paint einstein) that draws
photo of A. Einstein to the output log of DrRacket, it won't
paint anything to the console running racket executable.
Use (paint einstein) instead of (wave) and (rogers) painters.


Credits

Abelson & Sussman: Structure and Interpretation of Computer Programs.
Daniel Coore: Original MIT Scheme code.
Mike Sperber: PLT port.
Jens Axel Søgaard: Documentation.
