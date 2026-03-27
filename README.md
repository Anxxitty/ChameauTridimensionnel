# Chameau Tridimensionnel
## Build
Before trying to install the dependencies of the project, make sure you have installed development tools for C, OpenGL and GLFW.

Then install dependencies by running `opam install . --deps-only --with-test --with-doc`

If some development tools are missing, the above command should halt and let you install them before continuing.

Then build the project by running `dune build`

Run the program with `dune exec ChameauTridimensionnel`