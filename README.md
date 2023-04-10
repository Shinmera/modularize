About Modularize
----------------
`modularize` is an attempt at providing a common interface to segregate major application components. This is achieved by adding special treatment to packages. Each module is a package that is specially registered, which allows it to interact and co-exist with other modules in better ways. For instance, by adding module definition options you can introduce mechanisms to tie modules together in functionality, hook into each other, and so on.

How To
------
Each module should consist of a `define-module` form and an ASDF system with the superclass `modularize:module`. `define-module` acts as a wrapper around `defpackage`, so it will replace your usual `defpackage` form. Any option you would use in `defpackage` is also usable in `define-module`, but the latter also allows for custom-defined options that perform specific actions upon module creation.

```
(modularize:define-module test-module
  (:use #:cl #:modularize)
  (:export #:greet))
```

If for some reason you absolutely do not want to use `define-module`, you may define your package manually and call `modularize` on it afterwards. Note though that you also need to manually perform all changes that additional module options may otherwise perform for you. If your module-name somehow differs from the ASDF system, you will need to specify this in your ASDF system definition:

```
(asdf:defsystem some-test-module
  :class "modularize:virtual-module"
  :defsystem-depends-on (:modularize)
  :module-name "test-module"
  ...)
```

The main difference from packages is that each module has a central storage table, which you can access with `module-storage`. This allows you to save metadata on modules to keep track of the different parts each module might play in your application.

Another function that might prove useful is `delete-module`, which attempts to offer a mechanism to completely remove a module so as to revert its changes. Without any further additions, this will simply result in all symbols in the module package being `makunbound` and `fmakunbound` and the package getting deleted. Since a module might also cause changes outside of its own package, it is therefore advised to add deletion hooks through `define-delete-hook`, so as to make sure other kinds of changes can be cleaned up as well.

Similarly, if you want to tack on functionality to initialize a module once it is defined, you can hook into that with `define-modularize-hook` and `define-option-expander`. The former works like `define-delete-hook`, and is called once `modularize` is called on a package after the package's storage has been set up. The latter allows you to declare custom forms that a `define-module` call should expand to. Note that module options are only expanded _after_ `modularize` is called, so you may use the storage in your expansions.

If a component should extend the functionality of another, this can be more intuitively done through `define-module-extension`. In looks and form it is the same as `define-module`, with the difference that it won't create a new package, but use the one it is extending. Through this you can export new functions and other additions without running into a multiple-package mess.

If you would like to group multiple packages under a single module (probably the case if you use one package per file), then you must register the packages with the module. You can do this in your module definition with the `:packages` option, or by `module-packages` calls.

For a super small sample use of a module, have a look at [modularize-test-module.asd](https://github.com/shinmera/modularize/blob/master/modularize-test-module.asd) and [modularize-test-module.lisp](https://github.com/shinmera/modularize/blob/master/modularize-test-module.lisp). For extensions to the module system, have a gander at [modularize-interfaces](https://github.com/shinmera/modularize-interfaces) and [modularize-hooks](https://github.com/shinmera/modularize-hooks).
