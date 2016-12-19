#|
 This file is a part of Modularize
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.lib.modularize)

;; asdf.lisp
(docs:define-docs
  (variable *virtual-module-map*
    "Map of virtual module names to the asdf system instances of the virtual modules.")

  (function virtual-module
    "Accesses the virtual module instance associated with the identifier if one was found, otherwise NIL.")

  (function remove-virtual-module
    "Removes the association of the name with a virtual module.")

  (type virtual-module-not-found
    "Condition signalled when a virtual module was requested but could not be found.")

  (type virtual-module
    "ASDF System subclass that serves as the class for virtual modules.")

  (type module
    "ASDF System subclass that serves as the class for virtual modules.

Deprecated-- use VIRTUAL-MODULE instead.")

  (function virtual-module-name
    "Returns the module name associated with this virtual module.")

  (function register-virtual-module
    "Registers the given module in the virtual module map.")

  (function load-module
    "Attempts to find the module named by identifier and load its ASDF system."))

;; hooks.lisp
(docs:define-docs
  (variable *modularize-hooks*
    "Table mapping the modularization hook names to their functions.")

  (variable *delete-hooks*
    "Table mapping the deletion hook names to their functions.")

  (function modularize-hook
    "Accessor to the modularization hook function associated with the identifier.")

  (function remove-modularize-hook
    "Removes the modularization hook named by the identifier.")

  (function call-modularize-hooks
    "Calls all modularization hooks on the package.")

  (function define-modularize-hook
    "Defines a new modularization hook.

The identifier is defaulted to a keyword representation of the current package name.")

  (function delete-hook
    "Accesses the deletion hook function associated with the identifier.")

  (function remove-delete-hook
    "Removes the deletion hook named by the identifier.")

  (function call-delete-hooks
    "Calls all deletion hooks on the package.")

  (function define-delete-hook
    "Defines a new deletion hook.

The identifier is defaulted to a keyword representation of the current package name."))

;; module.lisp
(docs:define-docs
  (variable *module-storages*
    "Table mapping module packages to their storage tables.")

  (variable *module-deferrals*
    "Table mapping packages to their modules.")

  (type module-not-found
    "Condition signalled when a module is requested but not found.")

  (type not-a-module
    "Condition signalled when a module is requested but only a package of the requested name exists.")

  (function extract-name
    "Extracts the name from an identifier, which is to say the string after the last dot.")

  (function make-identifier
    "Turns a name into an identifier by prepending MODULARIZE.MOD. to it.")

  (function resolve-module
    "Resolves the object to the proper module package, if any.")

  (function module-p
    "Returns non-NIL if the passed object is or resolves to a module package, otherwise NIL.")

  (function module
    "Attempts to return the matching module package.

If not possible, a condition of type MODULE-NOT-FOUND is signalled.

You may pass a STRING, SYMBOL or PACKAGE as the identifier.
If NIL is passed, the current *PACKAGE* is used instead.")

  (function with-module
    "Binds the resolved MODULE to VAR.")

  (function module-storage
    "Accesses the module storage table of the module or a field from it if a key is passed.")

  (function module-storage-remove
    "Removes a key from the module storage table.")

  (function module-identifier
    "Returns the identifier of the module.")

  (function module-name
    "Returns the name of the module.")

  (function current-module
    "Macro that expands to the module in the current package.
Useful to establish a module context.")

  (function expand-option
    "Called to expand module options into forms.")

  (function define-option-expander
    "Defines a new option expander that is called whenever the option is used in the module definition.

This should run whatever is necessary to accomplish the
desired option effect. The expanders are run AFTER the
modularize call, so you can use the module storage in
your expansions.")

  (function expand-module
    "Expands the module options into their proper forms using EXPAND-OPTION for each.")

  (function define-module
    "Defines a new module.

This essentially defines a new package with the given name,
calls MODULARIZE on it and then expands all options to extend
the package/module.")

  (function define-module-extension
    "Defines a module extension.

This gives the existing module new nicknames and expands the
given options on it to add functionality.")

  (function module-packages
    "Accesses the packages that are sub-packages under the module.

The list may contain package objects or package-names.

You can only add packages that are not already registered
as sub-packages to a module, and are not themselves a module.")

  (function modularize
    "Turns the given package into one that is identified as a module.

What this does is register the package on the module
storage table, add the name and identifiers to it,
and then call the modularize hooks.")

  (function demodularize
    "Removes the module from the module storage table.

This essentially returning it back to a normal package.
Any additional effects by module options or
modularization hooks can of course not be undone by this.")

  (function delete-module
    "Attempts to completely delete the given module.

This first calls the delete hooks, then demodularizes
the package, unbinds all symbols in the package from
values and functions, and finally deletes the package.")

  (function map-modules
    "Calls the function once with each module in no particular order.")

  (function list-modules
    "Returns a list of all modules in no particular order."))

;; options.lisp
(docs:define-docs
  )

;; package-toolkit.lisp
(docs:define-docs
  (function ensure-package
    "Ensures that THING is a package-object, or errors if it cannot resolve it to a package.")

  (function with-package
    "Shortcut macro to bind the ENSURE-PACKAGE value of PACKAGE to VAR.")

  (function add-package-nickname
    "Adds the nickname onto the package's nicknames list.")

  (function collect-symbols-from
    "Collects all given symbols from the package.")

  (function extend-package
    "Extends the package with the package definition options.

Any option except for the SIZE option is allowed.
Note that this only ADDS onto the package and does not
remove any options defined prior. As in, nicknames are
only added on, but not removed.")

  (function unbind-and-delete-package
    "Unbinds all symbols in the package from their functions and values and finally deletes the package."))
