Postprocessing pass to enable use of nim closures to override C++ methods.
This allows to use OOP-heavy libraries that rely heavily on inheritance and
method overrids to provide user-defined functionality. For each class entry
passed to ``callbackOverride`` new class is generated, deriving from
original one. Derived class overrides all methods of parent class (as well
as method passed from superclass). Additionally, for each reimplemented
method following fields are generated:

.. code-block:: c++

   // Closure callback 'caller' - `{.cdecl.}` procedure defined on nim side,
   // which performs more type-safe casting of `void*` before calling actual
   // implementation
   <return-type> (*<method-name>Wrap)(void*, <method-arguments>, void*, void*) = 0;
   void* <method-name>Proc = 0; // Closure callback implementation
   void* <method-name>Env = 0; // Closure callback envrionment

Which hold necessary pointers to closure, it's environment and wrapper
implementation. On nim side ``set<method-name>`` procedure is defined, for
each method, which perfoms necessary casting and conversion between nim
callback closure, and C++ ``void*`` part.

In result, each method in the C++ object calls either default
implementation (if override wasn't set), or closure overide, with
respective environment.

In addition to enabling use of OOP code from *mostly procedural* language
this also makes behavior of the objects **instance-based** as opposed to
**class-based**, which provides greater overall flexibility.

To allow use of custom data associated with object generic wrapper type
``<class-name>Nim`` is defined -

.. code-block:: nim
    type
      <class-name>Nim*[T] = object
        rawImpl*: ptr <class-name>NimRaw
        userData*: T

where ``userData`` is an arbitrary nim type and ``rawImpl`` is pointer to
wrapper for `*NimRaw` class. Generate C++ class contains ``void*`` pointer
to nim wrapper and is passed as first argument to closure implementation.
Pointer to class is update each time `set<method-name>` is called. 

