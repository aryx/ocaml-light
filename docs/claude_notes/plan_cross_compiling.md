Cross-compiler plan for ocaml-light native backend
==================================================

Goal
----

Make `ocamlopt` choose a native-code target at runtime, using flags such as
`-i386`, `-arm`, and `-mips`, while also selecting the matching assembler,
native C compiler, linker, and runtime archive for that target.

Constraints
-----------

This codebase must keep compiling with ocaml-light, so the plan avoids:

- functors
- first-class modules
- reliance on modern module abstractions

That means the design has to stay in plain OCaml 2-era style:

- records of functions
- simple variant types
- shared generic code where possible
- very limited `Obj.magic`, only if there is no other viable escape hatch

Current situation
-----------------

Today the native backend is configured for exactly one architecture at build
time.

Observed structure:

- `Makefile` creates symlinks like `asmcomp/arch.ml`, `asmcomp/proc.ml`,
  `asmcomp/selection.ml`, `asmcomp/reload.ml`, `asmcomp/scheduling.ml`
  from a single selected backend directory.
- `asmcomp/asmgen.ml` calls a single backend pipeline:
  `Selection -> Reload -> Scheduling -> Emit`.
- `asmcomp/mach.ml` and `asmcomp/cmm.ml` already depend on `Arch` types and
  size constants.
- `asmrun/Makefile` builds one `libasmrun.a` from one selected target object.
- `asmcomp/asmlink.ml` always links against that one runtime archive.

Implication:

The current code is not just “single architecture by configuration”; it also
embeds architecture-dependent types into the compiler pipeline.

What runtime switching must mean
-------------------------------

The `-i386` / `-arm` / `-mips` flag should select a backend at runtime, not
just toggle assembler flags.

That selection must affect:

1. instruction selection
2. register allocation policy
3. scheduling
4. assembly emission
5. assembler invocation
6. native C compiler invocation for the runtime
7. linker invocation
8. runtime archive selection

Design principle
-----------------

Use a value-level backend descriptor, not a module-level one.

The compiler should carry a record such as:

```ocaml
type backend = {
  arch_name : string;
  word_addressed : bool;
  register_name : int -> string;
  num_register_classes : int;
  num_available_registers : int array;
  first_available_register : int array;
  loc_arguments : Reg.t array -> Reg.t array * int;
  loc_results : Reg.t array -> Reg.t array;
  loc_parameters : Reg.t array -> Reg.t array;
  loc_external_arguments : Reg.t array -> Reg.t array * int;
  loc_external_results : Reg.t array -> Reg.t array;
  loc_exn_bucket : Reg.t;
  safe_register_pressure : Mach.operation -> int;
  max_register_pressure : Mach.operation -> int array;
  destroyed_at_oper : Mach.instruction_desc -> Reg.t array;
  destroyed_at_raise : Reg.t array;
  assemble_file : string -> string -> int;
  runtime_lib : string;
}
```

This record can be selected once at startup from command-line flags and then
used throughout compilation.

What can be shared
------------------

The following pieces can probably remain shared:

- lambda to Cmm translation
- generic control flow and register allocation framework
- generic liveness, spilling, splitting, coloring logic
- generic linking logic apart from runtime archive/toolchain selection

The backend-specific pieces that need per-arch implementations are:

- `Arch`
- `Proc`
- `Selection`
- `Reload`
- `Scheduling`
- `Emit`

Type-system problem
-------------------

The hardest issue is that the current IR directly refers to backend-specific
types:

- `Mach.operation` contains `Arch.addressing_mode`
- `Mach.operation` contains `Arch.specific_operation`
- `Cmm` uses `Arch.size_addr`, `Arch.size_int`, `Arch.size_float`

That means a pure runtime dispatch table is not enough by itself unless the
backend-specific data are made abstract or generalized.

Possible strategies
-------------------

### Strategy A: Common superset IR

Define one shared `addressing_mode` and one shared `specific_operation` type
that covers i386, arm, and mips.

Pros:

- easy to dispatch at runtime
- no functors needed
- no first-class modules needed
- likely easiest to make typecheck in ocaml-light

Cons:

- lowest-common-denominator design
- backend-specific operations become awkward
- may add constructors that only one arch uses
- the IR gets less precise

### Strategy B: Backend-specific opaque payloads

Keep the IR generic and store backend-specific details in opaque values or
encoded variants, with the backend record responsible for interpreting them.

Pros:

- cleaner separation of shared pipeline and backend logic

Cons:

- hard to express cleanly without functors or first-class modules
- likely to require `Obj.magic` at a narrow boundary
- harder to reason about and debug

Recommended direction
---------------------

Prefer Strategy A unless it proves impossible.

Reason:

- it is the most ocaml-light-friendly option
- it avoids module-level abstraction features
- it keeps the compiler typechecked end-to-end
- it localizes target selection to plain values

`Obj.magic` should be avoided unless there is no way to make a specific
boundary typecheck otherwise.

If `Obj.magic` is used, keep it behind one tiny adapter and document it as a
single trusted cast site.

Implementation phases
---------------------

### Phase 1: Make target selection explicit

Add a runtime target flag parser:

- `-i386`
- `-arm`
- `-mips`

Represent the chosen target in a shared config value.

Add a backend descriptor record for the target-specific actions.

Refactor command selection so the runtime can choose:

- assembler
- native C compiler
- linker
- runtime library name

### Phase 2: Stop relying on symlinked backend names

Replace the current “single selected backend copied into shared filenames”
scheme with an explicit backend registry or target dispatch table.

Likely changes:

- introduce target-tagged backend values
- stop depending on a single `asmcomp/arch.ml`
- stop depending on a single `asmcomp/proc.ml`
- make code that needs backend behavior call through the selected backend

### Phase 3: Generalize the IR enough for multiple backends

Refactor `Mach` and `Arch`-dependent types so that i386, arm, and mips can
coexist in one binary.

Likely work:

- unify or generalize `addressing_mode`
- unify or generalize `specific_operation`
- make `Cmm` size queries backend-aware
- reduce direct `open Arch` / `open Proc` dependencies in shared code

### Phase 4: Split backend code into target-specific implementations

Each target needs its own implementation of:

- selection
- reload
- scheduling
- emission
- assembler command selection

For example:

- `Proc_i386`
- `Proc_arm`
- `Proc_mips`

with a runtime-selected `Proc.current`.

If the codebase cannot support multiple module names cleanly, use explicit
target-specific records and keep the generic code calling those records.

### Phase 5: Make `asmrun` multi-target

`asmrun` must stop producing just one `libasmrun.a`.

Need:

- one runtime archive per target
- target-specific object file naming
- target-specific assembly source selection
- target-specific compiler flags
- target-specific assembler flags

Recommended naming:

- `libasmrun-i386.a`
- `libasmrun-arm.a`
- `libasmrun-mips.a`

### Phase 6: Teach `asmlink.ml` to use the selected runtime

Linking must choose the runtime archive based on the selected target.

This includes:

- locating the right archive
- passing the right native C compiler
- passing the right linker
- using the right startup code

### Phase 7: Validate cross-compilation behavior

Test matrix:

- native host build targeting host architecture
- host build selecting a different target at runtime
- each supported target on at least one host

Focus tests on:

- generated assembly correctness
- runtime archive selection
- assembler command invocation
- external C call conventions
- float calling conventions
- stack alignment assumptions

Risks
-----

1. The IR may be too arch-specific to share without becoming messy.
2. `asmrun` may be easier to multi-target than the compiler backend itself.
3. Runtime target switching may expose assumptions that currently live in
   `configure` and `Makefile`.
4. `Obj.magic` can make progress faster but also makes miscompilation risks
   much harder to detect.

Suggested build order
---------------------

1. Add the runtime target flag and backend descriptor.
2. Replace the linker/runtime selection logic.
3. Split runtime archives per target.
4. Refactor backend modules into target-specific records.
5. Generalize the IR only as much as required.
6. Remove symlink-based backend selection.
7. Add regression tests for each target.

Simpler alternative: configure-time cross compiler
--------------------------------------------------

If runtime backend switching proves too invasive, there is a much simpler and
more practical option: keep `ocamlopt` single-target per build, but let
`configure` select the target architecture and cross toolchain explicitly.

This means:

- `configure` accepts a target architecture option
- `configure` accepts target toolchain paths or prefixes
- the build uses one backend only, chosen at configure time
- `ocamlopt` cross-compiles to that one target consistently

Example user-facing options:

- `--target-arch=i386`
- `--target-arch=arm`
- `--target-arch=mips`
- `--target-prefix=arm-linux-gnueabihf-`
- `--target-gcc=...`
- `--target-as=...`
- `--target-ld=...`
- `--target-ar=...`
- `--target-ranlib=...`

What `configure` would derive:

- `ARCH`
- `SYSTEM`
- `NATIVECC`
- `AS`
- `ASPP`
- `LD`
- `AR`
- `RANLIB`
- `NATIVECCCOMPOPTS`
- `ASFLAGS`
- `ASPPFLAGS`
- runtime archive location

Why this is attractive:

- it fits the current one-architecture-at-a-time build model
- it avoids backend duplication inside one compiler binary
- it avoids functors and first-class modules entirely
- it avoids `Obj.magic`
- it can be implemented incrementally

What it still does not solve:

- one `ocamlopt` binary switching targets at runtime
- linking multiple backend implementations into one executable

Recommendation
--------------

Treat the configure-time cross-compiler mode as the practical near-term path.
Keep the runtime-switchable backend design as a longer-term idea only if there
is a strong need for one compiler binary supporting several targets at once.

Bottom line
-----------

This is possible without functors or first-class modules, but only if the
design is kept value-based and mostly explicit.

The most likely successful approach is:

- backend record selected at runtime
- shared generic compiler pipeline
- target-specific backend data and commands
- multi-target runtime archives
- minimal or zero `Obj.magic`
