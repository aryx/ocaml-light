Native arm64 macOS (Mach-O) support
====================================

Context
-------

Building ocaml-light on an Apple Silicon Mac (aarch64-apple-darwin, Apple
clang / Xcode CLT). `./configure` already handles Darwin fine for the
*bytecode* compiler (there's already a `gcc,*-*-darwin*` case for
bytecccompopts), but the native-code arch-detection block never had a Darwin
case at all:

- configure's `case "$host" in` for native-code arch has no `*-*-darwin*`
  entry, so any 64-bit host without an explicit -target-arch falls through
  to arch=none ("native-code compiler not supported"). Confirmed on this
  machine: a prior ./configure run left config/Makefile with
  ARCH=none SYSTEM=unknown.
- Even `-target-arch arm64` always sets system=linux_elf and points at the
  aarch64-linux-gnu-* cross toolchain -- there is no Mach-O path anywhere
  in the tree yet.

Goal: make ./configure (no args, run natively on this Mac) produce a working
ocamlopt that emits real Mach-O arm64 executables, using the existing Apple
clang toolchain (/usr/bin/gcc -> clang, /usr/bin/as -> clang integrated
assembler) -- no -target-arch cross-compilation involved, this is a native
build.

Key finding that shrinks the scope
-----------------------------------

asmcomp/arm64/emit.mlp and asmrun/arm64.S already only ever do *direct*
adrp+add/:lo12: symbol addressing (never GOT-indirect) -- the existing
comment in emit_load_symbol_addr explains this fork never builds PIC/dlcode,
every target always links statically. Every symbol referenced from
OCaml-generated code or from asmrun/arm64.S is either another OCaml module
symbol or one of our own runtime C functions (caml_alloc, caml_call_gc,
caml_c_call, primitive stubs, ...) -- all statically linked into the same
Mach-O image. adrp+:lo12: is inherently PC-relative, so it stays correct
under macOS's mandatory PIE without needing any GOT/@GOTPAGE machinery -- we
only need to stop *statically linking libSystem* (impossible on macOS; drop
-static) and switch two things that Mach-O really does need:

1. Leading underscore on every symbol (Mach-O C-ABI convention -- clang
   auto-adds it when compiling byterun/*.c/asmrun/*.c, so our hand-emitted
   assembly must match it on both definition and reference sides).
2. No .type/.size ELF directives (Mach-O assembler doesn't have them).

This exact pattern (symbol_prefix keyed on Config.system) already exists as
precedent in asmcomp/i386/emit.mlp:45-54 -- its non-ELF/non-Solaris default
branch ("_" prefix) *is* the historical macOS/BSD case, just never wired up
to a real host. Reusing the same Config.system = "macosx" string.

.align semantics, .L local labels, and .quad/.short/.byte/.double/.space/
.ascii/.globl directives are all identical between GNU as (ELF arm64) and
LLVM's integrated assembler (Mach-O arm64) -- no changes needed there,
unlike i386's ELF-vs-Solaris split.

Changes
-------

1. configure

   - In the native-code `case "$host" in` block, add:
       aarch64-*-darwin*)   arch=arm64; system=macosx;;
     This only fires for a *native* Darwin arm64 host (no -target-arch
     needed -- target_arch stays "" and its case block no-ops, preserving
     arch/system).

   - In the `case "$arch,$nativecc,$system" in` block (where arm64,*,*)
     currently sets nativecclinkopts='-static' for the Linux cross target),
     add a more specific case before it:
       arm64,*,macosx)
           nativecccompopts='-Wall --std=gnu89'
           nativecclinkopts=''
           ;;
     macOS never supports -static (no static libSystem) and mandates PIE
     for arm64 executables; adrp+:lo12: stays correct under PIE since every
     symbol we reference is resolved at static-link time within this same
     Mach-O image.
     (nativecc itself needs no new case -- the existing
     `*) nativecc="$bytecc";;` fallback already gives nativecc="gcc", i.e.
     Apple clang.)

   - In the `case "$arch,$model,$system" in` block for as/aspp (where
     arm64,*,*) currently points at aarch64-linux-gnu-as/gcc), add before
     it:
       arm64,*,macosx)
           aspp='gcc'
           asppflags='-c -DSYS_$(SYSTEM)'
           ;;
     (leave `as` unset -- matches the i386 native case, so $(AS) falls back
     to make's default `as`, i.e. the host's own clang-based assembler,
     used by Proc.assemble_file for plain non-preprocessed .s files.)

   - Optional doc touch: add SYSTEM=macosx to the descriptive comment block
     in config/Makefile-templ next to the existing nextstep entry -- purely
     a comment, not consulted by configure.

2. asmcomp/arm64/emit.mlp

   - Replace emit_symbol/emit_load_symbol_addr's implicit no-prefix
     assumption with a Config.system-keyed prefix, mirroring i386:
       let symbol_prefix =
         match Config.system with
           "macosx" -> "_"
         | _ -> ""

       let emit_symbol s =
         emit_string symbol_prefix; Emitaux.emit_symbol '$' s
   - Guard the two ELF-only directives in fundecl (.type/.size): emit only
     when Config.system <> "macosx".
   - Add a short comment on emit_load_symbol_addr noting the Darwin PIE
     reasoning (why direct adrp+add stays correct there too).
   - (Nice-to-have, low risk) emit .subsections_via_symbols at the end of
     end_assembly when Config.system = "macosx" -- standard Mach-O
     convention for symbol-granular dead-stripping.

3. asmrun/arm64.S

   Hand-written runtime assembly, preprocessed by cpp with -DSYS_$(SYSTEM)
   already wired (asmrun/Makefile's FLAGS line), so #ifdef SYS_macosx is
   available for free. Need:

   - A symbol macro next to the existing ADDRGLOBAL/LOADGLOBAL/STOREGLOBAL
     macros:
       #ifdef SYS_macosx
       #define SYM(x) _##x
       #else
       #define SYM(x) x
       #endif
     and route ADDRGLOBAL/LOADGLOBAL/STOREGLOBAL's symb argument through
     SYM(symb) internally (covers most uses in one place).
   - Apply SYM(...) to the remaining bare symbol sites: every `.globl X` /
     `X:` label pair (caml_call_gc, caml_alloc1/2/3, caml_alloc,
     caml_c_call, caml_start_program, raise_caml_exception, callback,
     callback2, callback3, system_frametable) and the two bare `bl`
     targets (garbage_collection, mlraise) -- `bl .Lxxx` local-label calls
     stay untouched.
   - Drop the .type/.size pairs (17 occurrences) under SYS_macosx, same
     approach as asmrun/i386.S's existing #if defined(SYS_linux_elf) /
     sparc.S's #if defined(SYS_sunos) || defined(SYS_bsd) precedent --
     simplest as a small FUNCTION_TYPE(name)/FUNCTION_SIZE(name) macro
     pair that expands to nothing under SYS_macosx, replacing each
     `.type name, %function` / `.size name, .-name` line.

4. changes.txt

   Add an entry once the build is verified, matching the existing style
   seen in git log (`changes.txt: added arm64` etc).

Known edge case (not blocking, flag if it surfaces)
-----------------------------------------------------

Apple's arm64 C ABI passes *variadic* arguments differently from standard
AAPCS64 (all on the stack). Iextcall/caml_c_call marshals arguments using
the fork's normal (non-variadic) calling convention, so this only matters
if some `external` primitive's C stub itself calls a *variadic* libc
function expecting arm64-Darwin-ABI variadic marshalling -- that's inside
our own C code (compiled by clang, which gets it right automatically), not
something emit.mlp/arm64.S touch. Expected to be a non-issue; call out
during testing if any primitive misbehaves specifically on this platform.

Verification
------------

All done for real on this machine (we have the actual hardware):

1. ./configure (no args) -> confirm config/Makefile has
   ARCH=arm64 SYSTEM=macosx NATIVECC=gcc ASPP=gcc and
   NATIVECCLINKOPTS= (no -static).
2. make coldstart && make world -- bytecode toolchain (should already work,
   just confirming no regression).
3. make opt -- builds ocamlopt and the opt-compiled stdlib; this is where
   emit.mlp/arm64.S changes get exercised. Iterate on any assembler
   (clang -c) or linker (ld64, via Asmlink.call_linker) errors -- expect
   some back-and-forth here (unresolved/duplicate symbols, directive
   rejections) given this is untested-on-real-hardware code.
4. Compile and run a couple of small native test programs by hand (e.g.
   something under test/) via the freshly built ocamlopt, to check
   function calls, GC/allocation, and exceptions actually execute
   correctly (not just "linked without error").
5. make ocamlc.opt && make ocamlopt.opt -- native-compiled compiler
   self-hosting itself is a strong correctness signal.
6. Time permitting, make test (full suite, includes testasmcomp).
