# TODO

* [ ] Lens, Prism, Iso
* [ ] apply - unapply like Prism/Scala, not with abort prompt (removes need of stack hacks to preserve TCO)
* [ ] Always go through header links when deref:ing
* [ ] Initialize with ptr::write to avoid UB from dropping uninitialized fields
* GC
    - [ ] Testing (GCBench)
    - [ ] Refactoring
    - -
    - [ ] Lazy sweep
    - [ ] Thread local allocation
    - [ ] Remembered sets
    - [ ] Minor collections
    - [ ] Pinning
    - -
    - [ ] Concurrent collection of old generation
* Compile
    - [ ] Delimited continuation operations, thread contdump through
    - -
    - [ ] Read barriers
    - [ ] Safe points
    - -
    - [ ] Return continuation reification
    - [ ] Instruction selection
    - [ ] Register allocation
    - [ ] Code emission
* [ ] Ensure method bodies are TCO-eligible
* [ ] Reference Interpreter
* [ ] Null == 0x0
