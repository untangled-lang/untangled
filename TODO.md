# Todo list

- Types
  - [x] int
  - [x] float
  - [x] string
  - [x] tuples
    - [ ] Make copy of tuple when send message
    - [ ] Destructuring tuple in declaration assignment?
      - [x] AST
      - [ ] semantic type checking
      - [ ] Codegen
  - [ ] arrays
    - [ ] Array index / assignment / access
  - [ ] semaphores

- [x] Conditionals

- Loops
  - [x] while
  - [x] for
  - [x] break/continue

- Variables
  - [x] assignment
  - [x] declarations

- Binop
  - [x] Regular operations on ints/floats, **excluding** power
  - [x] String binary operations
  - [ ] Add / subtract on semaphores
  - [x] Boolean operands && and ||

- Unop
  - [x] ++ and -- on ints and floats
  - [ ] ++ and -- on semaphores
  - [x] Not and Negate

- Functions
  - [X] Regular function calls without passing threads
  - [ ] Passing threads to function calls
  - [ ] Passing an array in arguments

- Threading
  - [x] Message queue
  - [ ] Mutex lock on queue operations
  - [ ] Passing queue to functions
  - [ ] Passing self as message

<!-- - [x] Variable Declaration
  - [ ] Blocks/Scopes
- [ ] Assigning variables
- [ ] Function declarations
- [ ] Loops & conditionals
- [ ] Equality comparison on tuple / thread / array
- [ ] Fix compiler errors on the binops
- [ ] Power operations on ints and floats
- [ ] Multi-thread
  - [ ] Mutex
  - [ ] Some kind of global data structure to facilitate message passing
  - [ ] Semaphores (we have those in C++)
    - [ ] Restrict access so that its atomic
 -->
