# mu1

## Instructions

| Number | Human Readable | Action                                           |
|--------|----------------|--------------------------------------------------|
| 0      | MOV src dst    | Moves value from source to destination.          |
| 1      | ADD src dst    | Adds src to dst (result in dst).                 |
| 2      | SUB src dst    | Subtracts src from dst (result in dst).          |
| 3      | CMP src dst    | Compares source and destination. Sets the C bit. |
| 4      | BEQ offset     | Jump to PC + offset if the C bit is true.        |
| 5      | STOP           | Computer stops.                                  |
| 6      | BNE offset     | Jumps to PC + offset if the C bit is false.      |
