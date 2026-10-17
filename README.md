# PragmARC—the PragmAda Reusable Components
The PragmAda Reusable Components is a collection of (mostly) useful Ada components. The current version is for Ada 12.
## Documentation
The package names and their corresponding file names should identify the component a package implements. As with all well designed Ada software, the components are documented by their package specifications. If you find a component that is not sufficiently documented in its package specification, please open an issue.
### Hierarchies
A number of types of components are grouped in sub-hierarchies. These are

| Hierarchy name    | Description                                                                                                                   |
| ----------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| Cards             | Utilities for dealing with playing cards. Standard US cards are included                                                      |
| Conversions       | Common conversions                                                                                                            |
| Data Structures   | Common data structures not included in the standard library                                                                   |
| Encryption        | A number of encryption algorithms                                                                                             |
| Matching          | Regular-expression and quick-string matching algorithms                                                                       |
| Randomness        | A number of high-quality random-number generators                                                                             |
| Safety            | Safe versions of low-level constructs such as pointers and concurrency                                                        |
| Skein             | The Skein hash function with three block sizes                                                                                |
| Sorting           | A number of sorting algorithms                                                                                                |
| Splitting         | A general algorithm for splitting sequences into sub-sequences at a separator, with a specific instance for splitting Strings |
| Task Communication| Components for connecting incompatible tasks                                                                                  |
| UBI               | The Skein Unique Block Iteration (UBI) algorithm, for three block sizes                                                       |
| Unbounded Numbers | Unbounded integers and rational numbers ("big numbers")                                                                       |
