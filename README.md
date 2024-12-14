My chosen domain is a company that specializes in assembling custom PCs. It offers a range of components, such as CPUs, GPUs, motherboards, RAM, storage devices, power supplies, and cases. The company provides a wide range of choice from many different brands. Each PC can be uniquely configured according to user preference.

Examples of most common REPL commands:
- Add: adds a specific component to a build.

\> Add CPU Intel i9-14900K 5.6GHz

Component added successfully.

\> Add GPU NVIDIA 4090 24GB

Component added successfully.

- Remove: removes a specific component from a build.

\> Remove CPU Intel i9-14900K 5.6GHz

Component removed successfully.

\> Search GPU NVIDIA RTX 2060 8GB

This component is in the build.

- Compare: compares two components.

\> Compare CPU Intel i9-14900K 5.6GHz CPU AMD Ryzen9 3.7GHz

The first CPU is better.

- ShowBuild: shows the current state of the build.

\> ShowBuild

PC [CPU intel i9 6GHz GPU nvidia 4090 24GB]

An interesting recursion case: a PC made of PCs.

Example:

PC [CPU Intel i9-14900K 5.6GHz GPU NVIDIA 4090 24GB Storage Samsung SSD 1TB PC [CPU AMD Ryzen9 5900X 3.7GHz GPU NVIDIA 2060 8GB Corsair DDR5 16GB 3200MHz]]


Batch syntax:

BEGIN

query1

query2

...

query n

END

Example:

BEGIN

Add CPU intel i9 6GHz

Add GPU nvidia 4090 24GB

END