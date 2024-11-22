# Note - Professional Skills

## Version Control: Overview

- **Version control** is a method of tracking changes to a directory containing subdirectories and files, over time, across many contributors

Version control's advantages:
- It's easy to set up
- A git repository is a full backup of a project and its history
- Very few commands for most day-to-day version control tasks
- GitHub hosting provides a web-based collaboration service

Version Control can be understood as a Directed Acyclic Graph(DAG):
- whose direction is inherited from the flow of time(past $\rarr$ future)
- with no cycles, so future revisions are always distinct
- and a structure with potentially multiple trunks(product versions)

**Branches** are where an iteration points to more than one future iteration.

**Merges** are where an iteration is points to by more than one past iteration.

![](./imgs/Version%20Control%20DAG.png)

In this graph:
- Initial commit **A** updates to **B**,
- Commit **B** produces **C**,
- Commit **C** produces **D**,
- Commit **D** is abandoned,
- The end-deliverable(**F**) has
  - early contributions(**B**)
  - late contributions(**E**), and
  - some from a pull request, (**G**)

Significant `git` diversions from other Version Control
1. `git` stores snapshots of the files in a directory, rather than a base file and accumulated differences
2. Every operation is local-first - the remoteness of a repository is ancillary
3. `git` relies on hashing ensure integrity of the operations - no MITM
4. `git` generally adds data to a repository - the default actions are difficult to make lose information
5. There are three states for files in `git`: modified, staged, and committed; the staging step is where people tend to struggle

## Version Control: Getting Started

Command line git usage
```
git --version
```

Essential `git` commands

- Initializing a repository
    ```
    mkdir -p ./COMP51915
    cd COMP51915/
    git init .
    ```

- Cloning a repository
    ```
    git clone <url-ending-in.git>
    ```

- Adding files to a repository - staging
    ```
    touch file1.c file2.py 
    git add file1.c file2.py
    ```

- Committing to a repository
    ```
    git commit -m "..."
    ```

1. Commit small changes consistently, rather than large changes occassionally
2. Make your changes address one particular issue, rather than several
3. ensure your commit messages are descriptive, on-topic, and conform to the expectations of the project

## Version Control: Maintenance

Branches are divergent development histories of a `git` managed repository

- Creating Branches
    ```
    git branch newbranch
    git checkout newbranch #切换到newbranch
    git checkout main #切换到main
    ```

- Merging
    ```
    git merge newbranch
    ```

> Branching Startegies
> 
> We mentioned that there are a number of branching strategies used for `git`  
> We can not cover all possibilities; some of the more widely used are:
> - long-running branches, discriminated by code stability
> - short-lived branches, discriminated by feature implementation  

- Deleting Branches
```
git branch -d newbranch
```

### Maintenance Summary

1. Use branches liberally with `git` - they're cheap and useful
2. Delete branches after merging them, they'll clog up your labels
3. Developing a branch management strategy is a good idea, but you needn't stick to one
4. Merges can be resolved automatically if they're simple enough, but conflicts require manual resolution
5. Use the other tools to keep a clean working directory - i.e. `gitignore`

## Version Control: collaboration

- Pull Requests
```
git  request-pull v1.0 <project_url> <local_branch_name>
```

- Distributed Workflows  
  Any number of distributed workflows are possible. Some common ones:
  - centralized workflow
    - one central repository hosts the accepted version of the code base, and every developer synchronizes their local version with it to maintain consistency
  - integration-manager
    - one developer is selected to have (read-only) access to everyone’s repository; this developer is then responsible for managing pull requests from everyone’s repositories into the central repository.
  - dictator & lieutenants
    -  for very large projects, you might find the earlier options limiting; in this workflow, segments of the repository are under the stewardship of different lieutenants, whose work is integrated by the integration manager

## Build Systems & Containers

### What is a build system?

A Build System orchestrates multiple programs along with their respective inputs and outputs. Build Systems commonly use the output of a program as the input to other programs.

A build system produces a software artifact from code and data

- Desirable properties of build Systems
  - extensibility
  - maintainability
  - legibility
  - reproducibility

### Getting Started with `make`

The first thing we need to use make is a Makefile

These contain a list of rules following a particular pattern:

```
target: prereqs
    recipe
```

- `target` is an file or command name
- `prereqs` is a list of inputs for `target`
- and `recipe` is a tab-indented command

Recipes are executed by `make`, using prereqs to form target

- Our First Makefile
```Makefile
# make file for app
app : main.o
      g++ -o app main.o
main.o : main.c defs.h
      g++ -c main.c
docs.html : docs.md 
      pandoc -o docs.html docs.md
test : app
      ./app --test
clean :
      rm main.o docs.html
clean-$@ : docs.html
      rm docs.html
```

with this Makefile:
- calling `make` $\rarr$ `main.o` $\rarr$ `app`
- calling `make docs.html` $\rarr$ `pandoc` $\rarr$ `docs.html`
- calling `make test` runs `app --test`
- calling `make clean` deletes `main.o` and `docs.html`

Ignoring existing files and `.PHONY`:
1. the target already exists: or
2. there's no action associated with the target

Marking the rule as `.PHONY`: rule in our Makefile will prevent messages like:  
`make: target is up to date` or  
`make: Nothing to be done for target`  
The typical use-case is `.PHONY: clean - make clean` will always run rule `clean`:

Advanced Makefiles
- Create named variables
- Have implicit build rules
- Group commands by prereq
- Invoke other programs, like `git`

> GNU `make` is an extremely flexible and legible build system - it should be your first stop when assessing build systems for a project

```Makefile
# The final build step.
$(BUILD_DIR)/$(TARGET_EXEC): $(OBJS)
    $(CXX) $(OBJS) -o $@ $(LDFLAGS)

# Build step for C source
$(BUILD_DIR)/%.c.o: %.c
mkdir -p $(dir $@)
    $(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

# Build step for C++ source
$(BUILD_DIR)/%.cpp.o: %.cpp
mkdir -p $(dir $@)
    $(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
```

The syntax can be obscure
- `%`: matches strings
- `$<`: first prereq
- `$@`: target name
- `*`: matches filenames on your filesystem
- `$^`: all prereqs

> The Makefile Cookbook is a great resource for writing a Makefile

Makefile limitations & pitfalls

Your recipe should not be:
- Iterative, or
- Interactive, or
- Output to another program

Implicit Rules & c

`make` can also use implicite rules - recipes which are not explicitly in the Makefile but inferred by the `make` program.

These are mostly related to C compliation and very useful, but difficult to parse

```Makefile
CC = gcc
CFLAGS = -g

blah: blah.o

blah.c:
    echo "int main() { return 0; }" > blah.c
```

Note that `blah.o` is not an output for any rule - it is inferred by `make` implicitly from `blah.c`

### `Cmake`: In Theory & In Practice

`CMake` differs from GNU `make` in two important ways:
1. CMake can act as preprocessor for a build system, e.g. `make`
2. CMake is truly cross-platform, so you only specify the build once.
```
cmake .
cmake --build . # or make
```

Specifying `CMakeLists.txt`

`CMake` is configured by one-or-more `CMakeLists.txt` files

`CMakeLists.txt` contains(at least) three statements:
- `cmake_minimum_required(<version>)`
- `project(<name>)`
- `add_executable(<name> <source file>)`

And may specify: the C++ standard, common source fiels, optional liobraries(and their requirements), system-dependencies, compilation flags, etc...

### Containers & Images

A container is a sandboxed process running an image containing your application code together with all required dependencies.

An image is a file containing an isolated file system, and all files needed to run your application in a container environment

To run a program in a container, we must:
1. Build the application image, and then
2. Run the container.

These are the same steps for the build systems - first specify,then run.

Docker is the de-facto standard - it's unavoidable

Containerization
```Dockerfile
# Use an official base image
FROM gcc:latest

# Set the working directory
WORKDIR /app

# Copy your source code to the container
COPY . /app

# Compile the C++ application
RUN g++ -o myapp main.cpp

# Define the container start command
CMD ["./myapp"]
```

Docker uses a Dockerfile to specify the image build.
- starts FROM base image
- sets directory access
- compiles the application
- defines run command

Deployment with Containers

To build your image:  
`docker build -t my-cpp-app .`

To run your container:  
`docker run -it my-cpp-app`

> Communication is handled by exposing a network port.  
> Dual server-client applications can be packaged together

Limitations of Containers
- Containers promise a consistent environment for the development and deployment of an application - write once, run anywhere.
- Containers are lighter than a virtual machine, but not as efficient as an optimized application running on the OS.
- Containers are siloed and may be difficult to have interact with the rest of the system effectively.
- Containers are still not ubiquitous on clusters, especially University ones, so you may need to still engage with bespoke build environs.

## Code Analysis & Continous integration

### Unit Testing

Unit Testing is a dynamic code analysis process which identifies issues by restricting the attention of a test to a small unit of code

You should begin writing unit tests early in development - tests can serve as a demonstration of code usage, i.e. they're self-documenting.

Your test should begin by codifying expectations of the correct path through the codebase - once we've exhausted our expectations, we should consider how to handle unexpected situations

Unit tests are sufficient when there's no more capacity to surprise

You are not testing the language constrcts, just your productive usage

**Unit Testing vs Testing Frameworks**

The former is a skill, the latter is a matter of habit

### Code Linting

Linting is a static code analysis process which identifies issues in the source code defining a program, like bugs and stylistic issues

### Continuous Integration(CI)

Making large significant changes to a code base is difficult ... so instead you should make small, incremental, changes
