# Problem Sheet for Week 1

## Lab Video

We will begin with a short introductory
[video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=63337510-f4dc-4c28-a4cf-af1d00af4c6b).

## The Jupyter Notebook

We will be using Jupyter, a web-based interactive development
environment in order to have a standard platform for all the tasks in
this course.  The School of Computer Science maintains its own
instances of the service which you may connect to
[here](https://jupyter.apps.okd.aws.cs.bham.ac.uk).  Our first problem
set will be concerned with familiarizing yourselves with this
development environment.

## Important: Shutting Down

Time on the server is allocated with a quota.  If you use up all the
time allotted to you, you will no longer be able to access the system.
For this reason, please remember to shut down your server instance
when you are finished with a session.  You may do so by selecting "Log
Out" from the File menu.

## The Terminal

Jupyter includes a Unix terminal system which can be used to create
files and directories, compile source modules and run programs.  If
you are unfamiliar with the Unix terminal, this first exercise will
show you some of the most basic commands.

1. To open the terminal, select "Terminal" from the Jupyter launcher.
You should then see a command prompt with a blinking cursor.
2. The command `pwd` is for "print working directory".  Typing it and
pressing enter will show you your current location in the system.
3. You can create a new directory with the command `mkdir`.  Try,
for example, `mkdir scratch` to create a new scratch directory.
4. You can change directories with the command `cd`.  Try `cd scratch`
to change into the directory you just created.
5. There are many ways to create new files.  A simple one is with the
`touch` command.  Try `touch foo.txt` to create an empty text file.
6. You can list the files in the current directory with the `ls`
command.  Try it now to see the file you create.  For more information
about the files, you can try `ls -l` which will show you, for example, the creator, the size and the date and time of creation.
7. To include *hidden* files in the list (which are files beginning
with a ".") you can use the command `ls -la`. Note that there are two
special hidden files (which are actually hidden *directories*) namely
`.` which means the current directory, and `..` which means the parent
directory.  Try changing to these directories to see what happens.

We will meet more commands in some of the exercises below.

## Cloning the course repository

Git is one of the most popular available *version control systems*.
It is used to keep track of files as they are revised and changed
during the development process.  Your module team uses this system to
update and revise the course materials.


Our first step is to setup access to the course's GitLab repository
from Jupyter.

1. Log on to Jupyter
2. Open a terminal
3. Run the command `setup-git`
4. The script will print out a public key which you must copy by hand
   to the GitLab server.  Visit
   https://git.cs.bham.ac.uk/-/profile/keys 
   and paste the output from the script in the box labelled **Key**.
5. You may wish to give your key a name like "Jupyter Key" in the box labelled **Title**
6. Click `Add Key`.
7. Back in the Jupyter terminal, run the command
```
git clone git@git.cs.bham.ac.uk:fp/learning-2022.git
```
to copy the course repository to your Jupyter instance.

8. When the cloning process has finished, you should see a new folder
   named "learning-2022" in your working directory.
9. Explore the resulting folder with the Unix commands you have learned above.

**Note** If for any reason you need to see the the public ssh key generated
from the above steps, it is stored in the file `/jupyter/work/.ssh/gitlab.pub`.  You can 
view the contents of this file with the command
```
cat /jupyter/work/.ssh/gitlab.pub
```

## Running and Compiling the Game of Life

The source code for the Game of Life program discussed in this week's
lecture can be found in the directory
`learning-2022/files/LectureNotes/Sections`.

1. Let's first create a copy of the file to work with.  To copy a file, we use the `cp` command.  (Note the use of the parent directory `..` in the `cp` command).
```
cd /jupyter/work
mkdir life
cd life
cp ../learning-2022/files/LectureNotes/Sections/Life.hs .
```
Note that the `.` at the end of the final command is crucial.  It tells the `cp` command that the destination folder is the current one.

2. We can run the program directly with the following command
```
runhaskell Life.hs
```
3. Stop the program with `Ctrl-c`.
4. Alternatively, we can compile the source to an executable in order
   to be able to run it directly from the system.  This can be done
   with ghc's "make" option which will take care of compiling, linking,
   as well as including any dependencies.
```
ghc --make Life.hs
```
5. We can run the resulting executable by entering
```
./Life
```

## Modifying the Game of Life

1. To view the source, open the file with the text editor.
2. By default, the program uses the "glider" grid defined at the top of the file.  We
can see this in the line
```
main :: IO ()
main = life glider
```
3. Try switching to another one of the defined grids.
4. What character is being used to represent live cells on the screen?
   Try changing it to a character of your choice.
5. Try adding a grid of your own. Some fun examples can be found on
   the Wikipedia page for the [Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

## Making and Interpreting A Haskell Source module

1. Make a new file named `foo.hs` in a directory of your choice.  You
   may do so with the either the terminal or the file browser.
2. Add the following contents:
```hs
double :: Int -> Int
double x = x + x
```
2. Open a new terminal window.
3. Run `ghci` to start the Haskell interpreter.
4. Load the new file as follows:
```
Prelude> :l foo.hs
[1 of 1] Compiling Main             ( foo.hs, interpreted )
Ok, one module loaded.
```
5. Run the function `double` with your favorite number:
```
*Main> double 6
```

## Inspecting Some Types in Ghci

Haskell is a *strongly typed language* which means that all
expressions in the language are assigned a *type* describing how they
may be used.  We can use the Haskell interpreter `ghci` to find out
the type various expressions.  Inside of `ghci` enter
```
:t True
```
You should see the output 
```
True :: Bool
```
which indicates that the expression `True` is a boolean.

Use the same technique to find the types of the following expressions.

1. `not (not (not False))`
2. `(True,False)` (see Section 3.4 of Programming in Haskell)
3. `['a', 'b', 'x']` (see Section 3.3 of Programming in Haskell)
4. `[(3,4),(4,6)]`
4. `(++)` (What is strange about this type? See "polymorphic functions" discussed later.)

## More with Ghci

If you would like to continue playing with `ghci`, the first chapter
of [Learn You a Haskell for Great
Good!](https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/Resources/LearnYouaHaskell/LearnYouaHaskell.pdf)
contains many more examples which you can try out in order to start
familiarizing yourself with Haskell syntax.
