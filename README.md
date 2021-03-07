# fnote

## Description

This is a work in progress command-line notebook program for GNU/Linux operating systems. It has been developed in the [Fortran](fortran-lang.org) programming language. At the moment it offers following features:

* Adding a note,
* listing all notes,
* removing a user-specified note, and
* showing usage instructions.

Notes are stored in a "flat" text file, as simple rows of text. When the user enters a new note, the program reads the note text and assigns a numeric identifier (starting from 1) into it. This ID number is used for removing the note from the file.

## Usage

This program has very few features, and as such is simple to use. The following table lists the functions of the program. 

| Command-line switch | Alternate switch | Description | Example |
| --- | --- | --- | --- |
| (empty) | (none) | Brief usage instruction is displayed. | `fnote` |
| `--new <"note">` | `-n <"note">` | Creates a new note. | `fnote --new "My new note."` |
| `--list` | `-l` | Lists all notes. | `fnote --list` |
| `--remove <num>` | `-r <num>` | Removes a note identified by `num`. | `fnote -r 42` |


## Bugs, limitations

1. When creating a new note, the note text must be enclosed by quotation marks. Otherwise only the first word of the note text would get saved.
2. Probably more!

## Possible/planned future features

0. Storing date & time information for notes (creation, deadline, etc...).
1. Storing "finished/unfinished" or "to do/doing/ready" flags might be beneficial for certain types of notes.
2. Use more modern Object-Oriented programming idioms & constructs.
3. Consider using structured text format (XML or JSON with accompanying library) for note storage.

## Building

* Install [FPM](https://github.com/fortran-lang/fpm) (Fortran Package Manager) and copy it into your $PATH as `fpm`.
* Clone the git repository.
* Execute the command `fpm build` inside the base directory of the repository. This will compile and link the Fortran code into an executable program.
* Executable may be run by executing `fpm run --`.

This program was compiled with [GNU Fortran](https://gcc.gnu.org/fortran/) and packaged with [FPM](https://github.com/fortran-lang/fpm).














