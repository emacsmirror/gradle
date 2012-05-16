## Overview

gradle-el is a package that makes it easy to work with
[Gradle](http://www.gradle.org/) build automation tool.

It has gradle task discovery and caching support, and integrates with
various project root management libraries.

## Installation
1. download gradle-el package.
1. Either copy `gradle.el` from this package to a directory that is in your `load-path`, or
   modify your `load-path` to include the directory containing `gradle.el` by adding the following to
   your emacs startup file.

        (add-to-list 'load-path (expand-file-name "/path-to/<gradle-el>"))

1. Load gradle-el by adding the following to your emacs startup file:

        (require 'gradle)

## Configuration

There are only a handful of customizable items you can modify. You can
access them by typing `M-x customize-group` and entering `gradle`. The
settings you can modify are:

1. **Gradle Executable**: This is the name of the gradle executable.
   If this file is in your `exec-path`, you don't need to modif it.
   Otherwise, you will need to enter the full path to the gradle executable here.

   Default value is `gradle`.

1. **Gradle Auto Discover Tasks**: If you set this to true, when you execute
   `gradle-run` the first time in a directory, `gradle-el` will automatically run
   gradle to discover the possible tasks you can use and cache the results. This is
   a relatively slow operation, so you may want to disable the automatic discovery by
   toggling this option off. When it is off, you can still cache the tasks by executing
   `gradle-discover-tasks`.

   Default value is `on`.

1. **Gradle Execute in Project Root Function**: This setting can be used to select from a
   variety of project root discovery packages. There are four options for directly supported
   methods. Alternatively, you can provide a function name that will execute a given function
   after changing to the project root.

   The four supported methods are:
   * **Use current directory**: If you select this option, gradle will be executed in the
     same directory as the edited file.
   * **Use eclim**: Discover the project root of edited file using eclim. You would want to
     select this if you are using [emacs-eclim](https://github.com/senny/emacs-eclim) for
     managing your projects.
   * **Use Project-root**: Discover the project root of edited file using `project-root.el`.
     You should select this if you use [project-root.el](http://solovyov.net/project-root/)
     package.
   * **Use eproject**: Discover the project root of edited file using `eproject.el`.
     You should select this if you use [eproject.el](https://github.com/jrockway/eproject)
     package.

   If you are using some other project management package, or if you want to do more complex
   processing, say choosing a package depending on the directory you are in, you can provde
   the name of another function for this item. The function would look something like:

        (defun with-my-project-root (func)
          (let ((default-directory (discover-project-root-using-my-method)))
            (funcall func)))


## Usage

When you need to run gradle, execute `gradle-run`, then enter the
command line options. You can use the `<TAB>` key to complete task
names here if they are cached.

If you want to update the task name cache, you need to execute
`gradle-discover-tasks`.

## Contributing

Feel free to fork [the project on github](https://github.com/vhallac/gradle-el),
make modifications or bugfixes, and issue a pull request.
