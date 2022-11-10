export const help = (env, fs, ...args) => {
  return {
    streams: {
      stdout: `
Commands
========
* help - this help
* ls [directory]* - list all children of each directory
* tree [directory]* - list all children of each directory recursively
* cat [file]* - concatenate files and print on the standard output
* cd [directory] - change the current working directory
* echo [string]* - display a line of text
* ed [file] - edit a file
* touch [file] - create a file
* mkdir [directory] - create a directory
* rm [file]* - remove files or directories (no -rf required)
* clear - clear the screen
* trongle [min] [max] - generate a random number of trongles between min and max

Other
=====
* Wrap any multi-word or special-character-containing arguments in quotes
* Use "*" to match children of a path (no special regex here - you get ALL children)
* Use "." to match the current directory and ".." to match the parent directory
* Set environment variables with "VAR=value" and reference them with "$VAR"
* Use the "up" and "down" arrow keys to navigate through your command history

Environment
==========
* PS1 - the prompt string - "echo $PS1" to find its format
* PWD - the current working directory
* USER - the current user
* HOME - the home directory
* PATH - the path to search for executables
* HOSTNAME - the hostname
`,
    },
  };
};
