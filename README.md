# gitR

Routines for doing simple GitHub ops from within R, some in combination with
your text editor.

Example:

> editPush('x','"test case"') 

will open your (shell default) text editor, allow you to edit and save
**x**, then automatically push the file to GitHub -- all in one seamless
operation.  The message, in this case "test case", will be used for the
git commit.


