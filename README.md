# gitR

Routines for doing simple GitHub ops from within R, some in combination with
your text editor.

**Example:**
``` r
gitEdPush('x','"test case"') 
```

will open your (shell default) text editor, allow you to edit and save
**x**, then automatically push the file to GitHub -- all in one seamless
operation.  The message, in this case "test case", will be used for the
git commit.

**Example:**

``` r
gitOpPush('x','"not needed anymore"','rm')
```

Removes the file **x**, then does commit and push, with the specified
commit comment.  Other allowed ops are 'add', 'mv' and 'rm -r').

**Example:**

``` r
gitCO()
```

Executes 'git log' (does 'git checkout master' first), then gives user a
choice of which commit, if any, the user wishes to switch to.

**Example:**

``` r
gitCO(master=TRUE)
```

Return to the master version of the repo.

**Example:**

``` r
gitCO(files='rr')
```

Show all commits involving changes to the file **rr**.  

**Example:** 

``` r
gitFindFile('uu','x8')
```

Finds the most recent commit that included the file **uu**, which in turn
included the text 'x8'.  

**Example:**

``` r
gitLS()
```

Reports (separately), tracked and untracked files in the current
directory/commit.

**Example:**

``` r
gitClean()
```

Reports untracked files, and asks user whether to delete.


