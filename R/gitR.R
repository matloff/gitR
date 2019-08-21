
###################  gitOpPush  ########################

# example

#   gitOpPush('xy z','"new src files"')

# will do 'git add' and push xy and z in current directory,
# with the commit done with the message "new src files"

# arguments:

#    fileList: character string of file names to be git-ted, e.g.
#       'abc de f'
#    commitComment: string to be used with git commit -m; double quotes
#       within single quotes
#    op: 'add', 'rm', 'rm - r' or 'mv'
#    mvdest: destination directory of op = 'mv'
#    remote: 'origin' or...
#    quiet: no splash message with push (may depend on env. var.)
#    acceptEnter: Enter is acceptable response to 'OK?' query

gitOpPush <- function(fileList,commitComment,
      op='add',mvdest=NULL,remote='origin',
      quiet=FALSE,acceptEnter=FALSE) {
   nc <- nchar(commitComment)
   if (substr(commitComment,1,1) != '"' ||
       substr(commitComment,nc,nc) != '"')
          stop('arg 2 must be double quotes within single')
   if (!(op %in% c('add','rm','rm -r','mv'))) stop('bad op')
   if (op == 'mv' && is.null(mvdest)) stop('mv requires mvdest')
   cmd <- paste('git',op,fileList)
   if (op == 'mv') cmd <- paste(cmd,mvdest)
   askOK(paste('git command:',cmd,'  OK?'),acceptEnter=acceptEnter)
   system(cmd)
   system(paste('git commit -m ',commitComment))
   # commit may take a while
   readline('hit Enter when ready for push ')
   gitPush(remote,quiet=quiet)
}


######################  gitPush  ########################

# push to GitHub, final action; make it a loop in case of password
# mistyping :-)

gitPush <- function(remote='origin',quiet=FALSE) {
   if (!quiet) {
     cmd <- makeSysCmd('git push',remote)
   } else cmd <- makeSysCmd('git push -q',remote)
   while (TRUE) {
      if (cmd() == 0) return()
   }
}

######################  gitEdPush  #############################

# edit file, then do 'git add' and push

gitEdPush <- function(fname,commitComment,quiet=FALSE,acceptEnter=FALSE) {
   nc <- nchar(commitComment)
   if (substr(commitComment,1,1) != '"' ||
       substr(commitComment,nc,nc) != '"')
          stop('arg 2 must be double quotes within single')
   openShellEditor(fname)
   gitOpPush(fname,commitComment,quiet=quiet,acceptEnter=acceptEnter)
}

openShellEditor <- function(fname) 
{
   textEditor <- Sys.getenv('EDITOR')
   cmd <- makeSysCmd(textEditor,fname)
   cmd()
}

######################  gitCO  #############################

# executes "git log" in shell, and invites user to
# choose some previous commit; if 'master' is TRUE, change to master,
# no invitation to choose other
# since: Show commits more recent than the specified date
# files: Show commits related to the files choosen. Char vector
gitCO <- function(master=FALSE, files=NULL, since=NULL) {
   system('git checkout master')
   if (master) {
      return()
   }

   opt_file <- if(!is.null(file)) paste('--', paste(files, collapse = ' '))
   opt_since <- if(!is.null(since)) paste0('--since=', since)

   cmd <- paste('git log', opt_since, opt_file)
   glog <- system(cmd, intern=TRUE)

   page(trimws(glog), method = 'print')
   ans <- readline('line number of desired commit (Enter for none): ')
   if (ans != '') {
      num <- as.integer(ans)
      gitCOCommitLine(glog[num])
   }
}

# checkout from line of form "commit xxxxx'
gitCOCommitLine <- function(commitLine) {
   splitline <- strsplit(commitLine,' ')[[1]]
   if (splitline[1] != 'commit') stop('not a commit line')
   commitNum <- splitline[2]
   system(paste('git checkout -q',commitNum))
}

# go to specified commit; 'comm' must be quoted commit number
gitGo <- function(comm='master') {
   system(paste0('git checkout -q ',comm))
}

# wrapper for git status -v
gitSta <- function() system('git status -v')

# determines tracked and untracked files in current dir, usable return value
gitLS <- function() {
   tracked <- system('git ls-files',intern=TRUE)
   untracked <- setdiff(dir(),tracked)
   list(tracked=tracked,untracked=untracked)
}

# shows, then if directed, removes untracked files
gitClean <- function() {
   cat('untracked files:\n')
   unt <- gitLS()$untracked
   print(unt)
   askOK('remove untracked files?')
   unlink(unt)
}

# find the latest commit that contains the specified file or specified
# text in that file
gitFindFile <- function(fn,targetText=NULL)
{
   # for safety if error
   on.exit(system('git checkout -q master'))
   glog <- system('git log',intern=TRUE)
   commits <- grep('commit',glog)
   found <- TRUE
   for (linenum in commits) {
      # checkout that commit
      gitCOCommitLine(glog[linenum])
      fls <- gitLS()
      if (fn %in% fls$tracked) {
         # just want to find the file?
         if (is.null(targetText)) {
            cat('file found in ',glog[linenum],'\n')
            found <- TRUE
            break
         }
         # grep case
         grepOut <- system(paste('grep',targetText,fn),intern=TRUE)
         if (length(grepOut) > 0) {
            cat('target text found in ',glog[linenum],'\n')
            found <- TRUE
            break
         }
      }
   }
   if (!found) {
      print('not found')
   }  else {
      ans <- readline('open in editor? (Enter means no).')
      if (ans == 'y') openShellEditor(fn)
   }
   system('git checkout master')
}

#######################  makeSysCmd #########################

# utility function to construct a string containing an R command,
# involving system()

# e.g.
#
# g <- makeSysCmd('ls')  # Mac/Linux command to list files
# g()  # is then same as typing system('ls')

# rather indirect, but (a) more convenient when have nested quotes and
# (b) good for aliasing with my 'ksREPL' package

makeSysCmd <- function(...) {
   x <- paste(...)
   f <- function() {
       system(x)
   }
   f
}

###################  askOK ##################################

# user prompt for assent

askOK <- function(query,acceptEnter=FALSE) {
   ans <- readline(paste(query,' '))
   if (substr(ans,1,1) == 'y' ||
       acceptEnter && ans == '') return()
   stop('bad yes/no/Enter answer')
}
