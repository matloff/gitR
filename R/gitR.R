

###################  pushToGitHub  ########################

# the 'fileList' argument will be tacked on to 'git add' (or 'git mv' if
# op is 'mv'), with the file names relative to current directory, and
# the git op will be performed 

# example

#   pushToGitHub('xy z','"new src files"')

# will push xy and z in current directory, with the commit done with the
# message "new src files"

# arguments:
 
#    fileList: character vector of file names to be git-ted
#    commitComment: string to be used with git commit -m
#    op: 'add' or 'mv'
#    mvdest: destination directory of op = 'mv'

pushToGitHub <- function(fileList,commitComment,op='add',mvdest=NULL) {
   op1 <- strsplit(op,' ')[[1]][1]
   if (op1 == 'mv') stop('mv not implemented yet')
   if (!(op1 %in% c('add','rm'))) stop('bad op')
   partcmd <- paste('git',op)
   cmd <- paste(partcmd,fileList)
   print(paste('git command OK?', cmd))
   cmd <- makeSysCmd(cmd)
   cmd()
   cmd <- makeSysCmd('git commit -m ',commitComment)
   cmd()
   # commit may take a while
   readline('hit Enter when ready')
   ghPush()
}


######################  ghPush  ########################

# push to GitHub, final action; make it a loop in case of password
# mistyping :-)

ghPush <- function() {
   cmd <- makeSysCmd('git push origin')
   while (TRUE) {
      if (cmd() == 0) return()
   }
}

######################  editPush  #############################

# edit file, then push

editPush <- function(fname,commitComment) {
   print('make sure commitComment has double quotes within single')
   readline('hit Enter when ready')
   textEditor <- Sys.getenv('EDITOR')
   cmd <- makeSysCmd(textEditor,fname)
   cmd()
   pushToGitHub(fname,commitComment)
}

######################  gitCO  #############################

# executes "git log" in shell, and invites user to
# choose some previous commit; if 'master' is TRUE, change to master
gitCO <- function(master=FALSE) {
   if (master) {
      system('git checkout master')
      return()
   }
   glog <- system('git log',intern=TRUE)
   print(glog)
   ans <- readline('desired commit (Enter for none): ')
   if (ans != '') {
      num <- as.integer(ans)
      gitCOCommitLine(glog[num])
   }
}

# checkout from line of form "commit xxxxx'
gitCOCommitLine <- function(commitLine) {
   splitline <- strsplit(commitLine,' ')[[1]]
   commitNum <- splitline[2]
   system(paste('git checkout',commitNum))
}

# simple wrapper, usable return value
gitLS <- function() {
   system('ls',intern=TRUE)
}

# find the latest commit that contains the specified file or specified
# text in that file
gitFindFile <- function(fn,targetText=NULL) 
{
   # for safety if error
   on.exit(system('git checkout master')
   glog <- system('git log',intern=TRUE)
   commits <- grep('commit',glog)
   for (i in commits) {
      # checkout that commit
      gitCOCommitLine(glog[i])
      fls <- gitLS()
      if (fn %in% fls) {
         # just want to find the file?
         if (is.null(targetText)) {
            cat('file found in ',commits[i])
            break
         }
         # grep case
         grepOut <- system(paste('grep',targetText,fn),intern=TRUE)
         if (length(grepOut) > 0) {
            cat('target text found in ',commits[i])
            break
         }
      }
      return()
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

