

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

      
#######################  makeSysCmd #########################
      
# utility function to construct a string containing an R command,
# involving system()
      
# e.g.
#     
# g <- makeSysCmd('ls')  # Mac/Linux command to list files
# g()  # is then same as typing system('ls')

makeSysCmd <- function(...) {
   x <- paste(...)
   f <- function() {
       system(x)
   }
   f
}

