

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
#    op: 'add', 'rm', 'rmr' (rm -r) or 'mv'
#    mvdest: destination directory of op = 'mv'

gitOpPush <- function(fileList,commitComment,op='add',mvdest=NULL) {
   if (!(op %in% c('add','rm','rmr','mv'))) stop('bad op')
   if (op == 'mv' && is.null(mvdest)) stop('mv requires mvdest')
   cmd <- paste('git',op,fileList)
   if (op == 'mv') cmd <- paste(cmd,mvdest)
   ans <- readline(paste('git command OK?', cmd, ' '))
   if (substr(ans,1,1) != 'y') stop('bad command')
   system(cmd)
   system(paste('git commit -m ',commitComment))
   # commit may take a while
   readline('hit Enter when ready for push ')
   gitPush()
}


######################  ghPush  ########################

# push to GitHub, final action; make it a loop in case of password
# mistyping :-); only pushes to 'origin'

gitPush <- function() {
   cmd <- makeSysCmd('git push origin')
   while (TRUE) {
      if (cmd() == 0) return()
   }
}

######################  editPush  #############################

# edit file, then do 'git add' and push

editPush <- function(fname,commitComment) {
   print('make sure commitComment has double quotes within single')
   readline('hit Enter when ready')
   textEditor <- Sys.getenv('EDITOR')
   cmd <- makeSysCmd(textEditor,fname)
   cmd()
   gitOpPush(fname,commitComment)
}

######################  gitCO  #############################

# executes "git log" in shell, and invites user to
# choose some previous commit; if 'master' is TRUE, change to master,
# no invitation to choose other
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
   on.exit(system('git checkout master'))
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
