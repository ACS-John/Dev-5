! r: doNotInsert
library 'S:\Core\Library': fnOpenFile
pr 'this clip is not intended to be compiled directly nor run directly'
pr 'this clip replaces "include: fn_open" when processed with lexi'
end
! /r doNotInsert
! <updateable region: fn_open (supressprompt:=2)>
def fn_open(filename$*255, mat f$, mat fn, mat form$; inputOnly, keyNum, disableEnumSort, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
  dim form$(0)*2048
  dim _fileiosubs$(1)*800
  dim loadedsubs$(1)*32
  fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputOnly, keyNum, disableEnumSort, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
  if ~max(srch(loadedsubs$,uprc$(filename$)),0) then
    mat loadedsubs$(udim(mat loadedsubs$)+1)
    loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
    for index=1 to udim(mat _fileiosubs$)
      execute (_fileiosubs$(index))
    next index
  end if
fnend
! </updateable region: fnopen>