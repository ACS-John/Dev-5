 ! function\deleterecord.brs
 !
 !
 ! Created on 01/07/2009
 !
 !  This function deletes the currently edited record on an add/edit screen
 !
 !  The last read record of a data file always corresponds with the current
 !  record on the screen in an add/edit screen
 !
 !  This function asks the user for confirmation
 !  This function deletes the last read record of the data file
 !  Finally, this function instructs screenio to leave the current screen
 !   on an Edit Screen, if we delete a record, we also want to stop editing it.
 !
 !
 def fndeleterecord(&ExitMode,DataFile,Key$)
    if 2=msgbox("Are you sure you wish to delete the current record?","Delete Record?","yN","EXCL") then
       if len(trim$(key$)) then
          delete #DataFile:
       end if
       exitMode=QuitOnly
    end if
 fnend