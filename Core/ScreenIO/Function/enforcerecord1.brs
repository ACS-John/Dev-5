 ! function\enforcerecord1.brs
 ! Created on 08/07/2014
 !
 ! fnEnforceRecord1 - This Function enforces Record 1 for the current
 !  data file. If the file doesn't have a record 1, then a blank record
 !  1 is created
 !
 !  It should be used as the Enter event.
 !
 !
 def fnEnforceRecord1
    ! Set it to read Record 1
    record=1
    
    ! Make sure Record 1 Exists
    restore #Datafile, rec=Record : noRec Ignore
    
    ! If it doesn't, create it.
    if file(DataFile) then
       mat f$=("") : mat f=(0)
       write #DataFile, using form$(DataFile), rec=Record : mat F$, mat F
    end if
 fnend
