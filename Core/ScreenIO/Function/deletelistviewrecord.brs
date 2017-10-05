 ! function\deletelistviewrecord.brs
 !
 !
 !  Created on 01/07/2009
 !
 !  This function deletes the currently selected record on a lisview screen
 !
 !  The last read record of a data file always corresponds with the currently
 !  highlighted record in the listview
 !
 !  This function gives a record description of the last read
 !  record of the data file
 !  It deletes the last read record of the data file
 !  It does not, however, leave the current screen
 !
 !
 def fndeletelistviewrecord(&ExitMode,DataFile,mat f$,&RepopulateListview;___,Recorddescription$*255)
    if udim(f$)>1 then
       recordDescription$=f$(1)
    end if
    if udim(f$)>2 then
       recordDescription$=RecordDescription$&","&f$(2)
    end if
    if 2=msgbox("Are you sure you wish to delete the current record?: "&RecordDescription$,"Delete Record?","yN","EXCL") then
       reread #Datafile, using form$(Datafile) : mat f$, mat f
       delete #DataFile:
       RepopulateListviews=1
    end if
 fnend