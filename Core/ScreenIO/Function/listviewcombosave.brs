 ! function\listviewcombosave.brs
 ! Created on 05/30/2014
 !
 ! fnListviewComboSave - This function saves the current edits into the
 !  previously recorded last record read, and triggers the redisplaying
 !  of the listview.
 !
 ! #Include {listviewcomboread}
 !
 dim X$(1)*1023,X(1)
 !
 def fnListviewComboSave
    if LastReadRecord then
       mat X$(udim(mat F$))
       mat X(udim(mat F))
       read #DataFile, using form$(DataFile), rec=LastReadRecord : mat X$, mat X
       rewrite #DataFile, using form$(DataFile) : mat F$, mat F
       currentRec=LastRecordRead
    else
       write #DataFile, using form$(DataFile) : mat F$, mat F
       currentRec=LastReadRecord=rec(DataFile)
    end if

    repopulateListviews=1
 fnend
