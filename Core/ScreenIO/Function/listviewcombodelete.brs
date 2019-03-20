 ! function\listviewcombodelete.brs
 ! Created on 05/30/2014
 !
 ! fnListviewComboDelete - This function deletes the currently selected
 !  line in the listview, and triggers the redisplay of the listview.
 !
 ! #Include {listviewcomboread}
 !
 def fnListviewComboDelete
    if LastReadRecord then
       if (2==msgbox("Are you sure you want to delete this record from the list?","Are you sure?","yN","QST")) then
          mat X$(udim(mat F$))
          mat X(udim(mat F))
          read #DataFile, using form$(DataFile), rec=LastReadRecord : mat X$, mat X
          delete #DataFile:
          repopulateListviews=1
          currentRec=-1
       end if
    else
       ! We're in add. Nothing to delete.
       
       ! Instead, cancel the add.
    
    
    end if
 fnend
