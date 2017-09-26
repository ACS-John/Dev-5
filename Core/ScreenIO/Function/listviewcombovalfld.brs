 ! function\listviewcombovalfld.brs
 ! Created on 05/31/2014
 !
 ! fnListviewComboValFld - This function doesn't allow user changing
 !  fields when they also clicked on a new record in the listview
 !  indicating that they want to cancel all changes anyway.
 !
 ! #Include {listviewcomboread}
 !
 def fnListviewComboValFld
    if ~ChangedRead then
       let fnListviewComboValFld=1
    end if
 fnend
