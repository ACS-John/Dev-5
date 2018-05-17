 ! function\listviewcombopost.brs
 ! Created on 05/30/2014
 !
 ! fnListviewComboPost - This function makes sure the correct item
 !  has been read and the listview read is run properly when its
 !  repopulated
 !
 ! #Include {listviewcomboread}
 !
 def fnListviewComboPost
    if len(trim$(CurrentKey$)) then
       read #DataFile, using form$(DataFile), key=CurrentKey$ : mat F$, mat F
    else
       read #DataFile, using form$(DataFile), first : mat F$, mat F
    end if
    fnListviewComboRead
 fnend
