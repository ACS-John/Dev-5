 ! function\listviewcomboread.brs
 ! Created on 05/30/2014
 !
 ! fnListviewComboRead - This function records the last clicked on
 !  item so it can be saved later in a Combo Listview/Add/Edit screen.
 !  it is also necessary to have a listview read event for this type
 !  of screen in order to make the fields update properly, though the
 !  listview read event function doesn't need to do anything for that
 !  effect.
 !
 dim LastReadRecord
 dim ChangedRead
 !
 def fnListviewComboRead
    tTT = LastReadRecord
    tT=ChangedRead
    if LastReadRecord><rec(DataFile) then
       lastReadRecord=rec(Datafile)
       changedRead=1
    end if
 fnend