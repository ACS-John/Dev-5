 ! screenio\function\enterdoesnothing.brs
 ! Created on 07/28/2016
 !
 ! fnEnterDoesNothing - This function makes it so the Enter Key does nothing on a
 !  listview. Default is to select, we want to supress that action here.
 !
 def fnEnterDoesNothing
    if fkey=0 or fkey=201 then
       fkey(-1)
       exitMode=0
       functionkey=-1
    end if
    if fkey=99 then
       functionKey=-1
       fkey(-1)
       exitMode=QuitOnly
    end if
 fnend
