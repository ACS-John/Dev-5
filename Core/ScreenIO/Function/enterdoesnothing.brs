 ! screenio\function\enterdoesnothing.brs
 ! Created on 07/28/2016
 !
 ! fnEnterDoesNothing - This function makes it so the Enter Key does nothing on a
 !  listview. Default is to select, we want to supress that action here.
 !
 def fnEnterDoesNothing
    if fkey=0 or fkey=201 then
       let fkey(-1)
       let ExitMode=0
       let functionkey=-1
    end if
    if fkey=99 then
       let FunctionKey=-1
       let fkey(-1)
       let ExitMode=QuitOnly
    end if
 fnend
