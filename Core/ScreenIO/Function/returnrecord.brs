 ! function\returnrecord.brs
 ! Created on 12/18/2014
 !
 ! fnReturnRecord - This function returns the record number of the selected record.
 !
 !
 def fnReturnRecord
    if ExitMode=SelectAndQuit then currentKey$=str$(Rec(Datafile))
 fnend
