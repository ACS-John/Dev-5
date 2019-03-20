 ! function\returnselection.brs
 ! Created on 07/13/2009
 !
 ! fnReturnSelection - This Function returns the selected list to the
 !  calling program
 !
 !
 def fnReturnSelection(&CurrentKey$,DataFile;___,Index)
    currentKey$=""
    for Index=1 to udim(mat MarkedRecords$)
       currentKey$=CurrentKey$&fnKey$(DataFile,MarkedRecords$(Index))
    next Index
 fnend
