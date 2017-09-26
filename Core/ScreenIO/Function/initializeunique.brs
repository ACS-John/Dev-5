 ! function\initializeunique.brs
 !
 ! Created on 01/11/2009
 !
 ! This function will initialize any key to unique
 !
 def fnInitializeUnique(mat Subscripts$,mat f$,prefix$,DataFile;___,Subscript)
    let Subscript=fnFindSubscript(mat Subscripts$,prefix$,"id")
    if Subscript then
       let f$(Subscript)=fnMakeUniqueKey$(DataFile)
    end if
 fnend